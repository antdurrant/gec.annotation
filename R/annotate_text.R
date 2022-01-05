#' annotate digital text
#'
#' takes a character vector and runs grammar error correction on it
#' requires `gec_env` to be used, spacy to be instantiated, and sentence.py to have been sourced
#'
#' @param digital_text character vector
#'
#' @return a tibble of sentences with columns: `doc_id`, `original`, `correction_diff_standard` (standard [--] & {++} notation), `changes` (html `ins` and `del` tags)
#' @export
#'
#' @examples
#' #annotate_text("Ma, does ye have any potatos?")
annotate_text <- function(digital_text){


    original <- pos <- token <- is_word <- . <-
        correction_diff_standard <-
        NULL

    usethis::ui_info("Running grammar error correction model")
    # calling out to the python GEC model
    # putting both original sentences and corrected sentences in tempfiles so
    # we can do wdiffs on them
    text <-
        tibble::tibble(text = digital_text) %>%
        tidytext::unnest_sentences(
            output = "original",
            input = text,
            strip_punct = FALSE,
            to_lower = FALSE
        ) %>%
        # we know this model doesn't know what to do with quotation marks
        # and it wants to put spaces between numbers and letters
        # can't do much about the non-fancy ones, but we can at least fix the fancy ones
        # plus most kids will write in word, and it will automatically use them
        dplyr::mutate(
            text = purrr::map_chr(original, ~correct_sentence(.x)) %>%
                stringr::str_remove_all("(?<=\\d)\\s(?=th\\b|st\\b|nd\\b|rd\\b)") %>%
                stringr::str_replace_all("\u201D(?=[:alpha:])", " \u201C") %>%
                stringr::str_replace_all("(?<=[:alpha:])\u201C", "\u201D ") %>%
                stringr::str_replace_all("(?<=[:alpha:][\\.|\\,])(?=\\d)", " ") %>%
                stringr::str_replace_all("\u201C ", " \u201C") %>%
                stringr::str_replace_all(" \u201D", "\u201D ") %>%
                stringr::str_replace_all("\u201D \\.", "\u201D.") %>%
                stringr::str_replace_all("\u201D ,", "\u201D,") %>%
                stringr::str_replace_all("ES (?=\\d)", "ES") %>%
                # handle
                stringr::str_replace_all("s['|\u2019](?!re\\b|ll\\b|ve\\b|\\b,|\\.|\\:|\\;|-)", "s' ") %>%
                stringr::str_squish(),
            text = replace_na(text, "sentence has problems that model cannot understand"),
            doc_id = dplyr::row_number()

        )

    # find them ...
    find_spelling_mistakes <-
        spacyr::spacy_parse(
            text,
            lemma = FALSE,
            dependency = FALSE,
            entity = FALSE
        ) %>%
        # pos that will likely have out-of-vocabulary terms
        dplyr::filter(!pos %in% c("X", "SYM", "PRON", "PART",  "PROPN", "ADP", "PUNCT", "NUM"),
                      # characters that need escaping cause errors
                      !stringr::str_detect(token, "[:punct:]|\\+"),
                      # single letter spelling mistakes are probably wrong
                      nchar(token) > 1) %>%
        dplyr::mutate(is_word = hunspell::hunspell_check(token)) %>%
        dplyr::filter(!is_word) %>%
        dplyr::mutate(suggestion = purrr::map_chr(token, ~unlist(hunspell::hunspell_suggest(.x))[1]))

    usethis::ui_info("Checking spelling")
    # ... replace them
    if(nrow(find_spelling_mistakes) > 0){

        two_step_correction <-

            text %>%
            dplyr::mutate(text =
                              text %>%
                              purrr::reduce2(find_spelling_mistakes$token,
                                             find_spelling_mistakes$suggestion,
                                             ~..1 %>%
                                                 stringr::str_replace(
                                                     pattern = ..2,
                                                     replacement = ..3),
                                             .init = .)
            )



    } else{
        two_step_correction <- text
    }

    # --- diffs ----
    output <-
        two_step_correction %>%
        dplyr::mutate(correction_diff_standard = wdiff(original, text, " "),
                      changes =
                          correction_diff_standard %>%
                          stringr::str_replace_all("\\[-", "<del>") %>%
                          stringr::str_replace_all("-\\]", "</del>") %>%
                          stringr::str_replace_all("\\{\\+", "<ins>") %>%
                          stringr::str_replace_all("\\+\\}", "</ins>")) %>%
        dplyr::rename(corrected = text)
    usethis::ui_done("Complete!")
    output
}

