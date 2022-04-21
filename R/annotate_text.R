#' annotate digital text
#'
#' Takes a character vector and runs grammar error correction on it.
#' Requires `gec_env` to be used, spacy to be instantiated, and sentence.py to have been sourced.
#' Post-grammar-error correction, a combination of word2vec and hunspell are used to (hopefully)
#' intelligently add some spelling correction as well.
#'
#' @param digital_text character vector
#' @param mod word2vec model (as produced by `word2vec::word2vec(word2vec::txt_clean_word2vec({your texts}))`)
#'
#' @return a tibble of sentences with columns: `doc_id`, `original`, `correction_diff_standard` (standard [--] & {++} notation), `changes` (html `ins` and `del` tags)
#' @export
#'
#' @examples
#' #annotate_text("Ma, does ye have any potatos?")
annotate_text <- function(digital_text, mod){

    original <- pos <- token <- is_word <- . <-
        correction_diff_standard <-
        NULL

    usethis::ui_info("Running grammar error correction model")
    # calling out to the python GEC model
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
                stringr::str_squish() %>%
                tidyr::replace_na("This sentence has problems that model cannot understand."),
            doc_id = dplyr::row_number()

        )

    # embeddings
    emb <- as.matrix(mod)
    doc_mod <- word2vec::doc2vec(mod, digital_text)

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
        dplyr::filter(!hunspell::hunspell_check(token))

    # only get suggestions if there are any words
    if(nrow(find_spelling_mistakes) > 0){
        find_spelling_mistakes <-
            find_spelling_mistakes %>%
            dplyr::mutate(suggestion = hunspell::hunspell_suggest(token)) %>%
            tidyr::unnest(suggestion) %>%
            tidytext::unnest_tokens(output = "word", input = suggestion, drop = FALSE) %>%
            dplyr::group_by(token, suggestion) %>%
            dplyr::filter(all(word %in% rownames(emb)))
    }

    # only do predictions if there are any possible ones
    if(nrow(find_spelling_mistakes) > 0){
        find_spelling_mistakes <-
            find_spelling_mistakes %>%
            dplyr::mutate(pred = purrr::map(tolower(word), ~stats::predict(mod, .x, type = "embedding"))) %>%
            dplyr::mutate(similarity = purrr::map(pred, ~word2vec::word2vec_similarity(doc_mod, .x, top_n = 1))) %>%
            tidyr::unnest(similarity) %>%
            dplyr::group_by(token, suggestion) %>%
            dplyr::mutate(token = paste0("\\b", token, "\\b")) %>%
            dplyr::summarise(simil = mean(similarity)) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(token) %>%
            dplyr::filter(simil == max(simil)) %>%
            dplyr::filter(row_number() == 1) %>%
            dplyr::ungroup()
    }

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

