#' Prepare annotations for imagemagick
#'
#' Take the corrected text and the tokenized df with locations, and find where to put
#' what kind of annotation
#' Gives a warning when row counts do not quite match up (off-by-one or more errors are present)
#'
#' @param annotated_text output of `annotate_text`
#' @param digital_text_df output of `gcv_reorder_text` or `poppler_reorder_text`
#'
#' @return data.frame with annotation data
#' @export
#'
prep_annotations <- function(annotated_text, digital_text_df){


    doc_id <- original <- token <- order_id <- text <- correction <- common_tokens <-
        common <- common_id <- common_id_use <- end <- start <- reset <- add <-
        corrected <- replacement <- location <- colour <- y_order <-
        doc_common_id_cond <- doc_cond <- corrected_cond <- rem_rep <-
        NULL

    adjust_size <- (max(digital_text_df$y_bottom) - min(digital_text_df$y_top)) /75


    # tokenization troubles ----
    # spacy parses 's/n't as a participle.
    # tokenization of periods also seems to cause a bunch of trouble
    # it looks like google parses that as one token
    # this causes off-by-one errors downstream because we are
    # relying on order_id
    ref <-
        annotated_text %>%
        dplyr::mutate(doc_id = dplyr::row_number()) %>%
        dplyr::select(doc_id, text = original) %>%
        spacyr::spacy_parse(pos = FALSE, tag = FALSE, lemma = FALSE, entity = FALSE) %>%
        # there may need to be more cases added here as/when discovered
        dplyr::ungroup() %>%
        dplyr::mutate(text =
                          dplyr::case_when(
                              dplyr::lead(token) == "'s" ~ paste0(token, "'s"),
                              dplyr::lead(token) == "n't" ~ paste0(token, "n't"),
                              dplyr::lead(token) == "'m" ~ paste0(token, "'m"),
                              # dplyr::lead(token) == "." ~ paste0(token, "."),
                              # dplyr::lead(token) == "," ~ paste0(token, ","),
                              TRUE ~ token)
        ) %>%
        # spacy forces doc_id to character so ordering get out of whack
        dplyr::mutate(doc_id = readr::parse_number(doc_id)) %>%
        dplyr::filter(!token %in% c("n't", "'s",  "'m")) %>% #, ",", ".")) %>%
        dplyr::mutate(order_id = dplyr::row_number()) %>%
        dplyr::select(doc_id, order_id, text = token)


    # which tokens are the same in both the original and corrected text ----
    common_tokens_ref <-
        ref %>%
        dplyr::select(-order_id) %>%
        dplyr::group_by(doc_id) %>%
        dplyr::summarise(common_tokens = list(text))

    # what annotations should be made ----
    prep_annotation <-
        common_tokens_ref %>%
        dplyr::bind_cols(
            dplyr::tibble(common = annotated_text$original,
                          # just occassionally there is no space between removal and addition
                          correction = annotated_text$correction_diff_standard %>%
                              stringr::str_replace_all("\\]\\{", "] {") %>% stringr::str_split(" |(?=')"))
        ) %>%
        tidyr::unnest(correction) %>%
        dplyr::mutate(token =
                          correction %>%
                          stringr::str_remove_all("\\[-") %>%
                          stringr::str_remove_all("-\\]") %>%
                          stringr::str_remove_all("\\{\\+") %>%
                          stringr::str_remove_all("\\+\\}") %>%
                          # because the tokenization is fine-grained compared to digitally scraped pdf
                          stringr::str_remove("'s$") %>%
                          stringr::str_remove_all("[:punct:]")
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(common_id = list(which(token ==  common_tokens))) %>%
        dplyr::filter(nchar(correction) > 0) %>%
        dplyr::ungroup()  %>%
        dplyr::group_by(common, token) %>%
        # sort out which occurrence it is if the same word appears more than once in the sentence
        dplyr::mutate(common_id_use = dplyr::row_number()) %>%
        dplyr::rowwise() %>%
        # if the correction is _just_ an addition, but the word appears elsewhere in the sentence
        # will occur fairly often with missed pronouns/articles etc.
        dplyr::mutate(common_id =
                          dplyr::if_else(stringr::str_detect(correction, "\\{\\+.+\\+\\}"),
                                         NA_integer_,
                                         common_id[common_id_use]
                          )) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            start = cumsum(stringr::str_detect(correction, "\\{\\+|\\[-")),
            end = cumsum(stringr::str_detect(correction, "\\+\\}|-\\]")),
            reset = dplyr::lag(end, 1, default = -1),
            add = dplyr::if_else(start == 0 & end == 0, 0, start - reset)) %>%
        tidyr::fill(common_id, .direction = "down") %>%
        dplyr::group_by(doc_id, reset, add) %>%
        dplyr::mutate(corrected = paste(correction, collapse = " ")) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(check = stringr::word(corrected, 1)) %>%
        dplyr::filter(stringr::str_detect(corrected, "\\[-|\\{+")) %>%
        dplyr::select(doc_id, common, corrected, correct_from = common_id)  %>%
        dplyr::distinct(doc_id, corrected, common,  .keep_all = TRUE)

    if(nrow(digital_text_df) != nrow(ref)){
        warning("you have some alignment errors")
    }
    #  actual specifics of the annotations ----
    annotations <-
        digital_text_df %>%
        dplyr::left_join(ref) %>%
        dplyr::left_join(common_tokens_ref) %>%
        dplyr::rowwise() %>%
        dplyr::mutate( common_id = list(which(text ==  common_tokens))) %>%
        dplyr::ungroup() %>%
        # common id is now all of the tokens that match per doc id
        dplyr::group_by(doc_id, text) %>%
        dplyr::mutate(common_id_use = dplyr::row_number()) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(common_id = common_id[common_id_use]) %>%
        dplyr::ungroup() %>%
        tidyr::fill(common_id, .direction = "down") %>%
        dplyr::select(-common_id_use) %>%
        dplyr::inner_join(prep_annotation, by = c("doc_id", "common_id" = "correct_from")) %>%
        dplyr::mutate(rem_rep =
                          dplyr::case_when(
                              stringr::str_detect(corrected, "\\[-") ~ "rem",
                              stringr::str_detect(corrected, "\\{\\+") ~ "rep"),
                      replacement = dplyr::case_when(
                          rem_rep == "rem" ~ stringr::str_remove_all(corrected, "\\[|\\]") %>% stringr::str_replace_all(".", "-"),
                          rem_rep == "rep" ~ stringr::str_remove_all(corrected, "\\{\\+|\\+\\}")
                      ),
                      location = dplyr::case_when(rem_rep == "rem" ~ rem_coords,
                                                  rem_rep == "rep" ~ rep_coords
                      ),
                      colour = dplyr::case_when(rem_rep == "rem" ~ "red",
                                                rem_rep == "rep" ~ "green"
                      )
        ) %>%
        dplyr::distinct(replacement, location, colour, .keep_all = TRUE) %>%
        #
        # when there is just an addition,
        # no removal, put it at the _end_ of the word it should come after
        # and add a ^ mark to show where it should go
        dplyr::mutate(
            doc_common_id_cond = (dplyr::lag(doc_id) == doc_id & dplyr::lag(common_id) != common_id) %>% tidyr::replace_na(FALSE), # different common ids
            doc_cond = (dplyr::lag(doc_id) != doc_id) %>% tidyr::replace_na(FALSE), # different sentences
            corrected_cond = !stringr::str_detect(dplyr::lag(corrected), "\\[\\-") %>% tidyr::replace_na(FALSE)) %>%
        # gotta be the y_top for _that_ line
        dplyr::group_by(y_order) %>%
        dplyr::mutate(
            check = dplyr::if_else(
                condition = (doc_common_id_cond | doc_cond) & (corrected_cond | doc_cond) & rem_rep == "rep",
                true = glue::glue("+{x_right}+{min(y_top)-adjust_size}"),
                false = location
            )) %>%
        dplyr::ungroup()



    annotations
}
