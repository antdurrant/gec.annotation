
#' Extract tokenized dataframe from pdf
#'
#' takes a readable digitally produced pdf file and returns the equivalent reuslt of `gcv_reorder_text`
#'
#' @param pdf_file the file to be read
#'
#' @return data.frame of tokens and locations
#' @export
#'
poppler_reorder_text <- function(pdf_file){

    x_right <- x_left <- x <- y <- y_top <- y_bottom <-
        text <- data <- width <- height <-
        NULL



    adjust_size <- round(max(pdftools::pdf_pagesize(pdf_file)$"bottom"))/100



    # pdf text with xy coords
    digital_text_df <-
        tibble::tibble(data = pdftools::pdf_data(pdf_file)) %>%
        dplyr::mutate(page = dplyr::row_number()) %>%
        tidyr::unnest(data) %>%
        # one item per page - this is only one page, so a simple example
        #purrr::pluck(1) %>%
        # prep for annotation - put it above the text
        dplyr::mutate(
            x_right = x+width,
            x_left = x,
            y_top = y,
            y_bottom = y+height,
            x = (x_right + x_left)/2,
            y = (y_top + y_bottom)/2,
            rem_coords = paste0("+", x_left, "+", y_top),
            rep_coords = paste0("+", x_left, "+", y_top-adjust_size)) %>%
        dplyr::group_by(y) %>%
        dplyr::mutate(y_order = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            # spacy always annotates cannot as two words
            text =
                dplyr::if_else(text == "cannot", "can not", text) %>%
                tokenizers::tokenize_words(lowercase = FALSE, strip_punct = FALSE)) %>%
        tidyr::unnest(text) %>%
        # it also tokenizes on '
        dplyr::mutate(test = tokenizers::tokenize_regex(text, pattern = "(?=['|’])")) %>%
        tidyr::unnest(test) %>%
        dplyr::mutate(order_id = dplyr::row_number())

    digital_text_df
}


tibble::tibble(text = c("today's", "that'll")) %>%
    dplyr::mutate(new = tokenizers::tokenize_regex(text, pattern = "(?=')")) %>%
    tidyr::unnest(new)
