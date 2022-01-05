#' get returned text in (probably) the correct order
#'
#' - removes likely duplicates
#' - orders the text in logical lines, allowing for some amount of skew
#' - standardizes the tokenization as far as possible
#'
#' the order_id this produces is used in downstream tasks
#'
#' @param data the result of `gcv_xy`
#'
#' @return a reordered tibble with additional information
#' @export
#'
#' @examples
gcv_reorder_text <- function(data){

    image_path<- x_left<- x_right<- x<- y_top<- y_bottom<- y <-
        description <- y_round <- y_lag <- y_order <-token <- remove <-
        order_id <-
        NULL

    usethis::ui_indo("Organizing text")


    # something like 1% of total pagesize - assuming text does not reach
    # the absolute top and bottom of the page
    adjust_size <- (max(data$y_bottom) - min(data$y_top)) /100

    data %>%
        dplyr::group_by(image_path, x_left, x_right, x, y_top, y_bottom, y) %>%
        dplyr::summarise(token = paste(description, collapse = "")) %>%
        dplyr::ungroup() %>%
        # ---
        # get it basically in order by y at nearest 100 pixels
        dplyr::mutate(y_round = round(y, -2)) %>%
        dplyr::arrange(image_path, y_round, x) %>%
        # find the actual difference in y
        # use double our _sizing_ variable as the minimum allowable for
        # defining a new line (approx 2% of the page-height)
        dplyr::mutate(y_lag = dplyr::lag(y) - y,
                      y_order = as.numeric(abs(y_lag) >= adjust_size) %>% tidyr::replace_na(0) %>% cumsum()) %>%
        # reorder by newly refined y and then x
        dplyr::arrange(image_path, y_order, x) %>%
        dplyr::group_by(image_path, y_order, token) %>%
        dplyr::add_count()  %>%
        dplyr::ungroup() %>%
        # handle accidental duplicates
        dplyr::mutate(remove = (abs(dplyr::lag(x) - x) < adjust_size) %>% tidyr::replace_na(FALSE)) %>%
        dplyr::filter(!remove) %>%
        dplyr::group_by(image_path, y_order) %>%
        # always annotate removals in the middle of the line
        # always annotate corrections at the top of the highest
        # box in the line
        # prep coordinates as imagemagick wants to read them
        dplyr::mutate(
            rem_coords = paste0("+", x_left, "+", floor(mean(y))),
            rep_coords = paste0("+", x_left, "+", min(y_top)-adjust_size)
        ) %>%
        dplyr::ungroup() %>%

        # --- test removal ---
        # this broke one of the tests in tokenization
        # this is a hack
        dplyr::mutate(token = stringr::str_remove(token, "@"),
                      token = tokenizers::tokenize_words(token, lowercase = FALSE, strip_punct = FALSE)) %>%
        tidyr::unnest(token) %>%
        dplyr::filter(nchar(token) > 0) %>%

        # ---
        dplyr::mutate(order_id = dplyr::row_number())

    usethis::ui_done("Text organized")
}
