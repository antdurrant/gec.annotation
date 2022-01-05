#' extract relevant x and y location annotations from google cloud vision
#'
#' the API provides 4 x and y coordinates each, we just want to render that as a simple rectangle
#'
#' @param data result of `googleCloudVisionR::gcv_get_image_annotations`
#'
#' @return table with additional columns
#' @export
gcv_xy <- function(data) {
    x <- y <-
        NULL

    usethis::ui_info("Checking location data")

    data %>%
        dplyr::mutate(
            x = purrr::map(x, ~strsplit(.x, ", ") %>% unlist() %>% as.numeric() %>% tidyr::replace_na(0)),
            y = purrr::map(y, ~strsplit(.x, ", ") %>% unlist() %>% as.numeric() %>% tidyr::replace_na(0)),
            x_left = purrr::map_dbl(x, min),
            x_right = purrr::map_dbl(x, max),
            y_top = purrr::map_dbl(y, min),
            y_bottom = purrr::map_dbl(y, max),
            x = purrr::map_dbl(x, mean),
            y = purrr::map_dbl(y, mean))
}
