#' extract digital text
#'
#' simply turn the tokenized text into regular text.
#'
#' @param data either a dataframe with column "token" or a character vector to be squished down
#'
#' @return a character vector of length one with all the text
#' @export
#'
#' @examples
#' extract_digital_text(data = c("A bunch of text", "that hasn't been connected yet." ))
#' extract_digital_text(data.frame(token = c("A", "bunch", "of", "'", "text", "'", ".")))
extract_digital_text <- function(data){

    if("data.frame" %in% class(data)){
        data <- data$token
    }
    data %>%
        paste(collapse = " ") %>%
        # no whitespace before punctuation (not all, so must be listed manually)
        # no whitespace after opening parentheses
        stringr::str_remove_all(" (?=\\,|\\.|\\:|\\;|\\)|\\!|\\?)") %>%
        stringr::str_remove_all("(?<=\\() ") %>%
        stringr::str_replace_all("\u201D(?=[:alpha:])", " \u201C") %>%
        stringr::str_replace_all("(?<=[:alpha:])\u201C", "\u201D ") %>%
        stringr::str_replace_all("(?<=[:alpha:][\\.|\\,])(?=\\d)", " ") %>%
        stringr::str_replace_all("\u201C ", " \u201C") %>%
        stringr::str_replace_all(" \u201D", "\u201D ") %>%
        stringr::str_replace_all("\u201D \\.", "\u201D.") %>%
        stringr::str_replace_all("\u201D ,", "\u201D,") %>%
        stringr::str_replace_all("ES (?=\\d)", "ES")
}
