#' wdiff behaviour in R
#'
#' replicate the simple wdiff output without writing tempfiles our calling the command line
#'
#' @param original original sentence
#' @param correction corrected sentence
#' @param sep separator for tokenization (use " " to replicate wdiff, or "" for character by character diff)
#'
#' @return a character vector length 1 summarising diff
#'
wdiff_ <- function(original, correction, sep = " "){
    # tokenize _very_ simply
    a <- unlist(strsplit(original, sep))
    b <- unlist(strsplit(correction, sep))

    # get diff
    dat <- diffobj::ses_dat(a, b)

    # present it how we want
    diff <- dat[['val']]
    del <- dat[['op']] == 'Delete'
    ins <- dat[['op']] == 'Insert'
    if(any(del)) {
        diff[del] <- paste0("[-", diff[del], "-]")
    }
    if(any(ins)) {
        diff[ins] <- paste0("{+", diff[ins], "+}")
    }
    if(any(!ins & !del)){
        diff[!ins & !del] <- paste0("", diff[!ins & !del])
    }

    # bring it all back together
    paste(diff, collapse = sep)

}

#' wdiff behaviour in R
#'
#' replicate the simple wdiff output without writing tempfiles our calling the command line
#'
#' @param original character vector of original sentences
#' @param correction character vector of corrected sentences
#' @param sep separator for tokenization (use " " to replicate wdiff, or "" for character by character diff)
#'
#' @return a character vector summarising diffs between the two character vectors
#' @export
#'
#' @examples
#' wdiff("I haben't idea about that", "I have no idea about that.", "")
wdiff <- function(original, correction, sep = " "){
    purrr::map2_chr(original, correction, ~wdiff_(.x, .y, sep = sep))
}
