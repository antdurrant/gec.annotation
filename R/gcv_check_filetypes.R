#' convert files for gcv if necessary
#'
#' if a pdf is provided, it will be converted into 300 dpi pngs
#' in a temp directory to be sent to the API
#'
#' if an image file is provided, it will return that
#'
#' otherwise, it will return NULL with a warning
#'
#' @param file file to be converted
#'
#' @return tempfiles and their paths
#' @export
#'
gcv_check_filetypes <- function(file){
    if(stringr::str_detect(file, "pdf$")){
        pgs <- seq_len(pdftools::pdf_info(file)$pages)

        tmp <- tempdir()
        outfiles <-
            pdftools::pdf_convert(
                file,
                format = "png",
                filenames = c(paste0(tmp, "/", pgs, ".png")),
                dpi = 300
            )
    } else if(stringr::str_detect(file, "png$|jpg$|jpeg$|tiff$")){
        outfiles <- file
    } else {
        outfiles <- NULL
        usethis::ui_nope("No parsable files for sending to API. Files must be images or pdfs.")
    }
    outfiles
}
