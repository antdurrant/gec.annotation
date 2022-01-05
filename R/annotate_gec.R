#' annotate for grammar error correction
#'
#' wraps annotate_text, annotate_pdf_digital and annotate_pdf_handwriting
#'
#' @param input path to file or character vector of text
#' @param force_handwriting whether to skip the attempt to parse the text locally
#'
#' @return a list containing an imagemagick image and corrected text data.frame.
#'  If only text is provided, the imagemagick image will simply return NULL
#' @export
#'
annotate_gec <- function(input, force_handwriting = FALSE){

    doc_id <- original <- corrected <- changes <- correction_diff_standard <-
        NULL


    # is it a file?
    if(class(input) == "character" & file.exists(input) & !force_handwriting){
        usethis::ui_info("Seeing if file can be parsed locally")

        if(force_handwriting){
            usethis::ui_done("Passing file to handwriting API")
            # once it works
            annotate_pdf_handwriting(input)
        }

        check_for_scan <- paste(pdftools::pdf_info(input)$keys, collapse = " ")

        if(!force_handwriting){
            if(pdftools::pdf_text(input) %>%
               paste(collapse = "") %>%
               nchar()>10 &
               !stringr::str_detect(check_for_scan, "Scan|scan")){
                usethis::ui_done("Text found; reading locally")
                annotate_pdf_digital(input)
            } else{
                usethis::ui_info("Unable to read locally, passing file to handwriting API")
                # once it works
                annotate_pdf_handwriting(input)
            }
        }
        # or is it text directly?
    } else if(class(input) == "character"){
        result <-
            list(
                images = NULL,
                corrections = annotate_text(input) %>%
                    dplyr::select(doc_id, original, corrected, changes, correction_diff_standard)
            )
    } else{
        usethis::ui_oops("You need to feed this text or a file; returning useless output.")
        list(images = NULL,
             corrections = tibble::tibble(text = "You have to give me some text first."))
    }

}
