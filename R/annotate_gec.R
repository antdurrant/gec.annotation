#' annotate for grammar error correction
#'
#' wraps annotate_text, annotate_pdf_digital and annotate_pdf_handwriting
#'
#' @param input path to file or character vector of text
#' @param mod word2vec model for spelling correction
#' @param force_handwriting whether to skip the attempt to parse the text locally
#'
#' @return a list containing an imagemagick image and corrected text data.frame.
#'  If only text is provided, the imagemagick image will simply return NULL
#' @export
#'
annotate_gec <- function(input, mod, force_handwriting = FALSE){

    doc_id <- original <- corrected <- changes <- correction_diff_standard <-
        NULL

    # is it a file?
    if(class(input) == "character" & file.exists(input)){

        if(force_handwriting|!stringr::str_detect(input, "pdf$|PDF$")){

            usethis::ui_done("Passing file to handwriting API")

            annotate_pdf_handwriting(input, mod)

        } else if(!force_handwriting){

            usethis::ui_info("Seeing if file can be parsed locally")

            try_pdf_info <- purrr::possibly(pdftools::pdf_info, otherwise = list(keys = ""))
            check_for_scan <- paste(try_pdf_info(input)$keys, collapse = " ")


            if(pdftools::pdf_text(input) %>%
               paste(collapse = "") %>%
               nchar()>10 &
               !stringr::str_detect(
                   check_for_scan,
                   # common indicators of images instead of pdfs
                   "Scan|SCAN|scan|スキャン|カメラ|Camera|camera|CAMERA|RICOH|Quartz PDFContext|Capture Pro|IMG_|Canon|KONICA|DocuCentre|iTextSharp|Print To PDF|www.ilovepdf.com|iLovePDF|PdfEditor|^E$|HiPDF|Haru Free|Image to PDF|Image Conversion|LoiLo Inc|Office Lens"
               ) &
               stringr::str_detect(check_for_scan, "[a-zA-Z]")
            ){

                usethis::ui_done("Text found; reading locally")
                annotate_pdf_digital(input, mod)

            } else{
                usethis::ui_info("Unable to read locally, passing file to handwriting API")
                # once it works
                annotate_pdf_handwriting(input, mod)
            }
        }
        # or is it text directly?
    } else if(class(input) == "character"){
        result <-
            list(
                images = NULL,
                corrections = annotate_text(input, mod) %>%
                    dplyr::select(doc_id, original, corrected, changes, correction_diff_standard)
            )
    } else{
        usethis::ui_oops("You need to feed this text or a file; returning useless output.")
        list(images = NULL,
             corrections = tibble::tibble(text = "You have to give me some text first."))
    }


}
