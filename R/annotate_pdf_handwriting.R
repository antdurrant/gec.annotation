#' handwriting api pipeline
#'
#' takes a file, returns a table of corrected sentences and annotated images
#'
#' @param file file for correction
#'
#' @return a list: $images = imagemagick image pointer, corrections = table output from `annotate_text`
#' @export
#'
annotate_pdf_handwriting <- function(file){


    # split if necessary
    files <- gcv_check_filetypes(file) %>%
        orient_files()



    # call api
    api_return <- googleCloudVisionR::gcv_get_image_annotations(
        files,
        "DOCUMENT_TEXT_DETECTION"
    )
    usethis::ui_done("Text data retrieved")

    # order
    dat <- api_return %>%
        gcv_xy() %>%
        gcv_reorder_text()


    # gec
    corrections <-
        dat %>%
        extract_digital_text() %>%
        annotate_text()

    # find where to annotate
    annotations <- prep_annotations(annotated_text = corrections, digital_text_df = dat)

    usethis::ui_info("Annotating pdf file")

    # annotate
    res <- purrr::map(files, ~gcv_write_annotations(.x, annotations))
    # stitch images together
    out <- purrr::reduce(res, ~magick::image_append(c(.x, .y), stack = TRUE))

    usethis::ui_done("Annotations complete")

    # return annotated file and corrected text
    list(images = out,
         corrections = corrections)
}
