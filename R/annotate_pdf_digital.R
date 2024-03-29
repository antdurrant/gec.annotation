#' poppler pdf text pipeline
#'
#' takes a file, returns a table of corrected sentences and annotated images
#'
#' @param pdf_file file for correction
#' @param mod word2vec model for spelling correction
#'
#' @return a list: $images = imagemagick image pointer, corrections = table output from `annotate_text`
#'
#' @export
#'
annotate_pdf_digital <- function(pdf_file, mod){

    doc_id <- original <- text <- changes <- correction_diff_standard <-
        page <- corrected <-
        NULL



    ## pdf handling -----
    # pdf text
    digital_text <- pdftools::pdf_text(pdf_file) %>%
        paste(collapse = " ") %>%
        stringr::str_squish()

    # pdf text with xy coords
    digital_text_df <-
        poppler_reorder_text(pdf_file)

    adjust_size <- round(max(digital_text_df$y), 0) / 75

    # extracted functions
    two_step_correction <- annotate_text(digital_text, mod)

    annotations <- prep_annotations(annotated_text = two_step_correction, digital_text_df = digital_text_df)

    usethis::ui_info("Annotating pdf")

    # pdf image
    digital_pdf <- magick::image_read(pdf_file)


    if(nrow(annotations)>0){

        for(i in seq_len(length(digital_pdf))){

            page_annotations <- annotations %>%
                dplyr::filter(page == i)

            for(j in seq_len(nrow(page_annotations))){

                # one loop per page
                digital_pdf[i] <-
                    digital_pdf[i] %>%
                    magick::image_annotate(
                        text = page_annotations$replacement[j],
                        color = page_annotations$colour[j],
                        style = "italic",
                        location = page_annotations$check[j],
                        size = adjust_size * 1.4
                    )

            }

        }

    }

    usethis::ui_done("Annotation complete!")

    result <- list(images =  magick::image_scale(digital_pdf, geometry = magick::geometry_size_percent(400)),
                   corrections =
                       two_step_correction %>%
                       dplyr::select(doc_id, original, corrected, changes, correction_diff_standard)
    )

    result
}
