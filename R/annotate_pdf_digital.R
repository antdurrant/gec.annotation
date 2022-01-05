annotate_pdf_digital <- function(pdf_file){

    doc_id <- original <- text <- changes <- correction_diff_standard <-
        page <-
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
    two_step_correction <- annotate_text(digital_text)

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

    result <- list(image =  magick::image_scale(digital_pdf, geometry = magick::geometry_size_percent(400)),
                   correction_table =
                       two_step_correction %>%
                       dplyr::select(doc_id, original, corrected = text, changes, correction_diff_standard)
    )

    result
}
