poppler_write_annotations <- function(pdf_file, annotations){

    page <-
        NULL


    adjust_size <- round(max(pdftools::pdf_pagesize(pdf_file)$bottom))/100

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

    digital_pdf
}
