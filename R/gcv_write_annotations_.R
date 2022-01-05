gcv_write_annotations <- function(image, annotations){

    image_path <- NULL

    digital_pdf <- magick::image_read(image)

    adjust_size <- round(magick::image_info(digital_pdf)$height) /100

    annotations <- annotations %>%
        dplyr::filter(image_path == image)


    if(nrow(annotations) > 0){
        for(i in 1:nrow(annotations)){

            digital_pdf <-
                digital_pdf %>%
                magick::image_annotate(
                    text = annotations$replacement[i],
                    color = annotations$colour[i],
                    style = "italic",
                    location = annotations$check[i],
                    # this needs to be a parameter depending on the size of the canvas
                    size = (adjust_size * 1.4)
                )

        }
    }

    digital_pdf

}

