#' Conform images to exif metadata
#'
#' [Exif metadata](https://en.wikipedia.org/wiki/Exif) records the orientation of images. Before passing
#' images to the handwriting API, you will want to make sure that the x and y axes are the way the picture-taker intended.
#'
#' This is going to be fairly slow, as it is reading and writing images to/from (temp)files, so at some point there should be
#' some reading of metadata to find out whether it needs to be done or not.
#'
#' @param files paths to files
#'
#' @return re-oriented files
#' @export
#'
#' @examples
orient_files <- function(files, outfiles = NULL){

    if(is.null(outfiles)){

        outfiles <- files
    } else if(length(files) != length(outfiles)){
        outfiles <- paste(outfiles[1], "_", seq_len(length(files), tools::file_ext(files)))
        usethis::ui_warn("Outfiles and infiles not the same length, using first entry as base filename")

    }

    purrr::walk2(
        files, outfiles,
        ~magick::image_read(.x) %>%
            magick::image_orient() %>%
            magick::image_write(.y)
    )



    outfiles
}
