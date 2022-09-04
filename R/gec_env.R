#' create grammar error correction python environment
#'
#' @param envname name of environment if you don't want to use the default
#' @param packages packages to include beyond those specified in requirements.txt (none should be necessary)
#' @param forge whether to use forge
#' @param channel specific channels to prefer
#' @param conda which conda to use
#' @param python_version which python versionexists to use - defaults to 3.8 for guaranteed compatibility
#'
#' @return a python environment with requisite packages names "gec_env"
#' @export
#'
gec_env_create <- function(
    envname = "gec_env",
    packages = NULL,
    forge = TRUE,
    channel = character(),
    conda = "auto",
    python_version = 3.9
){

    pkgs <- c(
        c("spacy>=3.2.0",
          "huggingface-hub>=0.1",
          "torch>=1.10.0",
          "transformers>=4.12"
        ),
       packages
    )

    reticulate::conda_install(
        envname = envname,
        packages = pkgs,
        forge = forge,
        channel = channel,
        conda = conda,
        python_version = python_version,
        pip = TRUE
    )

    spacyr::spacy_download_langmodel(envname = envname, model = "en_core_web_sm'")
}

#' use gec env and load python grammar error correction function
#'
#' @param envname name of environment if you don't want to use the default
#'
#' @return spacy initialized python environment and access to `correct_sentences.py`
#' @export
#'
gec_env_use <- function(envname = "gec_env"){

    if(exists("spacy_version") & exists("nlp")){
        usethis::ui_done("Python ready")
    }
    if(!exists("spacy_version")){
    spacyr::spacy_initialize(condaenv = envname)
    }
    if(!exists("nlp")){
    reticulate::source_python(system.file("python/sentence.py", package = "gec.annotation"), envir = globalenv())
        usethis::ui_done("sentence.py successfully sourced")
    }
}
