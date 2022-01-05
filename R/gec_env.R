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
    python_version = 3.8
){

    pkgs <- c(
        c("spacy==3.2.0", "spacy-legacy==3.0.8", "spacy-loggers==1.0.1", "blis==0.7.5", "catalogue==2.0.6", "charset-normalizer==2.0.8", "click==8.0.3", "cymem==2.0.6", "filelock==3.4.0", "huggingface-hub==0.1.2", "idna==3.3", "importlib-metadata==4.8.2", "jinja2==3.0.3", "joblib==1.1.0", "langcodes==3.3.0", "markupsafe==2.0.1", "murmurhash==1.0.6", "nltk==3.6.5", "numpy==1.21.4", "packaging==21.3", "pathy==0.6.1", "pillow==8.4.0", "preshed==3.0.6", "pydantic==1.8.2", "pyparsing==3.0.6", "pyyaml==6.0", "regex==2021.11.10", "requests==2.26.0", "sacremoses==0.0.46", "sentencepiece==0.1.96", "six==1.16.0", "smart-open==5.2.1", "srsly==2.4.2", "thinc==8.0.13", "tokenizers==0.10.3", "torch==1.10.0", "tqdm==4.62.3", "transformers==4.12.5", "typer==0.4.0", "typing-extensions==3.10.0.2", "urllib3==1.26.7", "wasabi==0.8.2", "zipp==3.6.0"),
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
