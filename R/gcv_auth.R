#' authentication wrapper
#'
#' @param client_id your specific client id
#' @param client_secret your specific client secret
#' @param service_token your specific service token
#'
#' @return an authenticated session
#' @export
#'
gcv_auth <- function(client_id, client_secret, service_token){
    options(googleAuthR.client_id = client_id)
    options(googleAuthR.client_secret = client_secret)
    options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/cloud-vision")

    service_token <- googleAuthR::gar_auth_service(json_file =  service_token)
    googleAuthR::gar_auth(token = service_token)
}



