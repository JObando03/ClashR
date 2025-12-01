#' Set Clash Royale API token
#'
#' Saves the user's Clash Royale API token to their .Renviron file so that
#' ClashR can automatically authenticate API requests.
#'
#' @param token A character string containing the user's API token.
#'
#' @examples
#' \dontrun{
#'   set_cr_token("YOUR_TOKEN_HERE")
#' }
#' @export
set_cr_token <- function(token) {
  if (!is.character(token) || !nzchar(token)) {
    stop("`token` must be a non-empty character string.", call. = FALSE)
  }

  # Locate and/or create .Renviron
  env_path <- path.expand("~/.Renviron")

  # Read existing contents (if file exists)
  if (file.exists(env_path)) {
    env <- readLines(env_path, warn = FALSE)
    env <- env[!grepl("^CR_API_TOKEN=", env)]  # remove previous token if present
  } else {
    env <- character(0)
  }

  # Add token
  env <- c(env, paste0("CR_API_TOKEN=", token))
  writeLines(env, env_path)

  message("Token saved to .Renviron. Restart R for the change to take effect.")
  invisible(TRUE)
}
