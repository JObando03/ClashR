#' Get Clash Royale player profile
#'
#' Fetches a player's profile data from the official Clash Royale API.
#'
#' @param tag Character. The player's tag **without** the leading '#',
#'   e.g. "2PQ0YUPVJ".
#' @param token Character. API token for the Clash Royale API.
#'   By default this is read from the CR_API_TOKEN environment variable.
#'
#' @return A list (or data frame) with player information such as
#'   name, trophies, clan, arena, etc.
#' @examples
#' \dontrun{
#'   # Make sure CR_API_TOKEN is set in your .Renviron first
#'   get_player_data("2PQ0YUPVJ")
#' }
#' @export
get_player_data <- function(tag, token = Sys.getenv("CR_API_TOKEN")) {
  # basic checks
  if (!nzchar(token)) {
    stop(
      "No API token found.\n",
      "Please add a line like\n",
      "  CR_API_TOKEN=your_token_here\n",
      "to your .Renviron file (usethis::edit_r_environ()), then restart R.",
      call. = FALSE
    )
  }

  if (!is.character(tag) || length(tag) != 1L || !nzchar(tag)) {
    stop("`tag` must be a non-empty character string (without the leading '#').",
         call. = FALSE)
  }

  tag <- toupper(tag)

  # Build URL: /v1/players/%23TAG  (the # is URL-encoded as %23)
  base_url <- "https://api.clashroyale.com/v1"
  endpoint <- paste0("/players/%23", tag)
  url <- paste0(base_url, endpoint)

  # Make request
  resp <- httr::GET(
    url,
    httr::add_headers(Authorization = paste("Bearer", token))
  )

  status <- httr::status_code(resp)
  if (status >= 400L) {
    # try to show API error details if available
    body_txt <- tryCatch(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      error = function(e) ""
    )
    stop(
      sprintf("API request failed [%s]\nURL: %s\n%s",
              status, url, body_txt),
      call. = FALSE
    )
  }

  # Parse JSON into R
  jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = TRUE
  )
}
