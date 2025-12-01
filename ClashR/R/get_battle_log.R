#' Get Clash Royale Battle Log
#'
#' Fetches a player's most recent battles from the official Clash Royale API.
#'
#' @param tag Character. The player's tag **without** the leading '#'.
#'   For example: "9JR2YGV20".
#' @param token Character. API token for the Clash Royale API.
#'   By default this is read from the CR_API_TOKEN environment variable.
#'
#' @return A tibble where each row is a battle, including list-columns
#'   `team` and `opponent` containing the raw player/team structures.
#' @export
get_battle_log <- function(tag, token = Sys.getenv("CR_API_TOKEN")) {
  if (!nzchar(token)) {
    stop(
      "No API token found.\n",
      "Please add CR_API_TOKEN=your_token_here to your .Renviron file ",
      "(usethis::edit_r_environ()), then restart R.",
      call. = FALSE
    )
  }

  if (!is.character(tag) || length(tag) != 1L || !nzchar(tag)) {
    stop("`tag` must be a non-empty character string.", call. = FALSE)
  }

  # normalize tag (strip leading #, uppercase)
  tag <- toupper(tag)
  tag <- sub("^#", "", tag)

  base_url <- "https://api.clashroyale.com/v1"
  endpoint <- paste0("/players/%23", tag, "/battlelog")
  url <- paste0(base_url, endpoint)

  resp <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", token)))
  status <- httr::status_code(resp)

  if (status >= 400L) {
    body_txt <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"),
                         error = function(e) "")
    stop(
      sprintf("API request failed [%s]\nURL: %s\n%s",
              status, url, body_txt),
      call. = FALSE
    )
  }

  # Get raw list (one element per battle) WITHOUT flattening everything
  raw <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

  if (length(raw) == 0L) {
    return(tibble::tibble())
  }

  # small helper
  `%||%` <- function(x, y) if (is.null(x)) y else x

  battles <- tibble::tibble(
    battleTime = vapply(raw, function(b) b$battleTime %||% NA_character_, character(1)),
    type       = vapply(raw, function(b) b$type       %||% NA_character_, character(1)),
    gameMode   = vapply(raw, function(b) b$gameMode$name %||% NA_character_, character(1)),
    arena      = vapply(raw, function(b) b$arena$name %||% NA_character_, character(1)),
    team       = lapply(raw, function(b) b$team),
    opponent   = lapply(raw, function(b) b$opponent)
  )

  battles
}
