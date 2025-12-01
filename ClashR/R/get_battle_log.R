#' Get Clash Royale Battle Log
#'
#' Fetches a player's most recent battles from the official Clash Royale API
#' and returns a data frame structured to work with ClashR analytics functions
#' such as [most_used_cards()], [calculate_win_rate()],
#' [plot_trophy_progression()], and [calculate_three_crown_rate()].
#'
#' @param tag Character. The player's tag, with or without the leading '#'.
#'   For example: "9JR2YGV20" or "#9JR2YGV20".
#' @param token Character. API token for the Clash Royale API.
#'   By default this is read from the CR_API_TOKEN environment variable.
#'
#' @return A tibble with one row per battle and columns:
#'   \describe{
#'     \item{battleTime}{Battle timestamp (character)}
#'     \item{type}{Battle type (ladder, etc.)}
#'     \item{gameMode}{Game mode name}
#'     \item{arena}{Arena name}
#'     \item{team}{Data frame of your team players with columns
#'                 \code{crowns}, \code{startingTrophies},
#'                 \code{trophyChange}, and \code{cards}
#'                 (list-column of card data frames)}
#'     \item{opponent}{Same structure as \code{team} for the opposing side}
#'   }
#' @export
get_battle_log <- function(tag, token = Sys.getenv("CR_API_TOKEN")) {
  # small internal helper
  `%||%` <- function(x, y) if (is.null(x)) y else x

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

  # Normalize tag: strip leading '#', uppercase
  tag <- toupper(tag)
  tag <- sub("^#", "", tag)

  base_url <- "https://api.clashroyale.com/v1"
  endpoint <- paste0("/players/%23", tag, "/battlelog")
  url <- paste0(base_url, endpoint)

  resp <- httr::GET(
    url,
    httr::add_headers(Authorization = paste("Bearer", token))
  )

  status <- httr::status_code(resp)
  if (status >= 400L) {
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

  # Keep nested structure so we can build list-columns ourselves
  raw <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

  if (length(raw) == 0L) {
    return(tibble::tibble())
  }

  # helper to build a team/opponent data frame with expected columns
  build_side_df <- function(side_list) {
    if (is.null(side_list)) return(NULL)

    rows <- lapply(side_list, function(p) {
      # p = one player on a side
      crowns          <- p$crowns           %||% NA_integer_
      startingTrophies <- p$startingTrophies %||% NA_integer_
      trophyChange    <- p$trophyChange     %||% NA_integer_

      # Build cards data frame for this player, if present
      cards_df <- NULL
      if (!is.null(p$cards)) {
        cards_df <- dplyr::bind_rows(
          lapply(p$cards, function(cd) {
            data.frame(
              name = cd$name %||% NA_character_,
              id   = cd$id   %||% NA_integer_,
              stringsAsFactors = FALSE
            )
          })
        )
      }

      row <- data.frame(
        crowns          = crowns,
        startingTrophies = startingTrophies,
        trophyChange    = trophyChange,
        stringsAsFactors = FALSE
      )
      row$cards <- list(cards_df)
      row
    })

    dplyr::bind_rows(rows)
  }

  battles <- lapply(raw, function(b) {
    data.frame(
      battleTime = b$battleTime %||% NA_character_,
      type       = b$type       %||% NA_character_,
      gameMode   = b$gameMode$name %||% NA_character_,
      arena      = b$arena$name %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })

  battles_df <- dplyr::bind_rows(battles)

  # add list-columns for team and opponent
  battles_df$team <- lapply(raw, function(b) build_side_df(b$team))
  battles_df$opponent <- lapply(raw, function(b) build_side_df(b$opponent))

  tibble::as_tibble(battles_df)
}
