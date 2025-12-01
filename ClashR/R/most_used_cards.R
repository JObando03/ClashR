#' Most used cards in a battle log
#'
#' Summarise which cards you use most often, based on the
#' data frame returned by [get_battle_log()].
#'
#' @param battles A data frame returned by [get_battle_log()].
#' @param top_n Integer. How many cards to return (default: 10).
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{card}{Card name}
#'     \item{n}{How many times the card appears across your battles}
#'   }
#' @examples
#' \dontrun{
#'   battles <- get_battle_log("9JR2YGV20")
#'   most_used_cards(battles, top_n = 5)
#' }
#' @export
most_used_cards <- function(battles, top_n = 10) {
  if (!"team" %in% names(battles)) {
    stop("`battles` does not have a `team` column. Did you pass the output of get_battle_log()?",
         call. = FALSE)
  }

  # Extract cards from your team in each battle
  team_cards_list <- lapply(battles$team, function(team_entry) {
    # team_entry is typically a data.frame with one row per player on your team
    if (is.null(team_entry) || !is.data.frame(team_entry)) return(NULL)
    if (!"cards" %in% names(team_entry)) return(NULL)

    # 'cards' is usually a list column: one data frame (8 rows) per player
    card_dfs <- team_entry$cards

    # bind all players' cards in this battle into one data frame
    cards_this_battle <- dplyr::bind_rows(card_dfs)

    # keep just the name column if it exists
    if ("name" %in% names(cards_this_battle)) {
      cards_this_battle["name"]
    } else {
      cards_this_battle["id"]
    }
  })

  # Combine all battles
  cards <- dplyr::bind_rows(team_cards_list)

  if (nrow(cards) == 0L) {
    stop("No card data found in `battles$team`. Check the structure of `battles`.", call. = FALSE)
  }

  # Use name if available, otherwise id
  if ("name" %in% names(cards)) {
    out <- dplyr::count(cards, name, sort = TRUE, name = "n")
    out <- head(out, top_n)
    out <- dplyr::rename(out, card = name)
  } else {
    out <- dplyr::count(cards, id, sort = TRUE, name = "n")
    out <- head(out, top_n)
    out <- dplyr::rename(out, card = id)
  }

  out
}
