#' Calculate 3-crown win rate from a battle log
#'
#' Uses the data frame returned by [get_battle_log()] to compute how many
#' battles were won by 3 crowns and the percentage of 3-crown victories.
#'
#' @param battles A data frame returned by [get_battle_log()].
#'
#' @return A data frame with one row and columns:
#'   \describe{
#'     \item{three_crown_wins}{Number of wins with 3 crowns}
#'     \item{wins}{Total number of wins}
#'     \item{total}{Total number of battles with a result}
#'     \item{three_crown_rate}{Percentage of wins that were 3-crown victories}
#'   }
#'
#' @examples
#' \dontrun{
#'   battles <- get_battle_log("9JR2YGV20")
#'   calculate_three_crown_rate(battles)
#' }
#' @export
calculate_three_crown_rate <- function(battles) {
  if (!all(c("team", "opponent") %in% names(battles))) {
    stop(
      "`battles` must have `team` and `opponent` columns.\n",
      "Did you pass the result of get_battle_log()?",
      call. = FALSE
    )
  }

  rows <- lapply(seq_len(nrow(battles)), function(i) {
    team_entry <- battles$team[[i]]
    opp_entry  <- battles$opponent[[i]]

    if (is.null(team_entry) || is.null(opp_entry) ||
        !is.data.frame(team_entry) || !is.data.frame(opp_entry)) {
      return(NA_character_)
    }

    if (!("crowns" %in% names(team_entry)) ||
        !("crowns" %in% names(opp_entry))) {
      return(NA_character_)
    }

    team_crowns <- sum(team_entry$crowns, na.rm = TRUE)
    opp_crowns  <- sum(opp_entry$crowns, na.rm = TRUE)

    if (team_crowns > opp_crowns) {
      if (team_crowns == 3) {
        "3_crown_win"
      } else {
        "win"
      }
    } else if (team_crowns < opp_crowns) {
      "loss"
    } else {
      "draw"
    }
  })

  outcomes <- unlist(rows)
  outcomes <- outcomes[!is.na(outcomes)]

  if (length(outcomes) == 0L) {
    stop(
      "Could not determine outcomes from the battle log.\n",
      "Check structure of `battles$team`.",
      call. = FALSE
    )
  }

  three_crown_wins <- sum(outcomes == "3_crown_win")
  wins             <- sum(outcomes %in% c("win", "3_crown_win"))
  losses           <- sum(outcomes == "loss")
  draws            <- sum(outcomes == "draw")
  total            <- wins + losses + draws

  three_crown_rate <- if (wins > 0L) (three_crown_wins / wins) * 100 else NA_real_

  data.frame(
    three_crown_wins = three_crown_wins,
    wins             = wins,
    total            = total,
    three_crown_rate = round(three_crown_rate, 2),
    row.names        = NULL
  )
}
