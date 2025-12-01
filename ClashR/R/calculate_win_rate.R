#' Calculate win rate from a battle log
#'
#' Uses the data frame returned by [get_battle_log()] to compute how many
#' battles you won, lost, or drew, and your overall win rate.
#'
#' @param battles A data frame returned by [get_battle_log()].
#'
#' @return A data frame with one row and columns:
#'   \describe{
#'     \item{wins}{Number of battles won}
#'     \item{losses}{Number of battles lost}
#'     \item{draws}{Number of draws}
#'     \item{total}{Total number of battles with a result}
#'     \item{win_rate}{Win rate as a percentage (0–100)}
#'   }
#'
#' @examples
#' \dontrun{
#'   battles <- get_battle_log("9JR2YGV20")
#'   calculate_win_rate(battles)
#' }
#' @export
calculate_win_rate <- function(battles) {
  if (!all(c("team", "opponent") %in% names(battles))) {
    stop(
      "`battles` must have `team` and `opponent` columns.\n",
      "Did you pass the result of get_battle_log()?",
      call. = FALSE
    )
  }

  outcomes <- lapply(seq_len(nrow(battles)), function(i) {
    team_entry <- battles$team[[i]]
    opp_entry  <- battles$opponent[[i]]

    # If structure is weird, skip this battle
    if (is.null(team_entry) || is.null(opp_entry) ||
        !is.data.frame(team_entry) || !is.data.frame(opp_entry)) {
      return(NA_character_)
    }

    # Sum crowns for your team and opponent team
    if (!("crowns" %in% names(team_entry)) ||
        !("crowns" %in% names(opp_entry))) {
      return(NA_character_)
    }

    team_crowns <- sum(team_entry$crowns, na.rm = TRUE)
    opp_crowns  <- sum(opp_entry$crowns, na.rm = TRUE)

    if (team_crowns > opp_crowns) {
      "win"
    } else if (team_crowns < opp_crowns) {
      "loss"
    } else {
      "draw"
    }
  })

  outcomes <- unlist(outcomes)
  outcomes <- outcomes[!is.na(outcomes)]

  if (length(outcomes) == 0L) {
    stop(
      "Could not determine any outcomes from the battle log.\n",
      "Check the structure of `battles$team` and `battles$opponent`.",
      call. = FALSE
    )
  }

  wins   <- sum(outcomes == "win")
  losses <- sum(outcomes == "loss")
  draws  <- sum(outcomes == "draw")
  total  <- wins + losses + draws

  win_rate <- if (total > 0L) (wins / total) * 100 else NA_real_

  data.frame(
    wins = wins,
    losses = losses,
    draws = draws,
    total = total,
    win_rate = round(win_rate, 2),
    row.names = NULL
  )
}
