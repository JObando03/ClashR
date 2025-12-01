#' Plot trophy progression over time
#'
#' Uses a battle log (from [get_battle_log()]) to reconstruct how your
#' trophies change over time and plots them.
#'
#' @param battles A data frame returned by [get_battle_log()].
#'
#' @return A ggplot object (invisibly). Called for its side effect of
#'   drawing a line plot of trophies over time.
#'
#' @examples
#' \dontrun{
#'   battles <- get_battle_log("9JR2YGV20")
#'   plot_trophy_progression(battles)
#' }
#' @export
plot_trophy_progression <- function(battles) {
  # basic checks
  if (!all(c("battleTime", "team") %in% names(battles))) {
    stop(
      "`battles` must have at least `battleTime` and `team` columns.\n",
      "Did you pass the result of get_battle_log()?",
      call. = FALSE
    )
  }

  # Extract trophies for your player from each battle
  rows <- lapply(seq_len(nrow(battles)), function(i) {
    team_entry <- battles$team[[i]]

    # team_entry is typically a data.frame of players on your team
    if (is.null(team_entry) || !is.data.frame(team_entry)) return(NULL)
    if (!all(c("startingTrophies", "trophyChange") %in% names(team_entry))) {
      return(NULL)
    }

    # Use the first row = you (for 1v1) or one of you (for 2v2)
    player_row <- team_entry[1, , drop = FALSE]

    trophies_after <- player_row$startingTrophies + player_row$trophyChange

    data.frame(
      battleTime = battles$battleTime[i],
      trophies_after = trophies_after,
      stringsAsFactors = FALSE
    )
  })

  trophy_df <- dplyr::bind_rows(rows)

  if (nrow(trophy_df) == 0L) {
    stop(
      "Could not extract trophy data from `battles$team`.\n",
      "Check the structure of your battle log.",
      call. = FALSE
    )
  }

  # Convert battleTime to POSIXct
  # Clash Royale format is typically "YYYYMMDDThhmmss.000Z"
  trophy_df$time <- as.POSIXct(
    trophy_df$battleTime,
    format = "%Y%m%dT%H%M%S.%OSZ",
    tz = "UTC"
  )

  # Drop rows where time failed to parse
  trophy_df <- trophy_df[!is.na(trophy_df$time), ]

  if (nrow(trophy_df) == 0L) {
    stop("All battleTime values failed to parse into dates.", call. = FALSE)
  }

  # Sort by time ascending (oldest to newest)
  trophy_df <- trophy_df[order(trophy_df$time), ]

  p <- ggplot2::ggplot(trophy_df, ggplot2::aes(x = time, y = trophies_after)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = "Trophy Progression Over Time",
      x = "Time",
      y = "Trophies"
    ) +
    ggplot2::theme_minimal()

  print(p)
  invisible(p)
}
