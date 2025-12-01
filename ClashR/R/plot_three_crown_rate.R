#' Plot 3-crown win rate
#'
#' Visualizes how many of your wins were 3-crown victories based on the battle
#' log returned by [get_battle_log()]. Internally calls
#' [calculate_three_crown_rate()].
#'
#' @param battles A data frame returned by [get_battle_log()].
#'
#' @return A ggplot object (invisibly), called for its side effect of drawing
#'   the bar chart.
#'
#' @examples
#' \dontrun{
#'   battles <- get_battle_log("9JR2YGV20")
#'   plot_three_crown_rate(battles)
#' }
#' @export
plot_three_crown_rate <- function(battles) {
  stats <- calculate_three_crown_rate(battles)

  # extract stats
  three <- stats$three_crown_wins
  wins  <- stats$wins
  other <- wins - three

  # assemble plotting data
  df <- data.frame(
    type = c("3-Crown Wins", "Other Wins"),
    count = c(three, other)
  )

  rate <- stats$three_crown_rate

  p <- ggplot2::ggplot(df, ggplot2::aes(x = type, y = count)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = paste0("3-Crown Win Rate: ", rate, "%"),
      x = "",
      y = "Number of Wins"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )

  print(p)
  invisible(p)
}
