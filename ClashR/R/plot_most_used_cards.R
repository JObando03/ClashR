#' Plot most used cards
#'
#' Creates a bar plot of your most used cards based on a battle log
#' returned by [get_battle_log()]. Internally uses [most_used_cards()].
#'
#' @param battles A data frame returned by [get_battle_log()].
#' @param top_n Integer. How many cards to show (default: 10).
#'
#' @return A ggplot object (invisibly). Called for its side effect of
#'   drawing a plot.
#' @examples
#' \dontrun{
#'   battles <- get_battle_log("9JR2YGV20")
#'   plot_most_used_cards(battles, top_n = 8)
#' }
#' @export
plot_most_used_cards <- function(battles, top_n = 10) {
  # get summary table from your existing function
  card_counts <- most_used_cards(battles, top_n = top_n)

  p <- ggplot2::ggplot(
    card_counts,
    ggplot2::aes(x = stats::reorder(card, n), y = n)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Most Used Cards",
      x = "Card",
      y = "Count"
    ) +
    ggplot2::theme_minimal()

  print(p)
  invisible(p)
}
