# Shared internal helper functions

#' Check and standardize games data frame
#'
#' Internal helper function to validate the structure of the games data frame
#' used by rating functions. Ensures all required columns are present and
#' coerces basic types
#'
#' Required columns:
#' season
#' week
#' home_team
#' away_team
#' home_points
#' away_points
#'
#' @param games A data frame of game results
#'
#' @return A cleaned data frame with standardized types.
#' @keywords internal
check_games_df <- function(games) {
  if (!is.data.frame(games)) {
    stop("'games' must be a data.frame or tibble.", call. = FALSE)
  }

  required_cols <- c(
    "season", "week",
    "home_team", "away_team",
    "home_points", "away_points"
  )

  missing_cols <- setdiff(required_cols, names(games))

  if(length(missing_cols) > 0) {
    stop(
      "The 'games' data is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Coerce types
  games$season <- as.integer(games$season)
  games$week <- as.integer(games$week)
  games$home_team <- as.character(games$home_team)
  games$away_team <- as.character(games$away_team)
  games$home_points <- as.numeric(games$home_points)
  games$away_points <- as.numeric(games$away_points)

  # Drop rows with missing scores
  keep <- !is.na(games$home_points) & !is.na(games$away_points)
  if (!all(keep)) {
    games <- games[keep, , drop = FALSE]
  }

  return(games)
}
