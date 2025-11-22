###################
# Colley Ratings #
##################

#' Create team index for the Colley method
#'
#' Internal helper that maps team names to integer indices
#'
#' @param games A cleaned games data frame
#'
#' @return A named integer vector
#' @keywords internal
colley_team_index <- function(games) {
  # Get unique team names and give them integer index value
  teams <- sort(unique(c(games$home_team, games$away_team)))
  idx <- seq_along(teams)
  names(idx) <- teams
  return(idx)
}

#' Compute wins and losses per team for Colley method
#'
#' Internal helper that computes number of wins, losses, and games played
#' for each team
#'
#' @param games A cleaned games data frame
#' @param team_index  Named integer vector mapping team names to indices
#'
#' @return A list with elements:
#' wins - integer vector of wins per team
#' losses - integer vector of losses per team
#' games - integer vector of games played per team
#' @keywords internal
colley_win_loss_counts <- function(games, team_index) {
  # Get number of teams and create vector of wins and losses to store team data
  n_teams = length(team_index)
  w <- integer(n_teams)
  l <- integer(n_teams)

  # Calculate wins and losses for each team
  for (k in seq_len(nrow(games))) {
    home <- games$home_team[k]
    away <- games$away_team[k]
    hp <- games$home_points[k]
    ap <- games$away_points[k]

    if (hp > ap) {
      w[team_index[home]] <- w[team_index[home]] + 1L
      l[team_index[away]] <- l[team_index[away]] + 1L
    } else {
      w[team_index[away]] <- w[team_index[away]] + 1L
      l[team_index[home]] <- l[team_index[home]] + 1L
    }
  }

  list(
    wins = w,
    losses = l,
    games = w + l
  )
}

#' Build Colley Matrix for Colley method
#'
#' Internal helper that constructs the Colley matrix from game outcomes and
#' win/loss counts
#'
#' @param games A cleaned games data frame
#' @param team_index Named integer vector mapping team names to indices
#' @param wl A list of wins, losses, and games played by each team
#'
#' @return A list with elements:
#' C - Colley matrix
#' b - numeric right hand side vector
#' @keywords internal
colley_build_matrix <- function(games, team_index, wl) {
  n <- length(team_index)

  # build C matrix
  C <- Matrix::Diagonal(n, 2) + Matrix::Diagonal(n, wl$games)

  # build b matrix
  b <- 1 + 0.5 * (wl$wins - wl$losses)

  for (k in seq_len(nrow(games))) {
    i <- team_index[games$home_team[k]]
    j <- team_index[games$away_team[k]]

    C[i, j] <- C[i, j] - 1
    C[j, i] <- C[j, i] - 1
  }

  list(C = C, b = b)
}

#' Solve the Colley linear system
#'
#' Internal heper that solves the linear system to produce the rating vector
#'
#' @param C Colley matrix
#' @param b numeric vector
#'
#' @returns numeric vector of ratings
#' @keywords internal
colley_solve_system <- function(C, b) {
  ratings <- Matrix::solve(C, b)
  as.numeric(ratings)
}

#' Format Colley rating output as a tibble
#'
#' Internal helper to convert the rating vector into a tibble with team names
#' and ratings
#'
#' @param team_index Named integer vector mapping team names to indices
#' @param ratings Numeric vector of ratings, ordered as team_index
#'
#' @return A tibble with columns team and rating sorted highest to lowest
#' @keywords internal
colley_output_table <- function(team_index, ratings) {
  teams <- names(team_index)

  tibble::tibble(
    team = teams,
    rating = ratings
  ) |>
    dplyr::arrange(dplyr::desc(.data$rating))
}

#' Fit Colley ratings
#'
#' Compute team ratings using the Colley matrix method based on win/loss
#' outcomes in the supplied games data. They Colley system constructs a linear
#' system \eqn{C r = b}, where \eqn{C} is the Colley matrix and \eqn{b} is a
#' vector derived from wins and losses by each team in the season.
#'
#' Required columns in \code{games} are:
#' \itemize{
#' \item \code{season}
#' \item \code{week}
#' \item \code{home_team}
#' \item \code{away_team}
#' \item \code{home_points}
#' \item \code{away_points}
#' }
#'
#' @param games A data fram containing at least the columns listed above
#'
#' @return A tibble with columns:
#' \itemize{
#' \item \code{team} Team name
#' \item \code{rating} Colley rating
#' }
#' Sorted from highest to lowest rating
#' @export
#'
#' @examples
#' games <- tibble::tibble(
#' season = c(2024, 2024),
#' week = c(1, 2),
#' home_team = c("Texas A&M", "Alabama"),
#' away_team = c("Alabama","Texas A&M"),
#' home_points = c(21, 0),
#' away_points = c(14, 70)
#' )
#'
#' ratings <- fit_colley(games)
fit_colley <- function(games) {
  # validate and standardize input
  games <- check_games_df(games)

  # build team index and win/loss info
  team_index <- colley_team_index(games)
  wl <- colley_win_loss_counts(games, team_index)

  # build Colley matrix C and vector b
  cb <- colley_build_matrix(games, team_index, wl)

  # solve Cr = b
  ratings_vec <- colley_solve_system(cb$C, cb$b)

  # format output
  result <- colley_output_table(team_index, ratings_vec)

  return(result)
}


