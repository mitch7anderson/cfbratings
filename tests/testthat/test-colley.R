########################
# Tests Colley method #
#######################

test_that("colley_team_index returns teams sorted alphabetically", {
  games <- tibble::tibble(
    home_team = c("Texas", "Alabama"),
    away_team = c("Georgia", "Texas")
  )

  idx <- colley_team_index(games)

  expect_equal(names(idx), c("Alabama", "Georgia", "Texas"))
  expect_equal(as.integer(idx), 1:3)
})

test_that("colley_team_index handles repeated team names correctly", {
  games <- tibble::tibble(
    home_team = c("Texas", "Texas", "Alabama"),
    away_team = c("Georgia","Alabama", "Texas")
  )

  idx <- colley_team_index(games)

  expect_equal(names(idx), c("Alabama", "Georgia", "Texas"))
  expect_equal(length(idx), 3L)
})

test_that("colley_win_loss_counts correctly tabulates records", {
  games <- tibble::tibble(
    home_team = c("Alabama", "Texas A&M", "Georgia"),
    away_team = c("Texas A&M","Georgia", "Alabama"),
    home_points = c(14, 70, 64),
    away_points = c(21, 0, 7)
  )

  team_index <- colley_team_index(games)
  wl <- colley_win_loss_counts(games, team_index)

  expect_equal(wl$wins, c(0L, 1L, 2L))
  expect_equal(wl$losses, c(2L, 1L, 0L))
  expect_equal(wl$games, c(2L, 2L, 2L))
})

test_that("colley_build_matrix constructs correct C and b for 2-team example", {
  games <- tibble::tibble(
    home_team = c("Texas A&M", "Alabama"),
    away_team = c("Alabama","Texas A&M"),
    home_points = c(21, 0),
    away_points = c(14, 70)
  )

  team_index <- colley_team_index(games)
  wl <- colley_win_loss_counts(games, team_index)
  cb <- colley_build_matrix(games, team_index, wl)

  C_mat <- as.matrix(cb$C)
  b_vec <- cb$b

  expect_equal(dim(C_mat), c(2L, 2L))
  expect_equal(rownames(C_mat), colnames(C_mat))

  expect_equal(C_mat,
               matrix(c(4, -2,
                        -2, 4),
                      nrow = 2, byrow = TRUE))
  expect_equal(as.numeric(b_vec), c(0, 2))
})

test_that("fit_colley returns a tibble with team and rating", {
  games <- tibble::tibble(
    season = c(2024, 2024),
    week = c(1,2),
    home_team = c("A", "B"),
    away_team = c("B", "A"),
    home_points = c(10, 3),
    away_points = c(3, 0)
  )

  ratings <- fit_colley(games)

  expect_s3_class(ratings, "tbl_df")
  expect_true(all(c("team", "rating") %in% names(ratings)))

  expect_equal(nrow(ratings), 2L)

  expect_type(ratings$team, "character")
  expect_type(ratings$rating, "double")
})

test_that("fit_colley produces correct ratings for 2-team case", {
  games <- tibble::tibble(
    season = c(2024, 2024),
    week = c(1, 2),
    home_team = c("Texas A&M", "Alabama"),
    away_team = c("Alabama","Texas A&M"),
    home_points = c(21, 0),
    away_points = c(14, 70)
  )

  ratings <- fit_colley(games)

  expect_equal(ratings$team, c("Texas A&M", "Alabama"))

  expect_equal(ratings$rating[ratings$team == "Alabama"], 1/3, tolerance = 1e-6)
  expect_equal(ratings$rating[ratings$team == "Texas A&M"], 2/3, tolerance = 1e-6)
})


