##############################
# Tests for check_games_df() #
##############################

test_that("check_games_df accepts a valid games data frame", {
  # Create small games tibble
  games_raw <- tibble::tibble(
    season = c("2024", "2024"),
    week = c("1", "2"),
    home_team = c("Alabama", "Texas"),
    away_team = c("Texas", "Oklahoma"),
    home_points = c(31, 17),
    away_points = c(24, 28)
  )

  # Run function on test games tibble
  games_clean <- check_games_df(games_raw)

  # Check that function returns data frame with same number of rows
  expect_s3_class(games_clean, "data.frame")
  expect_equal(nrow(games_clean), nrow(games_raw))

  # Check that types are coerced as expected
  expect_type(games_clean$season, "integer")
  expect_type(games_clean$week, "integer")
  expect_type(games_clean$home_team, "character")
  expect_type(games_clean$away_team, "character")
  expect_type(games_clean$home_points, "double")
  expect_type(games_clean$away_points, "double")
})

test_that("check_games_df drops rows with missing scores", {
  # Create small games tibble with missing score
  games_raw <- tibble::tibble(
    season = c(2024, 2024),
    week = c(1, 2),
    home_team = c("Alabama", "Texas"),
    away_team = c("Texas", "Oklahoma"),
    home_points = c(31, NA_real_),
    away_points = c(24, 28)
  )

  games_clean <- check_games_df(games_raw)

  # Check that only complete case row remains
  expect_equal(nrow(games_clean), 1L)
  expect_equal(games_clean$home_team, "Alabama")
  expect_equal(games_clean$away_team, "Texas")
})

test_that("check_games_df returns error if games is not a data frame", {
  expect_error(
    check_games_df(1:10),
    "'games' must be a data.frame or tibble.",
    fixed = TRUE
  )
})

test_that("check_games_df errors if required columns are missing", {
  # Create games data frame with missing columns
  games_bad <- tibble::tibble(
    season = 2024,
    week = 1
  )

  expect_error(
    check_games_df(games_bad),
    "The 'games' data is missing required columns: home_team, away_team, home_points, away_points",
    ignore.case = TRUE
  )
})
