test_that("stat_mode computes true statistical mode instead of storage type", {
  x <- c(10, 20, 20, 30, NA)
  expect_equal(stat_mode(x), 20)
  expect_type(stat_mode(x), "double")
})

test_that("replace_sentinel_with_na converts sentinel values to NA", {
  df <- data.frame(v1 = c(1, 99, 3), v2 = c(-999, 5, 6))
  cleaned <- replace_sentinel_with_na(df, sentinels = c(99, -999))
  expect_true(is.na(cleaned$v1[2]))
  expect_true(is.na(cleaned$v2[1]))
})

test_that("impute_vector handles mean, median, mode, and constant strategies", {
  x <- c(2, 2, 4, 10, NA)
  expect_equal(impute_vector(x, "mean")[5], 4.5)
  expect_equal(impute_vector(x, "median")[5], 3)
  expect_equal(impute_vector(x, "mode")[5], 2)
  expect_equal(impute_vector(x, "constant", fill_value = 99)[5], 99)
})

test_that("inject_random_missingness is deterministic for a given seed", {
  df <- data.frame(a = 1:20, b = 21:40)
  sim1 <- inject_random_missingness(df, c(a = 3, b = 4), seed = 99L)
  sim2 <- inject_random_missingness(df, c(a = 3, b = 4), seed = 99L)
  expect_identical(sim1$masked_indices, sim2$masked_indices)
  expect_equal(sum(is.na(sim1$data$a)), 3)
  expect_equal(sum(is.na(sim1$data$b)), 4)
})
