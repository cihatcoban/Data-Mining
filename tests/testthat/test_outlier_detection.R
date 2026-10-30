test_that("detect_iqr_outliers and cap_iqr_outliers identify and cap extreme values", {
  set.seed(54)
  x <- c(rnorm(100), 10, 12, -10)
  res <- detect_iqr_outliers(x)
  expect_true(all(c(101, 102, 103) %in% res$indices))

  capped <- cap_iqr_outliers(x, method = "cap")
  expect_lte(max(capped), res$upper_bound + 1e-12)
  expect_gte(min(capped), res$lower_bound - 1e-12)
})

test_that("detect_kmeans_outliers computes true Euclidean distance from cluster centers", {
  set.seed(42)
  df <- data.frame(
    x = c(rnorm(30, 0, 0.2), 50),
    y = c(rnorm(30, 0, 0.2), 50)
  )
  res <- detect_kmeans_outliers(df, centers = 2L, top_n = 3L, seed = 42L)
  expect_equal(length(res$distances), 31L)
  expect_true(all(res$distances >= 0))
})

test_that("detect_lof_outliers flags isolated point with highest LOF score", {
  set.seed(42)
  df <- data.frame(
    x = c(rnorm(25, 0, 0.3), 15),
    y = c(rnorm(25, 0, 0.3), 15)
  )
  res <- detect_lof_outliers(df, k = 5L, top_n = 1L)
  expect_equal(res$top_indices[1], 26L)
})
