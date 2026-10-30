test_that("scale_min_max scales accurately to [0, 1] and rejects constant vectors", {
  x <- c(10, 20, 30, 40, 50)
  scaled <- scale_min_max(x)
  expect_equal(min(scaled), 0)
  expect_equal(max(scaled), 1)
  expect_equal(scaled, c(0, 0.25, 0.5, 0.75, 1))

  expect_error(scale_min_max(c(5, 5, 5)), "Sabit değerli")
})

test_that("scale_z_score centers to mean 0 and sd 1", {
  x <- c(2, 4, 6, 8, 10)
  z <- scale_z_score(x)
  expect_equal(mean(z), 0, tolerance = 1e-12)
  expect_equal(sd(z), 1, tolerance = 1e-12)
  expect_error(scale_z_score(c(3, 3, 3)), "Standart sapması 0")
})

test_that("scale_mean_norm bounds values within [-1, 1]", {
  x <- 1:100
  normed <- scale_mean_norm(x)
  expect_equal(mean(normed), 0, tolerance = 1e-12)
  expect_lte(max(abs(normed)), 1 + 1e-12)
})

test_that("scale_dataframe scales numeric columns while preserving non-numeric columns", {
  df <- data.frame(a = c(1, 2, 3), b = c(10, 20, 30), label = c("x", "y", "z"))
  res <- scale_dataframe(df, method = "min_max")
  expect_equal(res$a, c(0, 0.5, 1))
  expect_equal(res$b, c(0, 0.5, 1))
  expect_equal(res$label, c("x", "y", "z"))
})
