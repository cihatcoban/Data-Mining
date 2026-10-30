test_that("binarize_carseats_sales creates Low and High factor levels", {
  df <- data.frame(Sales = c(5.0, 8.0, 8.1, 12.0), Price = c(100, 110, 120, 130))
  res <- binarize_carseats_sales(df, threshold = 8.0)
  expect_s3_class(res$Sales, "factor")
  expect_equal(as.character(res$Sales), c("Low", "Low", "High", "High"))
})

test_that("evaluate_binary_classifier computes exact precision, recall, and F1", {
  actual <- factor(c("High", "High", "Low", "Low"), levels = c("Low", "High"))
  pred   <- factor(c("High", "Low",  "Low", "Low"), levels = c("Low", "High"))
  res <- evaluate_binary_classifier(actual, pred, positive_class = "High")

  expect_equal(res$metrics$accuracy, 0.75)
  expect_equal(res$metrics$precision, 1.0)
  expect_equal(res$metrics$recall, 0.5)
  expect_equal(res$metrics$f1_score, round(2 * 0.5 / 1.5, 4))
})
