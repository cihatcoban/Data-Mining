library(testthat)
for (f in list.files("../R", pattern = "\\.R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}
test_dir("testthat")
