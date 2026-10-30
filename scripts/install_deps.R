#!/usr/bin/env Rscript
# Keystone Bağımlılık Kurulum Betiği
# İş mantığı modüllerinden (R/) ve R Markdown dosyalarından tamamen izole edilmiştir.

required_packages <- c(
  "dplyr",
  "ggplot2",
  "caret",
  "e1071",
  "pROC",
  "ISLR",
  "mice",
  "VIM",
  "Hmisc",
  "DMwR2",
  "missForest",
  "testthat",
  "lintr",
  "rmarkdown",
  "knitr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) == 0L) {
  message("[OK] Tüm gerekli R paketleri zaten kurulu.")
} else {
  message("[INFO] Eksik paketler kuruluyor: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
