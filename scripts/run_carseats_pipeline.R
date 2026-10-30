#!/usr/bin/env Rscript
# Carseats Naive Bayes Sınıflandırma Uçtan Uca Üretim Hattı (CLI)

r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (f in r_files) {
  source(f, encoding = "UTF-8")
}

if (!requireNamespace("ISLR", quietly = TRUE)) {
  stop("Lütfen önce `Rscript scripts/install_deps.R` komutunu çalıştırın.", call. = FALSE)
}

message("1. Carseats veri seti yükleniyor ve hedef değişken dönüştürülüyor...")
data("Carseats", package = "ISLR", envir = environment())
df_prepared <- binarize_carseats_sales(Carseats, threshold = 8.0)

message("2. Veri seti eğitim (%80) ve test (%20) olarak tabakalı bölünüyor (seed = 123)...")
splits <- split_carseats_data(df_prepared, target_col = "Sales", train_ratio = 0.8, seed = 123L)

message("3. Naive Bayes modeli eğitiliyor ve test tahminleri üretiliyor...")
nb_fit <- train_carseats_naive_bayes(splits$train, splits$test, target_col = "Sales")

message("4. Sınıflandırma metrikleri ve ROC/AUC hesaplanıyor...")
eval_res <- evaluate_binary_classifier(
  actual = nb_fit$actual,
  pred_class = nb_fit$pred_class,
  pred_prob = nb_fit$pred_prob[, "High"],
  positive_class = "High"
)

cat("\n=== Confusion Matrix ===\n")
print(eval_res$confusion_table)

cat("\n=== Performans Metrikleri ===\n")
print(eval_res$metrics, row.names = FALSE)

output_plot_dir <- file.path("outputs", "plots")
if (!dir.exists(output_plot_dir)) {
  dir.create(output_plot_dir, recursive = TRUE)
}

cm_plot <- plot_confusion_matrix_heatmap(eval_res$confusion_table)
ggplot2::ggsave(
  filename = file.path(output_plot_dir, "carseats_confusion_matrix.png"),
  plot = cm_plot,
  width = 6,
  height = 5,
  dpi = 150
)

grDevices::png(file.path(output_plot_dir, "carseats_roc_curve.png"), width = 700, height = 600, res = 120)
plot_roc_curve(eval_res$roc_curve)
grDevices::dev.off()

message("[OK] Grafikler `outputs/plots/` dizinine kaydedildi.")
