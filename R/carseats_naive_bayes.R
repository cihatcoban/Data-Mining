#' Carseats Naive Bayes Sınıflandırma ve Performans Değerlendirme Modülü
#'
#' Eski `Data_Mining/191805010_Data_Mining/main.R` betiğinin modüler, tekrar
#' kullanılabilir ve `ggplot2` ısı haritası ile düzeltilmiş üretim sürümü.

#' Carseats Satış Değişkenini İkili Kategorik Hedefe Dönüştürme
#'
#' @param df `Sales` sütunu içeren `Carseats` veri çerçevesi.
#' @param threshold Satış eşik değeri (varsayılan `8`: `<= 8` ise `"Low"`, `> 8` ise `"High"`).
#' @return `Sales` sütunu faktöre dönüştürülmüş `data.frame`.
#' @export
binarize_carseats_sales <- function(df, threshold = 8.0) {
  assert_data_frame(df, arg_name = "df", min_rows = 2L, min_cols = 2L)
  if (!"Sales" %in% names(df)) {
    stop("`df` içinde `Sales` sütunu bulunmalıdır.", call. = FALSE)
  }
  if (!is.numeric(df$Sales)) {
    stop("`df$Sales` sayısal bir sütun olmalıdır.", call. = FALSE)
  }

  out <- df
  out$Sales <- factor(
    ifelse(out$Sales <= threshold, "Low", "High"),
    levels = c("Low", "High")
  )
  out
}

#' Tabakalı (Stratified) Eğitim ve Test Veri Bölünmesi
#'
#' @param df Hedef sütunu içeren `data.frame`.
#' @param target_col Hedef değişken adı (varsayılan `"Sales"`).
#' @param train_ratio Eğitim seti oranı (varsayılan `0.8`).
#' @param seed Deterministik bölünme için rastgelelik tohumu (varsayılan `123L`).
#' @return `train` ve `test` veri çerçevelerini içeren liste.
#' @export
split_carseats_data <- function(df,
                                target_col = "Sales",
                                train_ratio = 0.8,
                                seed = 123L) {
  assert_data_frame(df, arg_name = "df", min_rows = 10L, min_cols = 2L)
  if (!target_col %in% names(df)) {
    stop(sprintf("Hedef sütun `%s` veri setinde bulunamadı.", target_col), call. = FALSE)
  }
  if (train_ratio <= 0 || train_ratio >= 1) {
    stop("`train_ratio` (0, 1) açık aralığında olmalıdır.", call. = FALSE)
  }

  set.seed(as.integer(seed))
  if (requireNamespace("caret", quietly = TRUE)) {
    train_idx <- as.vector(
      caret::createDataPartition(df[[target_col]], p = train_ratio, list = FALSE, times = 1L)
    )
  } else {
    # Saf R tabakalı örnekleme yedeği
    target_vals <- df[[target_col]]
    idx_list <- lapply(split(seq_len(nrow(df)), target_vals), function(indices) {
      sample(indices, size = floor(length(indices) * train_ratio), replace = FALSE)
    })
    train_idx <- sort(unlist(idx_list, use.names = FALSE))
  }

  list(
    train = df[train_idx, , drop = FALSE],
    test = df[-train_idx, , drop = FALSE],
    train_indices = train_idx
  )
}

#' Naive Bayes Modeli Eğitimi ve Tahmin Üretimi
#'
#' Eski kodda `predict(nb_model, test)` 4 kez tekrar çağrılıyordu. Bu fonksiyon
#' modeli eğitir ve test seti üzerinde hem sınıf tahminlerini hem de olasılıkları
#' tek seferde üretip döndürür.
#'
#' @param train_df Eğitim `data.frame` nesnesi.
#' @param test_df Test `data.frame` nesnesi.
#' @param target_col Hedef değişken adı (varsayılan `"Sales"`).
#' @return `model`, `pred_class`, `pred_prob` ve `actual` içeren liste.
#' @export
train_carseats_naive_bayes <- function(train_df, test_df, target_col = "Sales") {
  assert_data_frame(train_df, arg_name = "train_df", min_rows = 5L, min_cols = 2L)
  assert_data_frame(test_df, arg_name = "test_df", min_rows = 2L, min_cols = 2L)

  if (!requireNamespace("e1071", quietly = TRUE)) {
    stop("Naive Bayes eğitimi için `e1071` paketi gereklidir.", call. = FALSE)
  }

  formula_obj <- stats::as.formula(paste(target_col, "~ ."))
  nb_model <- e1071::naiveBayes(formula_obj, data = train_df)

  pred_class <- stats::predict(nb_model, newdata = test_df, type = "class")
  pred_prob <- stats::predict(nb_model, newdata = test_df, type = "raw")

  list(
    model = nb_model,
    pred_class = pred_class,
    pred_prob = pred_prob,
    actual = test_df[[target_col]]
  )
}

#' İkili Sınıflandırma Performans Metriklerini Hesaplama
#'
#' Karmaşıklık matrisi (Confusion Matrix), Accuracy, Precision, Recall (Sensitivity),
#' Specificity, F1 Skoru ve ROC AUC değerlerini hesaplar.
#'
#' @param actual Gerçek sınıf etiketleri (`factor`).
#' @param pred_class Tahmin edilen sınıf etiketleri (`factor`).
#' @param pred_prob Pozitif sınıfa ait olasılık vektörü (opsiyonel, AUC için).
#' @param positive_class Pozitif sınıf etiketi (varsayılan `"High"`).
#' @return Metrikleri ve karmaşıklık matrisini içeren liste.
#' @export
evaluate_binary_classifier <- function(actual,
                                       pred_class,
                                       pred_prob = NULL,
                                       positive_class = "High") {
  actual <- as.factor(actual)
  pred_class <- factor(pred_class, levels = levels(actual))

  conf_table <- table(Predicted = pred_class, Actual = actual)
  neg_class <- setdiff(levels(actual), positive_class)[1]

  tp <- as.numeric(conf_table[positive_class, positive_class])
  fp <- as.numeric(conf_table[positive_class, neg_class])
  fn <- as.numeric(conf_table[neg_class, positive_class])
  tn <- as.numeric(conf_table[neg_class, neg_class])
  total <- tp + fp + fn + tn

  accuracy <- (tp + tn) / total
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  specificity <- if ((tn + fp) > 0) tn / (tn + fp) else 0
  f1_score <- if ((precision + recall) > 0) 2 * (precision * recall) / (precision + recall) else 0

  roc_obj <- NULL
  auc_val <- NA_real_
  if (!is.null(pred_prob) && requireNamespace("pROC", quietly = TRUE)) {
    binary_target <- ifelse(actual == positive_class, 1L, 0L)
    roc_obj <- pROC::roc(binary_target, as.numeric(pred_prob), quiet = TRUE)
    auc_val <- as.numeric(pROC::auc(roc_obj))
  }

  list(
    confusion_table = conf_table,
    metrics = data.frame(
      accuracy = round(accuracy, 4),
      precision = round(precision, 4),
      recall = round(recall, 4),
      specificity = round(specificity, 4),
      f1_score = round(f1_score, 4),
      auc = round(auc_val, 4),
      stringsAsFactors = FALSE
    ),
    roc_curve = roc_obj
  )
}

#' ggplot2 Tabanlı Confusion Matrix Isı Haritası (Heatmap)
#'
#' Eski `main.R:79` satırında 18 uyarıya sebep olan hatalı `heatmap(conf_matrix, annot = TRUE, ...)`
#' çağrısının yerine geçen, hücre içi gözlem sayılarını ve yüzdelerini gösteren
#' temiz `ggplot2` görselleştirmesi.
#'
#' @param conf_table `table(Predicted, Actual)` formatında karmaşıklık matrisi.
#' @param title Grafik başlığı.
#' @return `ggplot` nesnesi.
#' @export
plot_confusion_matrix_heatmap <- function(conf_table,
                                          title = "Confusion Matrix (Naive Bayes - Carseats)") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Görselleştirme için `ggplot2` paketi gereklidir.", call. = FALSE)
  }

  df_cm <- as.data.frame(conf_table, stringsAsFactors = FALSE)
  names(df_cm) <- c("Predicted", "Actual", "Count")
  total_n <- sum(df_cm$Count)
  df_cm$Label <- sprintf("%d\n(%.1f%%)", df_cm$Count, (df_cm$Count / total_n) * 100)

  ggplot2::ggplot(df_cm, ggplot2::aes(x = Actual, y = Predicted, fill = Count)) +
    ggplot2::geom_tile(color = "white", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = Label), size = 5, fontface = "bold", color = "#1E293B") +
    ggplot2::scale_fill_gradient(low = "#EFF6FF", high = "#3B82F6", name = "Gözlem\nSayısı") +
    ggplot2::labs(
      title = title,
      x = "Gerçek Değer (Actual)",
      y = "Tahmin Edilen (Predicted)"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      panel.grid = ggplot2::element_blank()
    )
}

#' ROC Eğrisi Görselleştirmesi
#'
#' @param roc_obj `pROC::roc` tarafından üretilen ROC nesnesi.
#' @param title Grafik başlığı.
#' @return Görünmez olarak `roc_obj` döner ve grafiği çizer.
#' @export
plot_roc_curve <- function(roc_obj, title = "ROC Curve - Carseats Naive Bayes") {
  if (is.null(roc_obj)) {
    stop("`roc_obj` NULL olamaz.", call. = FALSE)
  }
  auc_val <- round(as.numeric(pROC::auc(roc_obj)), 4)
  graphics::plot(
    roc_obj,
    main = sprintf("%s (AUC = %.4f)", title, auc_val),
    col = "#1D4ED8",
    lwd = 2.5
  )
  invisible(roc_obj)
}
