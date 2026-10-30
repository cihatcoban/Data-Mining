#' Değişken Ölçeklendirme ve Normalizasyon Modülü
#'
#' Eski `standardizasyon.Rmd` dosyasındaki dönüşümlerin sıfır varyans ve
#' eksik veri (`NA`) korumalı, modern `dplyr::across` uyumlu sürümü.

#' Min-Max [0, 1] Normalizasyonu
#'
#' Sayısal bir vektörü `[feature_range[1], feature_range[2]]` aralığına doğrusal
#' olarak ölçeklendirir. Sabit değerli vektörlerde sessizce `NaN` üretmek yerine
#' açık hata fırlatır.
#'
#' @param x Sayısal vektör.
#' @param na_rm Eksik gözlemlerin (`NA`) hesaplamada yok sayılıp sayılmayacağı.
#' @param feature_range Uzunluğu 2 olan hedef aralık vektörü (varsayılan `c(0, 1)`).
#' @return Ölçeklendirilmiş sayısal vektör.
#' @export
scale_min_max <- function(x, na_rm = TRUE, feature_range = c(0, 1)) {
  assert_numeric_vector(x, arg_name = "x", allow_na = na_rm, min_length = 2L)
  if (!is.numeric(feature_range) || length(feature_range) != 2L || feature_range[1] >= feature_range[2]) {
    stop("`feature_range` artan sırada 2 sayısal değer içermelidir, örn: c(0, 1).", call. = FALSE)
  }

  x_min <- min(x, na.rm = na_rm)
  x_max <- max(x, na.rm = na_rm)
  span <- x_max - x_min

  if (is.na(span) || span == 0) {
    stop("Sabit değerli (min == max) bir vektöre Min-Max normalizasyonu uygulanamaz.", call. = FALSE)
  }

  unit_scaled <- (x - x_min) / span
  unit_scaled * (feature_range[2] - feature_range[1]) + feature_range[1]
}

#' Z-Skoru (Standart Normal) Dönüşümü
#'
#' Sayısal bir vektörü ortalaması 0, standart sapması 1 olacak şekilde standartlaştırır.
#'
#' @param x Sayısal vektör.
#' @param na_rm Eksik gözlemlerin (`NA`) hesaplamada yok sayılıp sayılmayacağı.
#' @return Z-skoru dönüştürülmüş sayısal vektör.
#' @export
scale_z_score <- function(x, na_rm = TRUE) {
  assert_numeric_vector(x, arg_name = "x", allow_na = na_rm, min_length = 2L)

  x_mean <- mean(x, na.rm = na_rm)
  x_sd <- stats::sd(x, na.rm = na_rm)

  if (is.na(x_sd) || x_sd == 0) {
    stop("Standart sapması 0 olan bir vektöre Z-skoru dönüşümü uygulanamaz.", call. = FALSE)
  }

  (x - x_mean) / x_sd
}

#' Ortalama Normalizasyonu [-1, +1]
#'
#' Sayısal bir vektörü ortalaması etrafında `[-1, +1]` aralığına ölçeklendirir:
#' `(x - mean(x)) / max(abs(x - mean(x)))`.
#'
#' @param x Sayısal vektör.
#' @param na_rm Eksik gözlemlerin (`NA`) hesaplamada yok sayılıp sayılmayacağı.
#' @return `[-1, +1]` aralığına ölçeklendirilmiş sayısal vektör.
#' @export
scale_mean_norm <- function(x, na_rm = TRUE) {
  assert_numeric_vector(x, arg_name = "x", allow_na = na_rm, min_length = 2L)

  x_mean <- mean(x, na.rm = na_rm)
  centered <- x - x_mean
  max_dev <- max(abs(centered), na.rm = na_rm)

  if (is.na(max_dev) || max_dev == 0) {
    stop("Maksimum mutlak sapması 0 olan bir vektöre ortalama normalizasyonu uygulanamaz.", call. = FALSE)
  }

  centered / max_dev
}

#' Veri Çerçevesindeki Sayısal Sütunları Toplu Ölçeklendirme
#'
#' Eski `mutate_all()` yerine modern `dplyr::across()` yapısını kullanarak bir
#' `data.frame` içindeki tüm sayısal sütunlara seçilen ölçeklendirme yöntemini uygular.
#'
#' @param df Giriş `data.frame` nesnesi.
#' @param method Ölçeklendirme yöntemi: `"min_max"`, `"z_score"` veya `"mean_norm"`.
#' @param cols Ölçeklendirilecek sütun adları (varsayılan `NULL`: tüm sayısal sütunlar).
#' @param na_rm Eksik gözlemlerin hesaplamada yok sayılıp sayılmayacağı.
#' @return Ölçeklendirilmiş yeni `data.frame`.
#' @export
scale_dataframe <- function(df,
                            method = c("min_max", "z_score", "mean_norm"),
                            cols = NULL,
                            na_rm = TRUE) {
  assert_data_frame(df, arg_name = "df", min_rows = 2L, min_cols = 1L)
  method <- match.arg(method)

  scaler_fn <- switch(
    method,
    min_max = function(col) scale_min_max(col, na_rm = na_rm),
    z_score = function(col) scale_z_score(col, na_rm = na_rm),
    mean_norm = function(col) scale_mean_norm(col, na_rm = na_rm)
  )

  target_cols <- if (is.null(cols)) {
    names(df)[vapply(df, is.numeric, logical(1))]
  } else {
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0L) {
      stop(sprintf("Tabloda bulunmayan sütunlar: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
    cols
  }

  if (length(target_cols) == 0L) {
    stop("Ölçeklendirilecek sayısal sütun bulunamadı.", call. = FALSE)
  }

  out <- df
  for (col_name in target_cols) {
    out[[col_name]] <- scaler_fn(out[[col_name]])
  }
  out
}
