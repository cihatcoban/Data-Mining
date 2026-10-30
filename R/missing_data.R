#' Eksik Gözlem (Missing Data) Analizi ve Değer Atama Modülü
#'
#' Eski `eksik_veri.Rmd` dosyasındaki eksik veri sorgulama, sentinel dönüşümü,
#' basit atama (ortalama, medyan, mod, sabit) ve tahmine dayalı atama (KNN, RF)
#' performans karşılaştırma fonksiyonlarının temiz ve deterministik sürümü.

#' Kodlanmış (Sentinel) Eksik Değerleri NA'ya Dönüştürme
#'
#' Veri setlerinde `99`, `-999` veya `""` gibi özel kodlarla temsil edilen
#' eksik gözlemleri standart `NA` değerine dönüştürür.
#'
#' @param x Vektör veya `data.frame`.
#' @param sentinels `NA` olarak işaretlenecek değerler vektörü (örn. `c(99, -999)`).
#' @return Sentinel değerleri `NA` ile değiştirilmiş nesne.
#' @export
replace_sentinel_with_na <- function(x, sentinels = c(99, -999)) {
  if (length(sentinels) == 0L) {
    stop("`sentinels` en az bir değer içermelidir.", call. = FALSE)
  }
  if (is.data.frame(x)) {
    out <- x
    for (col_name in names(out)) {
      out[[col_name]][out[[col_name]] %in% sentinels] <- NA
    }
    return(out)
  }
  x[x %in% sentinels] <- NA
  x
}

#' Değişken Bazında Eksik Gözlem Özeti
#'
#' Her sütun için eksik gözlem sayısını, eksik gözlem oranını (`%`) ve tam
#' gözlem sayısını içeren yapılandırılmış bir özet tablosu döndürür.
#'
#' @param df İncelenecek `data.frame`.
#' @return Sütun bazlı eksik veri istatistiklerini içeren `data.frame`.
#' @export
summarize_missingness <- function(df) {
  assert_data_frame(df, arg_name = "df", min_rows = 1L, min_cols = 1L)
  n_rows <- nrow(df)
  na_counts <- vapply(df, function(col) sum(is.na(col)), integer(1))

  summary_df <- data.frame(
    variable = names(df),
    n_missing = as.integer(na_counts),
    n_complete = as.integer(n_rows - na_counts),
    pct_missing = round((na_counts / n_rows) * 100, 2),
    stringsAsFactors = FALSE
  )
  rownames(summary_df) <- NULL
  summary_df[order(-summary_df$n_missing, summary_df$variable), ]
}

#' Tek Değişkenli Basit Eksik Değer Atama (Imputation)
#'
#' Bir vektördeki `NA` gözlemleri ortalama (`"mean"`), medyan (`"median"`),
#' istatistiksel tepe değer (`"mode"`) veya sabit bir değer (`"constant"`) ile doldurur.
#' Eski koddaki `impute(df$Salary, mode)` hatasını `stat_mode()` ile düzeltir.
#'
#' @param x Eksik değer içeren vektör.
#' @param strategy Atama stratejisi: `"mean"`, `"median"`, `"mode"` veya `"constant"`.
#' @param fill_value `strategy = "constant"` seçildiğinde kullanılacak sabit değer.
#' @return Eksik gözlemleri doldurulmuş vektör.
#' @export
impute_vector <- function(x,
                          strategy = c("mean", "median", "mode", "constant"),
                          fill_value = NULL) {
  strategy <- match.arg(strategy)
  if (!anyNA(x)) {
    return(x)
  }
  valid_x <- x[!is.na(x)]
  if (length(valid_x) == 0L) {
    stop("Tüm gözlemleri NA olan bir vektöre değer ataması yapılamaz.", call. = FALSE)
  }

  replacement <- switch(
    strategy,
    mean = {
      assert_numeric_vector(x, arg_name = "x", allow_na = TRUE, min_length = 1L)
      mean(valid_x)
    },
    median = {
      assert_numeric_vector(x, arg_name = "x", allow_na = TRUE, min_length = 1L)
      stats::median(valid_x)
    },
    mode = stat_mode(valid_x, na_rm = TRUE),
    constant = {
      if (is.null(fill_value) || length(fill_value) != 1L) {
        stop("`strategy = 'constant'` için tekil bir `fill_value` belirtilmelidir.", call. = FALSE)
      }
      fill_value
    }
  )

  x[is.na(x)] <- replacement
  x
}

#' Deterministik Rastgele Eksik Gözlem Enjeksiyonu (Simülasyon)
#'
#' Imputation algoritmalarını (KNN, Random Forest vb.) gerçek değerlerle
#' karşılaştırmak için belirtilen sütunlara tekrarlanabilir (`seed`) şekilde
#' rastgele `NA` değerleri yerleştirir ve gerçek indeksleri saklar.
#'
#' @param df Giriş `data.frame` nesnesi.
#' @param counts_per_col Sütun adlarıyla eşleşen eksik gözlem sayıları (örn. `c(Hits = 7, Runs = 9)`).
#' @param seed Tekrarlanabilirlik için rastgelelik tohumu.
#' @return `data` (`NA` eklenmiş tablo) ve `masked_indices` (sütun bazlı gizlenen satır indeksleri) listesi.
#' @export
inject_random_missingness <- function(df, counts_per_col, seed = 42L) {
  assert_data_frame(df, arg_name = "df", min_rows = 2L, min_cols = 1L)
  if (is.null(names(counts_per_col)) || any(names(counts_per_col) == "")) {
    stop("`counts_per_col` isimlendirilmiş bir vektör olmalıdır, örn: c(Hits = 7, Runs = 9).", call. = FALSE)
  }

  set.seed(as.integer(seed))
  corrupted_df <- df
  masked_indices <- list()

  for (col_name in names(counts_per_col)) {
    if (!col_name %in% names(df)) {
      stop(sprintf("`%s` sütunu veri setinde bulunamadı.", col_name), call. = FALSE)
    }
    n_mask <- as.integer(counts_per_col[[col_name]])
    eligible_rows <- which(!is.na(df[[col_name]]))
    if (n_mask < 1L || n_mask > length(eligible_rows)) {
      stop(sprintf("`%s` için geçersiz maskeleme sayısı: %d", col_name, n_mask), call. = FALSE)
    }
    chosen_idx <- sort(sample(eligible_rows, size = n_mask, replace = FALSE))
    corrupted_df[chosen_idx, col_name] <- NA
    masked_indices[[col_name]] <- chosen_idx
  }

  list(
    data = corrupted_df,
    masked_indices = masked_indices
  )
}

#' Değer Atama (Imputation) Doğruluğunu Değerlendirme
#'
#' Gizlenen (`masked_indices`) gerçek gözlemler ile tahmin yoluyla doldurulan
#' gözlemler arasındaki Ortalama Hata (Mean Error), MAE ve RMSE metriklerini hesaplar.
#'
#' @param original_df Orijinal (eksiksiz) `data.frame`.
#' @param imputed_df Eksikleri doldurulmuş `data.frame`.
#' @param masked_indices `inject_random_missingness()` tarafından üretilen indeks listesi.
#' @return Her sütun için hata metriklerini içeren `data.frame`.
#' @export
evaluate_imputation_mae <- function(original_df, imputed_df, masked_indices) {
  assert_data_frame(original_df, arg_name = "original_df")
  assert_data_frame(imputed_df, arg_name = "imputed_df")

  rows <- lapply(names(masked_indices), function(col_name) {
    idx <- masked_indices[[col_name]]
    actual <- original_df[idx, col_name]
    predicted <- imputed_df[idx, col_name]
    diff_val <- actual - predicted

    data.frame(
      variable = col_name,
      n_evaluated = length(idx),
      mean_error = round(mean(diff_val, na.rm = TRUE), 4),
      mae = round(mean(abs(diff_val), na.rm = TRUE), 4),
      rmse = round(sqrt(mean(diff_val^2, na.rm = TRUE)), 4),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
