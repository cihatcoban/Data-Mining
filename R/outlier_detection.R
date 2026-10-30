#' Tek ve Çok Değişkenli Aykırı Gözlem (Outlier) Analizi Modülü
#'
#' Eski `aykiri_gozlem.Rmd` dosyasındaki Boxplot IQR tespiti, baskılama
#' (Winsorization), Local Outlier Factor (LOF) ve K-Means Öklid uzaklığı
#' yöntemlerinin hatasız ve modüler sürümü.

#' Tukey Boxplot (IQR) Yöntemi ile Tek Değişkenli Aykırı Gözlem Tespiti
#'
#' `[Q1 - coef * IQR, Q3 + coef * IQR]` sınırları dışında kalan gözlemlerin
#' indekslerini, değerlerini ve çeyreklik sınırlarını döndürür.
#'
#' @param x Sayısal vektör.
#' @param coef IQR çarpan katsayısı (varsayılan `1.5`).
#' @return `indices`, `values`, `lower_bound`, `upper_bound` ve `fivenum` içeren liste.
#' @export
detect_iqr_outliers <- function(x, coef = 1.5) {
  assert_numeric_vector(x, arg_name = "x", allow_na = TRUE, min_length = 4L)
  if (!is.numeric(coef) || length(coef) != 1L || coef <= 0) {
    stop("`coef` pozitif bir sayı olmalıdır.", call. = FALSE)
  }

  five <- stats::fivenum(x, na.rm = TRUE)
  q1 <- five[2]
  q3 <- five[4]
  iqr_val <- q3 - q1
  lower_bound <- q1 - coef * iqr_val
  upper_bound <- q3 + coef * iqr_val

  is_outlier <- !is.na(x) & (x < lower_bound | x > upper_bound)
  indices <- which(is_outlier)

  list(
    indices = indices,
    values = x[indices],
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    fivenum = stats::setNames(five, c("min", "q1", "median", "q3", "max"))
  )
}

#' Aykırı Gözlemleri Baskılama (Winsorization / Capping) veya Ortalama Atama
#'
#' Tespit edilen aykırı gözlemleri alt/üst çeyreklik sınırlarına (`"cap"`),
#' çeyreklik değerlerine (`"hinge"`), ortalamaya (`"mean"`) veya `NA` değerine dönüştürür.
#'
#' @param x Sayısal vektör.
#' @param method Müdahale yöntemi: `"cap"` (sınırlara çekme), `"hinge"` (Q1/Q3 atama),
#'   `"mean"` (ortalama atama) veya `"na"` (`NA` yapma).
#' @param coef IQR çarpan katsayısı (varsayılan `1.5`).
#' @return Aykırı gözlemleri işlenmiş sayısal vektör.
#' @export
cap_iqr_outliers <- function(x,
                             method = c("cap", "hinge", "mean", "na"),
                             coef = 1.5) {
  method <- match.arg(method)
  info <- detect_iqr_outliers(x, coef = coef)
  if (length(info$indices) == 0L) {
    return(x)
  }

  out <- x
  if (method == "cap") {
    out[!is.na(out) & out < info$lower_bound] <- info$lower_bound
    out[!is.na(out) & out > info$upper_bound] <- info$upper_bound
  } else if (method == "hinge") {
    out[!is.na(out) & out < info$lower_bound] <- info$fivenum[["q1"]]
    out[!is.na(out) & out > info$upper_bound] <- info$fivenum[["q3"]]
  } else if (method == "mean") {
    out[info$indices] <- mean(x, na.rm = TRUE)
  } else if (method == "na") {
    out[info$indices] <- NA_real_
  }
  out
}

#' K-Means Kümeleme ve Öklid Uzaklığı ile Çok Değişkenli Aykırı Gözlem Tespiti
#'
#' Eski `aykiri_gozlem.Rmd:244` satırındaki `sqrt(rowSums(df, merkez_df)^2)` hatasını
#' düzeltir. Her gözlemin atandığı küme merkezine olan gerçek Öklid uzaklığını
#' `sqrt(rowSums((X - C)^2))` formülüyle hesaplar ve en uzak `top_n` gözlemi döndürür.
#'
#' @param df Yalnızca sayısal sütunlardan oluşan `data.frame`.
#' @param centers Küme sayısı (`k`, varsayılan `3L`).
#' @param top_n Seçilecek en aykırı gözlem sayısı (varsayılan `10L`).
#' @param seed Deterministik K-Means başlangıcı için rastgelelik tohumu.
#' @return `top_indices`, `top_outliers`, `distances`, `clusters` ve `centers` içeren liste.
#' @export
detect_kmeans_outliers <- function(df, centers = 3L, top_n = 10L, seed = 42L) {
  assert_data_frame(df, arg_name = "df", min_rows = centers + 1L, min_cols = 1L)
  if (!all(vapply(df, is.numeric, logical(1)))) {
    stop("`df` içindeki tüm sütunlar sayısal olmalıdır.", call. = FALSE)
  }
  if (anyNA(df)) {
    stop("`df` eksik gözlem (NA) içeremez; önce eksik verileri doldurun.", call. = FALSE)
  }

  set.seed(as.integer(seed))
  km_fit <- stats::kmeans(df, centers = as.integer(centers), nstart = 10L)

  mat <- as.matrix(df)
  assigned_centers <- km_fit$centers[km_fit$cluster, , drop = FALSE]

  # Düzeltilmiş Öklid uzaklığı: sqrt(sum((x_i - c_i)^2))
  distances <- sqrt(rowSums((mat - assigned_centers)^2))
  n_select <- min(as.integer(top_n), nrow(df))
  top_indices <- order(distances, decreasing = TRUE)[seq_len(n_select)]

  top_table <- df[top_indices, , drop = FALSE]
  top_table$cluster <- km_fit$cluster[top_indices]
  top_table$euclidean_distance <- distances[top_indices]

  list(
    top_indices = top_indices,
    top_outliers = top_table,
    distances = distances,
    clusters = km_fit$cluster,
    centers = km_fit$centers
  )
}

#' Local Outlier Factor (LOF) Tabanlı Çok Değişkenli Aykırı Gözlem Tespiti
#'
#' `DMwR2::lofactor` yüklüyse onu kullanır; yüklü değilse saf R ile k-en yakın
#' komşu yoğunluk oranına dayalı LOF skorlarını hesaplar (CRAN'dan kaldırılan
#' eski `DMwR` bağımlılığına takılmaz).
#'
#' @param df Sayısal sütunlardan oluşan `data.frame`.
#' @param k Komşu sayısı (varsayılan `5L`).
#' @param top_n Döndürülecek en yüksek LOF skorlu gözlem sayısı (varsayılan `5L`).
#' @return `scores`, `top_indices` ve `top_outliers` içeren liste.
#' @export
detect_lof_outliers <- function(df, k = 5L, top_n = 5L) {
  assert_data_frame(df, arg_name = "df", min_rows = k + 2L, min_cols = 1L)
  if (!all(vapply(df, is.numeric, logical(1)))) {
    stop("`df` içindeki tüm sütunlar sayısal olmalıdır.", call. = FALSE)
  }

  if (requireNamespace("DMwR2", quietly = TRUE)) {
    scores <- as.numeric(DMwR2::lofactor(df, k = as.integer(k)))
  } else {
    # Saf R yedek (fallback) k-komşu erişilebilirlik yoğunluğu LOF hesaplaması
    dist_mat <- as.matrix(stats::dist(df, method = "euclidean"))
    n <- nrow(dist_mat)
    k_int <- as.integer(k)
    k_dist <- numeric(n)
    neighbors <- vector("list", n)

    for (i in seq_len(n)) {
      ord <- order(dist_mat[i, ])
      ord <- ord[ord != i]
      nn_idx <- ord[seq_len(k_int)]
      neighbors[[i]] <- nn_idx
      k_dist[i] <- dist_mat[i, nn_idx[k_int]]
    }

    lrd <- numeric(n)
    for (i in seq_len(n)) {
      nn_idx <- neighbors[[i]]
      reach_dists <- pmax(k_dist[nn_idx], dist_mat[i, nn_idx])
      lrd[i] <- 1 / (mean(reach_dists) + 1e-12)
    }

    scores <- numeric(n)
    for (i in seq_len(n)) {
      nn_idx <- neighbors[[i]]
      scores[i] <- mean(lrd[nn_idx]) / lrd[i]
    }
  }

  n_select <- min(as.integer(top_n), nrow(df))
  top_indices <- order(scores, decreasing = TRUE)[seq_len(n_select)]
  top_table <- df[top_indices, , drop = FALSE]
  top_table$lof_score <- scores[top_indices]

  list(
    scores = scores,
    top_indices = top_indices,
    top_outliers = top_table
  )
}
