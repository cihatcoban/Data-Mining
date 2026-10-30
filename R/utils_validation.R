#' Savunmacı Girdi Doğrulama ve Temel İstatistik Yardımcıları
#'
#' Keystone "No Silent Failures" ilkesi gereği tüm veri madenciliği
#' fonksiyonlarının kullandığı doğrulama ve yardımcı fonksiyonlar.

#' Sayısal vektör doğrulaması yapar
#'
#' @param x Kontrol edilecek nesne.
#' @param arg_name Hata mesajında gösterilecek argüman adı.
#' @param allow_na Eksik gözlemlere (`NA`) izin verilip verilmeyeceği.
#' @param min_length Minimum geçerli (NA olmayan) gözlem sayısı.
#' @return Görünmez olarak `TRUE` döner; aksi halde açık hata fırlatır.
#' @export
assert_numeric_vector <- function(x,
                                  arg_name = "x",
                                  allow_na = TRUE,
                                  min_length = 2L) {
  if (!is.numeric(x) || !is.atomic(x)) {
    stop(sprintf("`%s` sayısal (numeric) bir vektör olmalıdır.", arg_name), call. = FALSE)
  }
  if (!allow_na && anyNA(x)) {
    stop(sprintf("`%s` eksik gözlem (NA) içeremez.", arg_name), call. = FALSE)
  }
  valid_values <- x[!is.na(x)]
  if (length(valid_values) < min_length) {
    stop(
      sprintf(
        "`%s` en az %d adet geçerli (NA olmayan) gözlem içermelidir (mevcut: %d).",
        arg_name, min_length, length(valid_values)
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Data frame doğrulaması yapar
#'
#' @param df Kontrol edilecek tablo.
#' @param arg_name Hata mesajında gösterilecek argüman adı.
#' @param min_rows Minimum satır sayısı.
#' @param min_cols Minimum sütun sayısı.
#' @return Görünmez olarak `TRUE` döner; aksi halde açık hata fırlatır.
#' @export
assert_data_frame <- function(df,
                              arg_name = "df",
                              min_rows = 1L,
                              min_cols = 1L) {
  if (!is.data.frame(df)) {
    stop(sprintf("`%s` bir data.frame nesnesi olmalıdır.", arg_name), call. = FALSE)
  }
  if (nrow(df) < min_rows || ncol(df) < min_cols) {
    stop(
      sprintf(
        "`%s` en az %d satır ve %d sütun içermelidir (mevcut: %dx%d).",
        arg_name, min_rows, min_cols, nrow(df), ncol(df)
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' İstatistiksel Tepe Değer (Mode) Hesaplama
#'
#' R'daki `base::mode()` fonksiyonu nesnenin bellek tipini (`"numeric"`) döndürür.
#' Bu fonksiyon ise bir vektördeki en sık tekrar eden istatistiksel tepe değeri
#' deterministik olarak hesaplar.
#'
#' @param x Sayısal, karakter veya faktör vektörü.
#' @param na_rm Eksik değerlerin (`NA`) yok sayılıp sayılmayacağı.
#' @return Vektörle aynı tipte en sık tekrar eden tekil değer.
#' @export
stat_mode <- function(x, na_rm = TRUE) {
  if (length(x) == 0L) {
    stop("`x` boş bir vektör olamaz.", call. = FALSE)
  }
  vals <- if (na_rm) x[!is.na(x)] else x
  if (length(vals) == 0L) {
    stop("`x` içinde geçerli (NA olmayan) hiçbir gözlem bulunamadı.", call. = FALSE)
  }
  unique_vals <- unique(vals)
  counts <- tabulate(match(vals, unique_vals))
  unique_vals[which.max(counts)]
}
