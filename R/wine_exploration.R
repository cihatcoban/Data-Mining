#' UCI Şarap Kalitesi (Wine Quality) Veri Hazırlama ve Keşifçi Analiz Modülü
#'
#' Eski `NEW_R_temel_komutlar.r` betiğindeki veri okuma, faktör seviyelendirme,
#' filtreleme ve özet istatistik adımlarının `plyr` bağımlılığından arındırılmış
#' ve seviye ezme hatası giderilmiş sürümü.

#' Şarap Veri Setini Hazırlama ve Faktör Yapılandırması
#'
#' Eski kodda `plyr::revalue` ile tüm kalite düzeyleri `"Kalite1"` ve `"Kalite2"`
#' olarak ezildiği için sıralı faktör (`Kalite3 < ... < Kalite9`) yapısı bozuluyordu.
#' Bu fonksiyon hem sıralı kalite düzeyini (`quality_ordered`) hem de ikili kalite
#' sınıfını (`quality_tier`) ayrı sütunlar olarak korur.
#'
#' @param wine_df Ham şarap veri çerçevesi (`quality` sütunu içermelidir).
#' @param row_prefix Satır adlarına eklenecek önek (varsayılan `"Sarap "`).
#' @return Zenginleştirilmiş ve temizlenmiş `data.frame`.
#' @export
prepare_wine_dataset <- function(wine_df, row_prefix = "Sarap ") {
  assert_data_frame(wine_df, arg_name = "wine_df", min_rows = 1L, min_cols = 2L)
  if (!"quality" %in% names(wine_df)) {
    stop("`wine_df` içinde `quality` sütunu bulunmalıdır.", call. = FALSE)
  }

  out <- wine_df
  rownames(out) <- paste0(row_prefix, seq_len(nrow(out)))

  raw_levels <- sort(unique(as.integer(as.character(out$quality))))
  ordered_labels <- paste0("Kalite", raw_levels)

  out$quality_ordered <- factor(
    paste0("Kalite", as.integer(as.character(out$quality))),
    levels = ordered_labels,
    ordered = TRUE
  )

  # İkili gruplama: 6 ve üzeri "Kalite_Yuksek", altı "Kalite_Standart"
  out$quality_binary <- factor(
    ifelse(as.integer(as.character(out$quality)) >= 6L, "Kalite2", "Kalite1"),
    levels = c("Kalite1", "Kalite2")
  )

  out
}
