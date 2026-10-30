# Keystone Çok Mercekli Kod İnceleme ve Kök Neden Raporu (`/review` & `/debug`)

Eski `Data-Mining-main` deposu **Keystone Engineering Framework** kapsamındaki uzman perspektiflerle (*Architect*, *Silent Failure Hunter*, *Code Reviewer*, *Test Engineer*) denetlenmiş ve aşağıdaki kritik bulgular tespit edilerek giderilmiştir.

## Kritik Hata Bulguları (Severity: HIGH)

| # | Dosya ve Satır (Eski) | Tespit Edilen Hata / Kök Neden | Yeni Çözüm (Modernize Edilen Modül) |
| :--- | :--- | :--- | :--- |
| **1** | `aykiri_gozlem.Rmd:244` | `sqrt(rowSums(df, merkez_df)^2)` ifadesinde `merkez_df` ikinci argüman olarak `rowSums`'un `na.rm` parametresine düşmüştür! Noktaların küme merkezlerine olan Öklid uzaklığı yerine satır toplamlarının karesi hesaplanmıştır. | `R/outlier_detection.R` içinde `detect_kmeans_outliers()` fonksiyonunda gerçek Öklid uzaklığı `sqrt(rowSums((mat - centers_expanded)^2))` olarak düzeltildi. |
| **2** | `Data_Mining/.../main.R:79-81` | `heatmap(conf_matrix, annot = TRUE, fmt = "d", cmap = "Blues")` çağrısında Python `seaborn` argümanları R `stats::heatmap` fonksiyonuna verilmiş, `rapor.pdf` Sayfa 3'te görüldüğü üzere **18 adet grafiksel parametre uyarısı** ve bozuk bir ısı haritası üretmiştir. | `R/carseats_naive_bayes.R` içinde `plot_confusion_matrix_heatmap()` fonksiyonu `ggplot2::geom_tile()` ve `geom_text()` ile yeniden yazıldı. |
| **3** | `eksik_veri.Rmd:270-271` | `knn_data <- knnImputation(df, k = 5)` sonrasında `str(knn)` ve `summary(knn)` çağrılarak veri seti yerine `knn` fonksiyon nesnesi özetlenmiştir. | `notebooks/03_missing_data_analysis.Rmd` ve `R/missing_data.R` içinde doğru veri nesnesi (`knn_data`) doğrulaması eklendi. |
| **4** | `eksik_veri.Rmd:248` | `impute(df$Salary, mode)` çağrısında R'ın veri saklama tipini (`"numeric"`) döndüren `base::mode` fonksiyonu kullanılmıştır; R'da yerleşik istatistiksel tepe değer fonksiyonu yoktur. | `R/utils_validation.R` içinde deterministik `stat_mode()` fonksiyonu yazıldı ve `impute_vector(..., strategy = "mode")` içine entegre edildi. |

## Orta ve Düşük Seviye Bulgular (Severity: MEDIUM / LOW)

| # | Dosya (Eski) | Tespit Edilen Sorun | Yeni Çözüm |
| :--- | :--- | :--- | :--- |
| **5** | `NEW_R_temel_komutlar.r:37-50` | `plyr::revalue` ile 6 dışındaki tüm şarap kalitesi seviyeleri `"Kalite1"` yapılmış, hemen ardından gelen yorum satırında `Kalite3 < ... < Kalite9` çıktısı beklendiği yazılmıştır. Ayrıca `plyr` paketi emekliye ayrılmıştır. | `R/wine_exploration.R` içinde `prepare_wine_dataset()` fonksiyonu ile hem orijinal sıralı kalite seviyeleri (`Kalite_3 < ... < Kalite_9`) hem de ikili kalite gruplaması (`Standart` vs `Yuksek`) birbirini ezmeden ayrı sütunlar olarak yapılandırıldı. |
| **6** | `standardizasyon.Rmd:18-46, 75` | `st_sifir_bir`, `st_z` ve `bb` fonksiyonlarında `na.rm = TRUE` ve sıfır varyans (`max(x) == min(x)` veya `sd(x) == 0`) kontrolü yoktu; sabit sütunlarda sessizce `NaN` üretiyordu. Ayrıca kullanımdan kaldırılan `mutate_all()` kullanılmıştı. | `R/scaling.R` içinde sıfır varyans ve `NA` korumaları eklendi; çok değişkenli ölçeklendirme `dplyr::across(dplyr::where(is.numeric), ...)` ile modernize edildi. |
| **7** | Tüm `.R` ve `.Rmd` dosyaları | Kod bloklarının içinde doğrudan `install.packages(...)`, `setwd(getwd())` ve etkileşimli `View()` çağrıları bulunuyordu. Ayrıca `DMwR` paketi CRAN'dan kaldırıldığı için kurulum hatası veriyordu. | Tüm paket kurulumları `scripts/install_deps.R` dosyasına taşındı; `DMwR` yerine `DMwR2` kullanıldı; rastgele işlemler `seed` parametresi ile deterministik hale getirildi. |
