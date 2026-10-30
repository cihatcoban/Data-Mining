# Keystone Project Constraints (`CONSTRAINTS.md`)

Bu proje **Keystone Engineering Framework** prensipleri doğrultusunda yapılandırılmıştır. Tüm katkılar ve modüller aşağıdaki katı mühendislik sınırlarına uymak zorundadır.

## 1. Mimari ve Dosyalama Kuralları

* **Saf Fonksiyon Katmanı (`R/`)**: Tüm veri ön işleme, ölçeklendirme, eksik gözlem doldurma, aykırı değer analizi ve modelleme mantığı `R/` dizini altında tek sorumluluk (Single Responsibility) ilkesine uygun, yan etkisiz (side-effect-free) fonksiyonlar olarak yazılmalıdır.
* **Yürütme ve Kurulum Ayrımı (`scripts/`)**: `install.packages()` veya `setwd()` çağrıları **kesinlikle** `R/` modülleri veya `.Rmd` kod blokları (chunk) içinde yer alamaz. Paket kurulumları yalnızca `scripts/install_deps.R` içinde izole edilir.
* **Tekrarlanabilir Raporlama (`notebooks/`)**: R Markdown dosyaları yalnızca `R/` altındaki test edilmiş fonksiyonları çağırarak analiz ve görselleştirme sunar; iş mantığını kopyalamaz (DRY prensibi).

## 2. Kod Kalitesi ve Güvenlik İnvaryantları (No Silent Failures)

* **Sıfıra Bölünme ve Sabit Vektör Koruması**: Ölçeklendirme fonksiyonları (`scale_min_max`, `scale_z_score`, `scale_mean_norm`) sabit değerli (varyansı `0` olan) veya tümü `NA` olan vektörlerde sessizce `NaN`/`Inf` üretmemeli; açık ve anlamlı bir hata (`stop()`) fırlatmalıdır.
* **Deterministik Rastgelelik**: Rastgele örneklem (`sample`), veri bölme (`createDataPartition`), K-Means (`kmeans`) veya Random Forest (`missForest`) içeren her işlem açık bir `seed` parametresi almalıdır.
* **İstatistiksel Mod Doğruluğu**: R'daki `base::mode()` fonksiyonu nesnenin veri tipini (`"numeric"`) döndürür; istatistiksel tepe değer (mod) için her zaman `stat_mode()` fonksiyonu kullanılmalıdır.
* **Modern API Kullanımı**: Emekliye ayrılmış `plyr::revalue`, `DMwR` (CRAN'dan kaldırıldı) veya `dplyr::mutate_all` yerine modern `dplyr::across()`, `DMwR2` ve taban R / `ggplot2` API'leri kullanılmalıdır.
* **Görselleştirme Bütünlüğü**: R `stats::heatmap()` fonksiyonuna Python `seaborn` parametreleri (`annot`, `fmt`, `cmap`) geçirilemez; karmaşıklık matrisi görselleştirmeleri `ggplot2::geom_tile()` ile üretilmelidir.
