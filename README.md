# Data Mining in R — Modernized Architecture (Keystone Edition)

Bu depo, R diliyle veri madenciliği (Data Mining) ve makine öğrenmesi temel iş akışlarını **Keystone Engineering Framework** prensipleriyle (**Clean Code**, **Modüler Mimari**, **Savunmacı Programlama**, **Otomatik Birim Testleri** ve **CI/CD**) sunan üretim kalitesinde bir R projesi ve eğitim setidir.

## Öne Çıkan Özellikler ve Kapsam

1. **Değişken Ölçeklendirme ve Normalizasyon (`R/scaling.R`)**
   * Min-Max `[0, 1]` normalizasyonu (`scale_min_max`)
   * Z-Skoru (`N(0, 1)`) standartlaştırması (`scale_z_score`)
   * Ortalama normalizasyonu `[-1, +1]` (`scale_mean_norm`)
   * Sıfır varyans (`min == max` veya `sd == 0`) ve `NA` korumalı çok değişkenli tablo ölçeklendirme (`scale_dataframe`)
2. **Eksik Gözlem Analizi ve Değer Atama (`R/missing_data.R`)**
   * Sentinel değerlerin (`99`, `-999`) standart `NA` değerine dönüştürülmesi (`replace_sentinel_with_na`)
   * Değişken bazlı eksik gözlem oranı raporlaması (`summarize_missingness`)
   * Ortalama, medyan, gerçek istatistiksel tepe değer (`stat_mode`) ve sabit değer ataması (`impute_vector`)
   * Deterministik eksik gözlem simülasyonu (`inject_random_missingness`) ve MAE/RMSE doğruluk karşılaştırması (`evaluate_imputation_mae`)
3. **Tek ve Çok Değişkenli Aykırı Gözlem Analizi (`R/outlier_detection.R`)**
   * Tukey Boxplot IQR aykırı değer tespiti (`detect_iqr_outliers`) ve Winsorization/baskılama (`cap_iqr_outliers`)
   * Local Outlier Factor (LOF) yoğunluk tabanlı aykırı gözlem skorlama (`detect_lof_outliers`)
   * K-Means kümeleme ve Öklid uzaklığı (`sqrt(rowSums((X - C)^2))`) ile çok değişkenli aykırı gözlem tespiti (`detect_kmeans_outliers`)
4. **Naive Bayes Sınıflandırma ve Performans Değerlendirme (`R/carseats_naive_bayes.R`)**
   * `ISLR::Carseats` veri seti üzerinde tabakalı eğitim/test bölünmesi, Naive Bayes modelleme, Accuracy / Precision / Recall / Specificity / F1 / ROC-AUC hesaplaması ve `ggplot2` tabanlı Confusion Matrix ısı haritası
5. **Şarap Kalitesi Keşifçi Veri Analizi (`R/wine_exploration.R`)**
   * UCI Wine Quality veri seti üzerinde sıralı faktör (`ordered factor`) yapılandırması, filtreleme ve istatistiksel görselleştirme

## Dizin Yapısı

```text
Data-Mining/
├── .github/workflows/ci.yml          # GitHub Actions (lintr + testthat + pipeline testi)
├── R/                                # Saf, modüler ve roxygen2 belgeli R fonksiyonları
│   ├── utils_validation.R            # Girdi doğrulama ve deterministik stat_mode()
│   ├── scaling.R                     # Ölçeklendirme ve normalizasyon modülü
│   ├── missing_data.R                # Eksik veri analizi ve değer atama modülü
│   ├── outlier_detection.R           # IQR, Winsorization, LOF ve K-Means aykırı değer modülü
│   ├── wine_exploration.R            # UCI Şarap Kalitesi veri hazırlama modülü
│   └── carseats_naive_bayes.R        # Carseats Naive Bayes sınıflandırma ve görselleştirme
├── scripts/                          # Otomasyon ve CLI betikleri
│   ├── install_deps.R                # İzole paket kurulum betiği
│   └── run_carseats_pipeline.R       # Uçtan uca Carseats Naive Bayes çalıştırıcısı
├── notebooks/                        # Yeniden düzenlenmiş, temiz R Markdown eğitim defterleri
│   ├── 01_r_fundamentals_wine.Rmd
│   ├── 02_feature_scaling.Rmd
│   ├── 03_missing_data_analysis.Rmd
│   ├── 04_outlier_detection.Rmd
│   └── 05_naive_bayes_carseats.Rmd
├── tests/                            # testthat birim ve sınır durumu (edge-case) testleri
│   ├── testthat.R
│   └── testthat/
│       ├── test_scaling.R
│       ├── test_missing_data.R
│       ├── test_outlier_detection.R
│       └── test_naive_bayes.R
├── docs/                             # Keystone Engineering Framework belgeleri
│   ├── SPEC.md                       # Teknik spesifikasyon ve modül kontratları
│   ├── ARCHITECTURE.md               # Mimari tasarım ve karar kayıtları (ADR)
│   └── CODE_REVIEW_AUDIT.md          # Eski depodaki hataların kök neden incelemesi
├── reports/
│   └── legacy_carseats_report.pdf    # Arşivlenmiş orijinal analiz raporu
├── CONSTRAINTS.md                    # Proje kısıtları ve kod kalitesi kuralları
├── DESCRIPTION                       # Standart R paket/proje üstverisi
├── NAMESPACE                         # Dışa aktarılan fonksiyon bildirimleri
├── .lintr                            # Statik kod analizi yapılandırması
└── Makefile                          # Standart geliştirici komutları
```

## Hızlı Başlangıç

### 1. Bağımlılıkların Kurulumu

```bash
make setup
# veya doğrudan:
Rscript scripts/install_deps.R
```

### 2. Birim Testlerini ve Statik Kod Analizini Çalıştırma

```bash
make test
make lint
```

### 3. Carseats Naive Bayes Üretim Hattını Çalıştırma

```bash
make run
# veya doğrudan:
Rscript scripts/run_carseats_pipeline.R
```

### 4. R Markdown Defterlerini HTML Olarak Derleme

```bash
make render
```

## Eski Depoda Giderilen Kritik Hatalar (Keystone Audit Özeti)

Ayrıntılı kök neden analizi için [`docs/CODE_REVIEW_AUDIT.md`](docs/CODE_REVIEW_AUDIT.md) dosyasına bakabilirsiniz:

* **K-Means Öklid Uzaklığı Hatası (`aykiri_gozlem.Rmd`)**: `sqrt(rowSums(df, merkez_df)^2)` ifadesinde `merkez_df` ikinci argüman olarak `rowSums`'un `na.rm` parametresine düşüyordu. Gerçek Öklid uzaklığı `sqrt(rowSums((mat - assigned_centers)^2))` olarak düzeltildi.
* **Confusion Matrix Heatmap Hatası (`main.R`)**: Python `seaborn.heatmap` parametrelerinin (`annot = TRUE, fmt = "d", cmap = "Blues"`) R `stats::heatmap` fonksiyonuna verilmesi nedeniyle oluşan 18 grafiksel parametre uyarısı, `ggplot2::geom_tile()` tabanlı `plot_confusion_matrix_heatmap()` ile giderildi.
* **İstatistiksel Mod (`base::mode`) Hatası (`eksik_veri.Rmd`)**: Nesnenin veri tipini (`"numeric"`) döndüren `base::mode` yerine gerçek tepe değeri bulan `stat_mode()` yazıldı.
* **KNN Değişken Referansı Hatası (`eksik_veri.Rmd`)**: `knn_data` yerine `knn` fonksiyonunun `str(knn)` ile özetlenmesi düzeltildi.
* **Şarap Kalitesi Faktör Ezilmesi (`NEW_R_temel_komutlar.r`)**: `plyr::revalue` ile tüm kalite düzeylerinin `"Kalite1"` ve `"Kalite2"` olarak ezilmesi nedeniyle sıralı faktör seviyelerinin kaybolması sorunu giderildi.
* **Sıfır Varyans ve `mutate_all` Güncellemesi (`standardizasyon.Rmd`)**: Sabit sütunlarda sessizce `NaN` üreten ölçeklendirme fonksiyonlarına sıfır varyans kontrolü eklendi.

## Lisans

Bu proje [MIT Lisansı](LICENSE) altında lisanslanmıştır.
