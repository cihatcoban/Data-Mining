# Keystone Teknik Spesifikasyon (`/spec`)

## 1. Problem Tanımı ve Amaç

Mevcut `Data-Mining` deposu; üniversite ders notları ve ödev çıktılarından oluşan, dağınık dizin yapısına sahip, global değişken kirliliği barındıran ve çalışma zamanı hataları (sessiz `NaN` üretimi, yanlış Öklid uzaklığı formülü, Python `seaborn` argümanlarının R `heatmap` fonksiyonuna verilmesi, `base::mode` hatası) içeren eski bir kod tabanıydı.

Bu modernizasyonun amacı, depoyu **Keystone Engineering Framework** standartlarında, üretim kalitesinde (Senior Developer seviyesi) modüler bir R veri madenciliği kütüphanesi ve eğitim/analiz paketine dönüştürmektir.

## 2. Kapsam ve Modül Sözleşmeleri (Data Contracts)

| Modül (`R/`) | Kaynak (Eski Dosya) | Sorumluluk ve Kontrat |
| :--- | :--- | :--- |
| `R/utils_validation.R` | Tüm dosyalar | Tip doğrulama, `NA` ve sıfır varyans kontrolleri, deterministik istatistiksel tepe değer (`stat_mode`). |
| `R/scaling.R` | `standardizasyon.Rmd` | Min-Max `[0, 1]`, Z-skoru (`N(0, 1)`), Ortalama Normalizasyonu `[-1, +1]` ve `dplyr::across` tabanlı çok değişkenli tablo ölçeklendirme. |
| `R/missing_data.R` | `eksik_veri.Rmd` | Eksik veri örüntü özeti, sentinel (`99` vb.) değerlerin `NA`'ya çevrilmesi, ortalama/medyan/mod/sabit atama, deterministik eksik veri simülasyonu ve MAE/RMSE hata değerlendirmesi. |
| `R/outlier_detection.R` | `aykiri_gozlem.Rmd` | Boxplot IQR tabanlı tek değişkenli aykırı gözlem tespiti, baskılama (Winsorization/capping), LOF skorlama ve düzeltilmiş K-Means Öklid uzaklığı (`sqrt(rowSums((X - C)^2))`) ile çok değişkenli aykırı gözlem tespiti. |
| `R/wine_exploration.R` | `NEW_R_temel_komutlar.r` | UCI Beyaz Şarap Kalitesi veri setinin güvenli okunması, sıralı faktör (`ordered factor`) dönüşümü, yoğunluk filtreleme ve özet istatistikler. |
| `R/carseats_naive_bayes.R` | `Data_Mining/191805010_Data_Mining/main.R` | `ISLR::Carseats` satış hedefinin ikili sınıflandırılması (`Low` vs `High`), tabakalı eğitim/test bölünmesi, Naive Bayes eğitimi, ROC/AUC hesabı ve `ggplot2` tabanlı Confusion Matrix ısı haritası. |

## 3. Kabul Kriterleri (Definition of Done)

1. Eski depodaki 8 kritik hata ve kod kokusu (code smell) kök nedenleriyle giderilmiş olmalı.
2. Tüm fonksiyonlar `roxygen2` standartlarında belgelenmiş ve savunmacı (defensive) girdi kontrollerine sahip olmalı.
3. `tests/testthat/` altında her modülün normal çalışma ve sınır durumlarını (edge cases) doğrulayan birim testleri bulunmalı.
4. Tek komutla çalıştırılabilir CLI hattı (`scripts/run_carseats_pipeline.R`) ve CI iş akışı (`.github/workflows/ci.yml`) hazır olmalı.
