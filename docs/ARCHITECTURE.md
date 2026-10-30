# Keystone Mimari Tasarım ve Karar Kayıtları (`/design`)

## 1. Katmanlı Mimari Yapısı

Proje, veri bilimi ve R yazılım mühendisliği en iyi pratiklerini (R Packages & Tidyverse Design Guide) takip ederek 4 ayrık katmana bölünmüştür:

1. **Çekirdek Kütüphane Katmanı (`R/`)**:
   * Durumsuz (stateless), saf fonksiyonlardan oluşur.
   * Global ortamı (`GlobalEnv`) kirletmez, paket kurulumu yapmaz, çalışma dizinini değiştirmez.
2. **Otomasyon ve CLI Katmanı (`scripts/`)**:
   * `install_deps.R`: Eksik paketleri kontrol edip kuran tekil kurulum betiği.
   * `run_carseats_pipeline.R`: Uçtan uca Carseats Naive Bayes modellemesini çalıştırıp metrikleri konsola ve grafik dosyalarına (`outputs/plots/`) kaydeden üretim betiği.
3. **Etkileşimli Eğitim ve Analiz Katmanı (`notebooks/`)**:
   * Numaralandırılmış (`01_` .. `05_`) R Markdown dosyaları, `R/` katmanındaki fonksiyonları kullanarak adım adım veri madenciliği kavramlarını açıklar.
4. **Doğrulama ve Kalite Güvence Katmanı (`tests/testthat/`, `.lintr`, `.github/workflows/ci.yml`)**:
   * Her modül için deterministik birim testleri ve statik kod analizi kuralları içerir.

## 2. Mimari Karar Kayıtları (ADR)

### ADR-001: R Paket/Proje Hibrit Yapısının Seçilmesi
* **Bağlam**: Eski depo kök dizine dağılmış `.Rmd` ve `.R` dosyalarından oluşuyordu.
* **Karar**: Standart `DESCRIPTION`, `NAMESPACE`, `R/` ve `tests/testthat/` dizin yapısı benimsendi.
* **Sonuç**: Hem `devtools::load_all()` / `testthat` ile standart bir R paketi gibi test edilebilir hem de `notebooks/` altındaki R Markdown dosyalarıyla doğrudan eğitim materyali olarak kullanılabilir.

### ADR-002: Görselleştirmede Taban `stats::heatmap` Yerine `ggplot2` Kullanımı
* **Bağlam**: Eski `main.R` içinde Python `seaborn.heatmap` parametreleri (`annot = TRUE, fmt = "d", cmap = "Blues"`) R'ın `stats::heatmap` fonksiyonuna geçirilmiş ve 18 çalışma zamanı uyarısı üretmişti.
* **Karar**: Karmaşıklık matrisi (Confusion Matrix) ve ROC eğrisi görselleştirmeleri `ggplot2` ile yeniden yazıldı.
* **Sonuç**: Hücre içi gözlem sayıları ve yüzdeleri hatasız, yayın kalitesinde (publication-ready) grafikler olarak üretilir.
