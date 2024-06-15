
# Calisma klasoru belirlenir
# Menuden sirasiyla Session -> Set Working Directory -> To Source File Location
calismaKlasoru <- getwd()
calismaKlasoru
 setwd(calismaKlasoru)

# Veri dosyasi okunur - internette herhangi bir urlden
veriDosyasi <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
sarap <- read.csv(url(veriDosyasi), header = TRUE, sep=";")

# Veri dosyasi okunur - bilgisayardan veriDosyasi degiskenine dosya yolunu vermelisiniz!!!
# veriDosyasi <- "winequality-white.csv"
# sarap <- read.csv(veriDosyasi, header = TRUE, sep=";", stringsAsFactors = TRUE)

# Veri setinin boyut adlari kontrol edilir 
dimnames(sarap)
rownames(sarap) 
colnames(sarap)

# Veri setinde satir adlari degistirilir (Sarap 1, Sarap 2, ..., Sarap 1000 seklinde) 
rownames(sarap) <- paste0("Sarap ", seq(from = 1, to = nrow(sarap), by = 1))
rownames(sarap)

# Hedef nitelik factor veri tipine donusturulur (factor R dilindeki kategorik degiskenler icin kullanilir)
sarap$quality <- as.factor(sarap$quality)

# Hedef niteligin kategorileri
levels(sarap$quality)

# Hedef niteligin frekans degerleri
table(sarap$quality)

# Hedef nitelikteki kategorileri yeniden adlandirma
install.packages("plyr") # Eger bilgisayarda plyr paketi yuklu degilse, bu kod yardımı ile yuklenir
library(plyr) # Calisma icinden paket cagirilir
sarap$quality <- revalue(sarap$quality, c("3"="Kalite1", "4"="Kalite1", "5"="Kalite1", "6"="Kalite2", "7"="Kalite1","8"="Kalite1", "9"="Kalite1")) 
levels(sarap$quality)

# Frekans tablosu elde edilir 
table(sarap$quality)

# Referans deger degistirme
denemeDegisken <- sarap$quality # Hedef niteligi bir degiskende yedeklenir
table(denemeDegisken)
a	<- relevel(denemeDegisken, ref=c("Kalite2")) 
table(a)
b	<- factor(denemeDegisken, ordered = TRUE) # Kategorik degiskenin sirali kategorik oldugunu belirtmek icin
table(b) 
b # Kod satiri calistiginda cikan Levels: Kalite3 < Kalite4 < Kalite5 < Kalite6 < Kalite7 < Kalite8 < Kalite9 ifadesi kategorilerin sirali oldugunu gostermektedir

# Eleman cagirma 
sarap[,4] # Dorduncu sutun
sarap[[4]] # Dorduncu sutun
sarap[3000,] # Ucbininci satir ve ilgili satirlara ait tum sutun degerleri
sarap[1:30,] # Bir ile otuz arasindaki tum satirlar ve ilgili satirlara ait tum sutun degerleri
sarap[1:30,2:4] # Bir ile otuz arasindaki tum satirlar ve ilgili satirlara ait ikinci, ucuncu ve dorduncu sutun degerleri
sarap[,c(2,8)] # Tum satirlarin sadece ikinci ve sekizinci sutunlari
sarap[,-c(2,8)] # Verisetinden ikinci ve sekizinci sutunlar atilir

# Satir/sutun ekleme islemleri 
x <- sarap[3:6,]
y <- sarap[4000,] 
yeniVeriSeti <- rbind(x, y) # Satir ekleme

ekSutun <- seq(from = 1, to = 5, by = 1) 
yeniVeriSeti <- cbind(yeniVeriSeti, ekSutun) # satir ekleme
colnames(yeniVeriSeti)[13] <- "Siralama" # Sutun adi degistirme

# Cesitli kontrol islemleri
z <- c("Denizhan", "", "Demirkol") 
is.na(z)
z <- c("Denizhan", NA, "Demirkol") 
is.na(z)
is.data.frame(z) 
is.vector(z)

# Belirli bir kurala gore elemanlari cagirma
t <- sarap[which(sarap$density > 0.995 & sarap$density < 1),] # Sarap yogunlugu 0.995 ile 1 arasindaki tüm örnekler

# Siralama
k1 <- sarap$chlorides[1:30] 
k1
(k2 <- sort(k1, decreasing = FALSE)) # kucukten buyuge
(k2 <- sort(k1, decreasing = TRUE)) # buyukten kucuge

k3 <- sarap[1:10, c(3,4,5)]
k3
(k4 <- k3[order(k3$citric.acid),])  # bir data.frame_in tek bir sutuna gore siralanmasi
(k5 <- k3[order(k3$residual.sugar, k3$citric.acid,  k3$chlorides),])  # bir data.frame_in birden fazla sutuna gore siralanmasi

# Veri setine ait ozet bilgi 
summary(sarap)

# Cesitli matematiksel islemler 
log(3)
log10(2) 
exp(1.098612)
max(sarap$fixed.acidity) # maksimum 
min(sarap$fixed.acidity) # minimum 
sum(sarap$fixed.acidity) # toplam 
mean(sarap$fixed.acidity) # ortalama 
median(sarap$fixed.acidity) # ortanca deger 
var(sarap$fixed.acidity) # varyans 
sd(sarap$fixed.acidity) # standart sapma
fivenum(sarap$fixed.acidity) # minimum, lower-hinge, median, upper-hinge, maximum
round(summary(sarap$fixed.acidity),1) # yuvarlama

# Grafik cizimleri

# Serpilme diyagrami 2x2
plot(sarap[,c(5,6)])
# Serpilme diyagrami 3x3
plot(sarap[,c(5:7)])

# Kutu Grafiği (Box and Whisker) 
boxplot(citric.acid~quality, data=sarap, xlab="Sarap Kalitesi", ylab="Sitrik Asit", main="Sarap Kalitesi ve Sitrik Asit Degerleri")

# Histogram
hist(sarap$sulphates, col="red", main="Saraptaki Sulfata ait Histogram", xlab = "Saraptaki Sulfat Orani", ylab ="Frekans")

# Pasta grafigi
pie(table(sarap$quality), main="Saraplarin Kalitesine Gore Dagilimi", col = c("red", "yellow", "green", "orange", "blue", "pink", "cyan"))

# KAYNAKLAR
# Bu dokumanda bulunan alistirmalar icin UCI Machine Learning Repository'deki Wine Quality veri seti kullanilmistir. Veri setine https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv baglantisindan erisilebilir.
# Bache, K., & Lichman, M. (2013). UCI machine learning repository. http://archive.ics.uci.edu/ml/
# Balaban, M. E. ve Kartal, E. (2018). Veri Madenciliği ve Makine Öğrenmesi Temel Algoritmaları ve R Dili ile Uygulamaları (2. bs.). Beyoğlu, İstanbul: Çağlayan Kitabevi. (Dokuman olusturulurken kullanilan temel kaynak)
# Cortez, P., Cerdeira, A., Almeida, F., Matos, T. and Reis, J. Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553, 2009.
# Wickham, H. (2011). The Split-Apply-Combine Strategy for Data Analysis. Journal of Statistical Software, 40(1), 1–29.