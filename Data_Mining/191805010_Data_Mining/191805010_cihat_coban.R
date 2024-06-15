#------------------------INSTALL PACKAGES----------------------

#install.packages("e1071")
#install.packages("pROC")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("psych")
#install.packages("caret")
#install.packages("rpart.plot")
#install.packages("ISLR")

#------------------------------ Kütüphaneler-------------------

library(e1071)  # e1071 paketi Naive Bayes fonksiyonunu bunun içinden gelecek
library(pROC)
library(dplyr)
library(ggplot2)
library(psych)
library(caret)
library(rpart.plot)
library(ISLR)
#----------------------------------- Data set--------------------------

data(Carseats)
df <- Carseats

#--------------------- Veri setini görselleştirme ve inceleme-------------------

str(df)
summary(df)
hist(df$Sales , main = "cihat df$sales histogramı")

#------------------------- Hedef değişkeni kategorik hale getirme------------------

df$Sales <- as.factor(ifelse(df$Sales <= 8, "Low", "High"))

# ------------------------------Veri setini eğitim ve test setlerine bölme--------------------

set.seed(123)
train_indeks <- createDataPartition(df$Sales, p = 0.8, list = FALSE, times = 1)
train <- df[train_indeks,]
test  <- df[-train_indeks,]
train_x <- train %>% select(-Sales)
train_y <- train$Sales
test_x <- test %>% select(-Sales)
test_y <- test$Sales

# ----------------------Naive Bayes modeli oluşturma-----------------------


nb_model <- naiveBayes(Sales ~ ., data = train)

# -------------------------------Modeli inceleme-----------------------

summary(nb_model)

# -----------------------Test seti üzerinde tahmin ---------------------

predicted_values <- predict(nb_model, test , type="raw")

#------------------------- Confusion Matrix oluşturma ------------------------

tb <- table(predict(nb_model, test ), test_y)
confusionMatrix(tb, positive = "High")

# ------------------------- ROC eğrisini çizme ----------------------------

roc_curve <- roc(ifelse(test$Sales == "High", 1, 0), predicted_values[, "High"])
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)


# -------------------------Confusion Matrix oluşturma -----------------------------

tb <- table(predict(nb_model, test), test_y)
conf_matrix <- as.matrix(tb)

# ------------------------ Görselleştirme ------------------------------

heatmap(conf_matrix, annot = TRUE, fmt = "d", cmap = "Blues",
        xlab = "Tahmin Edilen", ylab = "Gerçek Değer",
        main = "Confusion Matrix")

# --------------------Sınıflandırma Performans Metrikleri-----------------------

precision <- posPredValue(predict(nb_model, test), test_y, positive = "High")
recall <- sensitivity(predict(nb_model, test), test_y, positive = "High")
f1_score <- 2 * (precision * recall) / (precision + recall)

#-------------------- Metrikleri Yazdıralım ------------------------

cat("\nPrecision:", precision, "\n")
cat("Recall (Sensitivity):", recall, "\n")
cat("F1 Score:", f1_score, "\n")

