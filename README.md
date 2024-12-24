Nama : Paskalia paulina beribin
NIM : 2415091044
Kelas : 1 DPS

DATA :
# Set seed untuk memastikan hasil yang konsisten
set.seed(123)

# Membuat data simulasi
n <- 100  # Jumlah observasi
x <- runif(n, 0, 10)  # Variabel independen (nilai acak antara 0 dan 10)
epsilon <- rnorm(n, mean = 0, sd = 1)  # Noise (error acak)
beta0 <- 2  # Intercept
beta1 <- 3  # Koefisien regresi (slope)

# Menghitung variabel dependen (y) dengan model linear
y <- beta0 + beta1 * x + epsilon

# Membuat data frame untuk analisis
data <- data.frame(x = x, y = y)

# Melakukan regresi linear sederhana
model <- lm(y ~ x, data = data)

# Menampilkan ringkasan hasil regresi
summary(model)

# Visualisasi data dan garis regresi
plot(data$x, data$y, main = "Regresi Linear Sederhana", 
     xlab = "Variabel Independen (x)", ylab = "Variabel Dependen (y)",
     pch = 19, col = "blue")
abline(model, col = "red", lwd = 2)
legend("topleft", legend = c("Data", "Garis Regresi"), 
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))


Uji asumsi (min 3) :
# Simulasi data
set.seed(123)
n <- 100
x <- rnorm(n, mean = 50, sd = 10) # Variabel independen
y <- 5 + 0.3 * x + rnorm(n, mean = 0, sd = 5) # Variabel dependen dengan noise

# Membuat model regresi linear sederhana
model <- lm(y ~ x)

# Ringkasan model
summary(model)

# Uji asumsi 1: Normalitas residual
# Residuals harus terdistribusi normal
library(ggplot2)
residuals <- model$residuals
ggplot(data = data.frame(residuals), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 15, color = "black", fill = "lightblue") +
  geom_density(color = "red") +
  labs(title = "Distribusi Residual", x = "Residuals", y = "Density")

# Uji asumsi 2: Homoskedastisitas
# Residuals memiliki varians yang sama di sepanjang nilai prediksi
fitted_values <- model$fitted.values
plot(fitted_values, residuals, main = "Homoskedastisitas", 
     xlab = "Fitted Values", ylab = "Residuals", pch = 20, col = "blue")
abline(h = 0, col = "red")

# Uji asumsi 3: Multikolinearitas
# Tidak relevan untuk regresi linear sederhana, tetapi untuk regresi ganda, 
# kita bisa menggunakan Variance Inflation Factor (VIF)
library(car)
vif_model <- vif(model) # Digunakan jika ada lebih dari satu prediktor

# Uji tambahan: Normalitas residual dengan uji statistik (Shapiro-Wilk Test)
shapiro.test(residuals)

# Catatan tambahan:
# Untuk regresi sederhana ini, asumsi multikolinearitas tidak berlaku karena hanya ada satu prediktor.
# Untuk regresi ganda, VIF biasanya digunakan untuk mendeteksi multikolinearitas.


Analisis :
# Simulasi data
set.seed(123)  # Untuk memastikan hasil simulasi yang sama setiap kali dijalankan
n <- 100  # Jumlah data

# Variabel independen (predictor)
x <- rnorm(n, mean = 50, sd = 10)  # Data random dengan distribusi normal

# Variabel dependen (response)
# Hubungan linear: y = 2.5 * x + 10 + error
y <- 2.5 * x + 10 + rnorm(n, mean = 0, sd = 20)

# Buat data frame
data <- data.frame(x, y)

# Analisis Regresi Linear
model <- lm(y ~ x, data = data)

# Output hasil
summary(model)

# Visualisasi
plot(data$x, data$y, main = "Regresi Linear Sederhana", xlab = "X", ylab = "Y", pch = 16, col = "blue")
abline(model, col = "red", lwd = 2)


Visualisasi :
# Set seed untuk memastikan hasil yang sama setiap kali dijalankan
set.seed(123)

# Simulasi data
n <- 100  # Jumlah data
x <- runif(n, 0, 10)  # Variabel independen (uniform antara 0 dan 10)
beta_0 <- 2           # Intercept
beta_1 <- 1.5         # Koefisien regresi
epsilon <- rnorm(n, mean = 0, sd = 1)  # Noise (normal distribusi)

# Variabel dependen
y <- beta_0 + beta_1 * x + epsilon

# Analisis regresi linear
model <- lm(y ~ x)

# Ringkasan model
summary(model)

# Visualisasi data dan garis regresi
plot(x, y, pch = 16, col = "blue", main = "Regresi Linear Sederhana",
     xlab = "Variabel Independen (x)", ylab = "Variabel Dependen (y)")
abline(model, col = "red", lwd = 2)  # Tambahkan garis regresi
legend("topleft", legend = c("Data", "Garis Regresi"), 
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1))


Interpretasi :
# 1. Membuat data simulasi
set.seed(123) # Untuk reproduksibilitas
n <- 100 # Jumlah data
x <- rnorm(n, mean = 50, sd = 10) # Variabel independen (misalnya umur)
y <- 3 + 0.5 * x + rnorm(n, mean = 0, sd = 5) # Variabel dependen (misalnya skor ujian)

# 2. Melakukan regresi linear sederhana
model <- lm(y ~ x)

# 3. Menampilkan ringkasan hasil regresi
summary(model)

# 4. Plot data dan garis regresi
plot(x, y, main = "Regresi Linear Sederhana", xlab = "X (Umur)", ylab = "Y (Skor Ujian)")
abline(model, col = "blue", lwd = 2)

# 5. Interpretasi hasil
cat("Koefisien Intersep:", coef(model)[1], "\n")
cat("Koefisien Slope:", coef(model)[2], "\n")
cat("R-squared:", summary(model)$r.squared, "\n")

