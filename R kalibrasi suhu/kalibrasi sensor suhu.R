# 1. Load Library
library(readxl)
library(ggplot2)

# 2. Baca File Excel
data <- read_excel(file.choose())

# 3. Cek nama kolom
colnames(data)

# (Pastikan nama kolom: Alat_Ukur dan Sensor)

# 4. Hitung Error
data$Error <- data$sensor - data$alat

# 5. Hitung Persentase Error
data$Persen_Error <- 
  ((data$sensor - data$alat) /
     data$alat) * 100

# 6. Hitung Bias
bias <- mean(data$Error)

# 7. Hitung RMSE
rmse <- sqrt(mean(data$Error^2))

# 8. Regresi Linear (Persamaan Kalibrasi)
model <- lm(sensor ~ alat, data = data)

# 9. Hitung R-Squared
r_squared <- summary(model)$r.squared

# 10. Rata-rata Persentase Error
mean_persen_error <- mean(abs(data$Persen_Error))

# 11. Tampilkan Hasil
cat("===== HASIL ANALISIS KALIBRASI =====\n")
cat("Bias :", round(bias,4), "\n")
cat("RMSE :", round(rmse,4), "\n")
cat("R-Squared :", round(r_squared,4), "\n")
cat("Rata-rata Persentase Error :", round(mean_persen_error,2), "%\n")

# 12. Grafik Kalibrasi
ggplot(data, aes(x = sensor, y = alat)) +
  geom_point(color = "green", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "pink") +
  labs(title = "Grafik Kalibrasi Sensor suhu",
       x = "Nilai sensor",
       y = "Nilai alat ukur") +
  theme_minimal()

