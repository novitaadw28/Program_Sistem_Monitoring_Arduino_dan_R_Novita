# 1. Load Library
library(readxl)
library(ggplot2)

# 2. Baca File Excel
data <- read_excel(file.choose())

# 3. Cek nama kolom
colnames(data)

# Pastikan nama kolom di Excel: sensor dan alat

# 4. Hitung Error
data$Error <- data$sensor - data$alat

# 5. Hitung Persentase Error
data$Persen_Error <- ((data$sensor - data$alat) / data$alat) * 100

# 6. Hitung Bias
bias <- mean(data$Error, na.rm = TRUE)

# 7. Hitung RMSE
rmse <- sqrt(mean(data$Error^2, na.rm = TRUE))

# 8. Hitung MAE
mae <- mean(abs(data$Error), na.rm = TRUE)

# 9. Hitung MAPE
mape <- mean(abs((data$sensor - data$alat) / data$alat), na.rm = TRUE) * 100

# 10. Regresi Linear
model <- lm(sensor ~ alat, data = data)

# 11. Hitung R-Squared
r_squared <- summary(model)$r.squared

# 12. Tampilkan Hasil
cat("===== HASIL ANALISIS KALIBRASI KELEMBAPAN =====\n")
cat("Bias :", round(bias, 4), "\n")
cat("RMSE :", round(rmse, 4), "\n")
cat("MAE :", round(mae, 4), "\n")
cat("MAPE :", round(mape, 2), "%\n")
cat("R-Squared :", round(r_squared, 4), "\n")

# 13. Grafik Kalibrasi
ggplot(data, aes(x = sensor, y = alat)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Grafik Kalibrasi Sensor Kelembapan",
       x = "Nilai Sensor",
       y = "Nilai Alat Ukur") +
  theme_minimal()
