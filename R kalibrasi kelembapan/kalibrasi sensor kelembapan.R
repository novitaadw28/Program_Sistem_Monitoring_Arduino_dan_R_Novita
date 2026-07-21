# =========================================================
# 1. LOAD LIBRARY
# =========================================================
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# =========================================================
# 2. BACA FILE EXCEL
# =========================================================
data <- read_excel(file.choose())

# =========================================================
# 3. CEK NAMA KOLOM
# =========================================================
colnames(data)

# Pastikan file Excel memiliki kolom:
# sensor = hasil pembacaan sensor kelembapan
# alat   = hasil pembacaan alat pembanding

if (!all(c("sensor", "alat") %in% colnames(data))) {
  stop("Kolom 'sensor' dan 'alat' tidak ditemukan dalam file Excel.")
}

# Pastikan kedua kolom bertipe numerik
data$sensor <- as.numeric(data$sensor)
data$alat <- as.numeric(data$alat)

# Menghapus data yang kosong
data_analisis <- data %>%
  filter(!is.na(sensor), !is.na(alat))

# =========================================================
# 4. HITUNG ERROR
# =========================================================
data_analisis$Error <- data_analisis$sensor - data_analisis$alat

# =========================================================
# 5. HITUNG PERSENTASE ERROR
# =========================================================
data_analisis$Persen_Error <- (
  data_analisis$Error / data_analisis$alat
) * 100

# =========================================================
# 6. HITUNG BIAS
# =========================================================
bias <- mean(data_analisis$Error, na.rm = TRUE)

# =========================================================
# 7. HITUNG RMSE
# =========================================================
rmse <- sqrt(
  mean(data_analisis$Error^2, na.rm = TRUE)
)

# =========================================================
# 8. HITUNG MAE
# =========================================================
mae <- mean(
  abs(data_analisis$Error),
  na.rm = TRUE
)

# =========================================================
# 9. HITUNG MAPE
# =========================================================
# Data alat bernilai 0 dikeluarkan agar tidak terjadi
# pembagian dengan nol
data_mape <- data_analisis %>%
  filter(alat != 0)

mape <- mean(
  abs((data_mape$sensor - data_mape$alat) / data_mape$alat),
  na.rm = TRUE
) * 100

# =========================================================
# 10. REGRESI LINEAR
# =========================================================
# Disesuaikan dengan grafik:
# X = sensor
# Y = alat pembanding
model <- lm(alat ~ sensor, data = data_analisis)

# =========================================================
# 11. HITUNG R-SQUARED
# =========================================================
r_squared <- summary(model)$r.squared

# =========================================================
# 12. TAMPILKAN HASIL ANALISIS
# =========================================================
cat("===== HASIL ANALISIS SENSOR KELEMBAPAN =====\n")
cat("Jumlah data :", nrow(data_analisis), "\n")
cat("Bias        :", round(bias, 4), "%RH\n")
cat("RMSE        :", round(rmse, 4), "%RH\n")
cat("MAE         :", round(mae, 4), "%RH\n")
cat("MAPE        :", round(mape, 2), "%\n")
cat("R-Squared   :", round(r_squared, 4), "\n")

# =========================================================
# 13. GRAFIK REGRESI SELURUH DATA
# =========================================================
grafik_regresi <- ggplot(
  data_analisis,
  aes(x = sensor, y = alat)
) +
  geom_point(
    color = "darkgreen",
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 1
  ) +
  labs(
    title = "Grafik Perbandingan Sensor Kelembapan dan Alat Pembanding",
    x = "Nilai Sensor (%RH)",
    y = "Nilai Alat Pembanding (%RH)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(face = "bold")
  )

print(grafik_regresi)

# =========================================================
# 14. MEMBUAT 10 KELOMPOK REPRESENTATIF
# =========================================================
# Data diurutkan berdasarkan nilai sensor, kemudian
# dibagi menjadi 10 kelompok dengan jumlah data seimbang
data_ringkas <- data_analisis %>%
  arrange(sensor) %>%
  mutate(
    Kelompok = ntile(row_number(), 10)
  ) %>%
  group_by(Kelompok) %>%
  summarise(
    Sensor = mean(sensor, na.rm = TRUE),
    `Alat Pembanding` = mean(alat, na.rm = TRUE),
    Jumlah_Data = n(),
    .groups = "drop"
  )

# Menampilkan tabel hasil pengelompokan
print(data_ringkas)

# =========================================================
# 15. UBAH DATA MENJADI FORMAT PANJANG
# =========================================================
plot_data <- data_ringkas %>%
  pivot_longer(
    cols = c(Sensor, `Alat Pembanding`),
    names_to = "Jenis",
    values_to = "Kelembapan"
  )

# Mengatur urutan batang
plot_data$Jenis <- factor(
  plot_data$Jenis,
  levels = c("Alat Pembanding", "Sensor")
)

plot_data$Kelompok <- factor(
  plot_data$Kelompok,
  levels = 1:10,
  labels = paste("Kelompok", 1:10)
)

# =========================================================
# 16. DIAGRAM BATANG VERTIKAL REPRESENTATIF
# =========================================================
grafik_batang <- ggplot(
  plot_data,
  aes(
    x = Kelompok,
    y = Kelembapan,
    fill = Jenis
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(
      label = round(Kelembapan, 2),
      group = Jenis
    ),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 3.5
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = "Perbandingan Kelembapan Sensor dan Alat Pembanding",
    subtitle = paste(
      "Seluruh",
      nrow(data_analisis),
      "data diringkas menjadi 10 kelompok representatif"
    ),
    x = "Kelompok Data",
    y = "Kelembapan Relatif (%RH)",
    fill = "Jenis Pengukuran"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 13,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5
    ),
    axis.title = element_text(
      face = "bold"
    ),
    axis.text.x = element_text(
      size = 10,
      angle = 30,
      hjust = 1
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold")
  )

print(grafik_batang)

# =========================================================
# 17. SIMPAN DIAGRAM BATANG
# =========================================================
ggsave(
  filename = "diagram_perbandingan_kelembapan.png",
  plot = grafik_batang,
  width = 13,
  height = 8,
  units = "in",
  dpi = 300
)
  labs(
    title = "Perbandingan Suhu Sensor dan Alat Terkalibrasi",
    subtitle = "",
    x = "Kelompok Data",
    y = expression(" ( "C)"),
    fill = "Jenis Pengukuran"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 11)
  )


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
