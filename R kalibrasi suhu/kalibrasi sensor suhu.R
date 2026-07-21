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
# sensor = hasil pembacaan sensor suhu
# alat   = hasil pembacaan alat terkalibrasi

if (!all(c("sensor", "alat") %in% colnames(data))) {
  stop("Kolom 'sensor' dan 'alat' tidak ditemukan dalam file Excel.")
}

# Pastikan kedua kolom bertipe numerik
data$sensor <- as.numeric(data$sensor)
data$alat <- as.numeric(data$alat)

# Menghapus baris yang memiliki data kosong
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
bias <- mean(
  data_analisis$Error,
  na.rm = TRUE
)

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
# Nilai alat yang sama dengan 0 dikeluarkan agar tidak
# terjadi pembagian dengan nol
data_mape <- data_analisis %>%
  filter(alat != 0)

mape <- mean(
  abs(
    (data_mape$sensor - data_mape$alat) /
      data_mape$alat
  ),
  na.rm = TRUE
) * 100

# =========================================================
# 10. REGRESI LINEAR
# =========================================================
# X = sensor
# Y = alat terkalibrasi
model <- lm(
  alat ~ sensor,
  data = data_analisis
)

# =========================================================
# 11. HITUNG R-SQUARED
# =========================================================
r_squared <- summary(model)$r.squared

# Persamaan regresi
intercept <- coef(model)[1]
slope <- coef(model)[2]

# =========================================================
# 12. TAMPILKAN HASIL ANALISIS
# =========================================================
cat("===== HASIL ANALISIS SENSOR SUHU =====\n")
cat("Jumlah data :", nrow(data_analisis), "\n")
cat("Bias        :", round(bias, 4), "°C\n")
cat("RMSE        :", round(rmse, 4), "°C\n")
cat("MAE         :", round(mae, 4), "°C\n")
cat("MAPE        :", round(mape, 2), "%\n")
cat("R-Squared   :", round(r_squared, 4), "\n")
cat(
  "Persamaan regresi: y =",
  round(intercept, 4),
  "+",
  round(slope, 4),
  "x\n"
)

# =========================================================
# 13. GRAFIK REGRESI SELURUH DATA
# =========================================================
grafik_regresi <- ggplot(
  data_analisis,
  aes(x = sensor, y = alat)
) +
  geom_point(
    color = "blue",
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 1
  ) +
  labs(
    title = "Grafik Kalibrasi Sensor Suhu",
    x = expression("Nilai Sensor (" * degree * "C)"),
    y = expression("Nilai Alat Terkalibrasi (" * degree * "C)")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = 0.5
    ),
    axis.title = element_text(
      face = "bold"
    )
  )

print(grafik_regresi)

# =========================================================
# 14. MEMBUAT 10 KELOMPOK REPRESENTATIF
# =========================================================
# Seluruh data diurutkan berdasarkan nilai sensor,
# kemudian dibagi menjadi 10 kelompok dengan jumlah
# anggota yang relatif seimbang
data_ringkas <- data_analisis %>%
  arrange(sensor) %>%
  mutate(
    Kelompok = ntile(row_number(), 10)
  ) %>%
  group_by(Kelompok) %>%
  summarise(
    Sensor = mean(sensor, na.rm = TRUE),
    `Alat Terkalibrasi` = mean(alat, na.rm = TRUE),
    Jumlah_Data = n(),
    .groups = "drop"
  )

# Menampilkan hasil pengelompokan
print(data_ringkas)

# =========================================================
# 15. UBAH DATA MENJADI FORMAT PANJANG
# =========================================================
plot_data <- data_ringkas %>%
  pivot_longer(
    cols = c(Sensor, `Alat Terkalibrasi`),
    names_to = "Jenis",
    values_to = "Suhu"
  )

# Mengatur urutan batang
plot_data$Jenis <- factor(
  plot_data$Jenis,
  levels = c("Alat Terkalibrasi", "Sensor")
)

# Mengubah nomor kelompok menjadi label
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
    y = Suhu,
    fill = Jenis
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(
      label = round(Suhu, 2),
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
    title = "Perbandingan Suhu Sensor dan Alat Terkalibrasi",
    subtitle = "",
    x = "Kelompok Data",
    y = expression("Suhu (" * degree * "C)"),
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
    legend.title = element_text(
      face = "bold"
    )
  )

print(grafik_batang)

# =========================================================
# 17. SIMPAN GRAFIK REGRESI
# =========================================================
ggsave(
  filename = "grafik_regresi_sensor_suhu.png",
  plot = grafik_regresi,
  width = 10,
  height = 7,
  units = "in",
  dpi = 300
)

# =========================================================
# 18. SIMPAN DIAGRAM BATANG
# =========================================================
ggsave(
  filename = "diagram_perbandingan_suhu.png",
  plot = grafik_batang,
  width = 13,
  height = 8,
  units = "in",
  dpi = 300
)
