# ============================================================
# LIBRARY
# ============================================================
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(zoo)
library(tidyr)

# ============================================================
# IMPORT DATA
# ============================================================
data <- read_excel(file.choose())

# Rapikan nama kolom
names(data) <- names(data) %>%
  trimws() %>%
  tolower() %>%
  gsub(" ", "_", .)

print(names(data))

# ============================
# STATISTIK DESKRIPTIF SUHU (SEBELUM CLEANING)
# ============================
 
mean_suhu_raw <- mean(data_full$suhu, na.rm = TRUE)
min_suhu_raw  <- min(data_full$suhu, na.rm = TRUE)
max_suhu_raw  <- max(data_full$suhu, na.rm = TRUE)
sd_suhu_raw   <- sd(data_full$suhu, na.rm = TRUE)

cat("===== SUHU (RAW) =====\n")
cat("Mean :", mean_suhu_raw, "\n")
cat("Min  :", min_suhu_raw, "\n")
cat("Max  :", max_suhu_raw, "\n")
cat("SD   :", sd_suhu_raw, "\n")

# ==============================
# STATISTIK DESKRIPTIF KELEMBAPAN (SEBELUM CLEANING)
# ==============================

mean_hum_raw <- mean(data_full$kelembapan, na.rm = TRUE)
min_hum_raw  <- min(data_full$kelembapan, na.rm = TRUE)
max_hum_raw  <- max(data_full$kelembapan, na.rm = TRUE)
sd_hum_raw   <- sd(data_full$kelembapan, na.rm = TRUE)

cat("\n===== KELEMBAPAN (RAW) =====\n")
cat("Mean :", mean_hum_raw, "\n")
cat("Min  :", min_hum_raw, "\n")
cat("Max  :", max_hum_raw, "\n")
cat("SD   :", sd_hum_raw, "\n")
# ============================================================
# PREPROCESSING
# ============================================================
data_raw <- data %>%
  mutate(
    waktu = ymd_hms(timestamp, tz = "UTC"),
    waktu = with_tz(waktu, "Asia/Jakarta"),
    suhu = as.numeric(temperature),
    kelembapan = as.numeric(humidity)
  ) %>%
  select(waktu, suhu, kelembapan) %>%
  arrange(waktu) %>%
  drop_na()

# ============================================================
# RESAMPLING 5 MENIT
# ============================================================
data_rs <- data_raw %>%
  mutate(waktu = floor_date(waktu, "5 min")) %>%
  group_by(waktu) %>%
  summarise(
    suhu = mean(suhu, na.rm = TRUE),
    kelembapan = mean(kelembapan, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# MELENGKAPI TIME SERIES
# ============================================================
data_full <- data_rs %>%
  complete(waktu = seq(min(waktu), max(waktu), by = "5 min")) %>%
  arrange(waktu)

# ============================================================
# INTERPOLASI DATA HILANG
# ============================================================
data_full$suhu <- na.approx(data_full$suhu, maxgap = 12, na.rm = FALSE)
data_full$kelembapan <- na.approx(data_full$kelembapan, maxgap = 12, na.rm = FALSE)

data_full <- data_full %>%
  drop_na()
# ============================================================
# PLOT GABUNGAN SEBELUM DETEKSI ANOMALI DAN CLEANING
# ============================================================
data_plot_awal <- data_full %>%
  select(waktu, suhu, kelembapan) %>%
  pivot_longer(
    cols = c(suhu, kelembapan),
    names_to = "variabel",
    values_to = "nilai"
  )

ggplot(data_plot_awal, aes(x = waktu, y = nilai)) +
  geom_line(color = "black") +
  facet_wrap(~ variabel, scales = "free_y", ncol = 1) +
  labs(
    title = "Data Suhu dan Kelembapan Sebelum Deteksi Anomali dan Cleaning",
    x = "Waktu",
    y = "Nilai"
  ) +
  theme_minimal()

# ============================================================
# DETEKSI ANOMALI: BATAS NORMAL + SPIKE
# ============================================================

# Batas normal sensor
batas_suhu_min <- 18
batas_suhu_max <- 27

batas_kelembapan_min <- 0
batas_kelembapan_max <- 90

# Batas spike
# Spike suhu: perubahan lebih dari 2°C dalam 5 menit
# Spike kelembapan: perubahan lebih dari 10% dalam 5 menit
batas_spike_suhu <- 2
batas_spike_kelembapan <- 10

data_full <- data_full %>%
  mutate(
    delta_suhu = abs(suhu - lag(suhu)),
    delta_kelembapan = abs(kelembapan - lag(kelembapan)),
    
    spike_suhu = delta_suhu > batas_spike_suhu,
    spike_kelembapan = delta_kelembapan > batas_spike_kelembapan,
    
    spike_suhu = ifelse(is.na(spike_suhu), FALSE, spike_suhu),
    spike_kelembapan = ifelse(is.na(spike_kelembapan), FALSE, spike_kelembapan),
    
    anom_suhu = suhu < batas_suhu_min | suhu > batas_suhu_max | spike_suhu,
    anom_kelembapan = kelembapan < batas_kelembapan_min | kelembapan > batas_kelembapan_max | spike_kelembapan
  )

# ============================================================
# PLOT ANOMALI SUHU
# ============================================================
ggplot(data_full, aes(x = waktu, y = suhu)) +
  geom_line(color = "black") +
  geom_point(aes(color = anom_suhu), size = 1.5) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  labs(
    title = "Deteksi Anomali Suhu",
    x = "Waktu",
    y = "Suhu (°C)",
    color = "Anomali"
  )

# ============================================================
# PLOT ANOMALI KELEMBAPAN
# ============================================================
ggplot(data_full, aes(x = waktu, y = kelembapan)) +
  geom_line(color = "black") +
  geom_point(aes(color = anom_kelembapan), size = 1.5) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  labs(
    title = "Deteksi Anomali Kelembapan",
    x = "Waktu",
    y = "Kelembapan (%)",
    color = "Anomali"
  )

# ============================================================
# CLEANING DATA
# ============================================================
data_clean <- data_full %>%
  mutate(
    suhu_clean = ifelse(anom_suhu, NA, suhu),
    kelembapan_clean = ifelse(anom_kelembapan, NA, kelembapan)
  )

# Interpolasi ulang setelah anomali dijadikan NA
data_clean$suhu_clean <- na.approx(data_clean$suhu_clean, maxgap = 12, na.rm = FALSE)
data_clean$kelembapan_clean <- na.approx(data_clean$kelembapan_clean, maxgap = 12, na.rm = FALSE)

data_clean <- data_clean %>%
  drop_na(suhu_clean, kelembapan_clean)

# ============================================================
# PLOT SUHU SETELAH CLEANING
# ============================================================
ggplot(data_clean, aes(x = waktu, y = suhu_clean)) +
  geom_line(color = "black") +
  labs(
    title = "Data Suhu Setelah Cleaning",
    x = "Waktu",
    y = "Suhu (°C)"
  ) +
  theme_minimal()

# ============================================================
# PLOT KELEMBAPAN SETELAH CLEANING
# ============================================================
ggplot(data_clean, aes(x = waktu, y = kelembapan_clean)) +
  geom_line(color = "black") +
  labs(
    title = "Data Kelembapan Setelah Cleaning",
    x = "Waktu",
    y = "Kelembapan (%)"
  ) +
  theme_minimal()

# ============================================================
# PLOT PERBANDINGAN SUHU (BEFORE vs AFTER CLEANING)
# ============================================================

ggplot() +
  geom_line(data = data_full,
            aes(x = waktu, y = suhu),
            color = "red", alpha = 0.5) +
  geom_line(data = data_clean,
            aes(x = waktu, y = suhu_clean),
            color = "black", linewidth = 0.7) +
  labs(
    title = "Perbandingan Suhu Sebelum dan Sesudah Cleaning",
    x = "Waktu",
    y = "Suhu (°C)",
    caption = "Merah = sebelum cleaning | Hitam = sesudah cleaning"
  ) +
  theme_minimal()

# ============================================================
# PLOT PERBANDINGAN KELEMBAPAN (BEFORE vs AFTER CLEANING)
# ============================================================

ggplot() +
  geom_line(data = data_full,
            aes(x = waktu, y = kelembapan),
            color = "red", alpha = 0.5) +
  geom_line(data = data_clean,
            aes(x = waktu, y = kelembapan_clean),
            color = "blue", linewidth = 0.7) +
  labs(
    title = "Perbandingan Kelembapan Sebelum dan Sesudah Cleaning",
    x = "Waktu",
    y = "Kelembapan (%)",
    caption = "Merah = sebelum cleaning | Biru = sesudah cleaning"
  ) +
  theme_minimal()


# ============================================================
# ============================================================
# TIME SERIES OBJECT
# ============================================================
ts_suhu <- ts(data_clean$suhu_clean, frequency = 1)
ts_kelembapan <- ts(data_clean$kelembapan_clean, frequency = 1)

# ============================================================
# TRAINING - TESTING SPLIT
# ============================================================
n <- length(ts_suhu)
train_size <- floor(0.8 * n)

train_suhu <- ts(as.numeric(ts_suhu[1:train_size]), frequency = 1)
test_suhu  <- as.numeric(ts_suhu[(train_size + 1):n])

train_kelembapan <- ts(as.numeric(ts_kelembapan[1:train_size]), frequency = 1)
test_kelembapan  <- as.numeric(ts_kelembapan[(train_size + 1):n])

# ============================================================
# JUMLAH DATA TRAINING DAN TESTING
# ============================================================
cat("===== JUMLAH DATA =====\n")
cat("Total data       :", n, "data\n")
cat("Data training    :", train_size, "data\n")
cat("Data testing     :", n - train_size, "data\n")
cat("Persentase train :", round((train_size / n) * 100, 2), "%\n")
cat("Persentase test  :", round(((n - train_size) / n) * 100, 2), "%\n")

# ============================================================
# MODEL AUTO ARIMA
# ============================================================
model_suhu <- auto.arima(
  train_suhu,
  seasonal = FALSE,
  stepwise = TRUE,
  approximation = FALSE,
)

model_kelembapan <- auto.arima(
  train_kelembapan,
  seasonal = FALSE,
  stepwise = TRUE,
  approximation = FALSE,
)


# ============================================================
# DETAIL MODEL AUTO ARIMA SUHU
# ============================================================

summary(model_suhu)


# ============================================================
# DETAIL MODEL AUTO ARIMA KELEMBAPAN
# ============================================================

summary(model_kelembapan)


# Model suhu
arimaorder(model_suhu)

# Model kelembapan
arimaorder(model_kelembapan)


# Suhu
model_suhu$aic
model_suhu$aicc
model_suhu$bic


# Kelembapan
model_kelembapan$aic
model_kelembapan$aicc
model_kelembapan$bic


# ============================================================
# RINGKASAN MODEL
# ============================================================

cat("===== MODEL SUHU =====\n")
print(arimaorder(model_suhu))
cat("AIC :", model_suhu$aic,"\n")
cat("AICc:", model_suhu$aicc,"\n")
cat("BIC :", model_suhu$bic,"\n")


cat("\n===== MODEL KELEMBAPAN =====\n")
print(arimaorder(model_kelembapan))
cat("AIC :", model_kelembapan$aic,"\n")
cat("AICc:", model_kelembapan$aicc,"\n")
cat("BIC :", model_kelembapan$bic,"\n")


# ============================================================
# FORECAST TESTING
# ============================================================
pred_suhu <- forecast(model_suhu, h = length(test_suhu))
pred_kelembapan <- forecast(model_kelembapan, h = length(test_kelembapan))

# ============================================================
# METRICS EVALUATION
# ============================================================
mae <- function(a, p) {
  mean(abs(a - p), na.rm = TRUE)
}

rmse <- function(a, p) {
  sqrt(mean((a - p)^2, na.rm = TRUE))
}

mape <- function(a, p) {
  mean(abs((a - p) / a)[a != 0], na.rm = TRUE) * 100
}

r2 <- function(a, p) {
  1 - sum((a - p)^2, na.rm = TRUE) / sum((a - mean(a, na.rm = TRUE))^2, na.rm = TRUE)
}

cat("===== SUHU =====\n")
cat("RMSE :", rmse(test_suhu, pred_suhu$mean), "\n")
cat("MAE  :", mae(test_suhu, pred_suhu$mean), "\n")
cat("MAPE :", mape(test_suhu, pred_suhu$mean), "%\n")


cat("\n===== KELEMBAPAN =====\n")
cat("RMSE :", rmse(test_kelembapan, pred_kelembapan$mean), "\n")
cat("MAE  :", mae(test_kelembapan, pred_kelembapan$mean), "\n")
cat("MAPE :", mape(test_kelembapan, pred_kelembapan$mean), "%\n")

# ============================================================
# FORECAST 1 HARI KE DEPAN
# ============================================================
forecast_suhu_288 <- forecast(model_suhu, h = 288)
forecast_kelembapan_288 <- forecast(model_kelembapan, h = 288)
# ============================================================
# PLOT FORECAST
# ============================================================
# ============================
# PLOT FORECAST SUHU (FIXED)
# ============================

plot(
  forecast_suhu_288,
  main = "Forecast Suhu 1 Hari ke Depan",
  xlab = "data ke-",
  ylab = "Suhu (°C)",
  col.main = "black",
  fcol = "blue",
  shadecols = c("gray80", "gray90")
)
# ==============================
# PLOT FORECAST KELEMBAPAN (FIXED)
# ==============================

plot(
  forecast_kelembapan_288,
  main = "Forecast Kelembapan 1 Hari ke Depan",
  xlab = "data ke-",
  ylab = "Kelembapan (%)",
  col.main = "black",
  fcol = "darkgreen",
  shadecols = c("gray80", "gray90")
)

