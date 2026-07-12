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
# TIME SERIES OBJECT
# ============================================================
ts_suhu <- ts(data_clean$suhu_clean, frequency = 288)
ts_kelembapan <- ts(data_clean$kelembapan_clean, frequency = 288)

# ============================================================
# TRAINING - TESTING SPLIT
# ============================================================
n <- length(ts_suhu)
train_size <- floor(0.8 * n)

train_suhu <- ts(data_clean$suhu_clean[1:train_size], frequency = 288)
test_suhu <- data_clean$suhu_clean[(train_size + 1):n]

train_kelembapan <- ts(data_clean$kelembapan_clean[1:train_size], frequency = 288)
test_kelembapan <- data_clean$kelembapan_clean[(train_size + 1):n]

# ============================================================
# JUMLAH DATA TRAINING DAN TESTING
# ============================================================
cat("===== JUMLAH DATA =====\n")
cat("Total data        :", n, "data\n")
cat("Data training     :", train_size, "data\n")
cat("Data testing      :", n - train_size, "data\n")
cat("Persentase train  :", round((train_size / n) * 100, 2), "%\n")
cat("Persentase test   :", round(((n - train_size) / n) * 100, 2), "%\n")
# ============================================================
# MODEL AUTO ARIMA
# ============================================================
model_suhu <- auto.arima(train_suhu, seasonal = TRUE)
model_kelembapan <- auto.arima(train_kelembapan, seasonal = TRUE)

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
plot(forecast_suhu_288, main = "Forecast Suhu 1 Hari ke Depan")
plot(forecast_kelembapan_288, main = "Forecast Kelembapan 1 Hari ke Depan")
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
# TIME SERIES OBJECT
# ============================================================
ts_suhu <- ts(data_clean$suhu_clean, frequency = 288)
ts_kelembapan <- ts(data_clean$kelembapan_clean, frequency = 288)

# ============================================================
# TRAINING - TESTING SPLIT
# ============================================================
n <- length(ts_suhu)
train_size <- floor(0.8 * n)

train_suhu <- ts(data_clean$suhu_clean[1:train_size], frequency = 288)
test_suhu <- data_clean$suhu_clean[(train_size + 1):n]

train_kelembapan <- ts(data_clean$kelembapan_clean[1:train_size], frequency = 288)
test_kelembapan <- data_clean$kelembapan_clean[(train_size + 1):n]

# ============================================================
# JUMLAH DATA TRAINING DAN TESTING
# ============================================================
cat("===== JUMLAH DATA =====\n")
cat("Total data        :", n, "data\n")
cat("Data training     :", train_size, "data\n")
cat("Data testing      :", n - train_size, "data\n")
cat("Persentase train  :", round((train_size / n) * 100, 2), "%\n")
cat("Persentase test   :", round(((n - train_size) / n) * 100, 2), "%\n")
# ============================================================
# MODEL AUTO ARIMA
# ============================================================
model_suhu <- auto.arima(train_suhu, seasonal = TRUE)
model_kelembapan <- auto.arima(train_kelembapan, seasonal = TRUE)

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
plot(forecast_suhu_288, main = "Forecast Suhu 1 Hari ke Depan")
plot(forecast_kelembapan_288, main = "Forecast Kelembapan 1 Hari ke Depan")
