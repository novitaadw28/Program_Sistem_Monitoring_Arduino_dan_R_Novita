// =====================================================
// ESP32 1: SENSOR + LCD + UART + ANOMALI OUTPUT
// =====================================================

#include <Wire.h>
#include <LiquidCrystal_I2C.h>
#include <DHT.h>

// =====================================================
// PIN CONFIG
// =====================================================
#define DHTPIN 18
#define DHTTYPE DHT11

#define SDA_PIN 23
#define SCL_PIN 19

#define LED_NORMAL 26
#define LED_GANGGUAN 27
#define BUZZER 25

// UART
HardwareSerial SerialESP(2);
#define UART_RX 4
#define UART_TX 2
#define UART_BAUD 9600

// =====================================================
// THRESHOLD
// =====================================================
const float BATAS_PANAS = 27.0;
const float BATAS_DINGIN = 15.0;

// =====================================================
// TIMER
// =====================================================
const unsigned long INTERVAL_SENSOR = 300000UL; // 5 menit
const unsigned long INTERVAL_LCD = 2000UL;      // 2 detik

unsigned long lastSensorMillis = 0;
unsigned long lastLCDMillis = 0;

// =====================================================
// OBJECT
// =====================================================
LiquidCrystal_I2C lcd(0x27, 16, 2);
DHT dht(DHTPIN, DHTTYPE);

// =====================================================
// DATA
// =====================================================
float suhu = 0.0;
float hum = 0.0;

enum Kondisi { NORMAL, PANAS, DINGIN };
Kondisi kondisi = NORMAL;

// =====================================================
// KONVERSI STATUS
// =====================================================
String kondisiToString(Kondisi k) {
  if (k == PANAS) return "PANAS";
  if (k == DINGIN) return "DINGIN";
  return "NORMAL";
}

// =====================================================
// SENSOR READ
// =====================================================
bool bacaSensor() {
  suhu = dht.readTemperature();
  hum = dht.readHumidity();

  if (isnan(suhu) || isnan(hum)) return false;
  return true;
}

// =====================================================
// KLASIFIKASI
// =====================================================
void tentukanKondisi() {
  if (suhu > BATAS_PANAS) kondisi = PANAS;
  else if (suhu < BATAS_DINGIN) kondisi = DINGIN;
  else kondisi = NORMAL;
}

// =====================================================
// ALARM
// =====================================================
void kontrolAlarm() {
  if (kondisi != NORMAL) {
    digitalWrite(LED_GANGGUAN, HIGH);
    digitalWrite(LED_NORMAL, LOW);
    tone(BUZZER, 2000);
  } else {
    digitalWrite(LED_GANGGUAN, LOW);
    digitalWrite(LED_NORMAL, HIGH);
    noTone(BUZZER);
  }
}

// =====================================================
// LCD DISPLAY (FIX STABIL - NO FLICKER)
// =====================================================
void tampilLCD() {
  lcd.setCursor(0, 0);
  lcd.print("T:");
  lcd.print(suhu, 1);
  lcd.print((char)223);
  lcd.print("C   ");

  lcd.setCursor(9, 0);
  lcd.print("H:");
  lcd.print(hum, 0);
  lcd.print("% ");

  lcd.setCursor(0, 1);
  lcd.print("Status:");
  lcd.print("        ");
  lcd.setCursor(8, 1);
  lcd.print(kondisiToString(kondisi));
}

// =====================================================
// UART SEND
// =====================================================
void kirimDataSerial() {
  SerialESP.print(suhu, 1);
  SerialESP.print(",");
  SerialESP.print(hum, 0);
  SerialESP.print(",");
  SerialESP.println(kondisiToString(kondisi));

  Serial.print("SEND -> ");
  Serial.print(suhu);
  Serial.print("C ");
  Serial.print(hum);
  Serial.print("% ");
  Serial.println(kondisiToString(kondisi));
}

// =====================================================
// SETUP
// =====================================================
void setup() {
  Serial.begin(115200);
  SerialESP.begin(UART_BAUD, SERIAL_8N1, UART_RX, UART_TX);

  Wire.begin(SDA_PIN, SCL_PIN);
  delay(100);

  lcd.begin();
  lcd.backlight();
  lcd.clear();

  dht.begin();

  pinMode(LED_NORMAL, OUTPUT);
  pinMode(LED_GANGGUAN, OUTPUT);
  pinMode(BUZZER, OUTPUT);

  digitalWrite(LED_NORMAL, HIGH);
  digitalWrite(LED_GANGGUAN, LOW);
  noTone(BUZZER);

  lcd.setCursor(0, 0);
  lcd.print("SYSTEM START");
  lcd.setCursor(0, 1);
  lcd.print("INITIALIZING");

  delay(1500);
  lcd.clear();
}

// =====================================================
// LOOP (FIX AUTO UPDATE + 5 MENIT SEND)
// =====================================================
void loop() {
  unsigned long now = millis();

  // =========================
  // UPDATE LCD SETIAP 2 DETIK
  // =========================
  if (now - lastLCDMillis >= INTERVAL_LCD) {
    lastLCDMillis = now;

    if (bacaSensor()) {
      tentukanKondisi();
      tampilLCD();
    }
  }

  // =========================
  // KIRIM DATA SETIAP 5 MENIT
  // =========================
  if (now - lastSensorMillis >= INTERVAL_SENSOR) {
    lastSensorMillis = now;

    if (bacaSensor()) {
      tentukanKondisi();
      kontrolAlarm();
      kirimDataSerial();
    }
  }
}