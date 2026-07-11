// =====================================================
// KODE ESP32 2: PENERIMA DATA DARI ESP32 SENSOR,
// PENGIRIM DATA KE BLYNK, THINGSPEAK,
// NOTIFIKASI, PRINT STATUS WIFI,
// DAN KIRIM STATUS WIFI KE ESP32 1
// =====================================================

// =====================================================
// BAGIAN 1: KONFIGURASI BLYNK
// =====================================================
#define BLYNK_PRINT Serial

// Menentukan Template ID dari Blynk Console.
#define BLYNK_TEMPLATE_ID "TMPL6AI9z_Pxy"

// Menentukan nama template Blynk yang digunakan.
#define BLYNK_TEMPLATE_NAME "Monitoring Suhu ESP32"

// Menentukan Auth Token dari device Blynk.
#define BLYNK_AUTH_TOKEN "gstghGvpGenc4gmlch46j0_OyvkDwAYf"

// =====================================================
// BAGIAN 2: LIBRARY
// =====================================================
#include <WiFi.h>
#include <HTTPClient.h>
#include <BlynkSimpleEsp32.h>

// =====================================================
// BAGIAN 3: KONFIGURASI WIFI
// =====================================================
#define WIFI_SSID "PAK EKO"
#define WIFI_PASS "1dua3empat"

// =====================================================
// BAGIAN 4: KONFIGURASI THINGSPEAK
// =====================================================
#define THINGSPEAK_API_KEY "C1M4PAC9D5OQ6QA9"
#define THINGSPEAK_URL "http://api.thingspeak.com/update"

// =====================================================
// BAGIAN 5: KONFIGURASI DATASTREAM BLYNK
// V0 = suhu
// V1 = kelembapan
// V2 = kondisi
// =====================================================
#define VPIN_SUHU V0
#define VPIN_HUM V1
#define VPIN_STATUS V2

const char* BLYNK_EVENT_CODE = "suhu_abnormal";

// =====================================================
// BAGIAN 6: KONFIGURASI UART DARI ESP32 SENSOR
// =====================================================
HardwareSerial SerialESP(2);

#define ESP_RX 16
#define ESP_TX 17
#define ESP_BAUD 9600

// =====================================================
// BAGIAN 7: INTERVAL SISTEM
// =====================================================
const unsigned long INTERVAL_BLYNK = 30000;
const unsigned long INTERVAL_THINGSPEAK = 20000;
const unsigned long INTERVAL_RECONNECT_WIFI = 10000;
const unsigned long INTERVAL_PRINT_WIFI = 5000;
const unsigned long INTERVAL_KIRIM_STATUS_WIFI = 5000;

// =====================================================
// BAGIAN 8: VARIABEL TIMER
// =====================================================
unsigned long lastBlynkMillis = 0;
unsigned long lastThingSpeakMillis = 0;
unsigned long lastReconnectMillis = 0;
unsigned long lastPrintWiFiMillis = 0;
unsigned long lastKirimStatusWiFiMillis = 0;

// =====================================================
// BAGIAN 9: VARIABEL DATA SENSOR
// =====================================================
float suhu = 0.0;
float hum = 0.0;

String statusKondisi = "NORMAL";
String statusSebelumnya = "NORMAL";

bool dataSudahMasuk = false;
bool dataBaruMasuk = false;

String bufferSerial = "";

// =====================================================
// BAGIAN 10: VARIABEL NOTIFIKASI
// =====================================================
bool notifTertunda = false;
String pesanNotifTertunda = "";

// =====================================================
// FUNGSI VALIDASI STATUS
// =====================================================
bool statusValid(String status) {
  if (status == "NORMAL") return true;
  if (status == "PANAS") return true;
  if (status == "DINGIN") return true;
  return false;
}

// =====================================================
// FUNGSI KONVERSI STATUS KE ANGKA
// NORMAL = 0
// PANAS  = 1
// DINGIN = -1
// =====================================================
int statusToCode(String status) {
  if (status == "PANAS") return 1;
  if (status == "DINGIN") return -1;
  return 0;
}

// =====================================================
// FUNGSI MEMBUAT PESAN NOTIFIKASI
// =====================================================
String buatPesanNotifikasi() {
  String pesan = "ALERT! Status: ";
  pesan += statusKondisi;
  pesan += ", Suhu: ";
  pesan += String(suhu, 1);
  pesan += " C, Hum: ";
  pesan += String(hum, 0);
  pesan += "%";
  return pesan;
}

// =====================================================
// FUNGSI KIRIM STATUS WIFI KE ESP32 1
// =====================================================
void kirimStatusWiFiKeESP1() {
  if (WiFi.status() == WL_CONNECTED) {
    SerialESP.println("WIFI_OK");
  }

  else {
    SerialESP.println("WIFI_ERROR");
  }
}

// =====================================================
// FUNGSI KIRIM STATUS WIFI BERDASARKAN INTERVAL
// =====================================================
void kirimStatusWiFiInterval() {
  unsigned long now = millis();

  if (now - lastKirimStatusWiFiMillis >= INTERVAL_KIRIM_STATUS_WIFI) {
    lastKirimStatusWiFiMillis = now;
    kirimStatusWiFiKeESP1();
  }
}

// =====================================================
// FUNGSI PRINT STATUS WIFI DAN BLYNK
// =====================================================
void tampilStatusWiFi() {
  unsigned long now = millis();

  if (now - lastPrintWiFiMillis < INTERVAL_PRINT_WIFI) {
    return;
  }

  lastPrintWiFiMillis = now;

  Serial.println("================================");

  if (WiFi.status() == WL_CONNECTED) {
    Serial.println("WiFi SUDAH TERSAMBUNG");
    Serial.print("SSID        : ");
    Serial.println(WIFI_SSID);
    Serial.print("IP Address  : ");
    Serial.println(WiFi.localIP());
    Serial.print("Sinyal RSSI : ");
    Serial.print(WiFi.RSSI());
    Serial.println(" dBm");

    if (Blynk.connected()) {
      Serial.println("Blynk       : TERSAMBUNG");
    }

    else {
      Serial.println("Blynk       : BELUM TERSAMBUNG");
    }
  }

  else {
    Serial.println("PERINGATAN: WiFi BELUM TERSAMBUNG / TERPUTUS");
    Serial.print("SSID tujuan : ");
    Serial.println(WIFI_SSID);
    Serial.println("Periksa nama WiFi, password, jarak router, atau koneksi internet.");
    Serial.println("Mencoba menyambungkan ulang...");
  }

  Serial.println("================================");
}

// =====================================================
// FUNGSI RECONNECT WIFI
// =====================================================
void reconnectWiFi() {
  if (WiFi.status() == WL_CONNECTED) {
    return;
  }

  unsigned long now = millis();

  if (now - lastReconnectMillis >= INTERVAL_RECONNECT_WIFI) {
    lastReconnectMillis = now;

    Serial.println("Mencoba koneksi ulang WiFi...");
    WiFi.disconnect();
    WiFi.begin(WIFI_SSID, WIFI_PASS);
  }
}

// =====================================================
// FUNGSI MENJALANKAN BLYNK
// =====================================================
void jalankanBlynk() {
  if (WiFi.status() == WL_CONNECTED) {
    if (!Blynk.connected()) {
      Blynk.connect(1000);
    }

    if (Blynk.connected()) {
      Blynk.run();
    }
  }
}

// =====================================================
// FUNGSI KIRIM DATA KE BLYNK
// =====================================================
void kirimBlynk() {
  if (!dataSudahMasuk) {
    Serial.println("Data sensor belum masuk, Blynk belum dikirim.");
    return;
  }

  if (!Blynk.connected()) {
    Serial.println("Blynk belum tersambung, data belum dikirim.");
    return;
  }

  Blynk.virtualWrite(VPIN_SUHU, suhu);
  Blynk.virtualWrite(VPIN_HUM, hum);
  Blynk.virtualWrite(VPIN_STATUS, statusKondisi);

  Serial.print("Blynk terkirim | V0 Suhu: ");
  Serial.print(suhu, 1);
  Serial.print(" C | V1 Hum: ");
  Serial.print(hum, 0);
  Serial.print(" % | V2 Status: ");
  Serial.println(statusKondisi);
}

// =====================================================
// FUNGSI KIRIM DATA KE THINGSPEAK
// =====================================================
void kirimThingSpeak() {
  if (!dataSudahMasuk) {
    Serial.println("Data sensor belum masuk, ThingSpeak belum dikirim.");
    return;
  }

  if (WiFi.status() != WL_CONNECTED) {
    Serial.println("WiFi belum tersambung, ThingSpeak belum dikirim.");
    return;
  }

  HTTPClient http;

  String url = String(THINGSPEAK_URL);
  url += "?api_key=" + String(THINGSPEAK_API_KEY);
  url += "&field1=" + String(suhu, 1);
  url += "&field2=" + String(hum, 0);
  url += "&field3=" + String(statusToCode(statusKondisi));

  http.begin(url);
  int httpCode = http.GET();

  Serial.print("ThingSpeak HTTP Code: ");
  Serial.println(httpCode);

  if (httpCode > 0) {
    String response = http.getString();
    Serial.print("ThingSpeak Response: ");
    Serial.println(response);
  }

  else {
    Serial.println("PERINGATAN: Gagal mengirim data ke ThingSpeak.");
  }

  http.end();
}

// =====================================================
// FUNGSI CEK NOTIFIKASI BLYNK
// =====================================================
void cekNotifikasi() {
  if (!dataSudahMasuk) {
    return;
  }

  if (statusKondisi == statusSebelumnya) {
    return;
  }

  if (statusKondisi == "PANAS" || statusKondisi == "DINGIN") {
    String pesan = buatPesanNotifikasi();

    if (Blynk.connected()) {
      Blynk.logEvent(BLYNK_EVENT_CODE, pesan);
      Serial.println("Notifikasi Blynk dikirim.");
    }

    else {
      notifTertunda = true;
      pesanNotifTertunda = pesan;
      Serial.println("Notifikasi disimpan sementara karena Blynk belum tersambung.");
    }
  }

  statusSebelumnya = statusKondisi;
}

// =====================================================
// FUNGSI KIRIM NOTIFIKASI TERTUNDA
// =====================================================
void kirimNotifikasiTertunda() {
  if (!notifTertunda) {
    return;
  }

  if (!Blynk.connected()) {
    return;
  }

  Blynk.logEvent(BLYNK_EVENT_CODE, pesanNotifTertunda);

  Serial.println("Notifikasi tertunda berhasil dikirim ke Blynk.");

  notifTertunda = false;
  pesanNotifTertunda = "";
}

// =====================================================
// FUNGSI MEMPROSES DATA SERIAL DARI ESP32 SENSOR
// Format data dari ESP32 1:
// suhu,kelembapan,status
// Contoh:
// 26.1,54,NORMAL
// =====================================================
void prosesDataSerial(String data) {
  data.trim();

  if (data.length() == 0) {
    return;
  }

  int koma1 = data.indexOf(',');
  int koma2 = data.indexOf(',', koma1 + 1);

  if (koma1 <= 0 || koma2 <= koma1) {
    Serial.print("Format data salah: ");
    Serial.println(data);
    return;
  }

  String suhuText = data.substring(0, koma1);
  String humText = data.substring(koma1 + 1, koma2);
  String statusText = data.substring(koma2 + 1);

  suhuText.trim();
  humText.trim();
  statusText.trim();

  if (!statusValid(statusText)) {
    Serial.print("Status tidak valid: ");
    Serial.println(statusText);
    return;
  }

  suhu = suhuText.toFloat();
  hum = humText.toFloat();
  statusKondisi = statusText;

  dataSudahMasuk = true;
  dataBaruMasuk = true;

  Serial.print("Data diterima dari ESP32 sensor: ");
  Serial.print(suhu, 1);
  Serial.print(" C, ");
  Serial.print(hum, 0);
  Serial.print(" %, ");
  Serial.println(statusKondisi);

  cekNotifikasi();
}

// =====================================================
// FUNGSI MEMBACA UART DARI ESP32 SENSOR
// =====================================================
void bacaSerialESP() {
  while (SerialESP.available()) {
    char c = SerialESP.read();

    if (c == '\n') {
      prosesDataSerial(bufferSerial);
      bufferSerial = "";
    }

    else if (c != '\r') {
      bufferSerial += c;

      if (bufferSerial.length() > 80) {
        bufferSerial = "";
      }
    }
  }
}

// =====================================================
// SETUP
// =====================================================
void setup() {
  Serial.begin(115200);

  delay(1000);

  Serial.println();
  Serial.println("ESP32 penerima mulai aktif...");

  SerialESP.begin(ESP_BAUD, SERIAL_8N1, ESP_RX, ESP_TX);

  WiFi.mode(WIFI_STA);

  Serial.println("Menghubungkan ESP32 ke WiFi...");
  Serial.print("SSID: ");
  Serial.println(WIFI_SSID);

  WiFi.begin(WIFI_SSID, WIFI_PASS);

  Blynk.config(BLYNK_AUTH_TOKEN);

  Serial.println("Menunggu data dari ESP32 sensor...");
}

// =====================================================
// LOOP
// =====================================================
void loop() {
  unsigned long now = millis();

  reconnectWiFi();

  tampilStatusWiFi();

  kirimStatusWiFiInterval();

  jalankanBlynk();

  kirimNotifikasiTertunda();

  bacaSerialESP();

  // Jika ada data baru dari ESP32 sensor, langsung kirim ke Blynk.
  if (dataBaruMasuk) {
    kirimBlynk();
    dataBaruMasuk = false;
    lastBlynkMillis = now;
  }

  // Kirim ulang ke Blynk sesuai interval.
  if (now - lastBlynkMillis >= INTERVAL_BLYNK) {
    lastBlynkMillis = now;
    kirimBlynk();
  }

  // Kirim ke ThingSpeak sesuai interval.
  if (dataSudahMasuk &&
      (lastThingSpeakMillis == 0 || now - lastThingSpeakMillis >= INTERVAL_THINGSPEAK)) {
    lastThingSpeakMillis = now;
    kirimThingSpeak();
  }
}