## Setting up electret sensor

## Step 1: Plug in your device to your computer.
If the device is configured for your local WiFi network proceed to step 2. If you need to configure your device for the local WiFi network open Chrome and go to setup.particle.io. Go through the steps there and at the end you will have the option to select a wireless network. Choose the appropriate network. Note: these devices cannot login to PAL or Eduroam.


## Step 2: Open VS Code and Configure your workspace
* Open the Command Palette using the CTL+SHIFT+P (Windows, Linux) or CMD+SHIFT+P (Mac OS, Linux) shortcut.
* In the text box, type "Particle: Create New Project" and hit enter.
* Follow the prompts to choose a folder in which to create your new project and set the project name.
* Once the project loads, configure things so that you can easily compile and flash firmware. Open the command palette again (CTL/CMD+SHIFT+P), type "Particle" and select the "Configure Project for Device" option. In the prompts, chose a DeviceOS version (as of writing use 6.4.1), select "argon" or "boron" as your target device. Leave "Device" blank when prompted (just hit enter).

## Step 3: Flash the firmware
* Using the navigation pane on the left, find the "src" folder and open the .cpp file inside.
* Delete the contents of the file and replace with the code below.
* In the code, change Site_ID to the site name of your choice.
* Use the command palette to Flash application (local).
* If it flashed successfully, you are done. Unplug the device from the computer and plugin a power supply or battery pack.


Particle Argon Code
```
/*
 * argon_soundscape_ndvi_lp.cpp
 *
 * Particle ARGON (nRF52840 + ESP32 WiFi) acoustic band-energy logger,
 * duty-cycled with ULTRA_LOW_POWER sleep.
 *
 * Analog electret mic (MAX4466, fixed gain) -> A0 -> blocking micros()-paced
 * sampler -> Hann-windowed FFT -> Welch-averaged power spectrum -> octave
 * bands -> published to the existing "ndvi-vals" webhook.
 *
 * Requires: Device OS 2.0.0 or later (ULTRA_LOW_POWER sleep mode)
 *           NO external libraries. Delete SparkIntervalTimer from lib/ if it
 *           is still in the project -- it is Gen 1/Gen 2 only (Core/Photon)
 *           and will not compile for the Argon at any version.
 *
 * ---------------------------------------------------------------------------
 * SAMPLING APPROACH
 *   Windows are captured in a blocking loop paced off micros(), not from a
 *   timer ISR. Gen 3 has no supported IntervalTimer library, and reaching into
 *   nrfx for a hardware timer is a lot of surface area for a 64 ms burst.
 *
 *   Blocking is fine here: Device OS runs networking on its own system thread
 *   (default since 6.2.0), so the cloud connection stays alive during a burst.
 *
 *   Each burst measures its own achieved sample rate. Bursts that drift more
 *   than SAMPLE_RATE_TOLERANCE from nominal -- because a system interrupt
 *   stole time -- are rejected rather than folded into the average, where they
 *   would smear the spectrum. Rejections are published as n_dropped.
 *
 * WHY ULTRA_LOW_POWER AND NOT HIBERNATE
 *   On the Argon, HIBERNATE cannot wake on a timer -- only on a pin. ULP is
 *   the only Gen 3 sleep mode supporting timed wake, and it resumes execution
 *   on the line after System.sleep() with all variables intact.
 *
 *   ULP powers down the ADC and GPIO, so pinMode() is re-asserted on wake.
 * ---------------------------------------------------------------------------
 *
 * ARGON SETUP
 *   1. Attach the WiFi antenna to the u.FL connector before powering up.
 *   2. Store WiFi credentials once over USB:  particle serial wifi
 *   3. Campus eduroam is WPA2-Enterprise and fiddlier than a PSK network;
 *      a dedicated IoT SSID is the easier path if one is available.
 *
 * WEBHOOK / DATABASE
 *   "ndvi-vals" currently expects band_r..band_w / temp_f / deviceid. These
 *   are new field names, so the webhook JSON template in the Particle console
 *   AND the receiving MyGeoHub table both need the new columns first.
 *
 * Band values are dB relative to ADC full scale (dBFS) -- RELATIVE, not SPL.
 */

#include "Particle.h"
#include <math.h>

// System thread is enabled by default in Device OS 6.2.0 and later, so
// SYSTEM_THREAD(ENABLED) is omitted here (it only emits a deprecation notice).
// Uncomment if you need to build against an older Device OS:
// SYSTEM_THREAD(ENABLED);

// ============================================================================
// DUTY CYCLE  --  the two main knobs
// ============================================================================

// Set false to run continuously (wall power / bench testing).
const bool ENABLE_SLEEP_CYCLE = true;

// How long the device stays awake each cycle: WiFi connect + sample + publish.
// Must be long enough to associate with the AP and get at least one spectrum
// out. 5 minutes is generous; 2 minutes is usually workable on a strong AP.
const unsigned long AWAKE_TIME_MS = 5UL * 60UL * 1000UL;    // 5 minutes

// How long the device sleeps between awake windows.
// Gen 3 maximum is roughly 24 days.
const unsigned long SLEEP_TIME_MS = 25UL * 60UL * 1000UL;   // 25 minutes

// Keep WiFi powered in standby through sleep. Makes wake-to-publish much
// faster but burns significantly more current -- only worth it for sleep
// periods of a few minutes or less. Leave false for the 25-minute default.
const bool KEEP_WIFI_IN_STANDBY = false;

// Settling time after power-up / wake before the first sample is trusted.
// The MAX4466 bias network needs a few hundred ms; sampling before that logs
// a decaying DC transient across every band.
const unsigned long MIC_WARMUP_MS = 500;

// ============================================================================
// ACOUSTIC CONFIGURATION
// ============================================================================

const int MIC_PIN = A4;   // free on the Argon; not used by the ESP32 interface

// --- Sample rate -----------------------------------------------------------
// Nyquist = SAMPLE_RATE_HZ / 2 is your maximum analyzable frequency.
//   16000 -> 8 kHz ceiling. RECOMMENDED DEFAULT for terrestrial soundscapes:
//            covers geophony, anthropophony, and nearly all passerine song.
//    8000 -> 4 kHz ceiling. Traffic/machinery/low-frequency work only.
//   32000 -> 16 kHz ceiling. Needed for orthopteran stridulation (peaks
//            5-15 kHz). Verify the mic capsule responds up there first.
// Practical ceiling: analogRead() takes ~10-15 us on nRF52, so above roughly
// 40 kHz the loop cannot keep its schedule and every window gets rejected.
// Watch n_dropped after changing this.
const uint32_t SAMPLE_RATE_HZ = 16000;

// Reject a captured window if its achieved rate drifts more than this
// fraction from nominal. 0.02 = 2%.
const float SAMPLE_RATE_TOLERANCE = 0.02f;

// --- FFT window ------------------------------------------------------------
// Power of two. Frequency resolution = SAMPLE_RATE_HZ / FFT_SIZE.
//    512 -> 31.3 Hz bins, 32 ms window
//   1024 -> 15.6 Hz bins, 64 ms window   RECOMMENDED DEFAULT
//   2048 ->  7.8 Hz bins, 128 ms window
const uint16_t FFT_SIZE = 1024;

// --- Averaging (Welch's method) -------------------------------------------
// Windows averaged into one reported spectrum. 31 x 64 ms ~= 2.0 s.
const uint16_t AVG_WINDOWS = 31;

// --- Reporting cadence within the awake window -----------------------------
// A spectrum is published every REPORT_INTERVAL_MS while awake. The first
// spectrum after each wake always publishes regardless of this value.
// Set >= AWAKE_TIME_MS for exactly one publish per wake cycle.
const uint32_t REPORT_INTERVAL_MS = 60000;

// --- Band edges (Hz) -------------------------------------------------------
//   20-125    wind, rain on canopy, distant traffic rumble, handling noise
//   125-250   engine/machinery fundamentals
//   250-500   mixed; low anthropophony
//   500-1000  transition
//   1000-2000 ANTHROPOPHONY (NDSI denominator term)
//   2000-4000 BIOPHONY low  (most passerine song)
//   4000-8000 BIOPHONY high (insects, high passerines)
//
// IMPORTANT: the named publish variables below assume exactly these 7 bands.
// If you change this array, update assignBandVariables() and the JSON keys.
const float BAND_EDGES_HZ[] = {20, 125, 250, 500, 1000, 2000, 4000, 8000};
const uint8_t N_EDGES = sizeof(BAND_EDGES_HZ) / sizeof(BAND_EDGES_HZ[0]);
const uint8_t N_BANDS = N_EDGES - 1;

const uint8_t NDSI_ANTHRO_BAND = 4;      // 1000-2000 Hz
const uint8_t NDSI_BIO_BAND_LO = 5;      // 2000-4000 Hz
const uint8_t NDSI_BIO_BAND_HI = 6;      // 4000-8000 Hz

// ============================================================================
// TRANSMISSION
// ============================================================================

const char * acousticEventName = "ndvi-vals";
const char * MySiteID = "Stewart";

// WiFi associates in seconds. Capped well inside AWAKE_TIME_MS so a failed
// association still leaves time to sleep on schedule rather than overrunning.
const unsigned long CONNECT_TIMEOUT = 60UL * 1000UL;

char msg[512];   // Particle.publish limit is 622 bytes

// --- Publish variables: acoustic band energies, dBFS -----------------------
float acBand125;   //   20 -  125 Hz
float acBand250;   //  125 -  250 Hz
float acBand500;   //  250 -  500 Hz
float acBand1k;    //  500 - 1000 Hz
float acBand2k;    // 1000 - 2000 Hz
float acBand4k;    // 2000 - 4000 Hz
float acBand8k;    // 4000 - 8000 Hz
float acNdsi;      // Normalized Difference Soundscape Index, -1..+1
float acWindows;   // FFT windows averaged (QC)
float acDropped;   // Windows rejected for bad sample timing (QC)
float acRssi;      // WiFi signal strength, dBm (QC / siting)

// ============================================================================
// STATE
// ============================================================================

const uint16_t N_BINS = FFT_SIZE / 2 + 1;

static uint16_t sampleBuf[FFT_SIZE];

static float fftRe[FFT_SIZE];
static float fftIm[FFT_SIZE];
static float hann[FFT_SIZE];
static float twRe[FFT_SIZE / 2];
static float twIm[FFT_SIZE / 2];

static float powerAcc[N_BINS];
static uint16_t windowsAccumulated = 0;
static uint32_t rejectedWindows = 0;
static float rateSumHz = 0.0f;          // for averaging achieved sample rate

static uint32_t lastReport = 0;
static unsigned long awakeStartTime = 0;
static uint32_t cycleCount = 0;

// Function prototypes
bool captureWindow(float *achievedRateHz);
void assignBandVariables(const float *bandDb, float ndsi);
void publishAcousticData();
void enterSleepMode();
void wakeUp();

// ============================================================================
// SAMPLING  --  blocking, paced off micros()
// ============================================================================

// Fills sampleBuf with FFT_SIZE samples. Returns false if the achieved rate
// drifted outside tolerance (system interrupt stole time), in which case the
// window should be discarded rather than averaged in.
bool captureWindow(float *achievedRateHz) {
    const uint32_t periodUs = 1000000UL / SAMPLE_RATE_HZ;

    uint32_t t0 = micros();
    uint32_t next = t0;

    for (uint16_t i = 0; i < FFT_SIZE; i++) {
        // Signed comparison handles the ~71 minute micros() rollover.
        while ((int32_t)(micros() - next) < 0) { /* spin */ }
        sampleBuf[i] = analogRead(MIC_PIN);     // 0..4095, 12-bit
        next += periodUs;
    }

    uint32_t elapsedUs = micros() - t0;
    if (elapsedUs == 0) return false;

    float rate = (float)FFT_SIZE * 1000000.0f / (float)elapsedUs;
    *achievedRateHz = rate;

    float drift = fabsf(rate - (float)SAMPLE_RATE_HZ) / (float)SAMPLE_RATE_HZ;
    return (drift <= SAMPLE_RATE_TOLERANCE);
}

// ============================================================================
// FFT  --  in-place iterative radix-2, decimation in time
// ============================================================================

static void fftInitTables() {
    for (uint16_t i = 0; i < FFT_SIZE; i++) {
        // Hann window; coherent gain 0.5, compensated in the power scaling.
        hann[i] = 0.5f * (1.0f - cosf(2.0f * (float)M_PI * i / (FFT_SIZE - 1)));
    }
    for (uint16_t k = 0; k < FFT_SIZE / 2; k++) {
        float ang = -2.0f * (float)M_PI * k / FFT_SIZE;
        twRe[k] = cosf(ang);
        twIm[k] = sinf(ang);
    }
}

static void fftRun() {
    // Bit-reversal permutation
    for (uint16_t i = 1, j = 0; i < FFT_SIZE; i++) {
        uint16_t bit = FFT_SIZE >> 1;
        for (; j & bit; bit >>= 1) j ^= bit;
        j ^= bit;
        if (i < j) {
            float t;
            t = fftRe[i]; fftRe[i] = fftRe[j]; fftRe[j] = t;
            t = fftIm[i]; fftIm[i] = fftIm[j]; fftIm[j] = t;
        }
    }

    // Butterflies
    for (uint16_t len = 2; len <= FFT_SIZE; len <<= 1) {
        uint16_t half = len >> 1;
        uint16_t step = FFT_SIZE / len;
        for (uint16_t i = 0; i < FFT_SIZE; i += len) {
            for (uint16_t j = 0; j < half; j++) {
                uint16_t k = j * step;
                float cr = twRe[k], ci = twIm[k];
                uint16_t a = i + j, b = i + j + half;
                float vr = fftRe[b] * cr - fftIm[b] * ci;
                float vi = fftRe[b] * ci + fftIm[b] * cr;
                fftRe[b] = fftRe[a] - vr;
                fftIm[b] = fftIm[a] - vi;
                fftRe[a] += vr;
                fftIm[a] += vi;
            }
        }
    }
}

// ============================================================================
// PROCESSING
// ============================================================================

static void accumulateWindow() {
    // 1. Remove DC. The MAX4466 self-biases near VCC/2, but the offset drifts
    //    with temperature and supply, so measure it per window rather than
    //    assuming 2048. A wrong offset dumps energy into bin 0 and leaks into
    //    the lowest band.
    float mean = 0.0f;
    for (uint16_t i = 0; i < FFT_SIZE; i++) mean += (float)sampleBuf[i];
    mean /= (float)FFT_SIZE;

    // 2. Normalize to +/-1.0 full scale and apply the window.
    for (uint16_t i = 0; i < FFT_SIZE; i++) {
        fftRe[i] = (((float)sampleBuf[i] - mean) / 2048.0f) * hann[i];
        fftIm[i] = 0.0f;
    }

    fftRun();

    // 3. Single-sided power spectrum, corrected for Hann coherent gain (0.5).
    const float scale = 2.0f / ((float)FFT_SIZE * 0.5f);
    for (uint16_t k = 0; k < N_BINS; k++) {
        float mag = fftRe[k] * fftRe[k] + fftIm[k] * fftIm[k];
        powerAcc[k] += mag * scale * scale;
    }
    windowsAccumulated++;
}

// binHz is derived from the MEASURED sample rate, not the nominal one, so band
// edges land where they should even if the loop ran slightly slow.
static void computeBands(float *bandDb, float *bandLinear, float binHz,
                         float nyquistHz) {
    for (uint8_t b = 0; b < N_BANDS; b++) {
        float loHz = BAND_EDGES_HZ[b];
        float hiHz = BAND_EDGES_HZ[b + 1];
        if (hiHz > nyquistHz) hiHz = nyquistHz;

        uint16_t kLo = (uint16_t)ceilf(loHz / binHz);
        uint16_t kHi = (uint16_t)floorf(hiHz / binHz);
        if (kLo < 1) kLo = 1;                       // never include the DC bin
        if (kHi >= N_BINS) kHi = N_BINS - 1;

        float sum = 0.0f;
        for (uint16_t k = kLo; k <= kHi && kLo <= kHi; k++) {
            sum += powerAcc[k] / (float)windowsAccumulated;
        }
        bandLinear[b] = sum;
        bandDb[b] = 10.0f * log10f(sum + 1e-12f);   // floor avoids log(0)
    }
}

static void resetAccumulator() {
    for (uint16_t k = 0; k < N_BINS; k++) powerAcc[k] = 0.0f;
    windowsAccumulated = 0;
    rateSumHz = 0.0f;
}

void assignBandVariables(const float *bandDb, float ndsi) {
    acBand125 = bandDb[0];
    acBand250 = bandDb[1];
    acBand500 = bandDb[2];
    acBand1k  = bandDb[3];
    acBand2k  = bandDb[4];
    acBand4k  = bandDb[5];
    acBand8k  = bandDb[6];
    acNdsi    = ndsi;
    acWindows = (float)windowsAccumulated;
    acDropped = (float)rejectedWindows;
    acRssi    = WiFi.ready() ? (float)WiFi.RSSI() : 0.0f;
}

// ============================================================================
// PUBLISH
// ============================================================================

void publishAcousticData() {
    if (!Particle.connected()) {
        Serial.printlnf("Waiting for WiFi cloud connection (up to %lu seconds)...",
                        CONNECT_TIMEOUT / 1000);
        waitFor(Particle.connected, CONNECT_TIMEOUT);
    }

    if (Particle.connected()) {
        snprintf(msg, sizeof(msg),
            "{\"band_125\":\"%.2f\",\"band_250\":\"%.2f\",\"band_500\":\"%.2f\","
            "\"band_1k\":\"%.2f\",\"band_2k\":\"%.2f\",\"band_4k\":\"%.2f\","
            "\"band_8k\":\"%.2f\",\"ndsi\":\"%.3f\",\"n_windows\":\"%.0f\","
            "\"n_dropped\":\"%.0f\",\"rssi\":\"%.0f\",\"deviceid\":\"%s\"}",
            acBand125, acBand250, acBand500, acBand1k, acBand2k,
            acBand4k, acBand8k, acNdsi, acWindows, acDropped, acRssi, MySiteID);

        Serial.println("Publishing acoustic band data via WiFi...");
        Serial.println(msg);

        bool published = Particle.publish(acousticEventName, msg, PRIVATE, NO_ACK);
        Serial.println(published ? "Acoustic data published successfully to webhook"
                                 : "Failed to publish acoustic data");

        delay(1000);   // let the publish go out
    } else {
        Serial.println("No WiFi connection within timeout - skipping publish this cycle");
    }
}

// ============================================================================
// SLEEP / WAKE
// ============================================================================

void enterSleepMode() {
    Serial.printlnf("Cycle %lu complete. Preparing for ultra low power sleep (%lu s)...",
                    (unsigned long)cycleCount, SLEEP_TIME_MS / 1000);

    SystemSleepConfiguration config;
    config.mode(SystemSleepMode::ULTRA_LOW_POWER)
          .duration(SLEEP_TIME_MS);

    if (KEEP_WIFI_IN_STANDBY) {
        // Faster wake-to-publish, materially more current. Only sensible for
        // short sleeps. NETWORK_INTERFACE_WIFI_STA is the station interface;
        // NETWORK_INTERFACE_WIFI_AP is the access-point side and is not what
        // you want here.
        config.network(NETWORK_INTERFACE_WIFI_STA,
                       SystemSleepNetworkFlag::INACTIVE_STANDBY);
        Serial.println("WiFi kept in standby through sleep");
    } else {
        Particle.disconnect();
        WiFi.off();
        delay(500);
    }

    Serial.flush();

    // ULP resumes on the next line with all variables intact.
    System.sleep(config);

    wakeUp();
}

void wakeUp() {
    Serial.begin(9600);
    delay(100);
    Serial.println("Waking from ultra low power sleep...");

    // ULP powers GPIO down; re-assert the analog input.
    pinMode(MIC_PIN, INPUT);

    if (!KEEP_WIFI_IN_STANDBY) {
        WiFi.on();
        WiFi.connect();
        Particle.connect();
    }

    // Let the mic bias network settle before trusting any sample.
    delay(MIC_WARMUP_MS);

    // Reset the cycle clock. lastReport is zeroed so the first spectrum of
    // each awake window always publishes, regardless of REPORT_INTERVAL_MS
    // and regardless of whether millis() advanced during sleep.
    awakeStartTime = millis();
    lastReport = 0;
    rejectedWindows = 0;
    resetAccumulator();
    cycleCount++;

    Serial.printlnf("Awake for %lu s", AWAKE_TIME_MS / 1000);
}

// ============================================================================
// MAIN
// ============================================================================

void setup() {
    Serial.begin(9600);
    delay(50);
    waitFor(Serial.isConnected, 10000);

    pinMode(MIC_PIN, INPUT);

    // Credentials live in flash -- set once with `particle serial wifi`.
    // WiFi.setCredentials("SSID", "password");
    WiFi.on();
    WiFi.connect();
    Particle.connect();

    fftInitTables();
    resetAccumulator();

    delay(MIC_WARMUP_MS);

    awakeStartTime = millis();
    lastReport = 0;

    Serial.printlnf("Soundscape logger (Argon): %lu Hz nominal, %u-pt FFT, "
                    "%.2f Hz bins, %u windows/report (%.2f s) -> event \"%s\"",
                    SAMPLE_RATE_HZ, FFT_SIZE,
                    (float)SAMPLE_RATE_HZ / (float)FFT_SIZE, AVG_WINDOWS,
                    AVG_WINDOWS * (float)FFT_SIZE / SAMPLE_RATE_HZ,
                    acousticEventName);
    if (ENABLE_SLEEP_CYCLE) {
        Serial.printlnf("Duty cycle: %lu s awake / %lu s ULP sleep",
                        AWAKE_TIME_MS / 1000, SLEEP_TIME_MS / 1000);
    } else {
        Serial.println("Duty cycle disabled - running continuously");
    }
}

void loop() {
    // End of the awake window? Check first so a slow capture cannot overrun.
    if (ENABLE_SLEEP_CYCLE && (millis() - awakeStartTime) >= AWAKE_TIME_MS) {
        enterSleepMode();
        return;
    }

    // Capture and accumulate one window (blocking, ~FFT_SIZE/SAMPLE_RATE_HZ).
    float achievedRate = 0.0f;
    if (captureWindow(&achievedRate)) {
        accumulateWindow();
        rateSumHz += achievedRate;
    } else {
        rejectedWindows++;
    }

    if (windowsAccumulated < AVG_WINDOWS) return;

    bool dueToReport = (lastReport == 0) ||
                       ((millis() - lastReport) >= REPORT_INTERVAL_MS);

    if (dueToReport) {
        float meanRate = rateSumHz / (float)windowsAccumulated;
        float binHz = meanRate / (float)FFT_SIZE;
        float nyquist = meanRate / 2.0f;

        float bandDb[N_BANDS];
        float bandLin[N_BANDS];
        computeBands(bandDb, bandLin, binHz, nyquist);

        float anthro = bandLin[NDSI_ANTHRO_BAND];
        float bio    = bandLin[NDSI_BIO_BAND_LO] + bandLin[NDSI_BIO_BAND_HI];
        float ndsi   = (bio - anthro) / (bio + anthro + 1e-12f);

        assignBandVariables(bandDb, ndsi);

        Serial.printf("t=%lu fs=%.0fHz", millis(), meanRate);
        for (uint8_t b = 0; b < N_BANDS; b++) {
            Serial.printf(" %.0f-%.0fHz=%.1fdB",
                          BAND_EDGES_HZ[b], BAND_EDGES_HZ[b + 1], bandDb[b]);
        }
        Serial.printlnf(" NDSI=%.3f rejected=%lu rssi=%.0f",
                        ndsi, rejectedWindows, acRssi);

        publishAcousticData();

        lastReport = millis();
        rejectedWindows = 0;
    }

    resetAccumulator();
}

/* ===========================================================================
 * NOTES
 *
 * IF n_dropped IS HIGH
 *   Windows are being rejected for timing drift. Most likely causes, in order:
 *   SAMPLE_RATE_HZ set too high for analogRead() to keep up (try 8000 to
 *   confirm), or heavy system-thread activity during WiFi association. Some
 *   rejections in the first seconds after wake are normal and expected.
 *   Persistent high counts across a whole awake window mean the configuration
 *   is not achievable and the spectra you do get are built on few windows.
 *
 * IF YOU NEED GAPLESS OR HIGHER-RATE SAMPLING
 *   The real fix is DMA: nrfx_saadc with a PPI-triggered TIMER, which samples
 *   into a buffer with zero CPU involvement and no jitter. That is a
 *   substantially bigger piece of work and pulls in nRF SDK headers directly,
 *   so it is only worth it if the blocking sampler proves inadequate.
 *
 * SIZING AWAKE_TIME_MS
 *   The awake window has to cover: WiFi association (seconds on a good AP,
 *   longer at the edge of range), MIC_WARMUP_MS, AVG_WINDOWS of audio, and
 *   the publish. If a cycle ends with no data in the database, lengthen this
 *   before touching anything else, and check the rssi field in rows that did
 *   land.
 *
 * WHAT THE DEVICE ACTUALLY MEASURES
 *   With the default 5/25 cycle it hears roughly 2 seconds of audio out of
 *   every 30 minutes -- about 0.1% duty. Fine for characterizing a site's
 *   general acoustic character, useless for detecting events (a single call,
 *   a passing vehicle). If you need event detection rather than ambient
 *   characterization, raise AVG_WINDOWS substantially so each wake integrates
 *   over minutes rather than seconds. Sampling more often is not the same as
 *   sampling longer.
 *
 * ALIGNING WAKES TO THE CLOCK
 *   This uses fixed durations, so wakes drift relative to wall-clock time.
 *   For :00/:30 alignment like the air quality kit, replace SLEEP_TIME_MS in
 *   enterSleepMode() with a value computed from Time.minute() and
 *   Time.second(). The RTC keeps running through ULP sleep.
 *
 * CALIBRATION
 *   dBFS is relative. Set the MAX4466 gain trimpot once per board and never
 *   touch it: play a steady 1 kHz tone at a fixed level, raise gain until the
 *   peak sits about 6 dB below clipping, and record the setting. Two boards at
 *   different gain settings produce band values you cannot compare directly.
 *
 * CLIPPING
 *   Not detected here, and a clipped waveform generates broadband harmonic
 *   energy that looks like real signal. Before a real deployment, check in
 *   accumulateWindow() for raw samples at 0 or 4095 and flag the window. Loud
 *   rain and close wind gusts are the usual culprits.
 *
 * WIND
 *   Outdoors, a foam windscreen is not optional. Without one the 20-250 Hz
 *   bands are mostly wind artifact.
 * =========================================================================== */
```
