# scripts/2_attack_scenarios.R
# Injects DDoS (network), intrusion (event log), and sensor spoofing attacks.

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

# ---------------- helper: regex-free timestamp parser -----------------------
parse_timestamp_safe <- function(x) {
  x0 <- trimws(as.character(x))
  x0[x0 == ""] <- NA_character_
  x0 <- sub("Z$", "+0000", x0, perl = TRUE)
  x0 <- sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", x0, perl = TRUE)
  as.POSIXct(
    x0, tz = "UTC",
    tryFormats = c(
      "%Y-%m-%dT%H:%M:%S%z","%Y-%m-%dT%H:%M%z","%Y-%m-%dT%H:%M:%S","%Y-%m-%dT%H:%M",
      "%Y-%m-%d %H:%M:%S%z","%Y-%m-%d %H:%M%z","%Y-%m-%d %H:%M:%S","%Y-%m-%d %H:%M",
      "%d-%m-%Y %H:%M:%S","%d-%m-%Y %H:%M","%m-%d-%Y %H:%M:%S","%m-%d-%Y %H:%M",
      "%Y-%m-%d"
    )
  )
}

# ------------------- Load baselines (raw CSVs) ------------------------------
dir.create("data", showWarnings = FALSE)
net_raw  <- fread("data/network_baseline.csv",  colClasses = list(character = "timestamp"))
sens_raw <- fread("data/sensors_baseline.csv", colClasses = list(character = "timestamp"))

# ------------------- Sanitise timestamps once --------------------------------
net_raw[,  ts := parse_timestamp_safe(timestamp)]
sens_raw[, ts := parse_timestamp_safe(timestamp)]

if (anyNA(net_raw$ts))  message("⚠️ Unparsed network timestamps (sample): ", paste(head(net_raw[is.na(ts)]$timestamp, 3), collapse=" | "))
if (anyNA(sens_raw$ts)) message("⚠️ Unparsed sensor timestamps (sample): ",  paste(head(sens_raw[is.na(ts)]$timestamp, 3), collapse=" | "))

net_raw  <- net_raw[ !is.na(ts) ]
sens_raw <- sens_raw[!is.na(ts) ]

if (nrow(net_raw)  == 0) stop("network_baseline.csv contains no parseable timestamps.")
if (nrow(sens_raw) == 0) stop("sensors_baseline.csv contains no parseable timestamps.")

# Write ISO-like strings so we can parse with a fixed format later
net_raw[,  timestamp := format(ts, "%Y-%m-%d %H:%M:%S")][,  ts := NULL]
sens_raw[, timestamp := format(ts, "%Y-%m-%d %H:%M:%S")][, ts := NULL]

fwrite(net_raw,  "data/network_baseline.csv")
fwrite(sens_raw, "data/sensors_baseline.csv")
message("Sanitised baseline CSVs written to data/")

# ------------------- Re-load typed & prepare for attacks --------------------
net     <- fread("data/network_baseline.csv")
sensors <- fread("data/sensors_baseline.csv")

# IMPORTANT: parse using the exact format we just wrote (no lubridate here)
net[,     timestamp := as.POSIXct(timestamp,  format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]
sensors[, timestamp := as.POSIXct(timestamp,  format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]

# Drop any residual NAs to protect seq()
net     <- net[!is.na(timestamp)]
sensors <- sensors[!is.na(timestamp)]

# --- DDoS injection (network) -----------------------------------------------
ddos_start <- ymd_hms("2025-08-05 10:00:00", tz = "UTC")
ddos_end   <- ddos_start + hours(2)        # <-- define BEFORE using

net[, is_attack := 0L]
net[timestamp >= ddos_start & timestamp <= ddos_end, is_attack := 1L]

# Increase traffic massively during attack
net[timestamp >= ddos_start & timestamp <= ddos_end,
    req_per_min := req_per_min + rpois(.N, lambda = 400L)]

# --- Intrusion event log ----------------------------------------------------
# Create synthetic events at 10-min cadence across the (clean) network timeline
t0 <- min(net$timestamp)   # finite after NA drop
t1 <- max(net$timestamp)
events <- data.table(timestamp = seq(t0, t1, by = "10 min"))

set.seed(99)
events[, event_type := sample(c("heartbeat","login_success","config_read"),
                              .N, replace = TRUE, prob = c(0.8, 0.15, 0.05))]
events[, user   := sample(c("ops1","ops2","sys"), .N, replace = TRUE)]
events[, src_ip := paste0(
  sample(1:255, .N, TRUE), ".", sample(0:255, .N, TRUE), ".",
  sample(0:255, .N, TRUE), ".", sample(1:254, .N, TRUE)
)]

# Inject intrusion burst around ddos_start (±1 hour)
intr_mask <- events$timestamp >= (ddos_start - hours(1)) & events$timestamp <= (ddos_start + hours(1))
n_intr <- sum(intr_mask)
if (n_intr > 0) {
  set.seed(100)
  events[intr_mask, event_type := sample(
    c("login_fail","login_fail","login_fail","login_success","user_add","priv_escalate"),
    n_intr, replace = TRUE, prob = c(0.35, 0.35, 0.20, 0.05, 0.03, 0.02)
  )]
}

# --- Sensor spoofing --------------------------------------------------------
spoof_start <- ymd_hms("2025-08-09 06:00:00", tz = "UTC")
spoof_end   <- spoof_start + hours(8)

sensors[, is_spoof := 0L]
sensors[timestamp >= spoof_start & timestamp <= spoof_end,
        `:=`(is_spoof = 1L, soil_moisture = soil_moisture + 15)]

# --- Save attacked artifacts ------------------------------------------------
fwrite(net,     "data/network_attacked.csv")
fwrite(events,  "data/events_log.csv")
fwrite(sensors, "data/sensors_attacked.csv")
message("Attack scenarios written to data/")

