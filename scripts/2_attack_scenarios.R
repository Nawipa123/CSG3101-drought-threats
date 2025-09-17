# scripts/2_attack_scenarios.R
# Injects DDoS (network), intrusion (event log), and sensor spoofing attacks.

# Option A: sanitize timestamps in CSVs (no script edits)
library(data.table)
library(lubridate)

# --- network baseline ---
net_raw <- fread("data/network_baseline.csv", colClasses = list(character = "timestamp"))

# Try flexible parsing orders; adjust if your format is different
net_raw[, timestamp := parse_date_time(
  timestamp,
  orders = c("ymd HMS","ymd HM","Ymd HMS","dmy HMS","dmy HM","mdy HMS","mdy HM","ymd_T_HMS"),
  tz = "UTC"
)]

# Drop rows with still-NA timestamps (safer than leaving NA)
net_raw <- net_raw[!is.na(timestamp)]
if (nrow(net_raw) == 0) stop("network_baseline.csv contains no parseable timestamps. Inspect the file.")

# Overwrite CSV (now with POSIX in ISO format)
fwrite(net_raw, "data/network_baseline.csv")

# --- sensors baseline ---
sens_raw <- fread("data/sensors_baseline.csv", colClasses = list(character = "timestamp"))
sens_raw[, timestamp := parse_date_time(
  timestamp,
  orders = c("ymd HMS","ymd HM","Ymd HMS","dmy HMS","dmy HM","mdy HMS","mdy HM","ymd_T_HMS"),
  tz = "UTC"
)]
sens_raw <- sens_raw[!is.na(timestamp)]
if (nrow(sens_raw) == 0) stop("sensors_baseline.csv contains no parseable timestamps. Inspect the file.")
fwrite(sens_raw, "data/sensors_baseline.csv")

# Quick check
net_check <- fread("data/network_baseline.csv")
sens_check <- fread("data/sensors_baseline.csv")
message("network rows: ", nrow(net_check), " ; sensors rows: ", nrow(sens_check))
message("First/Last network timestamps: ", min(as.POSIXct(net_check$timestamp)), " / ", max(as.POSIXct(net_check$timestamp)))
