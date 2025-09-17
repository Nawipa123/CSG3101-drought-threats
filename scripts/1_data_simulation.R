# scripts/1_data_simulation.R
# Generates baseline drought sensor data and network traffic logs.
set.seed(3101)
suppressPackageStartupMessages({
  library(data.table); library(lubridate); library(tidyverse)
})

# --- Baseline sensor telemetry (e.g., soil moisture, reservoir level) ----
start <- ymd_hms("2025-08-01 00:00:00")
end   <- ymd_hms("2025-08-14 23:59:00")
ts <- data.table(timestamp = seq(start, end, by="5 min"))  # 2 weeks, every 5 min

# Simulate baseline with daily seasonality + noise
daily_cycle <- function(n, base=50, amp=8) base + amp*sin(seq(0, 2*pi, length.out=n))
n <- nrow(ts)
ts[, soil_moisture := pmax(0, daily_cycle(n, base=40, amp=6) + rnorm(n, 0, 1.5))]
ts[, reservoir_level := pmax(0, daily_cycle(n, base=70, amp=4) + rnorm(n, 0, 1.0))]

# --- Baseline network traffic (requests per minute) -----------------------
# Aggregate minute-level counts for simplicity.
net <- data.table(timestamp = seq(start, end, by="1 min"))
# Normal traffic: Poisson with gentle diurnal cycle
hour_factor <- 1 + 0.5*sin(2*pi*hour(net$timestamp)/24)
lambda <- 20 * hour_factor  # avg 20 req/min with day-night variation
set.seed(42)
net[, req_per_min := rpois(.N, lambda = lambda)]

# Save baseline artifacts
dir.create("data", showWarnings=FALSE)
fwrite(ts,  "data/sensors_baseline.csv")
fwrite(net, "data/network_baseline.csv")
message("Baseline data written to data/")

