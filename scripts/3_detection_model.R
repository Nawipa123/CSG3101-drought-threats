# scripts/3_detection_model.R
suppressPackageStartupMessages({
  library(data.table); library(lubridate)
  library(caret); library(randomForest); library(forecast)
})

# ------------------------------- Load ---------------------------------------
net  <- fread("data/network_attacked.csv",  colClasses = list(character = "timestamp"))
ev   <- fread("data/events_log.csv",        colClasses = list(character = "timestamp"))
sens <- fread("data/sensors_attacked.csv",  colClasses = list(character = "timestamp"))

# -------------------------- Parse timestamps (fixed) -------------------------
# Script 2 writes "%Y-%m-%d %H:%M:%S" (UTC). Parse exactly that (no regex).
net[,  timestamp := as.POSIXct(timestamp,  format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]
ev[,   timestamp := as.POSIXct(timestamp,  format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]
sens[, timestamp := as.POSIXct(timestamp,  format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]

# Drop any rows with unparsed timestamps
net  <- net[ !is.na(timestamp) ]
ev   <- ev[  !is.na(timestamp) ]
sens <- sens[!is.na(timestamp) ]

# -------------------------- Coerce/clean columns -----------------------------
# Network
net[, req_per_min := as.numeric(req_per_min)]
if (!"is_attack" %in% names(net)) net[, is_attack := 0L]
net[is.na(is_attack), is_attack := 0L]
net[, is_attack := as.integer(is_attack)]

# Sensors
sens[, soil_moisture := as.numeric(soil_moisture)]
sens <- sens[is.finite(soil_moisture)]  # keep only finite readings
if (!"is_spoof" %in% names(sens)) sens[, is_spoof := 0L]
sens[is.na(is_spoof), is_spoof := 0L]
sens[, is_spoof := as.integer(is_spoof)]

# --------------------------- DDoS: rule-based --------------------------------
thr <- mean(net$req_per_min, na.rm = TRUE) + 4 * sd(net$req_per_min, na.rm = TRUE)
net[, ddos_flag := as.integer(req_per_min > thr)]

den <- sum(net$is_attack == 1L, na.rm = TRUE)
ddos_detect_rate <- if (den > 0) {
  sum(net$ddos_flag == 1L & net$is_attack == 1L, na.rm = TRUE) / den
} else NA_real_

# ------------------------- Intrusion: heuristics -----------------------------
ev[, hour := floor_date(timestamp, unit = "hour")]
sus_types <- c("login_fail","user_add","priv_escalate")
intr_hour <- ev[, .(sus_count = sum(event_type %in% sus_types)), by = hour]
intr_hour[, intr_flag := as.integer(sus_count >= 3L)]
ev <- merge(ev, intr_hour, by = "hour", all.x = TRUE)

# -------------------- Sensor spoofing detection ------------------------------
# Prefer ARIMA residuals; if not enough clean data, use z-score fallback.
normal_cut <- ymd_hms("2025-08-08 00:00:00", tz = "UTC")
sens <- sens[order(timestamp)]
norm_window <- sens[timestamp < normal_cut & is.finite(soil_moisture)]

use_arima <- nrow(norm_window) >= 30 && sum(is.finite(norm_window$soil_moisture)) >= 30

if (use_arima) {
  fit <- auto.arima(norm_window$soil_moisture)
  h <- nrow(sens) - nrow(norm_window)
  fc_all <- if (h > 0) forecast(fit, h = h) else NULL
  
  pred <- rep(NA_real_, nrow(sens))
  len_fit <- length(fitted(fit))
  pred[seq_len(len_fit)] <- as.numeric(fitted(fit))
  if (h > 0) pred[(len_fit + 1):nrow(sens)] <- as.numeric(fc_all$mean)
  
  sens[, resid := soil_moisture - pred]
  z <- (sens$resid - mean(sens$resid, na.rm = TRUE)) / sd(sens$resid, na.rm = TRUE)
  sens[, spoof_flag := as.integer(abs(z) > 3)]
} else {
  # Fallback: simple rolling-median residual (no forecast needed)
  k <- 25L  # ~2 hours if 5-min sampling; adjust if your cadence differs
  v <- sens$soil_moisture
  # rolling median (pad with NA on edges)
  med <- rep(NA_real_, length(v))
  if (length(v) >= k) {
    for (i in seq_along(v)) {
      lo <- max(1L, i - floor(k/2)); hi <- min(length(v), i + floor(k/2))
      med[i] <- median(v[lo:hi], na.rm = TRUE)
    }
  } else {
    med <- rep(median(v, na.rm = TRUE), length(v))
  }
  resid_alt <- v - med
  z <- (resid_alt - mean(resid_alt, na.rm = TRUE)) / sd(resid_alt, na.rm = TRUE)
  sens[, resid := resid_alt]
  sens[, spoof_flag := as.integer(abs(z) > 3)]
}

# ------------------ ML classifier (Random Forest) ---------------------------
net[, minute := lubridate::minute(timestamp)]
net[, hour   := lubridate::hour(timestamp)]
ml_df <- net[, .(req_per_min, minute, hour, is_attack)]
ml_df <- na.omit(ml_df)

rf <- NULL
cm <- NULL
acc <- sen_ <- spe <- NA_real_

if (nrow(ml_df) >= 20 && length(unique(ml_df$is_attack)) >= 2) {
  set.seed(123)
  idx <- createDataPartition(ml_df$is_attack, p = 0.7, list = FALSE)
  train <- ml_df[idx, ]
  test  <- ml_df[-idx, ]
  
  # Ensure both classes appear in train/test; if not, skip ML
  if (length(unique(train$is_attack)) >= 2 && nrow(test) > 0) {
    rf <- randomForest(
      as.factor(is_attack) ~ req_per_min + minute + hour,
      data = train,
      ntree = 200
    )
    yhat <- predict(rf, newdata = test)
    yref <- factor(test$is_attack, levels = levels(yhat))
    cm <- caret::confusionMatrix(yhat, yref, positive = "1")
    
    acc  <- suppressWarnings(as.numeric(cm$overall["Accuracy"]))
    sen_ <- suppressWarnings(as.numeric(cm$byClass["Sensitivity"]))
    spe  <- suppressWarnings(as.numeric(cm$byClass["Specificity"]))
  }
}

# ------------------------- Save metrics & model ------------------------------
lines_out <- c(
  sprintf("DDoS rule-based detection rate: %s",
          ifelse(is.na(ddos_detect_rate), "NA", round(ddos_detect_rate, 3))),
  sprintf("RF accuracy: %s",
          ifelse(is.na(acc), "NA", round(acc, 3))),
  sprintf("RF sensitivity: %s",
          ifelse(is.na(sen_), "NA", round(sen_, 3))),
  sprintf("RF specificity: %s",
          ifelse(is.na(spe), "NA", round(spe, 3)))
)

dir.create("data", showWarnings = FALSE, recursive = TRUE)
writeLines(lines_out, "data/metrics.txt")

if (!is.null(rf)) saveRDS(rf, "data/rf_model.rds")

message("Detection complete. Metrics written to data/metrics.txt")
