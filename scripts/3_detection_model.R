# scripts/3_detection_model.R
suppressPackageStartupMessages({
  library(data.table); library(lubridate)
  library(caret); library(randomForest); library(forecast)
})

# Load and parse
net  <- fread("data/network_attacked.csv")
ev   <- fread("data/events_log.csv")
sens <- fread("data/sensors_attacked.csv")

net[,  timestamp := ymd_hms(timestamp, tz="UTC")]
ev[,   timestamp := ymd_hms(timestamp, tz="UTC")]
sens[, timestamp := ymd_hms(timestamp, tz="UTC")]

# --- DDoS detection -------------------------------------------------------
thr <- mean(net$req_per_min) + 4*sd(net$req_per_min)
net[, ddos_flag := as.integer(req_per_min > thr)]
ddos_detect_rate <- with(net, sum(ddos_flag == 1 & is_attack == 1) / sum(is_attack == 1))

# --- Intrusion heuristics -------------------------------------------------
ev[, hour := floor_date(timestamp, unit="hour")]
sus_types <- c("login_fail","user_add","priv_escalate")
intr_hour <- ev[, .(sus_count = sum(event_type %in% sus_types)), by=hour]
intr_hour[, intr_flag := as.integer(sus_count >= 3)]
ev <- merge(ev, intr_hour, by="hour", all.x=TRUE)

# --- Sensor spoofing (ARIMA residuals) -----------------------------------
norm_window <- sens[timestamp < ymd_hms("2025-08-08 00:00:00")]
fit <- auto.arima(norm_window$soil_moisture)
fc_all <- forecast(fit, h = nrow(sens) - nrow(norm_window))

pred <- rep(NA_real_, nrow(sens))
pred[1:length(fitted(fit))] <- fitted(fit)
pred[(length(fitted(fit))+1):nrow(sens)] <- as.numeric(fc_all$mean)

sens[, resid := soil_moisture - pred]
z <- (sens$resid - mean(sens$resid, na.rm=TRUE)) / sd(sens$resid, na.rm=TRUE)
sens[, spoof_flag := as.integer(abs(z) > 3)]

# --- ML classifier (Random Forest) ----------------------------------------
net[, minute := minute(timestamp)]
net[, hour   := hour(timestamp)]
ml_df <- net[, .(req_per_min, minute, hour, is_attack)]

set.seed(123)
idx <- createDataPartition(ml_df$is_attack, p=0.7, list=FALSE)
train <- ml_df[idx,]; test <- ml_df[-idx,]

rf <- randomForest(as.factor(is_attack) ~ ., data=train, ntree=200)
pred <- predict(rf, newdata=test)
cm <- confusionMatrix(pred, as.factor(test$is_attack))

# --- Save metrics and model -----------------------------------------------
writeLines(c(
  paste0("DDoS rule-based detection rate: ", round(ddos_detect_rate,3)),
  paste0("RF accuracy: ", round(cm$overall['Accuracy'],3)),
  paste0("RF sensitivity: ", round(cm$byClass['Sensitivity'],3)),
  paste0("RF specificity: ", round(cm$byClass['Specificity'],3))
), "data/metrics.txt")

saveRDS(rf, "data/rf_model.rds")
message("Detection complete. Metrics written to data/metrics.txt")

