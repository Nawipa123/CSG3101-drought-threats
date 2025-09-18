# scripts/4_visualisation_dashboard.R
# Produce key plots and (optionally) launch Shiny app.
suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(ggplot2)
  # library(plotly)       # uncomment if you’ll export interactive plots
  # library(htmlwidgets)  # needed only when saving plotly html
})

# Ensure output dir
dir.create("figs", showWarnings = FALSE, recursive = TRUE)

# Load
net  <- fread("data/network_attacked.csv")
sens <- fread("data/sensors_attacked.csv")

# Parse timestamps (set TZ as appropriate for your project)
net[,  timestamp := ymd_hms(timestamp, tz = "UTC")]
sens[, timestamp := ymd_hms(timestamp, tz = "UTC")]

# Coerce flags to factors with nice labels (for legend)
net[,  attack_lbl := factor(ifelse(is_attack == 1, "Attack", "Normal"), levels = c("Normal","Attack"))]
sens[, spoof_lbl  := factor(ifelse(is_spoof  == 1, "Spoofed","Normal"), levels = c("Normal","Spoofed"))]

# ---- Plot 1: Network traffic with attack shading/legend --------------------
p1 <- ggplot(net, aes(x = timestamp, y = req_per_min, color = attack_lbl)) +
  geom_line(linewidth = 0.4) +
  labs(
    title = "Network Traffic (req/min)",
    x = "Time", y = "Requests per minute", color = "State"
  ) +
  theme_minimal()

ggsave("figs/network_traffic.png", p1, width = 9, height = 4, dpi = 150)

# ---- Plot 2: Sensor spoofing visualisation --------------------------------
p2 <- ggplot(sens, aes(x = timestamp, y = soil_moisture, color = spoof_lbl)) +
  geom_line(linewidth = 0.4) +
  labs(
    title = "Soil Moisture with Spoofed Segment",
    x = "Time", y = "Soil Moisture", color = "Segment"
  ) +
  theme_minimal()

ggsave("figs/sensor_spoof.png", p2, width = 9, height = 4, dpi = 150)

message("Saved figs/network_traffic.png and figs/sensor_spoof.png")

# ---- Optional: interactive plots (uncomment if desired) --------------------
# library(plotly); library(htmlwidgets)
# p1_html <- plotly::ggplotly(p1)
# p2_html <- plotly::ggplotly(p2)
# htmlwidgets::saveWidget(p1_html, "figs/network_traffic.html", selfcontained = TRUE)
# htmlwidgets::saveWidget(p2_html, "figs/sensor_spoof.html", selfcontained = TRUE)

# To launch the dashboard, open shiny/app.R and click 'Run App' in RStudio.

