# shiny/app.R - Prototype monitoring dashboard
library(shiny); library(shinydashboard)
library(data.table); library(ggplot2); library(lubridate)

# --- Load data (relative to app dir) ---------------------------------------
net  <- fread("../data/network_attacked.csv")
sens <- fread("../data/sensors_attacked.csv")

# Parse timestamps & coerce types
net[,  `:=`(timestamp = ymd_hms(timestamp, tz = "UTC"),
            req_per_min = as.numeric(req_per_min),
            is_attack = as.integer(is_attack))]
sens[, `:=`(timestamp = ymd_hms(timestamp, tz = "UTC"),
            soil_moisture = as.numeric(soil_moisture),
            is_spoof = as.integer(is_spoof))]

# Slider bounds (after parsing/coercion)
thr_min <- round(mean(net$req_per_min, na.rm = TRUE))
thr_max <- round(max(net$req_per_min, na.rm = TRUE))
thr_def <- round(mean(net$req_per_min, na.rm = TRUE) + 4 * sd(net$req_per_min, na.rm = TRUE))

header <- dashboardHeader(title = "Drought System Cyber Monitor")

sidebar <- dashboardSidebar(
  sliderInput("thr", "DDoS Threshold (req/min):",
              min = thr_min, max = thr_max, value = thr_def, step = 1),
  checkboxInput("show_spoof","Highlight Sensor Spoofing", TRUE)
)

body <- dashboardBody(
  fluidRow(
    valueBoxOutput("vb_ddos", width = 6),
    valueBoxOutput("vb_spoof", width = 6)
  ),
  fluidRow(
    box(width = 12, title = "Network Traffic", status = "primary", solidHeader = TRUE,
        plotOutput("plot_net", height = "250px"))
  ),
  fluidRow(
    box(width = 12, title = "Soil Moisture", status = "success", solidHeader = TRUE,
        plotOutput("plot_sens", height = "250px"))
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session){
  
  output$vb_ddos <- renderValueBox({
    thr <- input$thr
    ddos_now <- sum(net$req_per_min > thr, na.rm = TRUE)
    valueBox(ddos_now, "Minutes above threshold", icon = icon("exclamation-triangle"), color = "yellow")
  })
  
  output$vb_spoof <- renderValueBox({
    sp <- sum(sens$is_spoof == 1L, na.rm = TRUE)
    valueBox(sp, "Spoofed readings count", icon = icon("tint"), color = "green")
  })
  
  output$plot_net <- renderPlot({
    thr <- input$thr
    ggplot(net, aes(timestamp, req_per_min)) +
      geom_line(linewidth = 0.4) +
      geom_hline(yintercept = thr, linetype = "dashed") +
      labs(x = "Time", y = "Req/min") +
      theme_minimal()
  })
  
  output$plot_sens <- renderPlot({
    base <- ggplot(sens, aes(timestamp, soil_moisture)) +
      geom_line(linewidth = 0.4) +
      labs(x = "Time", y = "Soil Moisture") +
      theme_minimal()
    if (isTRUE(input$show_spoof)) {
      base <- base + geom_point(data = sens[is_spoof == 1L], size = 0.7, color = "red")
    }
    base
  })
}

shinyApp(ui, server)
