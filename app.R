library(shiny)
library(shinythemes)
library(tidyverse)
library(gridExtra)
library(grid)
library(grDevices)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("DPICS Coding Worksheet by @jrosecalabrese"),
  sidebarLayout(
    sidebarPanel(
      h3("Demographics"),
      textInput("child_name", "Child Name"),
      textInput("caregiver_name", "Caregiver Name"),
      dateInput("session_date", "Session Date", value = Sys.Date()),
      textInput("start_time", "Start Time (HH:MM)", value = format(Sys.time(), "%H:%M")),
      textInput("end_time", "End Time (HH:MM)"),
      h3("Instructions"),
      p("Track the frequency of behaviors by using the tally buttons. Data will be logged and summarized."),
      actionButton("log_button", "Log Observations"),
      downloadButton("download_pdf", "Download PDF Report")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Parent Statements",
                 fluidRow(
                   column(6, h4("Unlabeled Praise (UP)"),
                          actionButton("up_minus", "-1"),
                          actionButton("up_plus", "+1"),
                          textOutput("up_value")),
                   column(6, h4("Labeled Praise (LP)"),
                          actionButton("lp_minus", "-1"),
                          actionButton("lp_plus", "+1"),
                          textOutput("lp_value")),
                   column(6, h4("Reflection (RF)"),
                          actionButton("rf_minus", "-1"),
                          actionButton("rf_plus", "+1"),
                          textOutput("rf_value")),
                   column(6, h4("Behavior Description (BD)"),
                          actionButton("bd_minus", "-1"),
                          actionButton("bd_plus", "+1"),
                          textOutput("bd_value"))
                 )
        ),
        tabPanel("Avoid Behaviors",
                 fluidRow(
                   column(6, h4("Question (Q)"),
                          actionButton("q_minus", "-1"),
                          actionButton("q_plus", "+1"),
                          textOutput("q_value")),
                   column(6, h4("Reflective Question (RQ)"),
                          actionButton("rq_minus", "-1"),
                          actionButton("rq_plus", "+1"),
                          textOutput("rq_value")),
                   column(6, h4("Indirect Commands (IC)"),
                          actionButton("ic_minus", "-1"),
                          actionButton("ic_plus", "+1"),
                          textOutput("ic_value")),
                   column(6, h4("Direct Command (DC)"),
                          actionButton("dc_minus", "-1"),
                          actionButton("dc_plus", "+1"),
                          textOutput("dc_value")),
                   column(6, h4("Negative Talk (NTA)"),
                          actionButton("nta_minus", "-1"),
                          actionButton("nta_plus", "+1"),
                          textOutput("nta_value"))
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to track counts
  counts <- reactiveValues(
    up = 0,
    lp = 0,
    rf = 0,
    bd = 0,
    q = 0,
    rq = 0,
    ic = 0,
    dc = 0,
    nta = 0
  )
  
  # Initialize logged data as an empty data frame with defined column names
  logged_data <- data.frame(
    Category = character(),
    Behavior = character(),
    Count = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Update counts for child behaviors
  observeEvent(input$up_plus, { counts$up <- counts$up + 1 })
  observeEvent(input$up_minus, { counts$up <- counts$up - 1 })
  observeEvent(input$lp_plus, { counts$lp <- counts$lp + 1 })
  observeEvent(input$lp_minus, { counts$lp <- counts$lp - 1 })
  observeEvent(input$rf_plus, { counts$rf <- counts$rf + 1 })
  observeEvent(input$rf_minus, { counts$rf <- counts$rf - 1 })
  observeEvent(input$bd_plus, { counts$bd <- counts$bd + 1 })
  observeEvent(input$bd_minus, { counts$bd <- counts$bd - 1 })
  
  # Update counts for parent behaviors
  observeEvent(input$q_plus, { counts$q <- counts$q + 1 })
  observeEvent(input$q_minus, { counts$q <- counts$q - 1 })
  observeEvent(input$rq_plus, { counts$rq <- counts$rq + 1 })
  observeEvent(input$rq_minus, { counts$rq <- counts$rq - 1 })
  observeEvent(input$ic_plus, { counts$ic <- counts$ic + 1 })
  observeEvent(input$ic_minus, { counts$ic <- counts$ic - 1 })
  observeEvent(input$dc_plus, { counts$dc <- counts$dc + 1 })
  observeEvent(input$dc_minus, { counts$dc <- counts$dc - 1 })
  observeEvent(input$nta_plus, { counts$nta <- counts$nta + 1 })
  observeEvent(input$nta_minus, { counts$nta <- counts$nta - 1 })
  
  # Render text outputs for counts
  output$up_value <- renderText({ counts$up })
  output$lp_value <- renderText({ counts$lp })
  output$rf_value <- renderText({ counts$rf })
  output$bd_value <- renderText({ counts$bd })
  output$q_value <- renderText({ counts$q })
  output$rq_value <- renderText({ counts$rq })
  output$ic_value <- renderText({ counts$ic })
  output$dc_value <- renderText({ counts$dc })
  output$nta_value <- renderText({ counts$nta })
  
  # Log observations when button is clicked
  observeEvent(input$log_button, {
    new_data <- data.frame(
      Category = c("Statements", "Statements", "Statements", "Statements",
                   "Avoid", "Avoid", "Avoid", "Avoid", "Avoid"),
      Behavior = c("Unlabeled Praise", "Labeled Praise", "Reflection", "Behavior Description",
                   "Question", "Reflective Question", "Indirect Command", "Direct Command", "Negative Talk"),
      Count = c(counts$up, counts$lp, counts$rf, counts$bd,
                counts$q, counts$rq, counts$ic, counts$dc, counts$nta),
      stringsAsFactors = FALSE
    )
    
    # Append new data to logged_data
    logged_data <<- rbind(logged_data, new_data)
  })
  
  # Provide download of logged data as a PDF
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("observations", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 8.5, height = 11)  # Portrait format
      
      # Create a table for demographic info
      demographics_data <- data.frame(
        "Child Name" = input$child_name,
        "Caregiver Name" = input$caregiver_name,
        "Session Date" = input$session_date,
        "Start Time" = input$start_time,
        "End Time" = input$end_time,
        stringsAsFactors = FALSE
      )
      
      # Set up the layout: Left side for demographic data, right side for the table
      grid.arrange(
        tableGrob(demographics_data, rows = NULL),  # Demographics table
        tableGrob(logged_data, rows = NULL),        # DPICS behavior data table
        ncol = 1,                                  # One column layout
        heights = c(1, 4)                          # Adjust the height ratios as needed
      )
      
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
