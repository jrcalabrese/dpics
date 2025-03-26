library(shiny)
library(shinythemes)
library(tidyverse)
library(gridExtra)
library(grid)
library(grDevices)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("DPICS Coding Worksheet by me"),
  sidebarLayout(
    sidebarPanel(
      h3("Demographics"),
      textInput("child_name", "Child Name"),
      textInput("caregiver_name", "Caregiver Name"),
      dateInput("session_date", "Session Date", value = Sys.Date()),
      textInput("start_time", "Start Time (HH:MM)", value = format(Sys.time(), "%H:%M")),
      textInput("end_time", "End Time (HH:MM)"),
      h3("Instructions"),
      p("Track the frequency of behaviors by using the tally buttons. Data will be logged automatically and summarized."),
      downloadButton("download_pdf", "Download PDF Report")
    ),
    mainPanel(
      h3("Parent Statements"),
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
      ),
      h3("Avoid Behaviors"),
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

server <- function(input, output, session) {
  # Reactive values to track counts
  counts <- reactiveValues(
    up = 0, lp = 0, rf = 0, bd = 0, 
    q = 0, rq = 0, ic = 0, dc = 0, nta = 0
  )
  
  # Update counts dynamically
  observeEvent(input$up_plus, { counts$up <- counts$up + 1 })
  observeEvent(input$up_minus, { counts$up <- max(0, counts$up - 1) })
  observeEvent(input$lp_plus, { counts$lp <- counts$lp + 1 })
  observeEvent(input$lp_minus, { counts$lp <- max(0, counts$lp - 1) })
  observeEvent(input$rf_plus, { counts$rf <- counts$rf + 1 })
  observeEvent(input$rf_minus, { counts$rf <- max(0, counts$rf - 1) })
  observeEvent(input$bd_plus, { counts$bd <- counts$bd + 1 })
  observeEvent(input$bd_minus, { counts$bd <- max(0, counts$bd - 1) })
  
  observeEvent(input$q_plus, { counts$q <- counts$q + 1 })
  observeEvent(input$q_minus, { counts$q <- max(0, counts$q - 1) })
  observeEvent(input$rq_plus, { counts$rq <- counts$rq + 1 })
  observeEvent(input$rq_minus, { counts$rq <- max(0, counts$rq - 1) })
  observeEvent(input$ic_plus, { counts$ic <- counts$ic + 1 })
  observeEvent(input$ic_minus, { counts$ic <- max(0, counts$ic - 1) })
  observeEvent(input$dc_plus, { counts$dc <- counts$dc + 1 })
  observeEvent(input$dc_minus, { counts$dc <- max(0, counts$dc - 1) })
  observeEvent(input$nta_plus, { counts$nta <- counts$nta + 1 })
  observeEvent(input$nta_minus, { counts$nta <- max(0, counts$nta - 1) })
  
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
  
  # Reactive dataset that updates automatically
  logged_data <- reactive({
    data.frame(
      Category = c("Statements", "Statements", "Statements", "Statements",
                   "Avoid", "Avoid", "Avoid", "Avoid", "Avoid"),
      Behavior = c("Unlabeled Praise", "Labeled Praise", "Reflection", "Behavior Description",
                   "Question", "Reflective Question", "Indirect Command", "Direct Command", "Negative Talk"),
      Count = c(counts$up, counts$lp, counts$rf, counts$bd,
                counts$q, counts$rq, counts$ic, counts$dc, counts$nta),
      stringsAsFactors = FALSE
    )
  })
  
  # Provide automatic download of logged data as a PDF
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("DPICS_Report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 8.5, height = 11)  # Standard letter size
      
      # Title
      grid.newpage()
      grid.text("DPICS Coding Worksheet Report", x = 0.5, y = 0.95, 
                gp = gpar(fontsize = 14, fontface = "bold"))
      
      # Demographics Table (Lowered)
      demographics_data <- data.frame(
        "Field" = c("Child Name", "Caregiver Name", "Session Date", "Start Time", "End Time"),
        "Value" = c(input$child_name, input$caregiver_name, input$session_date, input$start_time, input$end_time),
        stringsAsFactors = FALSE
      )
      
      demographics_table <- tableGrob(demographics_data, rows = NULL, 
                                      theme = ttheme_default(
                                        core = list(bg_params = list(fill = rep(c("#F2F2F2", "white"), 
                                                                                length.out = nrow(demographics_data)))),
                                        colhead = list(fg_params = list(fontface = "bold"))
                                      ))
      
      # Behavior Data Table
      behavior_data <- logged_data()
      
      behavior_table <- tableGrob(behavior_data, rows = NULL, 
                                  theme = ttheme_default(
                                    core = list(bg_params = list(fill = rep(c("#F2F2F2", "white"), 
                                                                            length.out = nrow(behavior_data)))),
                                    colhead = list(fg_params = list(fontface = "bold"))
                                  ))
      
      # Arrange tables with more spacing to avoid overlap
      layout <- arrangeGrob(
        demographics_table, 
        textGrob(" "),  # Spacer to provide space below the title
        behavior_table, 
        ncol = 1, heights = c(1.5, 0.3, 3)  # Adjusted height for better positioning
      )
      
      grid.draw(layout)  # Draw everything neatly
      
      # Footer with timestamp
      grid.text(paste("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M")), 
                x = 0.5, y = 0.02, gp = gpar(fontsize = 8, fontface = "italic"))
      
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
