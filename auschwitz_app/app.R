library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

data <- read.csv("Auschwitz_Death_Certificates_1942-1943 - Auschwitz.csv", stringsAsFactors = FALSE)
residence <- unique(data$Residence)
birthplace <- unique(data$Birthplace)
religion <- unique(data$Religion)

ui <- fluidPage(
  titlePanel("Auschwitz Death Certificates Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataSelection",
                  "Select Data for Graph:",
                  choices = c("Birthplace", "Residence", "Religion")),
      uiOutput("filter")
    ),
    mainPanel(plotOutput("plot"), DTOutput("table")
)
  )
)

server <- function(input, output, session) {
  
  output$filter <- renderUI({
    if (input$dataSelection == "Residence") {
      selectizeInput(
        inputId = "residence", 
        label = "Filter by Residence",
        multiple = TRUE, # Allow multiselect
        choices = NULL
      )
    } else if (input$dataSelection == "Religion") {
      selectizeInput(
        inputId = "religion", 
        label = "Filter by Religion",
        multiple = TRUE, # Allow multiselect
        choices = NULL
      )
    } else {
      selectizeInput(
        inputId = "birthplace", 
        label = "Filter by Birthplace",
        multiple = TRUE, # Allow multiselect
        choices = NULL
      )
    }
  })
  
  output$table <- renderDT({
    if (input$dataSelection == "Birthplace") {
    data_to_display <- data %>%
      filter(Birthplace %in% input$birthplace)
    datatable(data_to_display, options = list(pageLength = 10, scrollX = TRUE))
    } else if (input$dataSelection == "Residence") {
      data_to_display <- data %>%
        filter(Residence %in% input$residence)
      datatable(data_to_display, options = list(pageLength = 10, scrollX = TRUE))
    } else {
      data_to_display <- data %>%
        filter(Religion %in% input$religion)
      datatable(data_to_display, options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  output$plot <- renderPlot({
    if (input$dataSelection == "Birthplace") {
      data_to_plot <- data %>%
        filter(Birthplace %in% input$birthplace) %>%
        group_by(Birthplace) %>%
        summarise(Count = n())
      ggplot(data_to_plot, aes(x = Birthplace, y = Count, fill = Birthplace)) +
        geom_bar(stat = "identity") +
        labs(x = "Religion", y = "Count", title = "Number of People Murdered by Birthplace") +
        theme_minimal()
    } else if (input$dataSelection == "Residence") {
      data_to_plot <- data %>%
        filter(Residence %in% input$residence) %>%
        group_by(Residence) %>%
        summarise(Count = n())
      
      ggplot(data_to_plot, aes(x = Residence, y = Count, fill = Residence)) +
        geom_bar(stat = "identity") +
        labs(x = "Residence", y = "Count", title = "Number of People Murdered by Residence") +
        theme_minimal()
    } else {
      data_to_plot <- data %>%
        filter(Religion %in% input$religion) %>%
        group_by(Religion) %>%
        summarise(Count = n())
      
      ggplot(data_to_plot, aes(x = Religion, y = Count, fill = Religion)) +
        geom_bar(stat = "identity") +
        labs(x = "Religion", y = "Count", title = "Number of People Murdered by Religion") +
        theme_minimal()
    }
  })
  
  observeEvent(input$dataSelection, {
    updateSelectizeInput(session, "residence", choices = residence, server=TRUE)
    updateSelectizeInput(session, "birthplace", choices = birthplace, server=TRUE)
    updateSelectizeInput(session, "religion", choices = religion, server=TRUE)
  })
  
  observe({
    if (!is.null(input$residence)) {
      print("Selected Residence:")
      print(input$residence)
    }
    if (!is.null(input$religion)) {
      print("Selected Religion:")
      print(input$religion)
    }
    if (!is.null(input$birthplace)) {
      print("Selected Birthplace:")
      print(input$birthplace)
    }
  })
  
}

shinyApp(ui, server)