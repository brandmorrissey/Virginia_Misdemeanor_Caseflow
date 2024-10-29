#library
install.packages(c("tidyverse", "networkD3", "shiny", "DT"))

library(tidyverse)
library(networkD3)
library(shiny)
library(DT)

VirginiaMisdemeanors_Trimmed <- read.csv("https://media.githubusercontent.com/media/brandmorrissey/Virginia_Misdemeanor_Caseflow/refs/heads/Sankey-Plot/VirginiaMisdemeanors_Trimmed.csv")

VirginiaMisdemeanors_Trimmed$OffenseMonth=as.Date(VirginiaMisdemeanors_Trimmed$OffenseMonth)

ui <- fluidPage(
  titlePanel("Misdmemeanors in Virginia District Courts"),
  tabsetPanel(
    tabPanel("Charge Outcomes",
             sidebarLayout(
               sidebarPanel(
                 selectInput("offenseCategory", "Select Offense Category:", 
                             choices = c("All", sort(unique(VirginiaMisdemeanors_Trimmed$OffenseCategory))),
                             selected = "All"),
                 selectInput("crimeFamily", "Select Revised Crime Family:", 
                             choices = c("All", sort(unique(VirginiaMisdemeanors_Trimmed$RevisedCrimeFamily))),
                             selected = "All"),
                 selectInput("Jurisdiction", "Select Jurisdiction:",
                             choices = c("All", sort(unique(VirginiaMisdemeanors_Trimmed$Jurisdiction))),
                             selected = "All")
               ),
               mainPanel(
                 sankeyNetworkOutput("sankeyPlot"),
                 dataTableOutput("dataTable")
               )
             )
    ),
    tabPanel("Charges Over Time",
             sidebarLayout(
               sidebarPanel(
                 selectInput("offenseCategory_time", "Select Offense Category:", 
                             choices = c("All", sort(unique(VirginiaMisdemeanors_Trimmed$OffenseCategory)))),
                 selectInput("crimeFamily_time", "Select Revised Crime Family:", 
                             choices = c("All", sort(unique(VirginiaMisdemeanors_Trimmed$RevisedCrimeFamily)))),
                 selectInput("Jurisdiction_time", "Select Jurisdiction:",
                             choices = c("All", sort(unique(VirginiaMisdemeanors_Trimmed$Jurisdiction))))
               ),
               mainPanel(
                 plotOutput("timePlot")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- VirginiaMisdemeanors_Trimmed
    
    if (input$offenseCategory != "All") {
      data <- data %>%
        filter(OffenseCategory == input$offenseCategory)
    }
    
    if (input$crimeFamily != "All") {
      data <- data %>%
        filter(RevisedCrimeFamily == input$crimeFamily)
    }
    
    if (input$Jurisdiction != "All") {
      data <- data %>%
        filter(Jurisdiction== input$Jurisdiction)
    }
    
    data
  })
  
  filtered_data_time <- reactive({
    data <- VirginiaMisdemeanors_Trimmed
    
    if (input$offenseCategory_time != "All") {
      data <- data %>%
        filter(OffenseCategory == input$offenseCategory_time)
    }
    
    if (input$crimeFamily_time != "All") {
      data <- data %>%
        filter(RevisedCrimeFamily == input$crimeFamily_time)
    }
    
    if (input$Jurisdiction_time != "All") {
      data <- data %>%
        filter(Jurisdiction == input$Jurisdiction_time)
    }
    
    data
  })
  
  output$sankeyPlot <- renderSankeyNetwork({
    data <- filtered_data()
    
    data <- data %>%
      mutate(ArrestedTarget = ifelse(Arrested == 1, "Arrested", "Not Arrested")) %>%
      group_by(OffenseCategory, ArrestedTarget, OutcomeCleaned) %>%
      summarise(count = n()) %>%
      ungroup()
    
    links <- data.frame(
      source = c(data$OffenseCategory, data$ArrestedTarget),
      target = c(data$ArrestedTarget, data$OutcomeCleaned),
      value = rep(data$count, 2)
    )
    
    links$source <- as.factor(links$source)
    links$target <- as.factor(links$target)
    
    nodes <- data.frame(
      name = unique(c(as.character(links$source), as.character(links$target)))
    )
    
    links$IDsource <- match(links$source, nodes$name) - 1
    links$IDtarget <- match(links$target, nodes$name) - 1
    
    sankey <- sankeyNetwork(Links = links, Nodes = nodes,
                            Source = "IDsource", Target = "IDtarget",
                            Value = "value", NodeID = "name",
                            sinksRight=FALSE)
    
    sankey
  })
  
  output$dataTable <- renderDataTable({
    data <- filtered_data()
    
    summary_data <- data %>%
      mutate(ArrestedTarget = ifelse(Arrested == 1, "Arrested", "Not Arrested")) %>%
      group_by(OffenseCategory, ArrestedTarget, OutcomeCleaned) %>%
      summarise(count = n()) %>%
      ungroup()
    
    datatable(summary_data, 
              options = list(
                dom = 't',
                paging = TRUE,
                searching = TRUE,
                scrollY = "400px",
                scrollCollapse = TRUE,
                pageLength = 100
              ))
  })
  
  output$timePlot <- renderPlot({
    data <- filtered_data_time()
    
    counts <- data %>%
      group_by(OffenseMonth) %>%
      summarise(count = n())
    
    ggplot(counts, aes(x = OffenseMonth, y = count)) +
      geom_line() +
      geom_point() +
      labs(title = "Charges Over Time", x = "Charge Year", y = "Count") +
      theme_minimal()
  })
}

shinyApp(ui, server)