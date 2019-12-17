# Author: Tanay Mukherjee
# EMPL ID: 23987468
# Subject: Final Project for STA 9750

# Process to connect the shiny dashboard to Rshiny IO server
# library(rsconnect)
# rsconnect::deployApp('C:/Users/its_t/Documents/CUNY Fall 2019/9750 - Software Tools and Techniques_Data Science/Project')

# Load the libraries
library(dplyr)
library(shiny)
library(plotly)
library(ggplot2)
library(waffle)
library(tidyr)
library(lubridate)
library(DT)

# Load the data for analysis
# Here we are working with a 'sales' data from a superstore to create an interactive dashbaord
superstore <- read.csv("C:\\Users\\its_t\\Documents\\CUNY Fall 2019\\9750 - Software Tools and Techniques_Data Science\\Project\\Superstore.csv")

# Variables in the dataset
# names(superstore)

# Define the UI of the page
ui <- fluidPage(
  
  # Title of the dashbaord
  titlePanel("Sales Dashboard for a Superstore"),
  
  # Create seperate panel for different analysis and maintain the indentation
  tabsetPanel(
    
    # This tab has the outline for datatable
    # It includes various UI links and tricks to play around with the data table
    tabPanel("Data Table",
             sidebarLayout(
               sidebarPanel(
                 selectInput("Region", "Region", choices = unique(superstore$Region)),
                 selectInput("State", "State", choices = NULL),
                 selectInput("Category", "Category", choices = NULL, size = 5, selectize = FALSE)
               ),
               mainPanel(uiOutput("customer"),
                         uiOutput("Segment"),
                         DT::dataTableOutput("data"),
                         downloadButton(outputId = "download_data", label = "Download")))),
    
    # This tab has analysis of how profit was distributed in a store
    # for multiple parameters and how each have contirbuted to revenue
    tabPanel("Profit Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("Yaxis", "Y-axis", 
                             choices = c("Region","Category",
                                         "Sub.Category",
                                         "Segment","Ship.Mode"),
                             selected = "Segment")
               ),
               mainPanel(textOutput("selected_var"),plotOutput("plot1")))),
    
    # This tab has waffle chart to read share of sales
    # This was to do something innovation with graphics
    tabPanel("Sales Analysis",plotOutput("plot3"),
             plotOutput("plot4")),
    
    # This tab has interactive graphs
    tabPanel("Daily Analysis",plotlyOutput("plot2"))
    
  )
)


# Server fucntion that will use the UI fucntion and 
# return values for each tabs based on defined rules
server <- function(input, output, session) {
  
  # Take input from user to decide which region they want to analyse
  Region <- reactive({
    req(input$Region)
    if (input$Region == "NA") {
      filter(superstore, is.na(Region))
    } else {
      filter(superstore, Region == input$Region)
    }
  })
  
  # Based on the chosen Region - this space will populate the correct states
  customer <- reactive({
    req(input$State)
    filter(Region(), State == input$State)
  })
  
  # This will return the top line section with highlights of what the user
  # has chosen and also the point of contact for the audience to 
  # dicuss and issue with an order or a shipment
  output$customer <- renderUI({
    row <- customer()[1, ]
    tags$div(
      class = "well",
      tags$p(tags$strong("Region: "), row$Region),
      tags$p(tags$strong("State: "), row$State),
      tags$p(tags$strong("Zonal Executive: "), row$Person),
      tags$p(tags$strong("Point of Contact: "), row$Contact.Number),
      tags$p(tags$strong("Category Analysed: "), ChosenCategory()$Category)
    )
  })
  
  # This space will be use to populate the category and rlevant info in the table
  ChosenCategory <- reactive({
    req(input$Category)
    customer() %>%
      filter(Category == input$Category) %>%
      arrange(Category) %>%
      select(Category)
  })
  
  # Define all the variables we want to inititae as part of the data table series
  order <- reactive({
    
    # Select the columns using the server fucntion declared earlier
    # and use it to filter out columns using dplyr
    req(input$Category)
    customer() %>%
      filter(Category == input$Category) %>%
      arrange(Category) %>%
      select(Order.ID, Order.Date, Ship.Date, Ship.Mode, 
             Customer.ID, Customer.Name, City, Segment, Quantity, Sales)
  })
  
  # The structure of the data table is defined below:
  output$data <- DT::renderDataTable(order(), filter = 'top', 
                                     extensions = c('Buttons', 'ColReorder','FixedHeader','FixedColumns'),
                                     options = list(pageLength = 5, fixedHeader = TRUE,
                                                    dom = 'Blfrtip', dom = 't',
                                                    scrollX = TRUE,
                                                    fixedColumns = TRUE,
                                                    buttons = list(list(extend = 'colvis', columns = c(2,3,4,5))),
                                                    colReorder = TRUE,
                                                    initComplete = JS(
                                                      "function(settings, json) {",
                                                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                      "}"),
                                                    searchHighlight = TRUE,
                                                    columnDefs = list(list(targets = c(10), searchable = FALSE))
                                     ))
  
  
  # Creating a download handler to help the user download the chosen data fields
  output$download_data <- downloadHandler(
    filename = "superstore.csv",
    content = function(file) {
      data <- order()
      
      # Write the filtered data into a CSV file
      write.csv(data, file, row.names = FALSE)
      
    })
  
  # The ggplot code to return bar charts for each paramter
  # It will display the profit distribution for any variable a user chooses
  output$plot1 <- renderPlot({
    ggplot(superstore, aes_string(x = input$Yaxis)) +
      geom_bar(aes_string(fill= superstore$Profit), position = position_stack(reverse = TRUE)) +
      coord_flip() + ylab("Profit in US dollars") +
      theme(legend.position = "top") +
      # geom_text(aes(y = superstore$Profit, label = superstore$Profit)) +
      ggtitle("How's is the profit distributed?")
    
  })
  
  # This the code for the interactive plot using plotly
  # One can play around with the graph.
  # Zoom in, zoom out, hover over, etc.
  output$plot2 <- renderPlotly({
    q <- ggplot(superstore, aes(superstore$Order.Date, superstore$Profit)) +
      geom_line( color="steelblue") + geom_point() +
      xlab("Profit") + ylab("Date Range by Order Year") +
      ggtitle(input$title)
    ggplotly(q)
  })
  
  # This is the ambitious section wherein I tried to create a new type of 
  # visualization not discussed or seen in class. 
  # This is called waffle chart and it helps the distribution of 
  # each paramter by percent. Each block is equvalent to 1%
  superstore_waffle <- superstore %>% count(Order.Year, Segment) %>% 
    group_by(Segment) %>% mutate(Percent = 100*prop.table(n))
  
  superstore_waffle <- superstore %>% group_by(Order.Year, Segment) %>% 
    count(Order.Year, Segment) %>% group_by(Order.Year) %>% 
    mutate(Total_byOY = sum(n)) %>% group_by(Order.Year) %>%
    mutate(Percentage = 100*(n/Total_byOY))
  
  superstore_waffle$Percentage_r <- round(superstore_waffle$Percentage)
  superstore_waffle[3,6] <- 15
  
  class(superstore)
  class(superstore_waffle)
  
  x <- data.frame(rbind(superstore_waffle))
  class(x)
  
  x$Segment <- as.character(x$Segment)
  
  output$plot3 <- renderPlot({
    
    ggplot(x, aes(fill = x$Segment, values = x$Percentage_r)) +
      geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
      facet_wrap(~Order.Year, nrow = 1, strip.position = "bottom") +
      scale_x_discrete() + 
      scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                         expand = c(0,0)) +
      ggthemes::scale_fill_tableau(name=NULL) +
      coord_equal() +
      labs(
        title = "Faceted Waffle Bar Chart",
        subtitle = "Segment Classification by Year",
        x = "Year",
        y = "Percentage share of Sales"
      ) +
      theme_minimal(base_family = "Roboto Condensed") +
      theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
      guides(fill = guide_legend(reverse = TRUE))
  })
  
  
  superstore_waffle_c <- superstore %>% count(Order.Year, Category) %>% 
    group_by(Category) %>% mutate(Percent = 100*prop.table(n))
  
  
  superstore_waffle_c <- superstore %>% group_by(Order.Year, Category) %>% 
    count(Order.Year, Category) %>% group_by(Order.Year) %>% 
    mutate(Total_byOY = sum(n)) %>% group_by(Order.Year) %>%
    mutate(Percentage = 100*(n/Total_byOY))
  
  
  superstore_waffle_c$Percentage_r <- round(superstore_waffle_c$Percentage)
  superstore_waffle_c[8,6] <- 60
  
  class(superstore)
  class(superstore_waffle_c)
  
  y <- data.frame(rbind(superstore_waffle_c))
  class(y)
  
  y$Category <- as.character(y$Category)
  
  output$plot4 <- renderPlot({
    
    ggplot(y, aes(fill = y$Category, values = y$Percentage_r)) +
      geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
      facet_wrap(~Order.Year, nrow = 1, strip.position = "bottom") +
      scale_x_discrete() + 
      scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                         expand = c(0,0)) +
      ggthemes::scale_fill_tableau(name=NULL) +
      coord_equal() +
      labs(
        title = "Faceted Waffle Bar Chart",
        subtitle = "Category Classification by Year",
        x = "Year",
        y = "Percentage share of Sales"
      ) +
      theme_minimal(base_family = "Roboto Condensed") +
      theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
      guides(fill = guide_legend(reverse = TRUE))
  })
  
  
  # This is declare the event for the filters chosen from tab 1
  # the event identifies the click and returns the relevant data in the table
  observeEvent(Region(), {
    updateSelectInput(session, "State", choices = unique(Region()$State), selected = character())
  })
  
  observeEvent(customer(), {
    updateSelectInput(session, "Category", choices = unique(customer()$Category))
  })
}

# Final call to run the app
# We use ShinyApp fucntion and pass our UI and Server arguments as objects
shinyApp(ui, server)
