library(shiny)
library(shinyWidgets)
library(ggplot2)  # for the dataset
library(ggmap)
library(leaflet)
library(png)

my_dataset = read.csv("vaccine_vv.csv")
covid = read.csv("covid.csv")
vaccine = read.csv("vaccine.csv")
objectA = read.csv("covidvacs.csv")


ui <- fluidPage(
  setBackgroundColor(
    color = "#b3ecff",
    gradient = "radial"
    #direction = c("bottom","right")
  ),
  title = "Data Frame",
  navbarPage("NASSCOM TIME!",
  
  tabPanel("Home",
           sidebarPanel(
             tags$style("h2{font-family:candara; font-size:35px; font-weight:bold;color:blue; text-shadow: 2px 2px yellow}"),
             tags$h2("COVID Vaccine Tracker"),
             tags$style(".well {background-color:#b3ecff;}"),
             img(src = "img2.png", height = 300, width = 400)
           ),
           mainPanel(
             tags$style("p{font-family:candara; font-size:20px; font-weight:bold}"),
             tags$p(
               "COVID -19 is now over 198 countries with a reported case of 492,465 and mounting day by day. WHO has declared the COVID 19 as pandemic. From a public health issue it is now driving the global economy into a recession. The Chief of International Monetary Fund stated that the rescission is started."
             ),
             br(),
             p(
               "Currently there is no vaccine for COVID-19 and a new vaccine is still at least another 12-18 months away for public use. Many a different treatment options have been proposed and some older drug seems to be associated with positive outcomes. For instance, it is reported that in India, we have successfully treated a patient using a combination of HIV, swine flu and malaria drugs whereas in China and Japan, they are said to be administering 'avigan' (Favipiravir) and had produced encouraging outcomes."
             ),
             br(),
             img(src = "img3.png", height = 320, width = 420, align = "right")
      )
             ),
  tabPanel("Vaccine Details",
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          'input.dataset === my_dataset',
          checkboxGroupInput("show_vars", "Columns in dataframe to show:",
                             names(my_dataset), selected = names(my_dataset))
        )
      ),
      mainPanel(
        tabsetPanel(
          id = 'dataset',
          tabPanel("DATASET", DT::dataTableOutput("mytable1"))
        )
      )
    )
  ),
  
  
  
  tabPanel("Vaccine Tracker Dataframe",
           sidebarLayout(
             sidebarPanel(
               conditionalPanel(
                 'input.datasetB === objectA',
                 checkboxGroupInput("show_varsB", "Columns in dataframe to show:",
                                    names(objectA), selected = names(objectA))
               )
             ),
             mainPanel(
               tabsetPanel(
                 id = 'datasetB',
                 tabPanel("DATASET", DT::dataTableOutput("mytable2"))
               )
             )
           )
  ),
  
  tabPanel("Plots",
           sidebarLayout(
             sidebarPanel(
               selectInput('xcol', 'X Variable', ""),
               selectInput('ycol', 'Y Variable', "", selected = "")
             ),
             mainPanel(
               plotOutput('MyPlot'),
               plotOutput('MyPlot2')
             )
           )
  ),
  tabPanel("MAP",
           mainPanel(
             leafletOutput(outputId = "map",height = "650", width="155%")
           )
  )
             )
)

server <- function(input, output, session) {
  
  # choose columns to display
  my_dataset2 = my_dataset[sample(nrow(my_dataset), 1000,replace = TRUE), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(my_dataset2[, input$show_vars, drop = FALSE])
  })
  
  
  objectB = objectA[sample(nrow(objectA), 1000,replace = TRUE), ]
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(objectB[, input$show_varsB, drop = FALSE])
  })
 
  data <- reactive({ 
    df <- read.csv("covid.csv")
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    
    return(df)
  })
  
  output$MyPlot <- renderPlot({
    x <- data()[, c(input$xcol, input$ycol)]
    plot(x)
    
  })
  output$MyPlot2 <- renderPlot({
    x <- data()[, c(input$xcol, input$ycol)]
    plot(x,type = "o")
    
    
  })
  output$map <- renderLeaflet({
    leaflet(vaccine) %>%
    setView(lng = 74.6179, lat = 22.0479, zoom = 3) %>% addMiniMap() %>% 
    addCircleMarkers( lat = ~lat, lng = ~long,radius = 10,stroke = FALSE, fillOpacity = 0.5,color = "navy", label = ~N) %>%
    addProviderTiles(providers$Esri.WorldTopoMap , options = providerTileOptions(noWrap = TRUE))
  })
}

shinyApp(ui, server)