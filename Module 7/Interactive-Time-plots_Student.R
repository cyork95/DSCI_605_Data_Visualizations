
list.of.packages <- c("shiny", "plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

###############the template
library(shiny)
ui <- fluidPage()

server <- function(input, output) {}

shinyApp(ui = ui, server = server)

############################



library(shiny)
library(plotly)
data(txhousing)

ui <- fluidPage(
  selectizeInput(
    inputId = "cities", 
    label = "Select a city", 
    choices = unique(txhousing$city), 
    selected = "Abilene",
    multiple = TRUE
  ),
  plotlyOutput(outputId = "p")
)

server <- function(input, output, ...) {
  output$p <- renderPlotly({
    plot_ly(txhousing, x = ~date, y = ~median) %>%
      filter(city %in% input$cities) %>%
      group_by(city) %>%
      add_lines()
  })
}

shinyApp(ui, server)

?shinyApp


##################two plots

ui <- fluidPage(
  selectizeInput(
    inputId = "idc", 
    label = "Select a city you are of interest", 
    choices = unique(txhousing$city), 
    selected = "Abilene",
    multiple = FALSE
  ),
  plotOutput(outputId = "myplot")
)

server <- function(input, output, ...) {
  selectedData <- reactive({
    txhousing%>% filter(city %in% input$idc)
  }) 
  output$myplot <- renderPlot({  
    par(mfrow=c(2,1))
    hist(selectedData()$median)
    plot(selectedData()$date,selectedData()$median,type = "l", lty = 1)
  })
}

shinyApp(ui, server)

##################two plots


shinyApp(
  shinyUI(
    fluidPage(
      selectizeInput(
        inputId = "cities", 
        label = "Select a city", 
        choices = unique(txhousing$city), 
        selected = "Abilene",
        multiple = FALSE),
      plotOutput("plot1"),
      plotOutput("plot2")
    )
  ),
  shinyServer(function(input, output) { 
    selectedData <- reactive({
      txhousing%>% filter(city %in% input$cities)
    }) 
    output$plot1 <- renderPlot({
      plot(selectedData()$date,selectedData()$median,type="l", lty=1) #() is needed
    })
    
    output$plot2 <- renderPlot({
      hist(selectedData()$median)
    })
  }
  )
)




# if Error: could not find function "shinyApp" then upload your packages.
#Update R to the version required by Shiny (>=3.0.0) then run update.packages() in R.

#The reason you're getting this error is probably because you installed Shiny under 
# an old R version, and thus, you got an old version of Shiny. When you updated R on
# your Windows computer, it used the packages already there in your old R installation,
# i.e., the old version of Shiny.

#close R and open R again
# update.packages()
#open in browser

############
