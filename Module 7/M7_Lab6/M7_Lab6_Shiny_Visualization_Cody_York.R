# Import the needed libraries
library(shiny)
library(ggplot2)

# Set Working Directory
setwd('C:/Users/cy_su/PycharmProjects/DSCI_605_Data_Visualizations/Module 7/M7_Lab6/')

# Load the dataset
hotel_data <- read.csv("hotel_bookings.csv")

# Define UI
ui <- fluidPage(
    titlePanel("Hotel Bookings Over Time"),
    sidebarLayout(
        sidebarPanel(
            selectInput("hotel_type", "Select Hotel Type:",
                        choices = c("City Hotel", "Resort Hotel"),
                        selected = "City Hotel")
        ),
        mainPanel(
            plotOutput("booking_plot")
        )
    )
)

# Define server logic
server <- function(input, output) {
    output$booking_plot <- renderPlot({
        # Filter data based on selected hotel type
        filtered_data <- subset(hotel_data, hotel == input$hotel_type)

        # Create a plot
        p <- ggplot(filtered_data, aes(x = arrival_date_year, fill = arrival_date_month)) +
              geom_bar(show.legend = FALSE) +
              scale_fill_brewer(palette = "Set3", name = "Month") +
              labs(title = paste("Number of Bookings for", input$hotel_type, "Over Time"),
                    subtitle = "Aggregated by Year and Month",
                    x = "Year",
                    y = "Number of Bookings") +
              theme_minimal() +
              theme(text = element_text(size = 12, family = "Arial"),
                      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                      plot.subtitle = element_text(size = 14, hjust = 0.5),
                      axis.title.x = element_text(size = 14, face = "bold"),
                      axis.title.y = element_text(size = 14, face = "bold"),
                      axis.text.x = element_text(angle = 45, hjust = 1),
                      legend.position = "bottom")
        print(p)
    })
}

# Run the application
shinyApp(ui = ui, server = server)