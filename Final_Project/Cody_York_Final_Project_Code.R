# Import the needed libraries
library(shiny)
library(tidyverse)
library(plotly)
library(reshape2)

# Set the working directory (if needed)
setwd('C:/Users/cy_su/PycharmProjects/DSCI_605_Data_Visualizations/Final_Project/')

# Define the Spotify color palette
spotify_colors <- c("#1DB954", "#7FFF00", "#32CD32", "#00FF00", "#66FF66", "#00FF7F", "#3CB371", "#2E8B57",
                    "#008000", "#228B22", "#00FA9A", "#ADFF2F", "#7CFC00", "#7FFF00", "#32CD32", "#00FF00")


# Load the data
songs <- read.csv("songs_normalize.csv")

# Normalize the genres
songs <- songs %>%
  separate_rows(genre, sep = ", ") %>%
  mutate(genre = trimws(genre))

# Remove "set()" and "trace 0" from genres, replaced with pop
songs$genre <- gsub("set\\(\\)|trace 0", "pop", songs$genre)


# Create the line chart
line_chart <- ggplot(songs, aes(x = year, y = tempo, color = genre)) +
  geom_line() +
  labs(title = "Change in Average Tempo Over Time", x = "Year", y = "Tempo") +
  theme_minimal() +  # Apply a minimal theme for a clean look
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Customize title appearance
    axis.text = element_text(size = 12, face = "bold"),  # Adjust axis label text size and make it bold
    axis.title = element_text(size = 14, face = "bold"),  # Make axis titles bold
    legend.title = element_blank(),  # Remove legend title
    legend.position = "bottom"  # Position legend at the bottom
  ) +
  scale_color_manual(values = spotify_colors)  # Use Spotify color palette

# Create the stacked area chart
stacked_area_chart <- songs %>%
  group_by(year, genre) %>%
  summarise(popularity = sum(popularity)) %>%
  ggplot(aes(x = year, y = popularity, fill = genre)) +
  geom_area() +
  labs(title = "Popularity of Different Genres Over Time", x = "Year", y = "Number of Popular Songs") +
  scale_fill_manual(values = spotify_colors) +  # Use Spotify color set
  theme_minimal() +  # Use minimal theme for clean look
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Title formatting
    axis.text = element_text(size = 12),  # Axis text size
    axis.title = element_text(size = 14, face = "bold"),  # Axis title formatting
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12),  # Legend text size
    legend.position = "right"  # Position the legend on the right side
  )

# Create the heatmap
heatmap <- songs %>%
  select(danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo) %>%
  cor() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  melt(id.vars = "rowname", variable.name = "attribute", value.name = "correlation") %>%
  ggplot(aes(x = rowname, y = attribute, fill = correlation)) +
  geom_tile() +
  labs(title = "Correlation Between Song Attributes", x = "Attribute", y = "Attribute") +
  scale_fill_gradient(low = "white", high = "green") +  # Set color scale to green
  theme_minimal() +  # Use minimal theme for clean look
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Title formatting
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10),  # Rotate x-axis labels vertically
    axis.text.y = element_text(size = 10),  # y-axis text size
    axis.title = element_text(size = 14, face = "bold"),  # Axis title formatting
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12)  # Legend text size
  )

# Create the stacked area chart
stacked_area_chart <- songs %>%
  group_by(year, genre) %>%
  summarise(popularity = sum(popularity)) %>%
  ggplot(aes(x = year, y = popularity, fill = genre)) +
  geom_area() +
  labs(title = "Popularity of Different Genres Over Time", x = "Year", y = "Number of Popular Songs") +
  scale_fill_manual(values = spotify_colors) +  # Use Spotify color set
  theme_minimal() +  # Use minimal theme for clean look
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Title formatting
    axis.text = element_text(size = 12),  # Axis text size
    axis.title = element_text(size = 14, face = "bold"),  # Axis title formatting
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12),  # Legend text size
    legend.position = "right"  # Position the legend on the right side
  )

# Create the stacked area chart
stacked_area_chart <- songs %>%
  group_by(year, genre) %>%
  summarise(popularity = sum(popularity)) %>%
  ggplot(aes(x = year, y = popularity, fill = genre)) +
  geom_area() +
  labs(title = "Popularity of Different Genres Over Time", x = "Year", y = "Number of Popular Songs") +
  scale_fill_manual(values = spotify_colors) +  # Use Spotify color set
  theme_minimal() +  # Use minimal theme for clean look
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Title formatting
    axis.text = element_text(size = 12),  # Axis text size
    axis.title = element_text(size = 14, face = "bold"),  # Axis title formatting
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12),  # Legend text size
    legend.position = "right"  # Position the legend on the right side
  )

# Create the bar chart
bar_chart <- songs %>%
  group_by(genre) %>%
  summarise(duration = mean(duration_ms) / 1000) %>%
  ggplot(aes(x = genre, y = duration, fill = genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Duration of Songs by Genre", x = "Genre", y = "Duration (seconds)") +
  scale_fill_manual(values = spotify_colors) +  # Use Spotify color set
  theme_minimal() +  # Use minimal theme for clean look
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Title formatting
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Rotate and align x-axis labels vertically
    axis.text.y = element_text(size = 12),  # Axis text size
    axis.title = element_text(size = 14, face = "bold"),  # Axis title formatting
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12)  # Legend text size
  )

# Create the app
ui <- fluidPage(
  titlePanel("Songs Analysis"),
  mainPanel(
    plotlyOutput(outputId = "line_chart"),
    plotlyOutput(outputId = "heatmap"),
    plotlyOutput(outputId = "stacked_area_chart"),
    plotlyOutput(outputId = "bar_chart")
  )
)

server <- function(input, output) {
  output$line_chart <- renderPlotly({
    ggplotly(line_chart)
  })
  output$heatmap <- renderPlotly({
    ggplotly(heatmap)
  })
  output$stacked_area_chart <- renderPlotly({
    ggplotly(stacked_area_chart)
  })
  output$bar_chart <- renderPlotly({
    ggplotly(bar_chart)
  })
}

shinyApp(ui, server)
