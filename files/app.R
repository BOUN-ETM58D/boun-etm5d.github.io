#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
# library(plotly)
# library(ggplot2movies)
#
# set.seed(61)
shiny_movie_set = movies %>% filter(year >= 2000) %>%
  select(title,year,length,rating,votes,Action:Short) %>%
  gather(key=genre,value=value,Action:Short) %>% filter(value==1) %>%
  select(-value)
# load("/Users/berkorbay/git_repositories/dotme/documents/shiny_movie_set.RData")
genres <- shiny_movie_set %>% distinct(genre) %>% unlist(.)
names(genres) <- NULL

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Movies Length and IMDB Scores"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         sliderInput("years",
                     "Years",
                     min = 2000,
                     max = 2005,
                     value = c(2002,2003),sep=""),
         selectInput(inputId="genre",
                     label="Genre",
                     choices=c("All",genres)),
         sliderInput("votes",
                     "En Az Oy",
                     min = 0,
                     max = ceiling(max(shiny_movie_set$votes)/1000)*1000,
                     value = 0,sep="")
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  plot_data <- reactive({
    pl_df <-
      shiny_movie_set %>%
      filter(year >= input$years[1] &
               year <= input$years[2] & votes >= input$votes )

      if(input$genre != "All"){
        pl_df <- pl_df %>% filter(genre == input$genre)
      }

    pl_df
  })

   output$distPlot <- renderPlot({
      ggplot(data=plot_data(),aes(x=length,y=rating,color=genre)) +
       geom_point()
   })
}

# Run the application
shinyApp(ui = ui, server = server)

