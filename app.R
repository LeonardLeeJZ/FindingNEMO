pacman::p_load(shiny, tidyverse, ggthemes)

# Read the data
exam <- readr::read_csv("data/Exam_data.csv")

ui <- fluidPage(
  
    titlePanel("Examination Results Dashboard"),
 
    sidebarLayout(
      position = "right",
  # Add Sidebar     
      sidebarPanel(
  # Add 'selection' input1
        selectInput(inputId = "variable",
                    label = "Subject:",
                    choices = c("English" = "ENGLISH",
                                "Maths" = "MATHS",
                                "Science" = "SCIENCE"),
  # set default selection as "english"
                    selected = "ENGLISH"),
  # Slider widget for selection input1     
        sliderInput(inputId = "bins",
                    label = "Number of Bins",
                    min = 5,
                    max = 20,
  # set default value of slider using 'value' parameter
                    value = 15)
      ),
      mainPanel(
  # How input1 will be displayed in the output
        plotOutput("distPlot") 
      )
    )
  )

server <- function(input, output) {
  
# Define how input1 gets visualised
  output$distPlot <- renderPlot({
    
    ggplot(exam, 
           aes_string(x = input$variable)) + 
      geom_histogram(bins = input$bins,
                     color = "grey") +
      labs(x = "Score",
           y = "No. of Pupils") +
      theme_economist() 
      
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
