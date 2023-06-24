pacman::p_load(igraph, tidygraph, ggraph, 
               visNetwork, lubridate, clock,
               tidyverse, graphlayouts, bslib)

# Read the data
mc2_nodes <- readRDS("data/mc2_nodes_extracted.rds")
mc2_edges <- readRDS("data/mc2_edges_aggregated.rds")
graph <- tbl_graph(nodes = mc2_nodes,
                   edges = mc2_edges, 
                   directed = TRUE)

ui <- fluidPage(
  titlePanel(title = "Network Analysis for Fishy Trading Activity"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      selectInput(
        inputId = "layout",
        label = "Select layout:",
        choices = c(
          `Kamada and Kawai` = "kk",
          `Fruchterman-Reingold` = "fr",
          `Distributed Recursive` = "drl",
          `Large Graph` = "lgl",
          Grid = "grid",
          Nicely = "nicely"
        ),
        selected = "kk"
      )
    ),
    mainPanel = mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Plot",
          plotOutput(
            outputId = "networkPlot",
            height = "500px"
          )
        ),
        tabPanel(
          title = "Summary",
          verbatimTextOutput(outputId = "summary")
        ),
        tabPanel(
          title = "Table",
          tableOutput(outputId = "table")
        ),
        type = "tabs"
      )
    ),
    position = "right"
  ),
  theme = bs_theme(bootswatch = "morph")
)

server <- function(input, output) {
  
  output$networkPlot <- renderPlot({
    set.seed(1234)
    
    ggraph(graph,
           layout = input$layout) +
      geom_edge_link(aes(width = weights),
                     alpha = .6) +
      scale_edge_width(range = c(0.1, 3)) +
      geom_node_point(aes(color = id)) +
      theme(legend.position = "none",
            panel.background = element_rect(fill='#d9e3f1'),
            plot.background = element_rect(fill='#d9e3f1'))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
