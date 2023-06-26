pacman::p_load(igraph, tidygraph, ggraph, 
               visNetwork, lubridate, clock,
               tidyverse, graphlayouts, bslib, plotly)

# Read the data
mc2_nodes <- readRDS("data/mc2_nodes_extracted.rds")
mc2_edges <- readRDS("data/mc2_edges_aggregated.rds")
graph <- tbl_graph(nodes = mc2_nodes,
                   edges = mc2_edges, 
                   directed = TRUE)

## Anomalies Data

## Groups



ui <- page_navbar(
  title = "Network Analysis for Fishy Trading Activity",
  sidebar = sidebar(
    bg = "white",
    accordion(
      accordion_panel(
        "Primary Controls")
    ),
    accordion_panel(
      "Other Controls",
      "Other Controls go here"
    )
  ),
  nav_panel("Anomalous Behaviours"),
  nav_panel("Anomalies",
    column(
      width = 2,
      fluidRow(
        selectInput("type", "Type", choices = c("Companies", "Beneficial Owners")),
        selectInput("countries", "Countries of Operation", choices = c("Single Country", "Multiple Countries")),
        selectInput("revenue", "Revenue", choices = c("High", "Medium", "Low", "Unreported"))
      )
    ),
    fluidRow(
      column(
        width = 5,
        plotly::plotlyOutput("graph")
      ),
      column(
        width = 5,
        p("Other content goes here.")
      )
    )
  ),       
  nav_panel("Groups"),
  nav_spacer(),
  theme = bs_theme((version = 5), #prevent dashboard breaking
    bootswatch = "morph"
  )
  
)


### SERVER PAGE###

server <- function(input, output) {
  
### Server > Anomalies
  
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
  
  
  ### Server > Groups
  
  
  ###
}

# Run the application 
shinyApp(ui = ui, server = server)
