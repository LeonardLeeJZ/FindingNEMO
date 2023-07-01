pacman::p_load(igraph, tidygraph, ggraph, 
               visNetwork, lubridate, clock,
               tidyverse, graphlayouts, bslib, plotly, jsonlite, stringr)

# BSLIB vs RShiny
#differences: grids used, function calls are different, + colour palette and font palette 
###install showtext

# Read the data
#mc2_nodes <- readRDS("data/mc2_nodes_extracted.rds")

#mc2_edges <- readRDS("data/mc2_edges_aggregated.rds")
#graph <- tbl_graph(nodes = mc2_nodes,
                  # edges = mc2_edges, 
                   #directed = TRUE)\
mc3_data <- fromJSON("data/MC3.json")


## Anomalies Data
anom_nodes <- read_csv("data/anom_nodes.csv")


anom_edges <- read_csv("data/anom_edges.csv")


## Groups



ui <- page_navbar(
  title = "Network Analysis for Fishy Trading Activity",
  nav_panel("Anomalous Behaviours"),
  nav_panel("Anomalies",
            column(
              width = 5,
              fluidRow(
                column(
                  width = 12,
                  checkboxGroupInput("type_filter", "Type", choices = unique(anom_nodes$type), selected = NULL)
                ),
                column(
                  width = 12,
                  checkboxGroupInput("transboundary_filter", "Transboundary", choices = c("Yes" = "yes", "No" = "no"), selected = NULL)
                ),
                column(
                  width = 12,
                  checkboxGroupInput("revenue_group_filter", "Revenue", choices = c("High" = 1, "Medium" = 2, "Low" = 3, "Undeclared" = NA), selected = NULL)
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  visNetworkOutput("Anom_Graph")
                )
              )
            )
  ),       
  nav_panel("Groups"),
  nav_spacer(),
  theme = bs_theme(version = 5, bootswatch = "morph")
)




### SERVER PAGE###

server <- function(input, output) {
  
  ### Server > Hailey
  
### Server > Anomalies

  output$Anom_Graph <- renderVisNetwork({
    filtered_nodes <- anom_nodes %>%
      mutate(type = str_trim(type)) %>%
      filter(type %in% input$type_filter) %>%
      filter(transboundary %in% input$transboundary_filter) %>%
      filter(revenue_group %in% input$revenue_group_filter)
    
    visNetwork(filtered_nodes, anom_edges) %>%
      visOptions(highlightNearest = TRUE)
  })
  
 
  
  ### Server > Groups
  
  
  ###
}

# Run the application 
shinyApp(ui = ui, server = server)
