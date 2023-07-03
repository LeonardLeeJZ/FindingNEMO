pacman::p_load(igraph, tidygraph, ggraph, 
               visNetwork, lubridate, clock,
               tidyverse, graphlayouts, bslib)

# Read the data
nodes <- read_csv("data/anom_nodes.csv")
links <- read_csv("data/mc3_links_new.csv")

ui <- fluidPage(
  titlePanel(title = "Network Analysis for Fishy Trading Activity"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      radioButtons(
        inputId = "entity",
        label = "Select Entity:",
        choices = c(
          `Ultimate Beneficial Owner` = "Ultimate Beneficial Owner",
          `Shareholder` = "Shareholder",
          `Multi-role Entity` = "Multi-role Entity",
          `Company Contact` = "Company Contact"
        ),
        selected = c("Ultimate Beneficial Owner")
      ),
      selectInput(
        inputId = "revenue",
        label = "Select Revenue Group:",
        choices = c(
          `High` = "High",
          `Medium` = "Medium",
          `Low` = "Low",
          `Unreported` = "Unreported"
        ),
        selected = "Unreported"
      ),
      selectInput(
        inputId = "transboundary",
        label = "Select Transboundary:",
        choices = c(
          `Yes` = "yes",
          `No` = "no"
        ),
        multiple = TRUE,
        selected = c("yes", "no")
      )
    ),
    mainPanel = mainPanel(
      title = "Network",
      visNetworkOutput("networkPlot")
    )
  ),
  theme = bs_theme(bootswatch = "morph")
)

server <- function(input, output) {
  
  output$networkPlot <- renderVisNetwork({
    
    # Extract nodes from input$entity
    
    filter_nodes <- nodes %>%
      filter(group == input$entity & revenue_group == input$revenue & transboundary == input$transboundary)
    
    
    filter_links <- links %>%
      filter(target %in% filter_nodes$id)
    
    # distinct source and target from filter_linksm
    distinct_source <- filter_links %>%
      distinct(source) %>%
      rename("id" = "source") 
    
    distinct_target <- filter_links %>%
      distinct(target) %>%
      rename("id" = "target")
    
    total_nodes <- bind_rows(distinct_source, distinct_target)
    total_links <- filter_links %>%
      rename("from" = "source",
             "to" = "target")
    
    # Plot network
    visNetwork(
      total_nodes, 
      total_links,
      width = "100%"
    ) %>%
      visIgraphLayout(
        layout = "layout_with_fr"
      ) %>%
      visLegend() %>%
      visGroups() %>%
      visEdges() %>%
      visOptions(
        # Specify additional Interactive Elements
        highlightNearest = list(enabled = T, degree = 2, hover = T),
        # Add drop-down menu to filter by company name
        nodesIdSelection = TRUE,
        collapse = TRUE
      ) %>%
      visInteraction(navigationButtons = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)