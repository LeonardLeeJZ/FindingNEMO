pacman::p_load(igraph, tidygraph, ggraph, 
               visNetwork, lubridate, clock,
               tidyverse, graphlayouts, bslib, shiny, shinyalert, shinyjs)

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
        choices = c(#radio button method
          `Ultimate Beneficial Owner` = "Ultimate Beneficial Owner",
          `Shareholder` = "Shareholder",
          `Multi-role Entity` = "Multi-role Entity",
          `Company Contact` = "Company Contact",
          `Company` = "Company"
        ),
        selected = c("Ultimate Beneficial Owner")
      ),
      selectInput(
        inputId = "revenue",
        label = "Select Revenue Group:",
        choices = c(
          `High` =  "High",
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
    afilter_nodes <- nodes %>%
      filter(group == input$entity & revenue_group == input$revenue & transboundary == input$transboundary)
    
    afilter_links <- links %>%
      filter(source %in% afilter_nodes$id | target %in% afilter_nodes$id
      )
    
    # distinct source and target from filter_links
    adistinct_source <- afilter_links %>%
      distinct(source) %>%
      rename("id" = "source") 
    
    adistinct_target <- afilter_links %>%
      distinct(target) %>%
      rename("id" = "target")
    
    atotal_nodes <- bind_rows(adistinct_source, adistinct_target)
    atotal_links <- afilter_links %>%
      rename("from" = "source",
             "to" = "target")
    # Set node colors based on entity
    atotal_nodes$color <- ifelse(atotal_nodes$id %in% adistinct_target$id, "#F8766D", "#aebbff")
    # Plot network
    visNetwork(
      atotal_nodes, 
      atotal_links,
      width = "100%"
    ) %>%
      visIgraphLayout(
        layout = "layout_with_fr"
      ) %>%
      visLegend() %>%
      visGroups(groupname = "Company",
                color = "#aebbff") %>%
      visEdges() %>%
      visOptions(
        # Specify additional Interactive Elements
        highlightNearest = list(enabled = T, degree = 2, hover = T),
        # Add drop-down menu to filter by company name
        nodesIdSelection = TRUE,
        collapse = TRUE
      ) %>%
      visInteraction(navigationButtons = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)