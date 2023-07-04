pacman::p_load(igraph, tidygraph, ggraph, 
               visNetwork, lubridate, clock,
               tidyverse, graphlayouts, bslib)

# Read the data
nodes <- read_csv("data/mc3_shinynodes.csv")
links <- read_csv("data/mc3_links_new.csv")

ui <- fluidPage(
  titlePanel(title = "Network Analysis for Fishy Trading Activity"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "entity",
        label = "Select Entity:",
        choices = c(
          `Ultimate Beneficial Owner` = "Ultimate Beneficial Owner",
          `Shareholder` = "Shareholder",
          `Multi-role Entity` = "Multi-role Entity",
          `Company Contact` = "Company Contact",
          `Company` = "Company"
        ),
        selected = "Ultimate Beneficial Owner"
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
    mainPanel(
      visNetworkOutput("networkPlot"),
      tags$div(
        id = "noResultsMsg",
        style = "text-align: center; font-size: 20px; color: red; margin-top: 100px; display: none;",
        "No results found for the selected filters."
      )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(c(input$entity, input$revenue, input$transboundary), {
    # Extract nodes from input$entity
    filter_nodes <- nodes %>%
      filter(
        group == input$entity,
        revenue_group == input$revenue,
        transboundary == input$transboundary
      )
    
    filter_links <- links %>%
      filter(
        source %in% filter_nodes$id | target %in% filter_nodes$id
      )
    
    if (nrow(filter_nodes) == 0 || nrow(filter_links) == 0) {
      output$networkPlot <- renderVisNetwork({
        visNetwork(nodes = NULL, edges = NULL)
      })
      showNoResultsMsg(TRUE)
    } else {
      output$networkPlot <- renderVisNetwork({
        visNetwork(
          nodes = filter_nodes,
          edges = filter_links,
          width = "100%"
        ) %>%
          visIgraphLayout(layout = "layout_with_fr") %>%
          visLegend() %>%
          visGroups(
            groupname = "Company",
            color = "#aebbff"
          ) %>%
          visEdges() %>%
          visOptions(
            # Specify additional Interactive Elements
            highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE),
            # Add drop-down menu to filter by company name
            nodesIdSelection = TRUE,
            collapse = TRUE
          ) %>%
          visInteraction(navigationButtons = FALSE)
      })
      showNoResultsMsg(FALSE)
    }
  })
  
  showNoResultsMsg <- function(show) {
    if (show) {
      tags$script("document.getElementById('noResultsMsg').style.display = 'block';")
    } else {
      tags$script("document.getElementById('noResultsMsg').style.display = 'none';")
    }
  }
}

shinyApp(ui = ui, server = server)