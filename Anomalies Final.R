pacman::p_load(igraph, tidygraph, ggraph, 
               visNetwork, lubridate, clock,
               tidyverse, graphlayouts, bslib, shiny, shinyalert)

# Read the data
nodes <- read_csv("data/anom_nodes.csv")
links <- read_csv("data/mc3_links_new.csv")

ui <- fluidPage(
  useShinyalert(force = TRUE),
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
  
  shinyalert(
    title = "First Time Here?",
    text = "Hi! Welcome to N.E.M.O. Kindly read the user guide before proceeding.",
    size = "l", 
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "I have read the User Guide",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  output$networkPlot <- renderVisNetwork({
    
    # Extract nodes from input$entity
    filter_nodes <- nodes %>%
      filter(group == input$entity & revenue_group == input$revenue & transboundary == input$transboundary)
    
    filter_links <- links %>%
      filter(source %in% filter_nodes$id | target %in% filter_nodes$id
      )
    
    # distinct source and target from filter_links
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
    # Set node colors based on entity
    total_nodes$color <- ifelse(total_nodes$id %in% distinct_target$id, "#F8766D", "#aebbff")
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