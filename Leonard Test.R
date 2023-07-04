pacman::p_load(igraph, tidygraph, ggraph, 
               visNetwork, lubridate, clock,
               tidyverse, graphlayouts, bslib)

# Read the data
nodes <- read_csv("data/anom_nodes.csv")
links <- read_csv("data/mc3_links_new.csv")

ui <- fluidPage(
  titlePanel(title = "Network Analysis for Fishy Trading Activity"),
  tabsetPanel(
    tabPanel("Companies",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   inputId = "entity",
                   label = "Select Entity:",
                   choices = c(`Company` = "Company"
                   ),
                   selected = "Company"
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
                 title = "Company Network",
                 visNetworkOutput("networkPlot")
               )
             )
    ),
    tabPanel("Individuals",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   inputId = "entity",
                   label = "Select Entity:",
                   choices = c(
                     `Ultimate Beneficial Owner` = "Ultimate Beneficial Owner",
                     `Shareholder` = "Shareholder",
                     `Multi-role Entity` = "Multi-role Entity",
                     `Company Contact` = "Company Contact"
                   ),
                   selected = "Ultimate Beneficial Owner"
                 ),
                 selectInput(
                   inputId = "revenue",
                   label = "Select Revenue Group:",
                   choices = c(`Unreported` = "Unreported"
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
                 title = "Individual Network",
                 visNetworkOutput("individualPlot")
               )
             )
    )
  ),
  theme = bs_theme(bootswatch = "morph")
)

server <- function(input, output) {
  
  output$networkPlot <- renderVisNetwork({
    
    filter_nodes <- nodes %>%
      filter(revenue_group == input$revenue & transboundary == input$transboundary)
    
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
      visInteraction(navigationButtons = TRUE)
  })
  
  output$individualPlot <- renderVisNetwork({
    # Extract nodes from input$entity
    filter_nodes2 <- nodes %>%
      filter(group == input$entity & revenue_group == input$revenue & transboundary == input$transboundary)
    
    filter_links2 <- links %>%
      filter(source %in% filter_nodes2$id | target %in% filter_nodes2$id)
    
    # distinct source and target from filter_links
    distinct_source2 <- filter_links2 %>%
      distinct(source) %>%
      rename("id" = "source") 
    
    distinct_target2 <- filter_links2 %>%
      distinct(target) %>%
      rename("id" = "target")
    
    total_nodes2 <- bind_rows(distinct_source2, distinct_target2)
    total_links2 <- filter_links2 %>%
      rename("from" = "source",
             "to" = "target")
    # Set node colors based on entity
    total_nodes2$color <- ifelse(total_nodes2$id %in% distinct_target2$id, "#F8766D", "#aebbff")
    
    # Plot network
    visNetwork(
      total_nodes2, 
      total_links2,
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
      visInteraction(navigationButtons = TRUE)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)