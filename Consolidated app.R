pacman::p_load(igraph, tidygraph, ggraph, 
               visNetwork, lubridate, clock,
               tidyverse, graphlayouts, bslib)

# Read the data
nodes_anom <- read_csv("data/anom_nodes.csv")
links_anom <- read_csv("data/mc3_links_new.csv")
links <- read_csv("data/mc3_links_new.csv")
nodes <- read_csv("data/mc3_shinynodes.csv")

### Header - Dashboard Header

header <- dashboardHeader(title = h3('ISSS608 VAA Grp Project - VAST Challenge 2023'), titleWidth = 300)

### Body - 3 Tab Items

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "tab1",
            fluidPage(
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
                  visNetworkOutput("anomPlot")
                )
              ),
              theme = bs_theme(bootswatch = "morph")
            )),


    tabItem(tabName = "tab2",
            fluidPage(
              titlePanel(title = "Visualising the Different Industries"),
              sidebarLayout(
                sidebarPanel = sidebarPanel(
                  selectInput(
                    inputId = "industry",
                    label = "Select Industry:",
                    choices = c(
                      `All Industries` = "All", 
                      `Fishing-related` = "Fishing-related Company",
                      `Industrial` = "Industrial Company",
                      `Food-related` = "Food Company",
                      `Seafood Processing` = "Seafood-processing Company",
                      `Consumer Goods` = "Consumer-goods Company",
                      `Transport & Logistics` = "Transport-logistics Company",
                      `Multi-industry` = "Multi-Industry Company"
                      
                    ), 
                    selected = "Fishing-related Company"),
                  selectInput(inputId = "measure",
                              label = "Select Similarity Measure",
                              choices = c("Degree Centrality", "Transitivity", "Assortativity", "Eigenvector_Centrality", "Closeness", "Page_Rank"),
                              selected ="Degree Centrality")
                  
                ),
                
              
                mainPanel = mainPanel(
                  title = "Industry-based Networks",
                  plotOutput("similarityPlot", height = "800px", width = "800px")
                )
              ),
              theme = bs_theme(bootswatch = "morph")
            )),

      tabItem(tabName = "tab3",
              fluidPage(
                titlePanel(title = "Network Comparisons between Industries"),
                sidebarLayout(
                  sidebarPanel = sidebarPanel(
                    selectInput(
                      inputId = "industry1",
                      label = "Select Industry:",
                      choices = c(
                        `Fishing-related` = "Fishing-related Company",
                        `Industrial` = "Industrial Company",
                        `Food-related` = "Food Company",
                        `Seafood Processing` = "Seafood-processing Company",
                        `Consumer Goods` = "Consumer-goods Company",
                        `Transport & Logistics` = "Transport-logistics Company",
                        `Multi-industry` = "Multi-Industry Company"
                        
                      ), 
                      selected = "Fishing-related Company"),
                    selectInput(
                      inputId = "industry2",
                      label = "Select Industry:",
                      choices = c(
                        `Fishing-related` = "Fishing-related Company",
                        `Industrial` = "Industrial Company",
                        `Food-related` = "Food Company",
                        `Seafood Processing` = "Seafood-processing Company",
                        `Consumer Goods` = "Consumer-goods Company",
                        `Transport & Logistics` = "Transport-logistics Company",
                        `Multi-industry` = "Multi-Industry Company"
                        
                      ),
                      selected = "Fishing-related Company"),
                    selectInput(inputId = "measure",
                                label = "Select Similarity Measure",
                                choices = c("Degree Centrality", "Transitivity", "Assortativity", "Eigenvector_Centrality", "Closeness", "Page_Rank"),
                                selected ="Degree Centrality")
                    
                  ),
                  mainPanel = mainPanel(
                    title = "Similarity of Industry-based Networks",
                    plotOutput("networkPlot", height = "730px", width = "800px")
                  )
                ),
                theme = bs_theme(bootswatch = "morph")
              ))))
              

### End of Tab Items
    
### UI page

ui <- dashboardPage(
  skin = 'blue', 
  header, 
  body)

####  4) Server Page  #############################################################################################################################################

server <- function(input, output){
  
#### Server > tab 1 -  Network Analysis for Fishy Trading Activity  ###########################################################################################################################################
  
  # shinyalert(
  #   title = "First Time Here?",
  #   text = "Hi! Welcome to N.E.M.O. Kindly read the user guide before proceeding.",
  #   size = "l", 
  #   closeOnEsc = TRUE,
  #   closeOnClickOutside = FALSE,
  #   html = FALSE,
  #   type = "info",
  #   showConfirmButton = TRUE,
  #   showCancelButton = FALSE,
  #   confirmButtonText = "I have read the User Guide",
  #   confirmButtonCol = "#AEDEF4",
  #   timer = 0,
  #   imageUrl = "",
  #   animation = TRUE
  # ),
  
  output$anomPlot <- renderVisNetwork({
    
    # Extract nodes from input$entity
    filter_nodes_anom <- nodes_anom %>%
      filter(group == input$entity & revenue_group == input$revenue & transboundary == input$transboundary)
    
    filter_links_anom <- links_anom %>%
      filter(source %in% filter_nodes_anom$id | target %in% filter_nodes_anom$id
      )
    
    # distinct source and target from filter_links
    distinct_source_anom <- filter_links_anom %>%
      distinct(source) %>%
      rename("id" = "source") 
    
    distinct_target_anom <- filter_links_anom %>%
      distinct(target) %>%
      rename("id" = "target")
    
    total_nodes_anom <- bind_rows(distinct_source_anom, distinct_target_anom)
    total_links_anom <- filter_links_anom %>%
      rename("from" = "source",
             "to" = "target")
    # Set node colors based on entity
    total_nodes_anom$color <- ifelse(total_nodes_anom$id %in% distinct_target_anom$id, "#F8766D", "#aebbff")
    # Plot network
    visNetwork(
      total_nodes_anom, 
      total_links_anom,
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
  
  
  
#### Server > tab 2 -  Visualising the Different Industries  ###################################################################################################################
  output$similarityPlot <- renderPlot({
    
    # Filter nodes data from input$industry
    filtered_nodes <- nodes %>%
      filter(if (input$industry == "All") TRUE else group == input$industry)
    
    # Filter links based filtered_nodes to get connections
    filtered_links <- links %>%
      filter(source %in% filtered_nodes$id)
    
    # Get unique source and target
    links_source <- filtered_links %>%
      distinct(source) %>%
      rename("id" = "source")
    
    links_target <- filtered_links %>%
      distinct(target) %>%
      rename("id" = "target")
    
    # bind links to get overall nodes dataframe
    filtered_nodes_new <- bind_rows(links_source, links_target) %>%
      left_join(nodes, by = "id") %>%
      select(id, group)
    
    # Create graph object
    filtered_graph <- tbl_graph(nodes = filtered_nodes_new,
                                edges = filtered_links, 
                                directed = FALSE)
    
    # Calculate all Similarity Measures first
    filtered_graph <- filtered_graph %>%
      activate(nodes) %>%
      mutate(
        degree = degree(filtered_graph, mode = "all"),
        transitivity = transitivity(filtered_graph, type = "global"),
        assortativity = assortativity_degree(filtered_graph, directed = FALSE),
        eigen = eigen_centrality(filtered_graph)$vector,
        closeness = closeness(filtered_graph),
        page_rank = page_rank(filtered_graph)$vector
      )
    
    set.seed(1234)
    ggraph(filtered_graph,
           layout = "nicely"
    ) +
      geom_edge_fan(
        alpha = .6,
        show.legend = FALSE
      ) +
      scale_edge_width(
        range = c(0.1,4)
      ) +
      geom_node_point(
        aes(size = ifelse(input$measure == "Degree Centrality", degree,
                          ifelse(input$measure == "Transitivity", transitivity,
                                 ifelse(input$measure == "Assortativity", assortativity,
                                        ifelse(input$measure == "Eigenvector_Centrality", eigen,
                                               ifelse(input$measure == "Closeness", closeness,
                                                      ifelse(input$measure == "Page_Rank", page_rank)))))),
            color = group),
        alpha = .9
      ) +
      # Remove the legend for "degree"
      guides(color = guide_legend(title = "Role:"),
             size = "none"
      ) + 
      geom_node_text(
        aes(label = ifelse(degree > quantile(degree, .75), id, "")), 
        size = 2,
        repel = TRUE
      ) +
      theme(
        plot.title = element_text(size = 16,
                                  color = "grey20"),
        legend.title = element_text()
      )
  })

#### Server > tab 3 -  Visualising the Different Industries  ########################################################################################
  
  output$networkPlot <- renderPlot({
    
    # Filter nodes1 data from input$industry1
    filtered_nodes1 <- nodes %>%
      filter(group == input$industry1)
    
    # Filter nodes2 data from input$industry2
    filtered_nodes2 <- nodes %>%
      filter(group == input$industry2)
    
    # Combine the nodes
    combined_nodes <- bind_rows(filtered_nodes1, filtered_nodes2) %>%
      distinct(id) %>%
      left_join(nodes, by = "id") %>%
      select(id, group)
    
    # Filter links based filtered_nodes1 to get connections
    filtered_links <- links %>%
      filter(source %in% combined_nodes$id)
    
    # Get unique source and target from filtered_links1
    links_source <- filtered_links %>%
      distinct(source) %>%
      rename("id" = "source")
    
    links_target <- filtered_links %>%
      distinct(target) %>%
      rename("id" = "target")
    
    
    # bind links to get overall nodes dataframe1
    filtered_nodes_new <- bind_rows(links_source, links_target) %>%
      left_join(nodes, by = "id") %>%
      select(id, group)
    
    # Create graph object1
    filtered_graph <- tbl_graph(nodes = filtered_nodes_new,
                                edges = filtered_links, 
                                directed = FALSE)
    
    filtered_graph <- filtered_graph %>%
      activate(nodes) %>%
      mutate(
        degree = degree(filtered_graph, mode = "all"),
        transitivity = transitivity(filtered_graph, type = "global"),
        assortativity = assortativity_degree(filtered_graph, directed = FALSE),
        eigen = eigen_centrality(filtered_graph)$vector,
        closeness = closeness(filtered_graph),
        page_rank = page_rank(filtered_graph)$vector
      )
    
    
    set.seed(1234)
    ggraph(filtered_graph,
           layout = "nicely"
    ) +
      geom_edge_fan(
        alpha = .6,
        show.legend = FALSE
      ) +
      scale_edge_width(
        range = c(0.1,4)
      ) +
      geom_node_point(
        aes(size = ifelse(input$measure == "Degree Centrality", degree,
                          ifelse(input$measure == "Transitivity", transitivity,
                                 ifelse(input$measure == "Assortativity", assortativity,
                                        ifelse(input$measure == "Eigenvector_Centrality", eigen,
                                               ifelse(input$measure == "Closeness", closeness,
                                                      ifelse(input$measure == "Page_Rank", page_rank)))))),
            color = group),
        alpha = .9
      ) +
      
      # Remove the legend for "degree"
      guides(color = guide_legend(title = "Role:"),
             size = "none"
      ) + 
      geom_node_text(
        aes(label = ifelse(degree > quantile(degree, .75), id, "")), 
        size = 2,
        repel = TRUE
      ) +
      theme(
        plot.title = element_text(size = 16,
                                  color = "grey20"),
        legend.title = element_text()
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)