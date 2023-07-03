pacman::p_load(igraph, tidygraph, ggraph, 
               visNetwork, lubridate, clock,
               tidyverse, graphlayouts, bslib)

# Read the data
nodes <- read_csv("data/mc3_shinynodes.csv")
links <- read_csv("data/mc3_links_new.csv")

ui <- fluidPage(
  titlePanel(title = "Comparing between Industries"),
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
)

server <- function(input, output) {
  
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
