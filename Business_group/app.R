pacman::p_load(shiny, tidyverse, DescTools, skimr, ggrepel, DT, kableExtra, ggplot2, scales, ggthemes, visNetwork, ggraph, igraph, gganimate, tidygraph, ggrain, patchwork, ggpubr, htmlwidgets)

# Read the data
mc3_links_new <- read_csv("data/mc3_links_new.csv")
mc3_nodes_all <- read_csv("data/mc3_shinynodes.csv")

# Create a dataframe of distinct companies, with industry column displaying one or more industry this company belongs to# Companies by Industry

## **I. Filter records belonging to companies in different industries**
# Create separate dataframes for nodes in each Industry to use as filters for **links** data. 

# Overall Company nodes
nodes_industry <- mc3_nodes_all %>%
  filter(grepl("company", group, ignore.case = TRUE) & group != "Company Contact")

# industrial companies
nodes_industrial <- nodes_industry %>%
  filter(group == "Industrial Company")

# Food companies
nodes_food <- nodes_industry %>%
  filter(group == "Food Company")

# Seafood-processing companies
nodes_seafood <- nodes_industry %>%
  filter(group == "Seafood-processing Company")

# Consumer-goods companies
nodes_goods <- nodes_industry %>%
  filter(group == "Consumer-goods Company")

# Transport Logistics companies
nodes_transport <- nodes_industry %>%
  filter(group == "Transport-logistics Company")

# fishing-related companies
nodes_fishing <- nodes_industry %>%
  filter(group == "Fishing-related Company")

# multi-industry companies
nodes_multi <- nodes_industry %>%
  filter(group == "Multi-Industry Company")


## **II. Filter links belonging to those companies**  
#Get links data of Individuals linked to each company in the each industry

# Overall company links
links_industry <- mc3_links_new %>%
  filter(source %in% nodes_industry$id)

# industrial companies
links_industrial <- mc3_links_new %>%
  filter(source %in% nodes_industrial$id)

# Food companies
links_food <- mc3_links_new %>%
  filter(source %in% nodes_food$id)

# Seafood-processing companies
links_seafood <- mc3_links_new %>%
  filter(source %in% nodes_seafood$id)

# Consumer-goods companies
links_goods <- mc3_links_new %>%
  filter(source %in% nodes_goods$id)

# Transport Logistics companies
links_transport <- mc3_links_new %>%
  filter(source %in% nodes_transport$id)

# fishing-related companies
links_fishing <- mc3_links_new %>%
  filter(source %in% nodes_fishing$id)

# multi-industry companies
links_multi <- mc3_links_new %>%
  filter(source %in% nodes_multi$id)


## **III. Get unique source and target from filtered links**

# Overall Companies
links_source <- links_industry %>%
  distinct(source) %>%
  rename("id" = "source")

links_target <- links_industry %>%
  distinct(target) %>%
  rename("id" = "target")

# industrial companies
links_source1 <- links_industrial %>%
  distinct(source) %>%
  rename("id" = "source")

links_target1 <- links_industrial %>%
  distinct(target) %>%
  rename("id" = "target")

# Food companies
links_source2 <- links_food %>%
  distinct(source) %>%
  rename("id" = "source")

links_target2 <- links_food %>%
  distinct(target) %>%
  rename("id" = "target")

# Seafood-processing companies
links_source3 <- links_seafood %>%
  distinct(source) %>%
  rename("id" = "source")

links_target3 <- links_seafood %>%
  distinct(target) %>%
  rename("id" = "target")

# Consumer-goods companies
links_source4 <- links_goods %>%
  distinct(source) %>%
  rename("id" = "source")

links_target4 <- links_goods %>%
  distinct(target) %>%
  rename("id" = "target")

# Transport Logistics companies
links_source5 <- links_transport %>%
  distinct(source) %>%
  rename("id" = "source")

links_target5 <- links_transport %>%
  distinct(target) %>%
  rename("id" = "target")

# fishing-related companies
links_source6 <- links_fishing %>%
  distinct(source) %>%
  rename("id" = "source")

links_target6 <- links_fishing %>%
  distinct(target) %>%
  rename("id" = "target")

# multi-industry companies
links_source7 <- links_multi %>%
  distinct(source) %>%
  rename("id" = "source")

links_target7 <- links_multi %>%
  distinct(target) %>%
  rename("id" = "target")

## **IV. Bind links together to get overall nodes dataframe**

# Overall Companies
nodes_industry_new <- bind_rows(links_source, links_target) %>%
  left_join(mc3_nodes_all, by = "id") %>%
  select(id, group, revenue_group, transboundary)

# industrial companies
nodes_industrial_new <- bind_rows(links_source1, links_target1) %>%
  left_join(mc3_nodes_all, by = "id") %>%
  select(id, group, revenue_group, transboundary)

# Food companies
nodes_food_new <- bind_rows(links_source2, links_target2) %>%
  left_join(mc3_nodes_all, by = "id") %>%
  select(id, group, revenue_group, transboundary)

# Seafood-processing companies
nodes_seafood_new <- bind_rows(links_source3, links_target3) %>%
  left_join(mc3_nodes_all, by = "id") %>%
  select(id, group, revenue_group, transboundary)

# Consumer-goods companies
nodes_goods_new <- bind_rows(links_source4, links_target4) %>%
  left_join(mc3_nodes_all, by = "id") %>%
  select(id, group, revenue_group, transboundary)

# Transport Logistics companies
nodes_transport_new <- bind_rows(links_source5, links_target5) %>%
  left_join(mc3_nodes_all, by = "id") %>%
  select(id, group, revenue_group, transboundary)

# fishing-related companies
nodes_fishing_new <- bind_rows(links_source6, links_target6) %>%
  left_join(mc3_nodes_all, by = "id") %>%
  select(id, group, revenue_group, transboundary)

# multi-industry companies
nodes_multi_new <- bind_rows(links_source7, links_target7) %>%
  left_join(mc3_nodes_all, by = "id") %>%
  select(id, group, revenue_group, transboundary)

# UI
ui <- fluidPage(
  titlePanel("Plot of Business Groups by Similarity Measure"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "industry_filter",
                  label = "Select Industry",
                  choices = c("All", "Industrial", "Food", "Seafood-processing", "Consumer-goods", 'Transport-Logistics', 'Fishing-related', 'Multi-Industry')),
      selectInput(inputId = "similarity_filter",
                  label = "Select Similarity Measure",
                  choices = c("Degree_Centrality", "Transitivity", "Assortivity"))
    ),
    mainPanel(
      plotOutput("business_plot")
      #visNetworkOutput("network_plot")
    )
  )
)

# Server
server <- function(input, output) {

  ## **V. Create Graph Objects**

  # Overall company graph
  industry_graph <- tbl_graph(nodes = nodes_industry_new,
                              edges = links_industry, 
                              directed = FALSE)
  
  # industrial companies
  industrial_graph <- tbl_graph(nodes = nodes_industrial_new,
                                edges = links_industrial, 
                                directed = FALSE)
  
  # Food companies
  food_graph <- tbl_graph(nodes = nodes_food_new,
                                edges = links_food, 
                                directed = FALSE)
  # Seafood companies
  seafood_graph <- tbl_graph(nodes = nodes_seafood_new,
                                edges = links_seafood, 
                                directed = FALSE)
  # Consumer Goods companies
  goods_graph <- tbl_graph(nodes = nodes_goods_new,
                                edges = links_goods, 
                                directed = FALSE)
  
  # Transport Logistics companies
  transport_graph <- tbl_graph(nodes = nodes_transport_new,
                           edges = links_transport, 
                           directed = FALSE)
  
  # Fishing related companies
  fishing_graph <- tbl_graph(nodes = nodes_fishing_new,
                           edges = links_fishing, 
                           directed = FALSE)
  
  # Multi-industry companies
  multi_graph <- tbl_graph(nodes = nodes_multi_new,
                           edges = links_multi, 
                           directed = FALSE)

  output$business_plot <- renderPlot({

    # Measure degree centrality and save as a column
    V(industrial_graph)$degree <- degree(industrial_graph, mode = "all")
    
    set.seed(1234)
    industrial_network <- industrial_graph %>%
      ggraph(layout = "nicely") +
      geom_edge_fan(
        alpha = .6,
        show.legend = FALSE
      ) +
      scale_edge_width(
        range = c(0.1,4)
      ) +
      geom_node_point(
        aes(size = degree,
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
      labs(
        title = "Industrial Company Network"
      ) +
      theme(
        plot.title = element_text(size = 16,
                                  color = "grey20"),
        legend.title = element_text(),
        legend.position = "bottom",
        # legend.direction = "horizontal",
        legend.background = element_rect(fill="#dfdfeb",colour="#dfdfeb"),
        panel.background = element_rect(fill="#dfdfeb",colour="#dfdfeb"),
        plot.background = element_rect(fill="#dfdfeb",colour="#dfdfeb"),
        plot.margin = margin(r = 10,
                             l = 10)
      )
  
  })
}

# Run the app
shinyApp(ui = ui, server = server)