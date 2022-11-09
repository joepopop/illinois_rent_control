library(shiny)
library(tidyverse)
library(bslib)
library(thematic)
library(sf)
library(patchwork)

thematic_shiny(
  bg = "auto",
  fg = "auto",
  accent = "auto",
  font = "auto",
  sequential = sequential_gradient(),
  qualitative = okabe_ito(),
  inherit = FALSE,
  session = shiny::getDefaultReactiveDomain()
)

light <- bs_theme(version = 4, bootswatch = "lux", primary = "#75B3CE")

df_2 <- read_csv("data/basic_dat.csv") %>% 
  janitor::clean_names() %>% 
  rename(properties_owned = properties_held_by_taxpayer_match_code)

chi_map <- read_sf("https://raw.githubusercontent.com/thisisdaryn/data/master/geo/chicago/Comm_Areas.geojson") %>% 
  rename(community_area = community) %>% 
  mutate(community_area = tools::toTitleCase(tolower(community_area))) 

df <- df_2 %>% 
  left_join(chi_map)

ui <- fluidPage(
    theme = light, 
    
    titlePanel(""),

    sidebarLayout(
        sidebarPanel(
            sliderInput("threshold",
                        "Adjust number of properties ",
                        min = 3,
                        max = 300,
                        value = 6),
            checkboxInput("swap", "Figure 1: greater/less"),
            checkboxInput("raw", "Figure 2: percentage/number"),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            p("Sources:"),
            p("1)",
              a("Find My Landlord", 
                href = "https://findmylandlord.chicagodsa.org/")
              ),
            p("2)",
              a("IL Community Area Data", 
                href = "https://raw.githubusercontent.com/thisisdaryn/data/master/geo/chicago/Comm_Areas.geojson")
              ),
            br(),
            br(),
            # p("Produced by Joe Omatoi")
        ),

        mainPanel(
           plotOutput("distPlot"),
           plotOutput("distPlot2")
        )
    )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    df_count <- df %>% 
      group_by(community_area) %>% 
      mutate(
        properties = properties_owned,
        count = if(input$swap == T) (if_else(properties < input$threshold, 1, 0)) else (if_else(properties > input$threshold, 1, 0)), 
        percentage = mean(count)*100,
        ) %>% 
      select(community_area, properties, percentage, geometry) %>% 
      distinct()
    
    df_label <- df_count %>%
      filter(if(input$swap == T) (percentage <= 50) else (percentage >= 50))
    
    df_count %>% 
      ggplot(aes(fill = percentage, geometry = geometry)) + 
      geom_sf() +
      geom_sf_label(
        data = df_label,
        aes(label = community_area), 
        color = "black",
        ) +
      scale_fill_gradient2(
        low = "blue", 
        mid = "white", 
        high = "orange", 
        midpoint = 50
      ) +
      labs(
        title = paste("Figure 1. Percentage of properties whose landlord owns", if_else(input$swap == T, "less", "more"), "than", input$threshold, "properties by community area", sep = " "),
        fill = "Percentage",
        x = NULL,
        y = NULL
      ) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
  })
  
  output$distPlot2 <- renderPlot({
    
    text <- df_2 %>% 
      filter(!is.na(properties_owned)) %>% 
      distinct(affiliated_with, .keep_all = T) %>% 
      mutate(
        properties_owned = if_else(properties_owned < input$threshold, paste('<', input$threshold), paste('>', input$threshold))
      ) %>% 
      group_by(properties_owned) %>% 
      count() 
    
    text2 <- text %>% 
      mutate(
        x = 'x',
        n = if_else(properties_owned == paste('<', input$threshold), sum(text$n), n)
      )
    
    text3 <- text %>% 
      mutate(
        n = paste(round(n/sum(text$n)*100, 0), '%')
      )
    
    df_2 %>% 
      filter(!is.na(properties_owned)) %>% 
      distinct(affiliated_with, .keep_all = T) %>% 
      mutate(
        x = "x",
        properties_owned = if_else(properties_owned <input$threshold, paste('<', input$threshold), paste('>', input$threshold))
      ) %>% 
      ggplot() +
      aes(x = x, fill = properties_owned) +
      geom_bar(color = "white", alpha = 0.75) +
      annotate(
        geom = "text",
        x = text2$x,
        y = text2$n - text$n/2,
        label = if(input$raw == T) (text$n) else (text3$n),
        size = 10
      ) +
      coord_polar("y") +
      theme(
        # panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
      ) +
      # theme_void() +
      labs(
        fill = "Properties owned",
        x = NULL,
        y = NULL,
        title = paste("Figure 2. Percentage/number of landlords that own more/less than", input$threshold, "properties", sep = " ")
      )
  })
  
}

shinyApp(ui = ui, server = server)
