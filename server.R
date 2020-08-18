#server.R
server <- function(input, output){
  filtered_data <- reactive({
    game_sales %>%
      filter(between(year_of_release,
                     input$year_select[1],
                     input$year_select[2]
                     )) %>%
      filter(genre == input$genre_select) %>%
      filter(platform == input$platform_select)
  })
  plot_stats <- reactive({
    game_sales %>%
      slice(1:10)
  })
  plot_stats_long <- reactive({
    game_sales %>%
      pivot_longer(cols = c("user_score", "critic_score", "sales"),
                   names_to = "ratings_scores",
                   values_to = "numbers") %>%
      group_by(platform) %>%
      summarise(count = n()/3)
  })
  plot_stats_long_sliced <- reactive({
    game_sales %>%
      pivot_longer(cols = c("user_score", "critic_score", "sales"),
                   names_to = "ratings_scores",
                   values_to = "numbers") %>%
      slice(1:30)
  })
  plot_stats_studio <- reactive({
    game_sales %>%
      pivot_longer(cols = c("user_score", "critic_score", "sales"),
                   names_to = "ratings_scores",
                   values_to = "numbers") %>%
      group_by(developer) %>%
      summarise(count = n()/3)
  })
  plot_stats_year <- reactive({
    game_sales %>%
      pivot_longer(cols = c("user_score", "critic_score", "sales"),
                   names_to = "ratings_scores",
                   values_to = "numbers") %>%
      group_by(year_of_release) %>%
      summarise(count = n()/3)
  })
  
# outputs
  #table output
    output$filtered_table <- renderDataTable({
      filtered_data() 
      #options = list(searching = FALSE, paging = TRUE)
  })
    
  #plot  text output
    output$popularity_plot_text <- renderText({
      if(input$stats_selection == "Top 10 critic rated games"){ 
        return(c("This chart shows Top 10 games and ranks them in ascending order by 
                 popularity, as rated by games critics, for each game. This metric may be useful 
                 for performing market analysis or for deciding which games to stock in 
                 a shop or warehouse."))
      }
      if(input$stats_selection == "Top 10 user rated games"){ 
        return(c("This chart shows Top 10 games and ranks them in ascending order by 
                 popularity, as rated by users, for each game. This metric may be useful 
                 for performing market analysis or for deciding which games to stock in 
                 a shop or warehouse."))
      }
      if(input$stats_selection == "Top 10 games sales"){ 
        return(c("This chart shows Top 10 games and ranks them in ascending order by 
                 the number of sales for each game. This metric is very useful for performing 
                 market analysis or for deciding which games to stock in a shop or warehouse."))
      }
      if(input$stats_selection == "All Top 10s"){ 
        return(c("This is a comparison of the Top 10 games by critic and user ratings, and sales. 
                 Critic scores are out of 100, user ratings are out of 10 and sales are 
                 in millions. This plot serves only as illustration rating scores don't 
                 always correlate with sales volumes. For example, Mario Kart DS, in comparison 
                 to other games in our Top 10 list, has fairly low review scores, but it grossed
                 more than other games in sales. This metric may be useful for market analysis."))
      }
      if(input$stats_selection == "Platform with most games"){ 
        return(c("This plot shows games platforms and shows them in descending order by number
              of games available for each platform. This metric may be interesting for someone,
              who is looking to rebalance stock holding in their shop or warehouse"))
      }
      if(input$stats_selection == "Studio with most releases"){ 
        return(c("This chart lists all the game studios in the database and measures 
              the total number of games each studio has released. This metric may be interesting
              for someone who is analysing the games market saturation."))
      }
      if(input$stats_selection == "The year with most game releases"){ 
        return(c("This plot is intended to illustrate how many games were released every year. 
               Here it is clearly visible that the amount of game releases peaks 
               between 2005-2008, before it starts to shrink."))
      }
      
    })
  #plots output  
    output$popularity_plot <- renderPlot({
      if(input$stats_selection == "Top 10 critic rated games"){ 
        return(
          ggplot(plot_stats()) +
            geom_col(aes(x = reorder(name, critic_score), y = critic_score)) +
            theme(axis.text = element_text(angle = 90)) +
            labs(title = "Top 10 critic rated games",
                 x = "game name",
                 y = "critic rating")
        )
      }
      if(input$stats_selection == "Top 10 user rated games"){ 
        return(
          ggplot(plot_stats()) +
            geom_col(aes(x = reorder(name, user_score), y = user_score)) +
            theme(axis.text = element_text(angle = 90)) +
            labs(title = "Top 10 user rated games",
                 x = "game name",
                 y = "user rating")
        )
      }
      if(input$stats_selection == "Top 10 games sales"){ 
        return(
          ggplot(plot_stats()) +
            geom_col(aes(x = reorder(name, sales), y = sales)) +
            theme(axis.text = element_text(angle = 90)) +
            labs(title = "Top 10 games sales",
                 x = "game name",
                 y = "sales (millions of copies)")
        )
      }
      if(input$stats_selection == "All Top 10s"){ 
        return(
          ggplot(plot_stats_long_sliced()) +
            geom_col(aes(x = name, y = numbers, fill = ratings_scores), position = "dodge") +
            theme(axis.text = element_text(angle = 90)) +
            labs(title = "Top 10 correlation of critic & user ratings against sales",
                 x = "game name",
                 y = "metric") + 
            scale_fill_manual(values = c("#141414", "#4f4f4f", "#767676"))
        )
      }
      if(input$stats_selection == "Platform with most games"){ 
        return(
          ggplot(plot_stats_long()) +
            geom_col(aes(x = reorder(platform, -count), y = count)) +
            theme(axis.text = element_text(angle = 90)) +
            labs(title = "Platform with most games available",
                 x = "Platform",
                 y = "Number of games")
          
        )
      }
      if(input$stats_selection == "Studio with most releases"){ 
        return(
          ggplot(plot_stats_studio()) +
            geom_col(aes(x = reorder(developer, -count), y = count)) +
            theme(axis.text = element_text(angle = 90)) +
            labs(title = "Studio with most releases",
                 x = "Studio",
                 y = "Number of game releases")
          
        )
      }
      if(input$stats_selection == "The year with most game releases"){ 
        return(
          ggplot(plot_stats_year()) +
            geom_col(aes(x = year_of_release, y = count)) +
            labs(title = "The year with most game releases",
                 x = "Year",
                 y = "Number of game releases")
        )
      }
    })
  
}
