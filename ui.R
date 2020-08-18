ui <- fluidPage(
  titlePanel(tags$h1("Gamer's Paradise",
                     tags$h5("Thank you for using Gamer's Paradise dashboard,
                     here you can browse a selection of vintage and modern games 
                     we have in our database.
                     We hope this Shiny dashboard will make it easier for you to
                     choose your perfect game!"))
  ),
  
  tabsetPanel(
    tabPanel("Games Filter",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year_select",
                  "Select years of release",
                  min = min_slider,
                  max = max_slider,
                  value = c(min_slider, max_slider),
                  sep = "",
                  ticks = FALSE
                  ),
                 selectInput("genre_select",
                  "Select genre", 
                  choices = genre_choice,
                  ),
                 selectInput("platform_select",
                  "Select your platform",
                  choices = platform_choice,
                  )
                 #actionButton("update_all",
                 #"Show me my Games!")
    ),
    
    mainPanel(
      dataTableOutput("filtered_table")
    )
      
  )
    ),
    tabPanel("Interesting Stats",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("stats_selection",
                             "Choose your stats",
                             choices = c(stats_choice)
                             )
               ),
               mainPanel(
                 plotOutput("popularity_plot"),
                 textOutput("popularity_plot_text")
               )
             )
            )
  )
)

