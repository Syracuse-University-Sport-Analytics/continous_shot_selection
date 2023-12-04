setwd("~/Desktop/NK_STUFF/NK_SYRACUSE/Berlin Scholar/App")

library(shiny)
library(dplyr)
library(shinythemes)
library(shinyjs)

players_teams_seasons <- read.csv("unique_players_teams_seasons.csv")
players_teams_seasons <- players_teams_seasons %>% rename(player = player_shooting)
players_teams_seasons <- players_teams_seasons %>% rename(team = player_shooting_team)

first_choices <- unique(players_teams_seasons$season)
first_choices <- sort(first_choices, decreasing=FALSE)
second_choices <- unique(players_teams_seasons$team)
second_choices <- sort(second_choices, decreasing=FALSE)


first_choices_r <- unique(players_teams_seasons$season)
first_choices_r <- sort(first_choices, decreasing=FALSE)
second_choices_r <- unique(players_teams_seasons$team)
second_choices_r <- sort(second_choices, decreasing=FALSE)

ui <- fluidPage(theme = shinytheme("slate"), 
      
      tags$head( 
        tags$style(HTML('
    .zoomable-image { transition: transform 0.2s;
    }
    .zoomable-image:hover {
      transform: scale(1.25); /* Adjust the scale factor to control the zoom level */
      cursor: zoom-in;
    }
  '))
         ),
         
        tags$style(
           HTML("#image-container {
      display: flex;
      justify-content: center;
      align-items: center;
      height: 116vh;
    }")
         ),

fluidRow(
  column(
    h1("NBA True Shot Charts"),
    h4(a("Dr. Justin Ehrlich", href = "https://falk.syr.edu/people/ehrlich-justin/"), "and", 
       a("Dr. Shane Sanders", href = "https://falk.syr.edu/people/sandersshane/")),
    h4("Syracuse University"),
         align = "center",
         width = 12)
),
  
  fluidRow(
    column(
           width = 6,
           div(
             div(uiOutput("image1"), 
                 id = "image-container")
           ),
           align = "center",
           ),
    column(width = 6,
           div(
              div(uiOutput("image2"), 
                  id = "image-container")
           ),
           align = "center"
    )
  ),
  
  fluidRow(
    column(width = 6,
           align = "center",
           selectInput("first_filter", "Season", choices = c("Select Season", first_choices), selected = "Select Season"),
           selectInput("second_filter", "Team", choices = c("Select Team", second_choices), selected = "Select Team"),
           selectInput("chart_filter", "Shot Chart Type", choices = c("Hexbin", "GAM"), selected = "GAM"),

         conditionalPanel(
             condition = "input.chart_filter == 'GAM'",
             selectInput("stat_filter", "GAM Type", choices = NULL)
           ),
         
         
         conditionalPanel(
           condition = "input.chart_filter == 'Hexbin'",
           selectInput("third_filter", "Player", choices = NULL)
         ),
         
         conditionalPanel(
           condition = "input.stat_filter == 'True' | input.stat_filter == 'FGA Expected Points' | input.stat_filter == 'True Over Expected'",
           selectInput("third_filter2", "Player", choices = NULL)
         ),
          textOutput("selected_filters")
          
           
    ),
    column(width = 6,
           align = "center",
           selectInput("first_filter_r", "Season", choices = c("Select Season", first_choices_r), selected = "Select Season"),
           selectInput("second_filter_r", "Team", choices = c("Select Team", second_choices_r), selected = "Select Team"),
           selectInput("chart_filter_r", "Shot Chart Type", choices = c("Hexbin", "GAM"), selected = "GAM"),

    conditionalPanel(
      condition = "input.chart_filter_r == 'GAM'",
      selectInput("stat_filter_r", "GAM Type", choices = NULL)
    ),
    
    
    conditionalPanel(
      condition = "input.chart_filter_r == 'Hexbin'",
      selectInput("third_filter_r", "Player", choices = NULL)
    ),
    
    conditionalPanel(
      condition = "input.stat_filter_r == 'True' | input.stat_filter_r == 'FGA Expected Points' | input.stat_filter_r == 'True Over Expected'",
      selectInput("third_filter2_r", "Player", choices = NULL)
    ),
           textOutput("selected_filters_r")
    )
  ),
  
tags$br(),
  
  fluidRow(
    column(
      h5("Play-by-Play Data Provided by", a("Big Data Ball", href = "https://www.bigdataball.com/")),
      h5("Dashboard Developed by", a("Nicholas Kamimoto", href = "https://www.linkedin.com/in/nkamimoto/")),   
      align = "center",
      width = 12)
  ),

tags$br()

)

server <- function(input, output, session) {
  third_choices <- reactive({
    filter1 <- input$first_filter
    filter2 <- input$second_filter
    return(c("Select Player", players_teams_seasons %>% filter(team == filter2, season == filter1) %>% select(player) %>% arrange(player)))
   
  })
  
  observe({
    choices3 <- third_choices()
    updateSelectInput(session, "third_filter", choices = choices3)
  })
  
  observe({
    choices3 <- third_choices()
    updateSelectInput(session, "third_filter2", choices = choices3)
  })
  
   fifth_choices <- reactive({
     filter4 <- input$chart_filter
     if (filter4 == "GAM") {
       return(c("True", "Density Over League", "FGA Expected Points", 
                "Shot Selection Efficiency Over League", "Opponent True", "True Over Expected"))
     } else if (filter4 == "Hexbin") {
       return(c("Select Stat"))
     }
   })
  
   observe({
     choices5 <- fifth_choices()
     updateSelectInput(session, "stat_filter", choices = choices5)
   })

    output$selected_filters <- renderText({
      filter1 <- input$first_filter
      filter2 <- input$second_filter
      filter3 <- input$third_filter
      filter3.2 <- input$third_filter2
      filter4 <- input$chart_filter
      filter5 <- input$stat_filter
      
      if (filter1 != "Select Season" & filter2 != "Select Team" & filter3 != "Select Player" & filter4 == "Hexbin") {
        paste(filter2, filter3, filter1, filter4, "Shot Chart")
      } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3 == "Select Player" & filter4 == "Hexbin") {
        paste(filter1, filter4, "Shot Chart")
      } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3 == "Select Player" & filter4 == "Hexbin") {
        paste(filter2)
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3 == "Select Player" & filter4 == "Hexbin") {
        paste(filter2, filter1, filter4, "Shot Chart")
      
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 != "Select Player" & filter4 == "GAM" & filter5 == "True") {
        paste(filter2, filter3.2, filter1, filter4, filter5, "Shot Chart")
      } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True") {
        paste(filter1, filter4, filter5, "Shot Chart")
      } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True") {
        paste(filter2)
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True") {
        paste(filter2, filter1, filter4, filter5, "Shot Chart")
        
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter4 == "GAM" & filter5 == "Density Over League") {
        paste(filter2, filter1, filter4, filter5, "Shot Chart")
      } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter4 == "GAM" & filter5 == "Density Over League") {
        paste()
      } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter4 == "GAM" & filter5 == "Density Over League") {
        paste(filter2)
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter4 == "GAM" & filter5 == "Density Over League") {
        paste(filter2, filter1, filter4, filter5, "Shot Chart")  
        
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter4 == "GAM" & filter5 == "Shot Selection Efficiency Over League") {
        paste(filter2, filter1, filter4, filter5, "Shot Chart")
      } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter4 == "GAM" & filter5 == "Shot Selection Efficiency Over League") {
        paste()
      } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter4 == "GAM" & filter5 == "Shot Selection Efficiency Over League") {
        paste(filter2)
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter4 == "GAM" & filter5 == "Shot Selection Efficiency Over League") {
        paste(filter2, filter1, filter4, filter5, "Shot Chart")  
        
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter4 == "GAM" & filter5 == "Opponent True") {
        paste(filter2, filter1, filter4, filter5, "Shot Chart")
      } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter4 == "GAM" & filter5 == "Opponent True") {
        paste()
      } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter4 == "GAM" & filter5 == "Opponent True") {
        paste(filter2)
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter4 == "GAM" & filter5 == "Opponent True") {
        paste(filter2, filter1, filter4, filter5, "Shot Chart")  

      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 != "Select Player" & filter4 == "GAM" & filter5 == "True Over Expected") {
        paste(filter2, filter3.2, filter1, filter4, filter5, "Shot Chart")
      } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True Over Expected") {
        paste(filter1, filter4, filter5, "Shot Chart")
      } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True Over Expected") {
        paste(filter2)
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True Over Expected") {
        paste(filter2, filter1, filter4, filter5, "Shot Chart")
        
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 != "Select Player" & filter4 == "GAM" & filter5 == "FGA Expected Points") {
        paste(filter2, filter3.2, filter1, filter4, filter5, "Shot Chart")
      } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "FGA Expected Points") {
        paste(filter1, filter4, filter5, "Shot Chart")
      } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "FGA Expected Points") {
        paste(filter2)
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "FGA Expected Points") {
        paste(filter2, filter1, filter4, filter5, "Shot Chart")
      } 

    })
    
    img1 <- reactive({
    filter1 <- input$first_filter
    filter2 <- input$second_filter      
    filter3 <- input$third_filter
    filter3.2 <- input$third_filter2
    filter4 <- input$chart_filter
    filter5 <- input$stat_filter
    
    if (filter1 != "Select Season" & filter2 != "Select Team" & filter3 != "Select Player" & filter4 == "Hexbin") {
      paste0(filter3, "_", filter1, "_", filter2, "_side_by_side.png")
    } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3 == "Select Player" & filter4 == "Hexbin") {
      paste0(filter1, "_side_by_side.png")
    } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3 == "Select Player" & filter4 == "Hexbin") {
      paste0(filter2, "_side_by_side.png")
    } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3 == "Select Player" & filter4 == "Hexbin") {
      paste0(filter1, "_", filter2, "_side_by_side.png")
    } else if (filter1 == "Select Season" & filter2 == "Select Team" & filter3 == "Select Player" & filter4 == "Hexbin") {
      paste0("NBA_side_by_side.png")
    
    } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 != "Select Player" & filter4 == "GAM" & filter5 == "True") {
      filter1 <- as.numeric(filter1)
      paste0("true_", filter2, " ", filter1, "-", substr(filter1+1, start = 3, stop = nchar(filter1+1)), "_", filter3.2, "_Season.png")
    } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True") {
      filter1 <- as.numeric(filter1)
      paste0(filter1, "-", substr(filter1+1, start = 3, stop = nchar(filter1+1)), "_Season.png")
    } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True") {
      paste0(filter2, "_side_by_side.png")
    } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True") {
      filter1 <- as.numeric(filter1)
      paste0("true_", filter2, " ", filter1, "-", substr(filter1+1, start = 3, stop = nchar(filter1+1)), "_Season.png")
    } else if (filter1 == "Select Season" & filter2 == "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True") {
      paste0("NBA_side_by_side.png")
     
   } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Density Over League") {
     filter1 <- as.numeric(filter1)
     paste0("density_over_league_", filter2, " ", filter1, "-", substr(filter1+1, start = 3, stop = nchar(filter1+1)), "_", "Season.png")
   } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Density Over League") {
     filter1 <- as.numeric(filter1)
     paste0("NBA_side_by_side.png")
   } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Density Over League") {
     paste0(filter2, "_side_by_side.png")
   } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Density Over League") {
     filter1 <- as.numeric(filter1)
     paste0("density_over_league_", filter2, " ", filter1, "-", substr(filter1+1, start = 3, stop = nchar(filter1+1)), "_Season.png")
   } else if (filter1 == "Select Season" & filter2 == "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Density Over League") {
     paste0("NBA_side_by_side.png")
     
   } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Opponent True") {
     filter1 <- as.numeric(filter1)
     paste0("opponent_true_", filter2, " ", filter1, "-", substr(filter1+1, start = 3, stop = nchar(filter1+1)), "_", "Season.png")
   } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Opponent True") {
     filter1 <- as.numeric(filter1)
     paste0("NBA_side_by_side.png")
   } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Opponent True") {
     paste0(filter2, "_side_by_side.png")
   } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Opponent True") {
     filter1 <- as.numeric(filter1)
     paste0("opponent_true_", filter2, " ", filter1, "-", substr(filter1+1, start = 3, stop = nchar(filter1+1)), "_Season.png")
   } else if (filter1 == "Select Season" & filter2 == "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Opponent True") {
     paste0("NBA_side_by_side.png")
     
   } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter4 == "GAM" & filter5 == "Shot Selection Efficiency Over League") {
     filter1 <- as.numeric(filter1)
     paste0("shot_selection_efficiency_over_league_", filter2, " ", filter1, "-", substr(filter1+1, start = 3, stop = nchar(filter1+1)), "_", "Season.png")
   } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Shot Selection Efficiency Over League") {
     filter1 <- as.numeric(filter1)
     paste0("NBA_side_by_side.png")
   } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter4 == "GAM" & filter5 == "Shot Selection Efficiency Over League") {
     paste0(filter2, "_side_by_side.png")
   } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter4 == "GAM" & filter5 == "Shot Selection Efficiency Over League") {
     filter1 <- as.numeric(filter1)
     paste0("shot_selection_efficiency_over_league_", filter2, " ", filter1, "-", substr(filter1+1, start = 3, stop = nchar(filter1+1)), "_Season.png")
   } else if (filter1 == "Select Season" & filter2 == "Select Team" & filter4 == "GAM" & filter5 == "Shot Selection Efficiency Over League") {
     paste0("NBA_side_by_side.png")
      
   } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 != "Select Player" & filter4 == "GAM" & filter5 == "True Over Expected") {
     filter1 <- as.numeric(filter1)
     paste0("true_over_expected_", filter2, " ", filter1, "-", substr(filter1+1, start = 3, stop = nchar(filter1+1)), "_", filter3.2, "_Season.png")
   } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True Over Expected") {
     filter1 <- as.numeric(filter1)
     paste0("true_over_expected_", filter1, "-", substr(filter1+1, start = 3, stop = nchar(filter1+1)), "_Season.png")
   } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True Over Expected") {
     paste0(filter2, "_side_by_side.png")
   } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True Over Expected") {
     filter1 <- as.numeric(filter1)
     paste0("true_over_expected_", filter2, " ", filter1, "-", substr(filter1+1, start = 3, stop = nchar(filter1+1)), "_Season.png")
   } else if (filter1 == "Select Season" & filter2 == "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True Over Expected") {
     paste0("NBA_side_by_side.png")
     
   } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 != "Select Player" & filter4 == "GAM" & filter5 == "FGA Expected Points") {
     filter1 <- as.numeric(filter1)
     paste0("fga_expected_points_", filter2, " ", filter1, "-", substr(filter1+1, start = 3, stop = nchar(filter1+1)), "_", filter3.2, "_Season.png")
   } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "FGA Expected Points") {
     filter1 <- as.numeric(filter1)
     paste0("fga_expected_points_", filter1, "-", substr(filter1+1, start = 3, stop = nchar(filter1+1)), "_Season.png")
   } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "FGA Expected Points") {
     paste0(filter2, "_side_by_side.png")
   } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "FGA Expected Points") {
     filter1 <- as.numeric(filter1)
     paste0("fga_expected_points_", filter2, " ", filter1, "-", substr(filter1+1, start = 3, stop = nchar(filter1+1)), "_Season.png")
   } else if (filter1 == "Select Season" & filter2 == "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "FGA Expected Points") {
     paste0("NBA_side_by_side.png")
   } 
    
    })
    
    output$image1 <- renderUI({
      filter1 <- input$first_filter
      filter2 <- input$second_filter
      filter3 <- input$third_filter
      filter3.2 <- input$third_filter2
      filter4 <- input$chart_filter
      filter5 <- input$stat_filter
        
      if (filter1 != "Select Season" & filter2 != "Select Team" & filter3 != "Select Player" & filter4 == "Hexbin") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3 == "Select Player" & filter4 == "Hexbin") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3 == "Select Player" & filter4 == "Hexbin") {
        tags$img(src = img1(),  height = "50%", width = "50%")
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3 == "Select Player" & filter4 == "Hexbin") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter1 == "Select Season" & filter2 == "Select Team" & filter3 == "Select Player" & filter4 == "Hexbin") {
        tags$img(src = img1(),  height = "75%", width = "75%")
        
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 != "Select Player" & filter4 == "GAM" & filter5 == "True") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True") {
        tags$img(src = img1(),  height = "50%", width = "50%")
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter1 == "Select Season" & filter2 == "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True") {
        tags$img(src = img1(),  height = "75%", width = "75%")
        
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Density Over League") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Density Over League") {
        tags$img(src = img1(),  height = "75%", width = "75%")
      } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Density Over League") {
        tags$img(src = img1(),  height = "50%", width = "50%")
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Density Over League") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter1 == "Select Season" & filter2 == "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Density Over League") {
        tags$img(src = img1(),  height = "75%", width = "75%")
        
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Opponent True") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Opponent True") {
        tags$img(src = img1(),  height = "75%", width = "75%")
      } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Opponent True") {
        tags$img(src = img1(),  height = "50%", width = "50%")
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Opponent True") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter1 == "Select Season" & filter2 == "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Opponent True") {
        tags$img(src = img1(),  height = "75%", width = "75%")
        
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter4 == "GAM" & filter5 == "Shot Selection Efficiency Over League") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3 == "Select Player" & filter4 == "GAM" & filter5 == "Shot Selection Efficiency Over League") {
        tags$img(src = img1(),  height = "75%", width = "75%")
      } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter4 == "GAM" & filter5 == "Shot Selection Efficiency Over League") {
        tags$img(src = img1(),  height = "50%", width = "50%")
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter4 == "GAM" & filter5 == "Shot Selection Efficiency Over League") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter1 == "Select Season" & filter2 == "Select Team" & filter4 == "GAM" & filter5 == "Shot Selection Efficiency Over League") {
        tags$img(src = img1(),  height = "75%", width = "75%")
        
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 != "Select Player" & filter4 == "GAM" & filter5 == "True Over Expected") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True Over Expected") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True Over Expected") {
        tags$img(src = img1(),  height = "50%", width = "50%")
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True Over Expected") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter1 == "Select Season" & filter2 == "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "True Over Expected") {
        tags$img(src = img1(),  height = "75%", width = "75%")

      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 != "Select Player" & filter4 == "GAM" & filter5 == "FGA Expected Points") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter1 != "Select Season" & filter2 == "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "FGA Expected Points") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter2 != "Select Team" & filter1 == "Select Season" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "FGA Expected Points") {
        tags$img(src = img1(),  height = "50%", width = "50%")
      } else if (filter1 != "Select Season" & filter2 != "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "FGA Expected Points") {
        tags$img(src = img1(),  height = "100%", width = "100%", class = "zoomable-image")
      } else if (filter1 == "Select Season" & filter2 == "Select Team" & filter3.2 == "Select Player" & filter4 == "GAM" & filter5 == "FGA Expected Points") {
        tags$img(src = img1(),  height = "75%", width = "75%")  
      } 
      
    })
  
#Right Side Fliter
    
  third_choices_r <- reactive({
    filter1_r <- input$first_filter_r
    filter2_r <- input$second_filter_r
    return(c("Select Player", players_teams_seasons %>% filter(team == filter2_r, season == filter1_r) %>% select(player) %>% arrange(player)))
    
  })
  
  observe({
    choices3_r <- third_choices_r()
    updateSelectInput(session, "third_filter_r", choices = choices3_r)
  })
  
  observe({
    choices3_r <- third_choices_r()
    updateSelectInput(session, "third_filter2_r", choices = choices3_r)
  })
  
  fifth_choices_r <- reactive({
    filter4_r <- input$chart_filter_r
    if (filter4_r == "GAM") {
      return(c("True", "Density Over League", "FGA Expected Points", 
               "Shot Selection Efficiency Over League", "Opponent True", "True Over Expected"))
    } else if (filter4_r == "Hexbin") {
      return(c("Select Stat"))
    }
  })
  
  observe({
    choices5_r <- fifth_choices_r()
    updateSelectInput(session, "stat_filter_r", choices = choices5_r)
  })
  
  output$selected_filters_r <- renderText({
    filter1_r <- input$first_filter_r
    filter2_r <- input$second_filter_r
    filter3_r <- input$third_filter_r
    filter3.2_r <- input$third_filter2_r
    filter4_r <- input$chart_filter_r
    filter5_r <- input$stat_filter_r
    
    if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r != "Select Player" & filter4_r == "Hexbin") {
      paste(filter2_r, filter3_r, filter1_r, filter4_r, "Shot Chart")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "Hexbin") {
        paste(filter1_r, filter4_r, "Shot Chart")
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3_r == "Select Player" & filter4_r == "Hexbin") {
        paste(filter2_r)
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "Hexbin") {
        paste(filter2_r, filter1_r, filter4_r, "Shot Chart")
    
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r != "Select Player" & filter4_r == "GAM" & filter5_r == "True") {
      paste(filter2_r, filter3.2_r, filter1_r, filter4_r, "Shot Chart")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True") {
      paste(filter1_r, filter4_r, "Shot Chart")
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True") {
      paste(filter2_r)
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True") {
      paste(filter2_r, filter1_r, filter4_r, "Shot Chart")
      
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter4_r == "GAM" & filter5_r == "Density Over League") {
      paste(filter2_r, filter1_r, filter4_r, filter5_r, "Shot Chart")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter4_r == "GAM" & filter5_r == "Density Over League") {
      paste()
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter4_r == "GAM" & filter5_r == "Density Over League") {
      paste(filter2_r)
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter4_r == "GAM" & filter5_r == "Density Over League") {
      paste(filter2_r, filter1_r, filter4_r, filter5_r, "Shot Chart")  
      
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter4_r == "GAM" & filter5_r == "Shot Selection Efficiency Over League") {
      paste(filter2_r, filter1_r, filter4_r, filter5_r, "Shot Chart")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter4_r == "GAM" & filter5_r == "Shot Selection Efficiency Over League") {
      paste()
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter4_r == "GAM" & filter5_r == "Shot Selection Efficiency Over League") {
      paste(filter2_r)
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter4_r == "GAM" & filter5_r == "Shot Selection Efficiency Over League") {
      paste(filter2_r, filter1_r, filter4_r, filter5_r, "Shot Chart")  
        
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter4_r == "GAM" & filter5_r == "Opponent True") {
      paste(filter2_r, filter1_r, filter4_r, filter5_r, "Shot Chart")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter4_r == "GAM" & filter5_r == "Opponent True") {
      paste()
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter4_r == "GAM" & filter5_r == "Opponent True") {
      paste(filter2_r)
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter4_r == "GAM" & filter5_r == "Opponent True") {
      paste(filter2_r, filter1_r, filter4_r, filter5_r, "Shot Chart")  
      
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r != "Select Player" & filter4_r == "GAM" & filter5_r == "True Over Expected") {
      paste(filter2_r, filter3.2_r, filter1_r, filter4_r, filter5_r, "Shot Chart")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True Over Expected") {
      paste(filter1_r, filter4_r, filter5_r, "Shot Chart")
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True Over Expected") {
      paste(filter2_r)
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True Over Expected") {
      paste(filter2_r, filter1_r, filter4_r, filter5_r, "Shot Chart")
      
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r != "Select Player" & filter4_r == "GAM" & filter5_r == "FGA Expected Points") {
      paste(filter2_r, filter3.2_r, filter1_r, filter4_r, filter5_r, "Shot Chart")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "FGA Expected Points") {
      paste(filter1_r, filter4_r, filter5_r, "Shot Chart")
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "FGA Expected Points") {
      paste(filter2_r)
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "FGA Expected Points") {
      paste(filter2_r, filter1_r, filter4_r, filter5_r, "Shot Chart")
    } 
 
  })
  
    
  img2 <- reactive({
    filter1_r <- input$first_filter_r
    filter2_r <- input$second_filter_r
    filter3_r <- input$third_filter_r
    filter3.2_r <- input$third_filter2_r
    filter4_r <- input$chart_filter_r
    filter5_r <- input$stat_filter_r

    if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r != "Select Player" & filter4_r == "Hexbin") {
      paste0(filter3_r, "_", filter1_r, "_", filter2_r, "_side_by_side.png")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "Hexbin") {
      paste0(filter1_r, "_side_by_side.png")
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3_r == "Select Player" & filter4_r == "Hexbin") {
      paste0(filter2_r, "_side_by_side.png")
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "Hexbin") {
      paste0(filter1_r, "_", filter2_r, "_side_by_side.png")
    } else if (filter1_r == "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "Hexbin") {
      paste0("NBA_side_by_side.png")
      
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r != "Select Player" & filter4_r == "GAM" & filter5_r == "True") {
      filter1_r <- as.numeric(filter1_r)
      paste0("true_", filter2_r, " ", filter1_r, "-", substr(filter1_r+1, start = 3, stop = nchar(filter1_r+1)), "_", filter3.2_r, "_Season.png")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True") {
      filter1_r <- as.numeric(filter1_r)
      paste0(filter1_r, "-", substr(filter1_r+1, start = 3, stop = nchar(filter1_r+1)), "_Season.png")
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True") {
      paste0(filter2_r, "_side_by_side.png")
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True") {
      filter1_r <- as.numeric(filter1_r)
      paste0("true_", filter2_r, " ", filter1_r, "-", substr(filter1_r+1, start = 3, stop = nchar(filter1_r+1)), "_Season.png")
    } else if (filter1_r == "Select Season" & filter2_r == "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True") {
      paste0("NBA_side_by_side.png")
  
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Density Over League") {
      filter1_r <- as.numeric(filter1_r)
      paste0("density_over_league_", filter2_r, " ", filter1_r, "-", substr(filter1_r+1, start = 3, stop = nchar(filter1_r+1)), "_", "Season.png")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Density Over League") {
      filter1_r <- as.numeric(filter1_r)
      paste0("NBA_side_by_side.png")
    } else if (filter1_r == "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Density Over League") {
      paste0(filter2_r, "_side_by_side.png")
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Density Over League") {
      filter1_r <- as.numeric(filter1_r)
      paste0("density_over_league_", filter2_r, " ", filter1_r, "-", substr(filter1_r+1, start = 3, stop = nchar(filter1_r+1)), "_Season.png")
    } else if (filter1_r == "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Density Over League") {
      paste0("NBA_side_by_side.png")
      
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Opponent True") {
      filter1_r <- as.numeric(filter1_r)
      paste0("opponent_true_", filter2_r, " ", filter1_r, "-", substr(filter1_r+1, start = 3, stop = nchar(filter1_r+1)), "_", "Season.png")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Opponent True") {
      filter1_r <- as.numeric(filter1_r)
      paste0("NBA_side_by_side.png")
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Opponent True") {
      paste0(filter2_r, "_side_by_side.png")
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Opponent True") {
      filter1_r <- as.numeric(filter1_r)
      paste0("opponent_true_", filter2_r, " ", filter1_r, "-", substr(filter1_r+1, start = 3, stop = nchar(filter1_r+1)), "_Season.png")
    } else if (filter1_r == "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Opponent True") {
      paste0("NBA_side_by_side.png")
      
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Shot Selection Efficiency Over League") {
      filter1_r <- as.numeric(filter1_r)
      paste0("shot_selection_efficiency_over_league_", filter2_r, " ", filter1_r, "-", substr(filter1_r+1, start = 3, stop = nchar(filter1_r+1)), "_", "Season.png")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Shot Selection Efficiency Over League") {
      filter1_r <- as.numeric(filter1_r)
      paste0("NBA_side_by_side.png")
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Shot Selection Efficiency Over League") {
      paste0(filter2_r, "_side_by_side.png")
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Shot Selection Efficiency Over League") {
      filter1_r <- as.numeric(filter1_r)
      paste0("shot_selection_efficiency_over_league_", filter2_r, " ", filter1_r, "-", substr(filter1_r+1, start = 3, stop = nchar(filter1_r+1)), "_Season.png")
    } else if (filter1_r == "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Shot Selection Efficiency Over League") {
      paste0("NBA_side_by_side.png")
      
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r != "Select Player" & filter4_r == "GAM" & filter5_r == "True Over Expected") {
      filter1_r <- as.numeric(filter1_r)
      paste0("true_over_expected_", filter2_r, " ", filter1_r, "-", substr(filter1_r+1, start = 3, stop = nchar(filter1_r+1)), "_", filter3.2_r, "_Season.png")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True Over Expected") {
      filter1_r <- as.numeric(filter1_r)
      paste0("true_over_expected_", filter1_r, "-", substr(filter1_r+1, start = 3, stop = nchar(filter1_r+1)), "_Season.png")
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True Over Expected") {
      paste0(filter2_r, "_side_by_side.png")
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True Over Expected") {
      filter1_r <- as.numeric(filter1_r)
      paste0("true_over_expected_", filter2_r, " ", filter1_r, "-", substr(filter1_r+1, start = 3, stop = nchar(filter1_r+1)), "_Season.png")
    } else if (filter1_r == "Select Season" & filter2_r == "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True Over Expected") {
      paste0("NBA_side_by_side.png")
      
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r != "Select Player" & filter4_r == "GAM" & filter5_r == "FGA Expected Points") {
      filter1_r <- as.numeric(filter1_r)
      paste0("fga_expected_points_", filter2_r, " ", filter1_r, "-", substr(filter1_r+1, start = 3, stop = nchar(filter1_r+1)), "_", filter3.2_r, "_Season.png")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "FGA Expected Points") {
      filter1_r <- as.numeric(filter1_r)
      paste0("fga_expected_points_", filter1_r, "-", substr(filter1_r+1, start = 3, stop = nchar(filter1_r+1)), "_Season.png")
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "FGA Expected Points") {
      paste0(filter2_r, "_side_by_side.png")
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "FGA Expected Points") {
      filter1_r <- as.numeric(filter1_r)
      paste0("fga_expected_points_", filter2_r, " ", filter1_r, "-", substr(filter1_r+1, start = 3, stop = nchar(filter1_r+1)), "_Season.png")
    } else if (filter1_r == "Select Season" & filter2_r == "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "FGA Expected Points") {
      paste0("NBA_side_by_side.png")
    }    
    
  })
  
  output$image2 <- renderUI({
    filter1_r <- input$first_filter_r
    filter2_r <- input$second_filter_r
    filter3_r <- input$third_filter_r
    filter3.2_r <- input$third_filter2_r
    filter4_r <- input$chart_filter_r
    filter5_r <- input$stat_filter_r
   
    if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r != "Select Player" & filter4_r == "Hexbin") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "Hexbin") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3_r == "Select Player" & filter4_r == "Hexbin") {
      tags$img(src = img2(),  height = "50%", width = "50%")
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "Hexbin") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter1_r == "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "Hexbin") {
      tags$img(src = img2(),  height = "75%", width = "75%")
      
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r != "Select Player" & filter4_r == "GAM" & filter5_r == "True") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True") {
      tags$img(src = img2(),  height = "50%", width = "50%")
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter1_r == "Select Season" & filter2_r == "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True") {
      tags$img(src = img2(),  height = "75%", width = "75%") 
      
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Density Over League") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Density Over League") {
      tags$img(src = img2(),  height = "75%", width = "75%") 
    } else if (filter1_r == "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Density Over League") {
      tags$img(src = img2(),  height = "50%", width = "50%")
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Density Over League") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter1_r == "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Density Over League") {
      tags$img(src = img2(),  height = "75%", width = "75%") 
      
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Opponent True") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Opponent True") {
      tags$img(src = img2(),  height = "75%", width = "75%") 
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Opponent True") {
      tags$img(src = img2(),  height = "50%", width = "50%")
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Opponent True") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter1_r == "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Opponent True") {
      tags$img(src = img2(),  height = "75%", width = "75%") 
      
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Shot Selection Efficiency Over League") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Shot Selection Efficiency Over League") {
      tags$img(src = img2(),  height = "75%", width = "75%") 
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Shot Selection Efficiency Over League") {
      tags$img(src = img2(),  height = "50%", width = "50%")
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Shot Selection Efficiency Over League") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter1_r == "Select Season" & filter2_r == "Select Team" & filter3_r == "Select Player" & filter4_r == "GAM" & filter5_r == "Shot Selection Efficiency Over League") {
      tags$img(src = img2(),  height = "75%", width = "75%") 
      
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r != "Select Player" & filter4_r == "GAM" & filter5_r == "True Over Expected") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True Over Expected") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True Over Expected") {
      tags$img(src = img2(),  height = "50%", width = "50%")
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True Over Expected") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter1_r == "Select Season" & filter2_r == "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "True Over Expected") {
      tags$img(src = img2(),  height = "75%", width = "75%") 

    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r != "Select Player" & filter4_r == "GAM" & filter5_r == "FGA Expected Points") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter1_r != "Select Season" & filter2_r == "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "FGA Expected Points") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter2_r != "Select Team" & filter1_r == "Select Season" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "FGA Expected Points") {
      tags$img(src = img2(),  height = "50%", width = "50%")
    } else if (filter1_r != "Select Season" & filter2_r != "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "FGA Expected Points") {
      tags$img(src = img2(),  height = "100%", width = "100%", class = "zoomable-image")
    } else if (filter1_r == "Select Season" & filter2_r == "Select Team" & filter3.2_r == "Select Player" & filter4_r == "GAM" & filter5_r == "FGA Expected Points") {
      tags$img(src = img2(),  height = "75%", width = "75%")
    }
    
  })
  
}

shinyApp(ui, server)




