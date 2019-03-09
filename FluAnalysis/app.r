library(shiny)

ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("FLU ANALYSIS"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    selectInput("keyword","Please choose one of the following keywords",
                choices = c("flu","#flu","influenza","all"))
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    img(src="latest_heatmap.png", align = "right",height=400,width=550),
    # img(src="latest_tweetHashFlu.png", align = "right",height=400,width=550),
    #img(src="latest_tweetsFlu.png", align = "right",height=400,width=550),
    #img(src="latest_tweetInfluenza.png", align = "right",height=400,width=550)
    uiOutput("img1")
  )
)


server <- function(input, output) {
  
  output$img1 <- renderUI(
    
    {
      if(input$keyword == "flu"){            
        
        img(height = 400, width = 550, src = "latest_tweetsFlu.png", align = "right")
      }                                        
      else if(input$keyword == "#flu"){
        img(height = 400, width = 550, src = "latest_tweetHashFlu.png", align = "right")
      }
      else if(input$keyword == "influenza")
      {
        img(height = 400, width = 550, src = "latest_tweetInfluenza.png", align = "right")
      }
      else if(input$keyword == "all")
      {
        img(height = 400, width = 550, src = "latest_totalTweets.png", align = "right")
      }
    })
  
}

shinyApp(ui, server)
