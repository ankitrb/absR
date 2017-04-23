ui = fluidPage(
  
  #Using shiny theme
  theme = shinytheme("darkly"),
  headerPanel(h3("Twitter Sentiment Analysis: Team abs(R)"), windowTitle = "abs(R)"),
  
  #Sidebar panel
  sidebarLayout(
    sidebarPanel(htmlOutput('uconn'),
                 radioButtons("typeInput", "Extract tweets by: ",
                              list("Hashtag & Location" = "hashtag", "Twitter Username"= "username")),
                 sliderInput("numberInput", "Select number of tweets",
                             min = 0, max = 3000, value = 100),
                 
                 #Only show this panel if input type is "Hashtag & Location"    
                 conditionalPanel(
                   condition = "input.typeInput == 'hashtag'",
                   textInput("hashtagInput", "Enter search string","", placeholder = "search string"),
                   textInput("zipInput", "Enter ZIP Code (from 00210 to 99950)", placeholder = "06105"),
                   textInput("radiusInput", "Enter radius (miles)", placeholder = "100")),
                 
                 #Only show this panel if Input type is "Twitter Username"
                 conditionalPanel(
                   condition = "input.typeInput == 'username'",
                   textInput("usernameInput", "Username", placeholder = "AnkitRB")),
                 actionButton("goButton", "Search", icon("twitter"),
                              style="color: #fff; background-color: #337ab7") ,width = 2),
    
    #Panel to display output
    mainPanel(
      
      #Dividing the main panel into multiple tabs
      tabsetPanel(
        tabPanel("Sentiment Plots TM", plotOutput("plot1")),
        tabPanel("Sentiment Plots TFIDF", plotOutput("plot3")),
        tabPanel("Polarity Plots TM", plotOutput("plot2")),
        tabPanel("Polarity Plots TFIDF", plotOutput("plot4")),
        navbarMenu("Word Clouds TM",
                   tabPanel("Positive", wordcloud2Output("wordCloud1",width = "100%", height = "400px")),
                   tabPanel("Negative", wordcloud2Output("wordCloud2", width = "100%", height = "400px"))),
        navbarMenu("Word Clouds TFIDF",
                   tabPanel("Positive", wordcloud2Output("wordCloud3", width = "100%", height = "400px")),
                   tabPanel("Negative", wordcloud2Output("wordCloud4", width = "100%", height = "400px"))),
        tabPanel("Tweets", dataTableOutput("tweetTable"))
        ,type = "pills"), width = 10)
  )
)
