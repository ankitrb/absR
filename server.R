
server = function(input, output)
{
  
  #Reactive input
  data1 = eventReactive(input$goButton, {
    
    if (input$typeInput == "hashtag") 
    {
      
      geocode.string = getLatLong.zip(enter.zipcode = input$zipInput,radius.mi = input$radiusInput)
      
      tweetOutput = searchThis(search_string = input$hashtagInput,
                                 number.of.tweets = input$numberInput, geocode_string = geocode.string)
      
    } 
    
    else if (input$typeInput == "username") 
    {
      tweetOutput <- userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    #Cleans the tweet
    df.tweets = cleanTweets(tweetOutput)
    #Get sentiments
    nrc.lexicons = get_nrc_sentiment(df.tweets$text_clean)
    })
  
  #Render TM plots
  output$plot1 = renderPlot({
    
      # Barplot for emotions
      barplot(
        sort(colSums(prop.table(data1()[, 1:8]))), 
        horiz = TRUE, 
        cex.names = 0.8, 
        las = 1, 
        main = "Emotions in tweets", xlab="Percentage", xlim = c(0,.4))}, 
          width = 700, height = 500)
  
  output$plot2 = renderPlot({
    
      # Barplot for positive vs negative
      barplot(
        sort(colSums(prop.table(data1()[, 9:10]))), 
        horiz = TRUE, 
        cex.names = 0.75, 
        las = 1, 
        main = "Ratio of positive to negative tweets",xlab="Percentage", xlim = c(0,1))},
          width = 700, height = 500)
  
  
  #Reactive input
  data2 = eventReactive(input$goButton, {
    
    if (input$typeInput == "hashtag") 
    {
      
      #Generate geocode string
      geocode.string = getLatLong.zip(enter.zipcode = input$zipInput,radius.mi = input$radiusInput)
      tweetOutput = searchThis(search_string = input$hashtagInput,
                                 number.of.tweets = input$numberInput, geocode_string = geocode.string)
      
    } 
    
    else if (input$typeInput == "username") 
    {
      tweetOutput = userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    searchtweet.clean = cleanTweets(tweetOutput)
    
    searchtweet.tdm.tfidf = tdm.TFIDF(searchtweet.clean)
    
    nrc.lex = getSentiments.TF_IDF.nrc(searchtweet.tdm.tfidf)
    
  })
  
  #Render TFIDF plots
  output$plot3 = renderPlot({
    barplot(
      sort(colSums(prop.table(data2()[, 1:8]))), 
      horiz = TRUE, 
      cex.names = 0.75, 
      las = 1, 
      main = "Emotions in tweets", xlab="Percentage",xlim = c(0,.4))}, width = 700, height = 500)
  
  output$plot4 = renderPlot({
    barplot(
      sort(colSums(prop.table(data2()[, 9:10]))), 
      horiz = TRUE, 
      cex.names = 0.8,
      las = 1, 
      main = "Polarity in tweets", xlab="Percentage", xlim = c(0,1))}, width = 700, height = 500)
  
  #Reactive input
  data3 = eventReactive(input$goButton, {
    
    if (input$typeInput == "hashtag") 
    {
      
      geocode.string = getLatLong.zip(enter.zipcode = input$zipInput,radius.mi = input$radiusInput)
      
      tweetOutput = searchThis(search_string = input$hashtagInput,
                                 number.of.tweets = input$numberInput, geocode_string = geocode.string)
      
    } 
    
    else if (input$typeInput == "username") 
    {
      userTL = function(user.name,number.of.tweets = 100)
      {
        userTimeline(user.name,n = number.of.tweets)
      }
      
      tweetOutput = userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    
    df.tweets <- cleanTweets(tweetOutput)
  
    searchtweet.tdm.tm.stopword = tdm.tmStopWord(df.tweets)
    
    tweets.positive = generateWordCloud.positive.tmStopWords(searchtweet.tdm.tm.stopword)
    
  })

  #Render Positive Wordcloud TM
  output$wordCloud1 = renderWordcloud2({wordcloud2(data = data3())})
  
  #Reactive input
  data4 = eventReactive(input$goButton, {
    
    if (input$typeInput == "hashtag") 
    {
      
      geocode.string = getLatLong.zip(enter.zipcode = input$zipInput,radius.mi = input$radiusInput)
      
      tweetOutput = searchThis(search_string = input$hashtagInput,
                                 number.of.tweets = input$numberInput, geocode_string = geocode.string)
      
    } 
    
    else if (input$typeInput == "username") 
    {
      userTL = function(user.name,number.of.tweets = 100)
      {
        userTimeline(user.name,n = number.of.tweets)
      }
      
      tweetOutput <- userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    
    df.tweets = cleanTweets(tweetOutput)
    
    searchtweet.tdm.tm.stopword = tdm.tmStopWord(df.tweets)
    
    tweets.negative = generateWordCloud.negative.tmStopWords(searchtweet.tdm.tm.stopword)
    
  })
  
  #Render negative wordcloud TM
  output$wordCloud2 = renderWordcloud2({wordcloud2(data = data4())})
  
  #Reactive input
  data5 = eventReactive(input$goButton, {
    
    if (input$typeInput == "hashtag") 
    {
      
      geocode.string = getLatLong.zip(enter.zipcode = input$zipInput,radius.mi = input$radiusInput)
      
      tweetOutput = searchThis(search_string = input$hashtagInput,
                                 number.of.tweets = input$numberInput, geocode_string = geocode.string)
      
    } 
    
    else if (input$typeInput == "username") 
    {
      userTL = function(user.name,number.of.tweets = 100)
      {
        userTimeline(user.name,n = number.of.tweets)
      }
      
      tweetOutput = userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    
    df.tweets = cleanTweets(tweetOutput)
    
    tdm.tfidf = tdm.TFIDF(df.tweets)
    
    tdm.tm.nostop = tdm.tm(df.tweets)
    
    tweets.positive = generateWordCloud.positive.TF_IDF(tdm.tfidf, tdm.tm.nostop)
    
  })
  
  #Render positive wordcloud TFIDF
  output$wordCloud3 = renderWordcloud2({wordcloud2(data = data5())})
  
  #Reactive input
  data6 = eventReactive(input$goButton, {
    
    if (input$typeInput == "hashtag") 
    {
      
      geocode.string = getLatLong.zip(enter.zipcode = input$zipInput,radius.mi = input$radiusInput)
      
      tweetOutput = searchThis(search_string = input$hashtagInput,
                                 number.of.tweets = input$numberInput, geocode_string = geocode.string)
      
    } 
    
    else if (input$typeInput == "username") 
    {
      userTL = function(user.name,number.of.tweets = 100)
      {
        userTimeline(user.name,n = number.of.tweets)
      }
      
      tweetOutput = userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    
    df.tweets = cleanTweets(tweetOutput)
    
    tdm.tfidf = tdm.TFIDF(df.tweets)
    
    tdm.tm.nostop = tdm.tm(df.tweets)
    
    tweets.negative = generateWordCloud.negative.TF_IDF(tdm.tfidf, tdm.tm.nostop)
    
  })
  
  #Render negative wordcloud TFIDF
  output$wordCloud4 = renderWordcloud2({wordcloud2(data = data6())})
  
  #Reactive input
  data7 = eventReactive(input$goButton, {
    
    if (input$typeInput == "hashtag") 
    {
      
      geocode.string = getLatLong.zip(enter.zipcode = input$zipInput,radius.mi = input$radiusInput)
      
      tweetOutput = searchThis(search_string = input$hashtagInput,
                                 number.of.tweets = input$numberInput, geocode_string = geocode.string)
      
    } 
    
    else if (input$typeInput == "username") 
    {
      tweetOutput = userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    #Converting the Tweets into data frame
    df.tweets = twListToDF(tweetOutput)
    
    #only displaying Text, Created, Screen Name, RT count, and Location
    
    # Remove all nongraphical characters
    text = str_replace_all(df.tweets$text,"[^[:graph:]]", " ")
    df.tweets = cbind(text, df.tweets[c(5,11,3,12,17)])
  
  })
  
  #Render tweets
  output$tweetTable = renderDataTable({data7()}, options = list(lengthMenu = c(10, 30, 50), pageLength = 5))
 
  #Render UConn image
  #output$uconn = renderPrint({
   # src = "https://cdn3.vox-cdn.com/uploads/blog/sbnu_logo_minimal/270/large_theuconnblog.com.minimal.png"
    #cat(sprintf('<img src=%s></img>', src))})
  
}


