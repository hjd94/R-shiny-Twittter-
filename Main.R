
#
library(tidyverse)
library(rtweet)
library(reshape2)
library(wordcloud2)
library(vader)
library(igraph)
library(ggraph)
library(shiny)
library(reactable)
library(tidytext)
library(lubridate)
library(plotly)
library(twitterwidget)
library(shinydashboard)
library(widyr)

#---------------------------------------------------------------------------------------
#import stops wors and add some custom stopwords

stop_words <- stop_words %>%
  rbind(tibble(word = c('t.co', 'http', 'https', "amp", "ha"),
               lexicon = c('url', 'url', 'url', "url", "url")))

#---------------------------------------------------------------------------------------
#filter function:

filter_words <- function(dataset){
  dataset <- dataset %>%
    anti_join(stop_words) %>%
    filter(!grepl('[0-9]', word)) %>%
    na.omit()
  return(dataset)
}

#---------------------------------------------------------------------------------------
#function to allow Wordclouds and Plotly graphs to show in the same page 

wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}

#---------------------------------------------------------------------------------------

wordcloud_data_pro <- function(data, type){
  data %>%
    filter(sentiment == type) %>%
    slice_max(tf_idf ,  n = 20) %>%
    rename(freq = tf_idf) %>%
    select(word, freq) %>%
    wordcloud2a(shape = "ciricle", size=0.5, color='random-dark')
}



#---------------------------------------------------------------------------------------

sentiment_tdf <- function(dataframe, dataset){
  data <- dataframe %>%
    ungroup() %>%
    filter(!sentiment == "Neutral") %>%
    select(sentiment, status_id) %>%
    left_join(dataset[, c("status_id", "text")]) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    group_by(sentiment, word) %>%
    count(word) %>%
    mutate(word = str_extract(word, "[a-z']+")) %>%
    na.omit() %>%
    bind_tf_idf(word, sentiment, n) %>%
    ungroup()
  
  return(data)
}

#---------------------------------------------------------------------------------------

data <- tweets

#---------------------------------------------------------------------------------------

ui <- fluidPage(
    titlePanel("Hayden", windowTitle = "hayden"),
    
    navlistPanel("Options:",
                 tabPanel("Dashboard",
    fluidRow(
        column(width = 3, selectInput("mp", "Select a MP",
                                      choices = c("a", "b", "c", "d"))  ,
               offset = 3), 
        column(width = 2,
               actionButton("activate", "Analysis", class = "btn-success"), 
               offset = 1, align = "center",
               style = "margin-bottom: 20px;",
               style = "margin-top: 25px;")
        
    ),
    
    fluidRow(
        
        column(width = 2, valueBoxOutput("followers",
                                   width = 1.5), style = "color:white;" ,
               style = "background-color:red;" ,
               align = 'center',
              offset = 0.5),
        column(width = 2, valueBoxOutput("following",
                                         width =1.5), style = "color:white;" ,
               style = "background-color:blue;" ,
               align = 'center',
               offset = 1),
        
        column(width = 2, valueBoxOutput("likes",
                                         width = 1.5), style = "color:white;" ,
               style = "background-color:yellow;" ,
               align = 'center',
               offset = 1),
        column(width = 2, valueBoxOutput("numre",
                                         width = 1.5), style = "color:white;" ,
               style = "background-color:purple;" ,
               align = 'center',
               offset = 1)
    ),
    fluidRow(
        column(width = 11, plotlyOutput("time_graph"),
               offset = 0)
    ),
    
    
    fluidRow(
        column(width = 5,
               twitterwidgetOutput("favtweet"), 
               offset = 1,
               align = "center"),
        column(width = 5, 
               twitterwidgetOutput("retweet"),
               align = "center")
    )),
    
    tabPanel("Frequency", 
             fluidRow(
                 column(width = 12, wordcloud2Output("freqwc"),
                        tags$script(HTML(
                            "$(document).on('click', '#canvas', function() {",
                            'word = document.getElementById("wcSpan").innerHTML;',
                            "Shiny.onInputChange('selected_word', word);",
                            "});"))
                        )
             ),
             fluidRow(
                 column(width = 8, plotlyOutput("try")),
                 column(width = 4, reactableOutput("seltweets"),
                        align = "center"))
             
             ),
    
    tabPanel("Hashtags & emojis",
      fluidRow(
        column(width = 6, reactableOutput("emojis")),
        column(width = 6, reactableOutput("hashtags"))
      )
    ),
    
    "Sentiment Analysis",
    
    tabPanel("Lexicons",
             
    fluidRow(
        column(width = 2, 
               selectInput("lexicon", "Select Lexicon:",
                           c("afinn", "bing"),
                           "afinn"),
               offset = 5, align = "center")
    ),
    fluidRow(
        column(width = 7,
               plotlyOutput("scoregraph")
        ),
        column(width = 4, plotlyOutput("ratinggraph"),
               offset = 0)
        
    ),
    
    fluidRow(
        column(width = 6,
               wordcloud2Output("poswordcloud")
               ),
        column(width = 6, wordcloud2Output("negwordcloud"),
               offset = 0)
        
    )),
    
    
    tabPanel("Vader", 
               fluidRow(
                 column(width = 11, plotlyOutput('varderbar')
                 )
               ),
             fluidRow(
               column(width = 6,
                      wordcloud2Output("vposwordcloud")
               ),
               column(width = 6, wordcloud2Output("vnegwordcloud"),
                      offset = 0)
             )),
    
    
    tabPanel("Ngrams",
            fluidRow(column(width = 12,
                    plotOutput("node"))),
            fluidRow(
              column(width = 12, plotOutput("word_cor"))
            )
    ), "-----",
    tabPanel("Options"), 
    widths = c(2, 10)),
    
    

)
#---------------------------------------------------------------------------------------

server <- function(input, output) {
    
    dataset <- reactive({
        data %>%
            select(status_id, text, created_at, hashtags, favorite_count, retweet_count,  followers_count, friends_count)
    })
    
    word_lost <- reactive({
        dataset() %>%
            select(status_id, text) %>%
            unnest_tokens(word, text)
        
    })
    
    filter_word_list <- reactive({
      word_lost() %>%
        anti_join(stop_words) %>%
        filter(!grepl('[0-9]', word)) %>%
        na.omit()
    })
    
    bigram_list <- reactive({
        dataset() %>%
        select(status_id, text) %>%
        unnest_tokens(word, text, token = "ngrams", n = 2) %>%
        separate(word , c('word1', 'word2'), sep = " ") %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word)
        
    })
    
    
    vader_score <- reactive({ dataset() %>%
            select(text, created_at) %>%
            mutate(score = vader::vader_df(text,
                                   incl_nt = T, 
                                   neu_set = T, 
                                   rm_qm = T)$compound,
           sequence = row_number()) 
    })
    
    output$tdidf <- renderReactable(reactable({
        word_lost() %>%
            group_by(word) %>%
            count(word) %>%
            anti_join(stop_words) %>%
            ungroup() %>%
            slice_max(order_by = n, n=10)

            
    }))
    
    output$time_graph <- renderPlotly(
        time_graph <- dataset() %>%
            mutate(date = as.Date(created_at)) %>%
            plot_ly(x= ~date, type = "histogram")
    )
    

    
    output$varderbar <- renderPlotly({
        vader_score() %>%
            plot_ly(x= ~created_at, y= ~score, type = "bar")
    })

    output$node <- renderPlot({
        a <- grid::arrow(type = "closed", length = unit(.15, 'inches'))
        
        bigram_list() %>%
            count(word1, word2) %>%
            filter(n >= 4) %>%
            graph_from_data_frame() %>%
            ggraph(layout = "fr") +
            geom_edge_link(aes(edge_alpha = n), show.legend = F,
                           arrow = a, end_cap = circle(.07,"inches")) +
            geom_node_point(colour = "lightblue", size = 5) +
            geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
            theme_void()
    })
    
    output$favtweet <- renderTwitterwidget({
        fav <- dataset() %>%
            select(status_id, favorite_count) %>%
            slice_max(order_by = favorite_count, n=1)
            
        
        twitterwidget(fav$status_id, list(cards='none'))
    })
    
  
    output$retweet <- renderTwitterwidget({
        re <- dataset() %>%
            select(status_id, favorite_count) %>%
            slice_max(order_by = favorite_count, n=1)
        twitterwidget(re$status_id, list(cards='none'))
    })
    
    output$followers <- renderValueBox({
        
        valueBox(dataset()$followers_count[1], "Followers",
                 
                 width = 2.5
        )
        
    
    })
    
    output$following <- renderValueBox({
        
        valueBox(dataset()$friends_count[1], "Following",
                 
                 width = 2.5
        )
        
        
    })
    
    output$likes <- renderValueBox({
        number_of_likes <- dataset()$favorite_count %>%
            sum()
        valueBox(number_of_likes, "Number of likes",
                 
                 width = 2.5
        )
        
        
    })
    
    output$numre <- renderValueBox({
        number_of_likes <- dataset()$retweet_count %>%
            sum()
        valueBox(number_of_likes, "Number of Retweets",
                         )
        
        
    })
    
    output$freqwc <- renderWordcloud2({
        word_lost() %>%
            group_by(word) %>%
            count(word) %>%
            anti_join(stop_words) %>%
            ungroup() %>%
            rename(freq = n) %>%
            slice_max(freq, n= 100) %>%
            wordcloud2(shape = "square", size=1, color='random-dark') 
        
    })        

    output$try <-renderPlotly({
        target <- stringr::str_split(input$selected_word, ":", simplify = T)[1]
        dataset() %>%
            select(created_at, status_id, text) %>%
            unnest_tokens(word, text) %>%
            filter(word == target) %>%
            group_by(created_at) %>%
            count(word) %>%
            rename("occurences" = n) %>%
            mutate(date = date(created_at)) %>%
            plot_ly(x= ~date, y= ~occurences, type = 'bar')
        })
    output$seltweets <- renderReactable( reactable({
        
        target <- stringr::str_split(req(input$selected_word), ":", simplify = T)[1]
            dataset() %>%
                select(status_id,created_at, text) %>%
                filter(stringr::str_detect(text, target)) %>%
                arrange(desc(created_at))
                
        })
        )
    
    
    output$hashtags <- renderReactable(reactable({
      hashtags <- dataset()$hashtags %>%
        unlist()  %>%
        tolower() %>%
        na.omit() 
      tibble(hashtags) %>%
        group_by(hashtags) %>%
        count(hashtags)%>%
        ungroup() %>%
        slice_max(order_by = n,  n=10) %>%
        mutate(hashtags = glue::glue("#{hashtags}"))
    }
    ))
    
    output$emojis <- renderReactable(reactable({
      dataset() %>%
        mutate(emojis = emoji::emoji_extract_all(text)) %>%
        select(emojis) %>%
        as.list() %>%
        unlist() %>%
        as.tibble() %>%
        group_by(value) %>%
        count() %>%
        ungroup() %>%
        slice_max(order_by = value, n =10) %>%
        arrange(desc(n))
    }
    ))
    
    
    scored_list <- reactive({
        dataset() %>%
            select(status_id, created_at, text) %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            filter(!grepl('[0-9]', word)) %>%
            left_join(get_sentiments(input$lexicon))
    })
    
    
    afinn_list <- reactive({
      scored_list() %>%
        group_by(status_id, created_at) %>%
        summarise(score = sum(value, na.rm = T)) %>%
        mutate(sentiment = case_when(
          score > 0 ~ "Positive",
          score < 0  ~ "Negative",
          TRUE ~ "Neutral"
        ))
    })
    
    words_afinn <- reactive({
      scored_list() %>%
        group_by(word) %>%
        summarise(score = sum(value, na.rm = T)) %>%
        mutate(sentiment = case_when(
          score > 0 ~ "Positive",
          score < 0  ~ "Negative",
          TRUE ~ "Neutral")) %>%
        filter(!sentiment == "Neutral") %>%
        ungroup()
    })
    
    
    afinn_tdf <- reactive({
      sentiment_tdf(afinn_list(), dataset())}) 
    
    
    bing_list <- reactive({
      scored_list() %>%
        group_by(status_id, created_at, sentiment) %>%
        count(status_id) %>%
        pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
        mutate(score = positive - negative) %>%
        mutate(sentiment = case_when(
          score > 0 ~ "Positive",
          score < 0  ~ "Negative",
          TRUE ~ "Neutral"
        ))
      
    })
    words_bing <- reactive({
      scored_list() %>%
        group_by(word, sentiment) %>%
        count(word) %>%
        na.omit() %>%
        group_by(sentiment) 
    })
    
    bing_tdf <- reactive({
      sentiment_tdf(bing_list(), dataset())}) 
      
    output$scoregraph <- renderPlotly({
      #afinn top
        if("value" %in% names(scored_list())){
          afinn_list() %>%
            plot_ly(x = ~created_at, y= ~score,
                    color = ~sentiment, type = "bar")
        }else{
          bing_list() %>%
                plot_ly(x = ~created_at, y= ~score, color = ~sentiment, type = "bar")
        }
            
    })
    
    output$ratinggraph <- renderPlotly({
      #afinn top'
        if("value" %in% names(scored_list())){
          words_afinn() %>%
            group_by(sentiment) %>%
            slice_max(abs(score), n = 15) %>%
            ungroup() %>%
            mutate(word = reorder(word, score)) %>%
            plot_ly(x = ~score, y= ~word, color = ~sentiment, type = "bar")
        } else{
          words_bing() %>%
                slice_max(abs(n), n = 15)  %>%
                mutate(n = case_when(
                    sentiment == "negative" ~ (as.double(n) * -1),
                    TRUE ~ as.double(n))) %>%
                ungroup() %>%
                mutate(word = reorder(word, n)) %>%
                plot_ly(x = ~n, y= ~word, color = ~sentiment, type = "bar")
        }
    })
    
    output$poswordcloud <- renderWordcloud2({
      if("value" %in% names(scored_list())){
        wordcloud_data_pro(afinn_tdf(), 'Positive')
      }
      else{
        wordcloud_data_pro(bing_tdf(), 'Positive')
      }
    })
    
    output$negwordcloud <- renderWordcloud2({
      if("value" %in% names(scored_list())){
        wordcloud_data_pro(afinn_tdf(), 'Negative')
      }
      else{
        wordcloud_data_pro(bing_tdf(), 'Negative')
      }
    })
    
    
    ngrams <- reactive({
      dataset() %>%
        select(status_id, text) %>%
        unnest_tokens(ngram, text, token = "ngrams", n = input$ngramsize) %>%
        group_by()
      
    })
    
    wordcors <- reactive({
      dataset() %>%
        select(status_id, text) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        filter(!grepl('[0-9]', word)) %>%
        na.omit() %>%
        group_by(word) %>%
        filter(n() >= 10) %>%
        pairwise_cor(word, status_id, sort = T) %>%
        filter(correlation > 0.2) %>%
        graph_from_data_frame()
    }
    )
    
    output$word_cor <- renderPlot({
      wordcors() %>%
        ggraph(layout = "fr") + geom_edge_link(aes(edge_alpha = correlation), show.legend = F) +
        geom_node_point(color = "lightblue", size = 5) + geom_node_text(aes(label = name), repel = T) +
        theme_void()
    })
    
    vader_tdif <- reactive({
      vader_score() %>%
        mutate(sentiment = case_when(
          score > 0 ~ "Positive",
          score < 0  ~ "Negative",
          TRUE ~ "Neutral"
        )) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        group_by(sentiment, word) %>%
        count(word) %>%
        mutate(word = str_extract(word, "[a-z']+")) %>%
        na.omit() %>%
        ungroup() %>%
        bind_tf_idf(word, sentiment, n)
    })
    
    output$vposwordcloud <- renderWordcloud2({
      vader_tdif() %>%
        wordcloud_data_pro("Positive")
      
    })
      
    output$vnegwordcloud <- renderWordcloud2({
      vader_tdif() %>%
        wordcloud_data_pro("Negative")
      
    })
       
}

#---------------------------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)
