# functio nto help generate the wordclouds for each question

createTDM <- function(text.data, index){
  text.corpus <- Corpus(VectorSource(text.data[[index]]))
  
  text.corpus <- text.corpus %>% 
    tm_map(removeNumbers) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords, stopwords("english")) %>% # removes unnecesary words for the word cloud
    tm_map(removeWords, stopwords("SMART"))
  
  tdm <- TermDocumentMatrix(text.corpus) %>% 
    as.matrix()
  
  words <- sort(rowSums(tdm), decreasing = TRUE)
  
  return(words)
  # create dataframe
  # df <-  data.frame(word = names(words), freq = words)
  
  # v <- terms
  # wordcloud_rep(df)
  # wordcloud_rep(names(words), words, scale = c(4,0.5),
  #               min.freq = input$wordcloud.freq, max.words=input$wordcloud.max,
  #               colors=brewer.pal(8, "Dark2"))
}