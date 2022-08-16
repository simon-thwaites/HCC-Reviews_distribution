library(wordcloud2)
library(tm) # text mining
library(dplyr)
library(textclean) # to handle contractions

test <- resp$yr.20.21 %>% 
  rename(text.positives = `What were the standout positives from the season?`,
         text.negatives = `What were the standout negatives from the season?`,
         text.training = `How could we improve training?`,
         text.presentations = `How could we improve presentation nights?`,
         text.selections = `How could we improve selection nights?`,
         text.gamedayprep = `How could we improve game day preparation?`,
         text.funcideas = `Do you have any ideas for Club functions?`,
         text.comments = `Any other comments?`,
         text.moment = `What was your moment of the season?`)

text.colnames <- c("text.positives",
                   "text.negatives",
                   "text.training",
                   "text.presentations",
                   "text.selections",
                   "text.gamedayprep",
                   "text.funcideas",
                   "text.comments",
                   "text.moment")

# Note: Using an external vector in selections is ambiguous.
# Use `all_of(text.colnames)` instead of `text.colnames` to silence this message.
# See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
# text.data <- test %>% 
#   select(all_of(text.colnames))
# 
# text.data <- replace_contraction(text.data)
# 
# # need to handle the case of the proper ’ symbol and remove
# text.data <- gsub("[’]", "", text.data)


text.data <- test %>% 
  select(all_of(text.colnames)) %>% 
  replace_contraction() %>% 
  { gsub("[’]", "", .) }

# With pipe your data are passed as a first argument to the next function, so if you want to use it somewhere else you need to wrap the next line in {} and use . as a data "marker".

# text.data <- replace_contraction(text.data)
# 
# # need to handle the case of the proper ’ symbol and remove
# text.data <- gsub("[’]", "", text.data)


# create corpus. corpus is a selection of documents and allows use of text cleaning functions
text.corpus <- Corpus(VectorSource(text.data))

text.corpus <- text.corpus %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeWords, stopwords("english")) %>% # removes unnecesary words for the word cloud
  tm_map(removeWords, stopwords("SMART"))

# now create term document matrix, tdm
tdm <- TermDocumentMatrix(text.corpus) %>% 
  as.matrix()

# now want row sums and to sort by frequency
words <- sort(rowSums(tdm), decreasing = TRUE)

# words

# create dataframe
df <-  data.frame(word = names(words), freq = words)

# hmmm need to figure out the contraction handling here ...

wordcloud2(df)










  
