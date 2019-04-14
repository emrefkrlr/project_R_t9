library(tidytext)
library(tidyr)
library(dplyr)
library(stringr)


size = 1000
for (document in c('en_US.blogs.txt', 
                   'en_US.news.txt',
                   'en_US.twitter.txt')){
  
  con <- file(document, 'r')
  #siplit_name <- unlist(strsplit(document, "/"))
  assign(paste('data',document,  sep= "_"),  readLines(con, size, encoding = "UTF-8"))
  assign(paste('df', document, sep= "_"), tibble(line = 1:size, text = assign(paste('data', document, sep= "_"),  readLines(con, size, encoding = "UTF-8"))))
  
}
text_words <- bind_rows(mutate(df_en_US.blogs.txt,source="blogs" ),
                        mutate(df_en_US.news.txt,source="news" ), 
                        mutate(df_en_US.twitter.txt,source="tweets"))
# bigram
text_bigrams <- text_words %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)%>%
  separate(bigram, c("word1", "word2"), sep = " ")
# trigram
text_trigram <- text_words %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)%>%
  separate(trigram, c("word1", "word2","word3"), sep = " ")
##' 4-gram
text_quadgram <- text_words %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4)%>%
  separate(quadgram, c("word1", "word2","word3","word4"), sep = " ")
##'  5-gram
text_pentagram <- text_words %>%
  unnest_tokens(pentagram, text, token = "ngrams", n = 5)%>%
  separate(pentagram, c("word1", "word2","word3","word4","word5"), sep = " ")

word_prediction <- function(input){
  
  word_list = unlist(strsplit(tolower(input)," "))
  index <- length(word_list)
  solution_tibble <- data.frame()
  if (index == 0) {
    
    result <- 'tahminleme yapamiyorum.'
    
  }else if(index == 1){
    
    solution_tibble <- text_bigrams %>%
      filter(
        word1 == word_list[index]) %>%
      count(word2, sort = TRUE) %>%
      mutate(probability = (n / sum(n))) %>%
      top_n(n = 10, wt = probability) %>%
      select(word2, probability) %>%
      mutate(gram = 'bigrams')%>%
      rename(newname=word2) %>%
      bind_rows(solution_tibble)
    
    result <- solution_tibble %>%
      arrange(desc(probability)) %>%
      head(3)
    
  }else if (index == 2) {
    
    solution_tibble <- text_trigram %>%
      filter(
        word2 == word_list[index],
        word1 == word_list[index-1]) %>%
      count(word3, sort = TRUE) %>%
      mutate(probability = (n / sum(n))) %>%
      top_n(n = 10, wt = probability) %>%
      select(word3, probability) %>%
      mutate(gram = 'trigram')%>%
      rename(newname=word3) %>%
      bind_rows(solution_tibble)
    
    solution_tibble <- text_bigrams %>%
      filter(
        word1 == word_list[index]) %>%
      count(word2, sort = TRUE) %>%
      mutate(probability = (n / sum(n))*0.4) %>%
      top_n(n = 10, wt = probability) %>%
      select(word2, probability) %>%
      mutate(gram = 'bigrams')%>%
      rename(newname=word2) %>%
      bind_rows(solution_tibble)
    
    result <- solution_tibble %>%
      arrange(desc(probability)) %>%
      head(3)
    
  }else if (index == 3){
    
    solution_tibble <- text_quadgram %>%
      filter(
        word3 == word_list[index],
        word2 == word_list[index-1],
        word1 == word_list[index-2]) %>%
      count(word4 ,sort = TRUE) %>%
      mutate(probability = (n / sum(n))) %>%
      top_n(n = 10, wt = probability) %>%
      select(word4, probability)%>%
      mutate(gram = 'quadgram')%>%
      rename(newname=word4) %>%
      bind_rows(solution_tibble)
    
    solution_tibble <- text_trigram %>%
      filter(
        word2 == word_list[index],
        word1 == word_list[index-1]) %>%
      count(word3, sort = TRUE) %>%
      mutate(probability = (n / sum(n))*0.4) %>%
      top_n(n = 10, wt = probability) %>%
      select(word3, probability) %>%
      mutate(gram = 'trigram')%>%
      rename(newname=word3) %>%
      bind_rows(solution_tibble)
    
    solution_tibble <- text_bigrams %>%
      filter(
        word1 == word_list[index]) %>%
      count(word2, sort = TRUE) %>%
      mutate(probability = (n / sum(n))*0.16) %>%
      top_n(n = 10, wt = probability) %>%
      select(word2, probability) %>%
      mutate(gram = 'bigrams')%>%
      rename(newname=word2) %>%
      bind_rows(solution_tibble)
    
    result <- solution_tibble %>%
      arrange(desc(probability)) %>%
      head(3)
    
  }else if ( index == 4){
    solution_tibble <- text_pentagram %>%
      filter(
        word4 == word_list[index],
        word3 == word_list[index-1],
        word2 == word_list[index-2],
        word1 == word_list[index-3]) %>%
      count(word5, sort = TRUE) %>%
      mutate(probability = n / sum(n)) %>%
      top_n(n = 10, wt = probability) %>%
      select(word5, probability) %>%
      mutate(gram = 'pentagram') %>%
      rename(newname=word5)
    
    solution_tibble <- text_quadgram %>%
      filter(
        word3 == word_list[index],
        word2 == word_list[index-1],
        word1 == word_list[index-2]) %>%
      count(word4 ,sort = TRUE) %>%
      mutate(probability = (n / sum(n))*0.4) %>%
      top_n(n = 10, wt = probability) %>%
      select(word4, probability)%>%
      mutate(gram = 'quadgram')%>%
      rename(newname=word4) %>%
      bind_rows(solution_tibble)
    
    solution_tibble <- text_trigram %>%
      filter(
        word2 == word_list[index],
        word1 == word_list[index-1]) %>%
      count(word3, sort = TRUE) %>%
      mutate(probability = (n / sum(n))*0.16) %>%
      top_n(n = 10, wt = probability) %>%
      select(word3, probability) %>%
      mutate(gram = 'trigram')%>%
      rename(newname=word3) %>%
      bind_rows(solution_tibble)
    
    solution_tibble <- text_bigrams %>%
      filter(
        word1 == word_list[index]) %>%
      count(word2, sort = TRUE) %>%
      mutate(probability = (n / sum(n))*0.064) %>%
      top_n(n = 10, wt = probability) %>%
      select(word2, probability) %>%
      mutate(gram = 'bigrams')%>%
      rename(newname=word2) %>%
      bind_rows(solution_tibble)
    
    result <- solution_tibble %>%
      arrange(desc(probability)) %>%
      head(3)
  }else{
    
    solution_tibble <- text_pentagram %>%
      filter(
        word4 == word_list[index],
        word3 == word_list[index-1],
        word2 == word_list[index-2],
        word1 == word_list[index-3]) %>%
      count(word5, sort = TRUE) %>%
      mutate(probability = n / sum(n)) %>%
      top_n(n = 10, wt = probability) %>%
      select(word5, probability) %>%
      mutate(gram = 'pentagram') %>%
      rename(newname=word5)
    
    solution_tibble <- text_quadgram %>%
      filter(
        word3 == word_list[index],
        word2 == word_list[index-1],
        word1 == word_list[index-2]) %>%
      count(word4 ,sort = TRUE) %>%
      mutate(probability = (n / sum(n))*0.4) %>%
      top_n(n = 10, wt = probability) %>%
      select(word4, probability)%>%
      mutate(gram = 'quadgram')%>%
      rename(newname=word4) %>%
      bind_rows(solution_tibble)
    
    solution_tibble <- text_trigram %>%
      filter(
        word2 == word_list[index],
        word1 == word_list[index-1]) %>%
      count(word3, sort = TRUE) %>%
      mutate(probability = (n / sum(n))*0.16) %>%
      top_n(n = 10, wt = probability) %>%
      select(word3, probability) %>%
      mutate(gram = 'trigram')%>%
      rename(newname=word3) %>%
      bind_rows(solution_tibble)
    
    solution_tibble <- text_bigrams %>%
      filter(
        word1 == word_list[index]) %>%
      count(word2, sort = TRUE) %>%
      mutate(probability = (n / sum(n))*0.064) %>%
      top_n(n = 10, wt = probability) %>%
      select(word2, probability) %>%
      mutate(gram = 'bigrams')%>%
      rename(newname=word2) %>%
      bind_rows(solution_tibble)
    
    result <- solution_tibble %>%
      arrange(desc(probability)) %>%
      head(3)
    
  }
  
  output <- c(result$newname, result$probability)
  return(output)
  
}
