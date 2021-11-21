#########################################################################
# wtf is going on with lorem ipsum?

#########################################################################
# required libraries

# digging into the ngram paradox...
source("_shared_functions.R") 

# function to explore this a bit more
explore <- function(data, n) {
  
  # what is the most frequent unigram?
  tmp <- data %>%
    group_by(token) %>%
    summarise(freq=n()) %>%
    arrange(desc(freq))
  most_freq_unigram <- tmp[1,]$token
  most_freq_unigram_count <- tmp[1,]$freq  
  if(n==1) {
    return(data.frame(source=max(data$source), 
                      n=n, 
                      most_freq_ngram=most_freq_unigram, 
                      most_freq_ngram_count=most_freq_unigram_count, 
                      most_freq_ngram_p=1))
  }
  
  if(n > 1) {
    ngrams <- data %>%
      mutate(
      ngram=sapply(
        1:nrow(.),
        function(x) paste(token[pmax(1, x):pmin(x + (n-1), nrow(.))], collapse = " ")
      )) %>%
      group_by(ngram) %>%
      summarise(freq=n()) %>%
      arrange(desc(freq))
    the_most_freq_ngram <- ngrams[1,]$ngram
    the_most_freq_ngram_count <- ngrams[1,]$freq      
    #return(ngrams)
    return(data.frame(source=max(data$source), 
                      n=n, 
                      most_freq_ngram=the_most_freq_ngram, 
                      most_freq_ngram_count=the_most_freq_ngram_count, 
                      most_freq_ngram_p=the_most_freq_ngram_count/most_freq_unigram_count))
  }
}

# look closer at lorem ipsum (tokenized by words)
lorem_ipsum <- readRDS("../part1/clean_data/lorem_ipsum_words.Rds") 

cities <- readRDS("../part1/clean_data/a_tale_of_two_cities_words.Rds") 

wild <- readRDS("../part1/clean_data/the_call_of_the_wild_words.Rds") 

quijote <- readRDS("../part1/clean_data/don_quijote_part1_words.Rds") 

dick <- readRDS("../part1/clean_data/moby_dick_words.Rds") 

sawyer <- readRDS("../part1/clean_data/the_adventures_of_tom_sawyer_words.Rds") 

results <- data.frame()
for(i in seq(1,10,1)) {
  print(i)
  #results <- rbind(results, explore(lorem_ipsum,i))
  #results <- rbind(results, explore(cities,i))  
  #results <- rbind(results, explore(wild,i))  
  #results <- rbind(results, explore(quijote,i))  
  #results <- rbind(results, explore(dick,i))  
  results <- rbind(results, explore(sawyer,i))  
}


ggplot(results, aes(x=n, y=log10(most_freq_ngram_p), color=source)) +
  geom_point()
         
# this is going somewhere but I'm not sure where yet...

