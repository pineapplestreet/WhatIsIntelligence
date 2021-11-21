#########################################################################
# let's dig deep into ngrams

# interesting way to spot the bigram 7 and trigram 7 texts 
# but who cares? that just distinguishes different types
# of non-intelligent messages... this isn't useful either

#########################################################################
# required libraries
source("_shared_functions.R") 

# hold results
results <- data.frame()

sources <- c(
  "clean_dickens_first100000",
  "clean_lorem_ipsum_first100000",
  "clean_moby_dick_first100000",
  "clean_quijote_first100000",
  "clean_tom_sawyer_first100000",
  "clean_wild_first100000",
  "clean_dickens_first100000_random",
  "clean_dickens_first100000_chaos",
  "clean_dickens_first100000_vocab6",
  "clean_dickens_first100000_vocab7",
  "clean_dickens_first100000_bigram7",
  "clean_dickens_first100000_trigram7",
  "clean_dickens_first100000_vocab8",
  "clean_dickens_first100000_vocab25",
  "clean_dickens_first100000_vocab50",
  "clean_dickens_first100000_vocab100",
  "clean_dickens_first100000_vocab200"
)

for(item in sources) {
  
  # get our data
  my_data <- get_dataframe(item)
  print(item)
  
  # check out how many 2grams-25grams exist in proportion to the entire corpus
  for(n in seq(1,101,5)) {
    print(max(my_data$source))
    print(n)
    ngrams <- get_ngrams(my_data,n)
    ngrams_used_2_or_more_times <- ngrams %>%
      group_by(ngram) %>%
      summarise(freq=n()) %>%
      filter(freq > 1) %>%
      arrange(desc(freq))
    
    results <- rbind(results, data.frame(source=max(my_data$source),
                                         n=n,
                                         unique_ngrams=nrow(ngrams_used_2_or_more_times),
                                         total_freq=sum(ngrams_used_2_or_more_times$freq),
                                         most_pop_ngram=ngrams_used_2_or_more_times[1,"ngram"][[1]],
                                         most_pop_ngram_freq=ngrams_used_2_or_more_times[1,"freq"][[1]]))
  }  
}

saveRDS(results, "data/ngram_analysis2.Rds")


ggdata <- results %>%
  filter(source %in% c("Lorem Ipsum",
                       "Don Quijote",
                       "Tale of Two Cities",
                       "Moby Dick",
                       "Tom Sawyer",
                       "Call of the Wild",
                       "Tale of Two Cities (vocab 7)",
                       "Tale of Two Cities (trigram 7)",
                       "Tale of Two Cities (bigram 7)"))
ggplot(ggdata, aes(x=n, y=unique_ngrams, color=source)) +
  geom_point() +
  geom_line() +
  scale_color_manual(name=" ",
                     values = c("Lorem Ipsum"="#F8766D",
                                "Don Quijote"="#B79F00",
                                "Tale of Two Cities"="#00BA38",
                                "Moby Dick"="#00BFC4",
                                "Tom Sawyer"="#619CFF",
                                "Call of the Wild"="#C77CFF",
                                "Tale of Two Cities (vocab 200)"="#bf534b",                                                              
                                "Tale of Two Cities (vocab 100)"="#bf534b",                                                              
                                "Tale of Two Cities (vocab 50)"="#bf534b",                                                              
                                "Tale of Two Cities (vocab 25)"="#bf534b",                                                              
                                "Tale of Two Cities (vocab 8)"="#bf534b",                                                              
                                "Tale of Two Cities (vocab 6)"="#bf534b",                               
                                "Tale of Two Cities (random)"="#bf534b",
                                "Tale of Two Cities (chaos)"="#bf534b" ,                                     
                                "Tale of Two Cities (vocab 7)"="#bf534b",
                                "Tale of Two Cities (bigram 7)"="#bf534b",
                                "Tale of Two Cities (trigram 7)"="#bf534b"))