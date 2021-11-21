#########################################################################
# wtf is going on with lorem ipsum?

#########################################################################
# required libraries
source("_shared_functions.R") 

# version 1, allow 7 words
# use a limited vocabulary 
words <- data.frame(words=c("it","was","the","best","of","times","worst"))


# version 2, use a limited vocabulary, allow top 25, 50, 100 and 200 words
# look closer at tale of two cities (which contains words)
vocab <- readRDS("../part1/clean_data/a_tale_of_two_cities_words.Rds") %>%
  group_by(token) %>%
  summarise(freq=n()) %>%
  arrange(desc(freq)) %>%
  top_n(200)


# version 2, allow top X words
words <- data.frame(words=vocab$token)

# version 3, allow an 8th word
words <- data.frame(words=c("it","was","the","best","of","times","worst", "age"))

# version 4, limit to 6 words
words <- data.frame(words=c("it","was","the","best","of","times"))

# version 5, try a 7 bigrams -- AHA! no we are on to something!
words <- data.frame(words=c("it was","the best","of times","age wisdom","season darkness","everything before","direct heaven"))

# version 6, try a 7 bigrams -- AHA! no we are on to something!
words <- data.frame(words=c("it was the", 
                            "best of times",
                            "worst times wisdom", 
                            "age foolishness epoch",
                            "belief incredulity season",
                            "Light Darkness spring",
                            "hope winter despair"))

# pick 50,000 words from our limited vocabulary randomly
new_corpus <- sample_n(words, 50000, replace=TRUE) %>%
  mutate(words=paste0(words,"_"),
         symbol=strsplit(words,"")) %>%
  unnest(symbol) %>%
  mutate(symbol=str_replace(symbol,"_"," ")) %>%
  mutate(source="Tale of Two Cities (trigram 7)") %>%
  select(source, symbol) %>%
  mutate(symbol_order=row_number()) %>%
  filter(symbol_order <= 100000)
saveRDS(new_corpus,"data/clean_dickens_first100000_trigram7.Rds")

# sample of new corpus
sample <- new_corpus %>%
  filter(symbol_order < 52) %>%
  mutate(line=paste(symbol, collapse = ""))
sample[1,"line"]

#########################################################################
# run our analysis

my_data <- get_combined_dataframe(c("clean_lorem_ipsum_first100000",
                                    "clean_dickens_first100000",
                                    "clean_dickens_first100000_vocab7",
                                    "clean_dickens_first100000_bigram7",                                    
                                    "clean_dickens_first100000_trigram7"))

my_data$source <- factor(my_data$source, levels=c("Lorem Ipsum",
                                                  "Tale of Two Cities",
                                                  "Tale of Two Cities (vocab 7)",
                                                  "Tale of Two Cities (bigram 7)",
                                                  "Tale of Two Cities (trigram 7)"))

res = run_zipf_analysis(my_data, 0, c("Lorem Ipsum"="#F8766D",
                                      "Tale of Two Cities"="#00BA38",
                                      "Tale of Two Cities (vocab 7)"="#64d185",
                                      "Tale of Two Cities (bigram 7)"="#91c4a1",
                                      "Tale of Two Cities (trigram 7)"="#b8c2bb"))
grid.arrange(res[[1]],res[[2]],widths = c(3, 2), nrow=1)


#########################################################################
# visualize entropy
my_data <- get_combined_dataframe(c(
  "entropy_results_dickens_first100000",
  "entropy_results_lorem_ipsum_first100000",
  "entropy_results_moby_dick_first100000",
  "entropy_results_quijote_first100000",
  "entropy_results_tom_sawyer_first100000",
  "entropy_results_wild_first100000",
  "entropy_results_dickens_first100000_random",
  "entropy_results_dickens_first100000_chaos",
  #"entropy_results_dickens_first100000_vocab6",
  "entropy_results_dickens_first100000_vocab7",
  "entropy_results_dickens_first100000_bigram7",
  "entropy_results_dickens_first100000_trigram7" 
  #"entropy_results_dickens_first100000_vocab8"  
  # "entropy_results_dickens_first100000_vocab25",
  # "entropy_results_dickens_first100000_vocab50",
  # "entropy_results_dickens_first100000_vocab100",
  # "entropy_results_dickens_first100000_vocab200"
  )) 

#my_data <- my_data %>% filter(n <= 50)

# res = plot_entropy_results(my_data, c("Lorem Ipsum"="#F8766D",
#                                       "Don Quijote"="#B79F00",
#                                       "Tale of Two Cities"="#00BA38",
#                                       "Moby Dick"="#00BFC4",
#                                       "Tom Sawyer"="#619CFF",
#                                       "Call of the Wild"="#C77CFF",
#                                       "Tale of Two Cities (vocab 7)"="#64d185",
#                                       "Tale of Two Cities (bigram 7)"="#91c4a1",
#                                       "Tale of Two Cities (trigram 7)"="#b8c2bb"))


res = plot_entropy_results(my_data, c("Lorem Ipsum"="#bf534b",
                                      "Don Quijote"="#00BA38",
                                      "Tale of Two Cities"="#00BA38",
                                      "Moby Dick"="#00BA38",
                                      "Tom Sawyer"="#00BA38",
                                      "Call of the Wild"="#00BA38",
                                      "Tale of Two Cities (random)"="#bf534b",
                                      "Tale of Two Cities (chaos)"="#bf534b" ,                                     
                                      "Tale of Two Cities (vocab 7)"="#bf534b",
                                      "Tale of Two Cities (bigram 7)"="#bf534b",
                                      "Tale of Two Cities (trigram 7)"="#bf534b"))
res




