#########################################################################
# let's explore the curve even more

#########################################################################
# required libraries
source("_shared_functions.R") 


#########################################################################
#########################################################################
# visualize entropy
my_data <- get_combined_dataframe(c(
  "entropy_results_dickens_first100000",
  "entropy_results_lorem_ipsum_first100000",
  "entropy_results_101_to_200_lorem_ipsum_first100000",  
  "entropy_results_moby_dick_first100000",
  "entropy_results_quijote_first100000",
  "entropy_results_tom_sawyer_first100000",
  "entropy_results_wild_first100000",
  "entropy_results_dickens_first100000_random",
  "entropy_results_dickens_first100000_chaos",
  "entropy_results_dickens_first100000_vocab6",
  "entropy_results_dickens_first100000_vocab7",
  "entropy_results_dickens_first100000_bigram7",
  "entropy_results_101_to_200_dickens_first100000_bigram7",  
  "entropy_results_dickens_first100000_trigram7",
  "entropy_results_101_to_200_dickens_first100000_trigram7",
  "entropy_results_dickens_first100000_vocab8",  
  "entropy_results_dickens_first100000_vocab25",
  "entropy_results_dickens_first100000_vocab50",
  "entropy_results_dickens_first100000_vocab100",
  "entropy_results_dickens_first100000_vocab200"
))



# function to examine the full curves of each source
examine_percent_change <- function(data) {
  
  # hold one row of results per source
  data.entropy_change <- data.frame()
  
  # look at each source one at a time
  for(item in unique(data$source)) {
    
    # nth-order entropy for the source of interest
    # ignore any points of zero entropy
    print(item)
    entropy <- data %>% 
      filter(source==item) %>%
      filter(entropy>0) %>%
      mutate(next_n=lead(n)) %>%
      mutate(next_entropy=lead(entropy)) %>%
      filter(!is.na(next_entropy)) %>%
      mutate(percent_change=(next_entropy-entropy)/abs(entropy)) %>%
      mutate(cum_percent_change=cumsum(percent_change))
    data.entropy_change <- rbind(data.entropy_change, entropy)
  }
  
  # return all results
  return(data.entropy_change)
}

# get the cumulative % change of entropy at each n
data.entropy_change = examine_percent_change(my_data)


ggplot(data.entropy_change, aes(x=next_n, y=percent_change, color=source)) +
  geom_point() +
  theme_bw() +
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
