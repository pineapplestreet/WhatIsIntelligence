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
examine_the_curves <- function(data) {
  
  # hold one row of results per source
  data.full_entropy <- data.frame()
  
  # look at each source one at a time
  for(item in unique(data$source)) {
    
    # nth-order entropy for the source of interest
    print(item)
    entropy <- data %>% filter(source==item)
    
    # what is the maximum n that still contains entropy?
    max_n <- max((entropy %>% filter(entropy > 0))$n)
    print(max_n)
    
    # ignore any n's that have no entropy (plot slope ignoring any -Inf log10)
    entropy <- entropy %>% 
      filter(entropy > 0) %>%
      mutate(clean_n=row_number()-1) %>%
      mutate(log10_entropy=log10(entropy)) %>%
      filter(clean_n <= 50)
    
    # run linear regression
    model <- lm(data=entropy, log10_entropy~clean_n)
    data.full_entropy <- rbind(data.full_entropy, data.frame(source=item, 
                                                             slope=model$coefficients[2],
                                                             r2=summary(model)$r.squared))    
    
    #return(entropy)
  }
  
  # return all results
  return(data.full_entropy)
}

jojo = examine_the_curves(my_data)


# ok, we need to think deeper now...