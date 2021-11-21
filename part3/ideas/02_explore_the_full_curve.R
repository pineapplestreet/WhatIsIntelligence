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
  results <- data.frame()
  
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
      mutate(log10_entropy=log10(entropy))
    
    # window through all available slopes
    my_slopes <- data.frame()
    for(i in seq(min(entropy$clean_n),max(entropy$clean_n)-1,1)) {
      window <- entropy %>%
        filter(source==item) %>%
        filter(n >= i) %>%
        filter(n <= i+1)  
      
      # find our slopes
      model <- lm(data=window, log10_entropy~n)
      my_slopes <- rbind(my_slopes, data.frame(iteration=i, slope=model$coefficients[2]))
    }    
    
    # look at rolling variance of our slopes
    my_slopes <- my_slopes %>%
      mutate(rolling_var_window2 = rollapply(slope, 2, var, fill = NA, align = "right")) %>%
      mutate(rolling_var_window5 = rollapply(slope, 5, var, fill = NA, align = "right")) %>%      
      mutate(rolling_var_window6 = rollapply(slope, 6, var, fill = NA, align = "right")) %>%
      mutate(rolling_var_window10 = rollapply(slope, 10, var, fill = NA, align = "right")) %>%
      mutate(rolling_var_window20 = rollapply(slope, 20, var, fill = NA, align = "right")) %>%
      mutate(rolling_var_window30 = rollapply(slope, 30, var, fill = NA, align = "right"))
    #return(my_slopes)
    
    # get summary stats per source
    results <- rbind(results, data.frame(source=item,
                                         max_n_before_zero_entropy = max_n,
                                         rolling_var_window2=max(my_slopes$rolling_var_window2, na.rm=TRUE),
                                         rolling_var_window5=max(my_slopes$rolling_var_window6, na.rm=TRUE),                                         
                                         rolling_var_window6=max(my_slopes$rolling_var_window6, na.rm=TRUE),
                                         rolling_var_window10=max(my_slopes$rolling_var_window10, na.rm=TRUE),
                                         rolling_var_window20=max(my_slopes$rolling_var_window20, na.rm=TRUE),
                                         rolling_var_window30=max(my_slopes$rolling_var_window30, na.rm=TRUE)))    
  }
  
  # return all results
  return(results)
}

jojo = examine_the_curves(my_data)

# this doesn't seem to be leading anywhere useful
