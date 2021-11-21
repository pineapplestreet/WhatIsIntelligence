#########################################################################
# let's explore the curve even more

#########################################################################
# required libraries
source("_shared_functions.R") 


#########################################################################
#########################################################################
# visualize entropy
my_data <- get_combined_dataframe(c(
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
))



# consider the number of distinct symbols, and how quickly we enounter them all
consider_information <- function(data) {
  
  # hold one row of results per source
  results <- data.frame()
  
  # look at each source one at a time
  for(item in unique(data$source)) {
    
    # get data for each source
    print(item)
    my_data <- data %>% 
      filter(source==item) 
    
    # how many unique symbols do we have?
    unique_symbols <- length(unique(my_data$symbol))
    print(unique_symbols)
    
    for(i in seq(10,100000,100)) {
      tmp <- my_data %>%
        filter(symbol_order <= i)
      found_symbols <- length(unique(tmp$symbol))
      results <- rbind(results, data.frame(source=max(tmp$source),i=i,unique=unique_symbols, found=found_symbols, found_per=found_symbols/unique_symbols))
    }
    return(results)
  }
  
  # return all results
  return(results)
}

# get the cumulative % change of entropy at each n
jojo = consider_information(my_data)

ggplot(jojo, aes(x=i, y=found_per, color=source)) +
  geom_point()
