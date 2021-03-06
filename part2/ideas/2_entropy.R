#########################################################################
# required libraries
source("_shared_functions.R") 

#########################################################################
# calculate our entropy using the same shared function
# include these sources
sources <- c(#"clean_dickens_first100000",
             "clean_lorem_ipsum_first100000",
             #"clean_moby_dick_first100000",
             #"clean_quijote_first100000",
             #"clean_tom_sawyer_first100000",
             #"clean_wild_first100000",
             #"clean_dickens_first100000_backwards",
             #"clean_dickens_first100000_random",
             #"clean_dickens_first100000_chaos",
             #"clean_dickens_first100000_vocab7",
             #"clean_dickens_first100000_vocab25",
             #"clean_dickens_first100000_vocab50",
             #"clean_dickens_first100000_vocab100",
             #"clean_dickens_first100000_vocab200",
             #"clean_dickens_first100000_vocab6",
             #"clean_dickens_first100000_vocab8",
             "clean_dickens_first100000_bigram7",
             "clean_dickens_first100000_trigram7")

# load all data sources of interest in order
# do the next 100
for(item in sources) {
  save_as <- paste0(str_replace(item, "clean_","data/entropy_results_101_to_200_"),".Rds")
  my_data <- readRDS(paste0("data/",item,".Rds")) 
  results = calculate_higher_order_entropies(my_data,101,200)  
  saveRDS(results,save_as)
}  


#########################################################################
# visualize entropy
my_data <- get_combined_dataframe(c("entropy_results_dickens_first100000",
                                    "entropy_results_lorem_ipsum_first100000",
                                    "entropy_results_moby_dick_first100000",
                                    "entropy_results_quijote_first100000",
                                    "entropy_results_tom_sawyer_first100000",
                                    "entropy_results_wild_first100000"))

res = plot_entropy_results(my_data, c("Lorem Ipsum"="#F8766D",
                                      "Don Quijote"="#B79F00",
                                      "Tale of Two Cities"="#00BA38",
                                      "Moby Dick"="#00BFC4",
                                      "Tom Sawyer"="#619CFF",
                                      "Call of the Wild"="#C77CFF"))
res



#########################################################################
# explore entropy
source("_shared_functions.R") 
my_data <- readRDS(paste0("data/clean_dickens_first100000_backwards.Rds")) 
res1 = explore_entropy(my_data, 89) %>%
  group_by(ngram) %>%
  summarise(freq=n()) %>%
  filter(freq > 1)
