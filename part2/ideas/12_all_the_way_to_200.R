#########################################################################
# all the way to n=200 higher order entropy

#########################################################################
# required libraries
source("_shared_functions.R") 


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
  #"entropy_results_dickens_first100000_vocab6",
  "entropy_results_dickens_first100000_vocab7",
  "entropy_results_dickens_first100000_bigram7",
  "entropy_results_101_to_200_dickens_first100000_bigram7",  
  "entropy_results_dickens_first100000_trigram7",
  "entropy_results_101_to_200_dickens_first100000_trigram7"
  #"entropy_results_dickens_first100000_vocab8"  
  # "entropy_results_dickens_first100000_vocab25",
  # "entropy_results_dickens_first100000_vocab50",
  # "entropy_results_dickens_first100000_vocab100",
  # "entropy_results_dickens_first100000_vocab200"
)) 

my_data = my_data %>%
  filter(n <= 160)

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

