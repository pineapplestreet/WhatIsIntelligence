#########################################################################
# we found this with vocab 100 or 200

#########################################################################
# required libraries
source("_shared_functions.R") 

#########################################################################
# run our analysis

my_data <- get_combined_dataframe(c("clean_dickens_first100000",
                                    "clean_dickens_first100000_vocab100",
                                    "clean_lorem_ipsum_first100000"))

res = run_zipf_analysis(my_data, 0, c("Lorem Ipsum"="#F8766D",
                                      "Tale of Two Cities"="#00BA38",
                                      "Tale of Two Cities (vocab 100)"="#ff8c00"))
grid.arrange(res[[1]],res[[2]],widths = c(3, 2), nrow=1)


#########################################################################
# visualize entropy
my_data <- get_combined_dataframe(c("entropy_results_dickens_first100000",
                                    "entropy_results_lorem_ipsum_first100000",
                                    "entropy_results_moby_dick_first100000",
                                    "entropy_results_quijote_first100000",
                                    "entropy_results_tom_sawyer_first100000",
                                    "entropy_results_wild_first100000",
                                    "entropy_results_dickens_first100000_vocab100",
                                    "entropy_results_lorem_ipsum_first100000")) %>%
  filter(n > 10 & n <= 25)
  #filter(n > 0 & n <= 12)

res2 = plot_entropy_results(my_data, c("Lorem Ipsum"="#F8766D",
                                      "Don Quijote"="#B79F00",
                                      "Tale of Two Cities"="#00BA38",
                                      "Moby Dick"="#00BFC4",
                                      "Tom Sawyer"="#619CFF",
                                      "Call of the Wild"="#C77CFF",
                                      "Tale of Two Cities (vocab 100)"="#ff8c00"),
                            FALSE)
grid.arrange(res1, res2, widths = c(2, 3), nrow=1)
res2
res1


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
  "entropy_results_dickens_first100000_trigram7", 
  "entropy_results_dickens_first100000_vocab100"
  #"entropy_results_dickens_first100000_vocab8"  
  # "entropy_results_dickens_first100000_vocab25",
  # "entropy_results_dickens_first100000_vocab50",
  # "entropy_results_dickens_first100000_vocab100",
  # "entropy_results_dickens_first100000_vocab200"
)) 
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
                                      "Tale of Two Cities (trigram 7)"="#bf534b",
                                      "Tale of Two Cities (vocab 100)"="#ff8c00"))
res
