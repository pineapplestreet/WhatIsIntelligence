#########################################################################
# compare the bible in 3 different languages - just Genesis 1:1

#########################################################################
# required libraries
source("_shared_functions.R") 

# which character is missing?
#tmp1 <- get_dataframe("clean_lorem_ipsum_first4088","data2")
#tmp2 <- get_dataframe("clean_lorem_ipsum_first10000","data2")


#########################################################################
# Zipf
my_data <- get_combined_dataframe(sources=c(
                                    "clean_english",
                                    "clean_english2",
                                    "clean_chinese",
                                    "clean_hebrew",
                                    "clean_hebrew2",
                                    "clean_lorem_ipsum_first4088"), 
                                  folder="data2")

my_data$source <- factor(my_data$source, levels=c(
                                                  "English (4,088)",
                                                  "English, 1st-order (4,088)",
                                                  "Lorem Ipsum (4,088)",
                                                  "Hebrew (2,136)",
                                                  "Hebrew, backwards (2,136)",
                                                  "Chinese (983)"
                                                  ))

# summary stats
summary <- get_summary_stats(my_data) %>%
  arrange(`Total Tokens`)


# use 0 for ALL tokens
# or set a MAX RANK to cut off the right tail
res = run_zipf_analysis(my_data, 0, c(
                                      "English (4,088)"="#457ed9",
                                      "English, 1st-order (4,088)"="#7b91b5",
                                      "Lorem Ipsum (4,088)"="#F8766D",
                                      "Hebrew (2,136)"="#19913d",
                                      "Hebrew, backwards (2,136)"="#869c8c",
                                      "Chinese (983)"="#8b2c9c"))
grid.arrange(res[[1]],res[[2]],widths = c(3, 2), nrow=1)



#########################################################################
# higher order entropy
my_data2 <- get_combined_dataframe(sources=c(
                                          "entropy_results_english",
                                            "entropy_results_chinese",
                                            "entropy_results_hebrew",
                                            "entropy_results_hebrew2",
                                            "entropy_results_english2",
                                            "entropy_results_lorem_ipsum_first4088"
                                            ), 
                                  folder="data2") %>% filter(n <= 25)

p1 <- plot_entropy_results(my_data2,  color_key=c(
  #"English (4,088)"="#457ed9",
  #"English, 1st-order (4,088)"="#7b91b5",
  "Lorem Ipsum (4,088)"="#F8766D",
  #"Hebrew (2,136)"="#19913d",
  #"Hebrew, backwards (2,136)"="#869c8c"
  "Chinese (983)"="#8b2c9c"
  ))
p1






#########################################################################
# higher order entropy
my_data3 <- get_combined_dataframe(sources=c(
  "entropy_results_dickens_first100000",
  #"entropy_results_lorem_ipsum_first100000",
  #"entropy_results_101_to_200_lorem_ipsum_first100000",  
  "entropy_results_moby_dick_first100000",
  "entropy_results_quijote_first100000",
  "entropy_results_tom_sawyer_first100000",
  "entropy_results_wild_first100000"
  # "entropy_results_dickens_first100000_random",
  # "entropy_results_dickens_first100000_chaos",
  # "entropy_results_dickens_first100000_vocab6",
  # "entropy_results_dickens_first100000_vocab7",
  # "entropy_results_dickens_first100000_bigram7",
  # "entropy_results_101_to_200_dickens_first100000_bigram7",  
  # "entropy_results_dickens_first100000_trigram7",
  # "entropy_results_101_to_200_dickens_first100000_trigram7",
  # "entropy_results_dickens_first100000_vocab8",  
  # "entropy_results_dickens_first100000_vocab25",
  # "entropy_results_dickens_first100000_vocab50",
  # "entropy_results_dickens_first100000_vocab100",
  # "entropy_results_dickens_first100000_vocab200"
  ), 
  folder="data")

#my_data_all <- rbind(my_data2,my_data3)
my_data_all <- my_data2
jojo <- analyze_third_condition(my_data_all, color_key=c(
  "English (4,088)"="#457ed9",
  "English, 1st-order (4,088)"="#7b91b5",
  "Lorem Ipsum (4,088)"="#F8766D",
  "Hebrew (2,136)"="#19913d",
  "Hebrew, backwards (2,136)"="#869c8c",
  "Chinese (983)"="#8b2c9c"))

p2 <- jojo[[3]]


grid.arrange(p1,p2,widths = c(1, 1), nrow=1)



# read a peek of each bible
tmp <- get_dataframe("clean_english2","data2") %>%
  filter(symbol_order <= 100)
paste(tmp$symbol,collapse="")


x <- 470000
x <- x/2
x
