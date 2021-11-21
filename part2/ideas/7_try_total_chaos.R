#########################################################################
# I want an example of total chaos

#########################################################################
# required libraries
source("_shared_functions.R") 

# use "A Tale of Two Cities" 
original <- readRDS("data/clean_dickens_first100000.Rds")

# unique symbols
symbols <- original %>%
  group_by(symbol) %>%
  summarise(freq=n()) %>%
  arrange(symbol)

# create total chaos, all symbols have equal probability
chaos <- sample_n(symbols, 100000, replace=TRUE) %>%
  mutate(source="Tale of Two Cities (chaos)") %>%
  select(source, symbol) %>%
  mutate(symbol_order=row_number())
saveRDS(chaos,"data/clean_dickens_first100000_chaos.Rds")

# sample of new corpus
sample <- chaos %>%
  filter(symbol_order < 52) %>%
  mutate(line=paste(symbol, collapse = ""))
sample[1,"line"]

#########################################################################
# run our analysis

my_data <- get_combined_dataframe(c("clean_dickens_first100000",
                                    "clean_dickens_first100000_chaos",
                                    "clean_lorem_ipsum_first100000"))

res = run_zipf_analysis(my_data, 0, c("Lorem Ipsum"="#F8766D",
                                      "Tale of Two Cities"="#00BA38",
                                      "Tale of Two Cities (chaos)"="#64d185"))
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
                                    "entropy_results_dickens_first100000_chaos")) 


res = plot_entropy_results(my_data, c("Lorem Ipsum"="#F8766D",
                                      "Don Quijote"="#B79F00",
                                      "Tale of Two Cities"="#00BA38",
                                      "Moby Dick"="#00BFC4",
                                      "Tom Sawyer"="#619CFF",
                                      "Call of the Wild"="#C77CFF",
                                      "Tale of Two Cities (random)"="#333333",
                                      "Tale of Two Cities (chaos)"="#64d185"))
res

