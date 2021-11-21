#########################################################################
# can we find a counter-example by simply running a "backwards book"?

#########################################################################
# required libraries
source("_shared_functions.R") 

# use "A Tale of Two Cities" 
original <- readRDS("data/clean_dickens_first100000.Rds")

# put everything backwards
backwards <- original %>% arrange(desc(symbol_order))

# new corpus
new_corpus <- original
new_corpus$new_symbol <- backwards$symbol
new_corpus$new_source <- "Tale of Two Cities (backwards)"
new_corpus <- new_corpus %>%
  select(-symbol) %>%
  mutate(symbol=new_symbol,
         source=new_source) %>%
  select(source, symbol, symbol_order)
saveRDS(new_corpus,"data/clean_dickens_first100000_backwards.Rds")


#########################################################################
# run our analysis

my_data <- get_combined_dataframe(c("clean_dickens_first100000",
                                    "clean_dickens_first100000_backwards",
                                    "clean_lorem_ipsum_first100000"))

res = run_zipf_analysis(my_data, 0, c("Lorem Ipsum"="#F8766D",
                                      "Tale of Two Cities"="#00BA38",
                                      "Tale of Two Cities (backwards)"="#64d185"))
grid.arrange(res[[1]],res[[2]],widths = c(3, 2), nrow=1)


#########################################################################
# visualize entropy
my_data <- get_combined_dataframe(c("entropy_results_dickens_first100000",
                                    "entropy_results_lorem_ipsum_first100000",
                                    "entropy_results_moby_dick_first100000",
                                    "entropy_results_quijote_first100000",
                                    "entropy_results_tom_sawyer_first100000",
                                    "entropy_results_wild_first100000",
                                    "entropy_results_dickens_first100000_backwards"))


res = plot_entropy_results(my_data, c("Lorem Ipsum"="#F8766D",
                                      "Don Quijote"="#B79F00",
                                      "Tale of Two Cities"="#00BA38",
                                      "Moby Dick"="#00BFC4",
                                      "Tom Sawyer"="#619CFF",
                                      "Call of the Wild"="#C77CFF",
                                      "Tale of Two Cities (backwards)"="#64d185"))
res

