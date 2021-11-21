#########################################################################
# found a bug in the calculate entropy code
# it was confusing 1st order for 0th order
# fixed!
# need to re-run all of these. 

#########################################################################
# required libraries
source("_shared_functions.R") 

#########################################################################
# calculate our entropy using the same shared function
# include these sources
sources <- c(
  "clean_moby_dick_first100000",
  "clean_quijote_first100000",
  "clean_wild_first100000",
  "clean_tom_sawyer_first100000",
  "clean_lorem_ipsum_first100000",  
  "clean_dickens_first100000",  
  "clean_dickens_first100000_vocab200",
  "clean_dickens_first100000_vocab100",  
  "clean_dickens_first100000_vocab50",
  "clean_dickens_first100000_vocab8",
  "clean_dickens_first100000_vocab7",
  "clean_dickens_first100000_vocab6",  
  "clean_dickens_first100000_bigram7",
  "clean_dickens_first100000_trigram7", 
  "clean_dickens_first100000_random",
  "clean_dickens_first100000_chaos"
)


# load all data sources of interest in order
# do the next 100
for(item in sources) {
  save_as <- paste0(str_replace(item, "clean_","data2/entropy_results_"),".Rds")
  my_data <- readRDS(paste0("data/",item,".Rds")) 
  results = calculate_higher_order_entropies(my_data,0,10)  
  saveRDS(results,save_as)
}  




#########################################################################
my_data3 <- get_combined_dataframe(sources=c(
  "entropy_results_moby_dick_first100000",
  #"entropy_results_quijote_first100000",
  "entropy_results_wild_first100000",
  "entropy_results_tom_sawyer_first100000",
  "entropy_results_lorem_ipsum_first100000",  
  "entropy_results_dickens_first100000"
  # "entropy_results_dickens_first100000_vocab200",
  # "entropy_results_dickens_first100000_vocab100",
  # "entropy_results_dickens_first100000_vocab50",
  # "entropy_results_dickens_first100000_vocab8",
  # "entropy_results_dickens_first100000_vocab7",
  # "entropy_results_dickens_first100000_vocab6",
  # "entropy_results_dickens_first100000_bigram7",
  # "entropy_results_dickens_first100000_trigram7",
  # "entropy_results_dickens_first100000_random",
  # "entropy_results_dickens_first100000_chaos"
), 
folder="data2")


my_data_all <- my_data3
jojo <- analyze_third_condition(my_data_all, color_key=c("Lorem Ipsum (100,000)"="#F8766D",
                                                         "Don Quijote"="#B79F00",
                                                         "Tale of Two Cities"="#00BA38",
                                                         "Moby Dick"="#00BFC4",
                                                         "Tom Sawyer"="#619CFF",
                                                         "Call of the Wild"="#C77CFF"
                                                         
                                                         # "Tale of Two Cities (vocab 200)"="#bf534b",                                                              
                                                         # "Tale of Two Cities (vocab 100)"="#bf534b",                                                              
                                                         # "Tale of Two Cities (vocab 50)"="#bf534b",                                                              
                                                         # "Tale of Two Cities (vocab 25)"="#bf534b",                                                              
                                                         # "Tale of Two Cities (vocab 8)"="#bf534b",                                                              
                                                         # "Tale of Two Cities (vocab 6)"="#bf534b",                               
                                                         # "Tale of Two Cities (random)"="#bf534b",
                                                         # "Tale of Two Cities (chaos)"="#bf534b" ,                                     
                                                         # "Tale of Two Cities (vocab 7)"="#bf534b",
                                                         # "Tale of Two Cities (bigram 7)"="#bf534b",
                                                         # "Tale of Two Cities (trigram 7)"="#bf534b"
                                                         ))

p2 <- jojo[[3]]
p2

grid.arrange(p1, p2, nrow=1)



#########################################################################
# fix Lorem Ipsum 
sources <- c(
  "clean_lorem_ipsum_first100000",
  "clean_lorem_ipsum_first50000",
  "clean_lorem_ipsum_first25000",
  "clean_lorem_ipsum_first10000",
  "clean_lorem_ipsum_first4088"
)


# load all data sources of interest in order
# do the next 100
for(item in sources) {
  save_as <- paste0(str_replace(item, "clean_","data2/entropy_results_"),".Rds")
  my_data <- readRDS(paste0("data2/",item,".Rds")) 
  results = calculate_higher_order_entropies(my_data,0,100)  
  saveRDS(results,save_as)
}  






#########################################################################
# higher order entropy
my_data2 <- get_combined_dataframe(sources=c(
  "entropy_results_lorem_ipsum_first100000",
  "entropy_results_lorem_ipsum_first50000",
  "entropy_results_lorem_ipsum_first25000",
  "entropy_results_lorem_ipsum_first10000",
  "entropy_results_lorem_ipsum_first4088"
), 
folder="data2")

#%>% filter(n <= 25)

p1 <- plot_entropy_results(my_data2,  color_key=c(
  "Lorem Ipsum (100,000)"="#F8766D",
  "Lorem Ipsum (50,000)"="#bf544d",
  "Lorem Ipsum (25,000)"="#8c403b",
  "Lorem Ipsum (10,000)"="#692a26",
  "Lorem Ipsum (4,088)"="#421714"  
                                                  ))
p1



#########################################################################
my_data3 <- get_combined_dataframe(sources=c(
  "entropy_results_moby_dick_first4088",
  #"entropy_results_quijote_first4088",
  "entropy_results_wild_first4088",
  "entropy_results_tom_sawyer_first4088",
  "entropy_results_dickens_first4088",
  "entropy_results_english"
), 
folder="data2") %>%
  mutate(source=ifelse(source=="English (4,088)","Genesis Chapter 1 (4,088)",source))

my_data_all <- rbind(my_data3, my_data2)

jojo <- analyze_third_condition(my_data3, color_key=c("Genesis Chapter 1 (4,088)"="#457ed9",
                                                      "Lorem Ipsum"="#F8766D",
                                                         "Don Quijote"="#B79F00",
                                                         "Tale of Two Cities"="#00BA38",
                                                         "Moby Dick"="#00BFC4",
                                                         "Tom Sawyer"="#619CFF",
                                                         "Call of the Wild"="#C77CFF",
                                                         "Don Quijote (4,088)"="#B79F00",
                                                         "Tale of Two Cities (4,088)"="#00BA38",
                                                         "Moby Dick (4,088)"="#00BFC4",
                                                         "Tom Sawyer (4,088)"="#619CFF",
                                                         "Call of the Wild (4,088)"="#C77CFF",
                                                         "Lorem Ipsum (100,000)"="#bf534b",
                                                         "Lorem Ipsum (50,000)"="#bf534b",
                                                         "Lorem Ipsum (25,000)"="#bf534b",
                                                         "Lorem Ipsum (10,000)"="#bf534b",
                                                         "Lorem Ipsum (4,088)"="#bf534b", 
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

p1 <- jojo[[3]]
p1

grid.arrange(p1, p2, nrow=1)




#########################################################################
# calculate our entropy using the same shared function
# include these sources
sources <- c(
  "clean_moby_dick_first4088",
  "clean_quijote_first4088",
  "clean_wild_first4088",
  "clean_tom_sawyer_first4088",
  "clean_dickens_first4088"
)


# load all data sources of interest in order
# do the next 100
for(item in sources) {
  save_as <- paste0(str_replace(item, "clean_","data2/entropy_results_"),".Rds")
  my_data <- readRDS(paste0("data2/",item,".Rds")) 
  results = calculate_higher_order_entropies(my_data,0,10)  
  saveRDS(results,save_as)
}  

