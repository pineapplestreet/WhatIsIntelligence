#########################################################################
# compare the bible in 3 different languages - just Genesis 1:1

#########################################################################
# required libraries
source("_shared_functions.R") 

#########################################################################
# calculate our entropy using the same shared function
# include these sources
sources <- c(
  "clean_english",
  "clean_english2",
  "clean_chinese",
  "clean_hebrew",
  "clean_hebrew2",
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
# apples-to-apples with Lorem Ipsum (4,088)
# damn it- this breaks condition #3
tmp <- get_dataframe("clean_tom_sawyer_first100000","data") %>%
  filter(symbol_order <= 4088) %>%
  mutate(source="Tom Sawyer (4,088)")
saveRDS(tmp, "data2/clean_tom_sawyer_first4088.Rds")



#########################################################################
# what can we compare these bibles to?
tmp <- get_dataframe("clean_english","data2")






