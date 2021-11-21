#########################################################################
# let's explore the curve a bit more

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


#######################################################################
# visualize what we are doing
tmp <- my_data %>% 
  mutate(log10_entropy=log10(entropy)) %>%
  filter(n <= 5) %>%
  filter(source=="Lorem Ipsum")


window <- tmp %>%
  filter(n >= 4) %>%
  filter(n <= 5)  

p5 <- ggplot() +
  geom_point(data=tmp,aes(x=n, y=log10_entropy, color=source)) +
  geom_smooth(data=window,aes(x=n, y=log10_entropy, color=source), method="lm", se=FALSE) +
  labs(x="nth-order", y="log10 entropy") +
  theme_bw() +
  theme(legend.position="none") +
  facet_wrap(vars(source)) 

grid.arrange(p1,p2,p3,p4,p5, nrow=2)
p1





#######################################################################
# this looks promising...

# look at first 5 orders only
input_data <- my_data %>% 
  mutate(log10_entropy=log10(entropy)) %>%
  filter(n <= 5)

# hold our results
data.window <- data.frame()

# cycle through all sources
for(item in unique(input_data$source)) {
  print(item)
  
  # window through the first 5 slopes
  for(i in seq(0,4,1)) {
    window <- input_data %>%
      filter(source==item) %>%
      filter(n >= i) %>%
      filter(n <= i+1)  

    # get slope via linear regression (only 2 points, perfect regression)
    model <- lm(data=window, log10_entropy~n)
    data.window <- rbind(data.window, data.frame(source=item, iteration=i, slope=model$coefficients[2]))
  }
}



# look at average slope and variance for each source
summary <- data.window %>%
  group_by(source) %>%
  summarise(avg_slope=mean(slope),
            variance=var(slope))






p1 <- ggplot(summary, aes(x=reorder(source,-variance), y=variance, fill=source)) +
  geom_bar(stat="identity", color="black") +
  theme_bw() +
  scale_fill_manual(name=" ",
                    values = c("Lorem Ipsum"="#F8766D",
                               "Don Quijote"="#B79F00",
                               "Tale of Two Cities"="#00BA38",
                               "Moby Dick"="#00BFC4",
                               "Tom Sawyer"="#619CFF",
                               "Call of the Wild"="#C77CFF",
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
                               "Tale of Two Cities (trigram 7)"="#bf534b")) +
  theme(legend.position="none") +
  coord_flip() +
  labs(x=" ", y="Mean Slope Variance") +
  ggtitle("0th to 5th-Order Entropy")  


p2 <- ggplot(summary %>% filter(variance <= 0.007), aes(x=reorder(source,-variance), y=variance, fill=source)) +
  geom_bar(stat="identity", color="black") +
  theme_bw() +
  scale_fill_manual(name=" ",
                    values = c("Lorem Ipsum"="#F8766D",
                               "Don Quijote"="#B79F00",
                               "Tale of Two Cities"="#00BA38",
                               "Moby Dick"="#00BFC4",
                               "Tom Sawyer"="#619CFF",
                               "Call of the Wild"="#C77CFF",
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
                               "Tale of Two Cities (trigram 7)"="#bf534b")) +
  theme(legend.position="none") +
  coord_flip() +
  labs(x=" ", y="Mean Slope Variance") +
  ggtitle("0th to 5th-Order Entropy")
p2

grid.arrange(p1,p2, nrow=1)


#######################################################################
# does the average length of words correlate to this?
get_word_stats <- function(items) {
  
  # cycle through all items provided
  data.summary <- data.frame()
  for(item in items) {
    print(item)
    
    # break our first 100,000 symbols into words
    words <- readRDS(paste0("data/",item,".Rds")) %>%
      mutate(word_break=ifelse(symbol %in% c(" ","\n","-",",",".",";"),1,0)) %>%
      mutate(word=1+cumsum(word_break)) %>%
      filter(word_break==0) %>%
      group_by(source, word) %>%
      summarise(word_string=paste(symbol, collapse = "")) %>%
      mutate(word_length=str_length(word_string))
    #return(words)
    
    # get summary stats
    row <- data.frame(source=max(words$source),
                      min_word_length=min(words$word_length),
                      max_word_length=max(words$word_length),                    
                      mean_word_length=mean(words$word_length),
                      unique_words=length(unique(words$word_string)))
    row <- row %>% mutate(jojo=unique_words*mean_word_length)
    data.summary <- rbind(data.summary, row)
  }
  
  # return the data
  return(data.summary)
}
word_stats = get_word_stats(c(
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

