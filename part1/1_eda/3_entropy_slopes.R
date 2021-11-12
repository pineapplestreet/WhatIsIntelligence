#########################################################
# settings go here

# # include these sources
# sources <- c("lorem_ipsum",
#              "a_tale_of_two_cities",
#              "moby_dick",
#              "the_adventures_of_tom_sawyer",
#              "the_call_of_the_wild",
#              "don_quijote_part1")

# include these sources
sources <- c("the_call_of_the_wild")

##################################################################
# libraries I'm using
library(dplyr)
library(tictoc)
library(tidytext)
library(stringr)


##################################################################
# calculate Shanon's entropy (n=1) or conditional entropy (n>1)
calculate_entropy <- function(df, n) {
  
  # show our progress
  my_source <- max(df$source)
  my_mode <- max(df$mode)  
  print(paste0(my_source," - ",my_mode," - Iteration ",n))
  
  # break into n-grams of length n
  df <- df %>% unnest_tokens(ngram, line, token="ngrams", n=n, to_lower=FALSE) 
  
  # if n = 1 we use Shanon's Entropy
  if(n==1) {
    probs <- df %>%
      group_by(ngram) %>%
      summarise(count=n()) %>%
      mutate(p = count/nrow(df),
             log2p = log2(p),
             entropy = p*log2p) 
    entropy <- sum(probs$entropy)*-1
    return(data.frame(source=my_source, mode=my_mode, n=n, entropy=entropy, total_choices=length(unique(probs$ngram)), avg_choices_per_node=length(unique(probs$ngram)), note="Shannon's Entropy"))
  }
  
  # if n > 1 we need Conditional Entropy
  else if(n > 1) {
    
    # Stamp gives us the the formula
    # P(abc) log(P(c | ab))
    # example: for n = 2 we want P(ab)*log(P(b|a)) => p_ngram * log2(p_b_given_a)
    # example: for n = 3 we want P(abc)*log(P(c|ab)) => p_ngram * log2(p_b_given_a)
    # example: for n = 5 we want P(abcde)*log(P(e|abcd)) => p_ngram * log2(p_b_given_a)
    
    # for simple code we just use p(B|A) regardless of length
    tmp <- df %>%
      mutate(A = word(ngram, 1, (n-1)),
             B = word(ngram,-1))
    
    # if n is greater than the available length of a line, remove it
    tmp <- tmp %>%
      filter(!is.na(ngram))
    
    # if nothing exists with the given length, we have no entropy
    if(nrow(tmp)==0) {return(data.frame(source=my_source, mode=my_mode, n=n, entropy=0, total_choices=0, avg_choices_per_node=0, note="Doesn't Exist"))}

    # get freq(B|A)
    freq_ab <- tmp %>%
      group_by(A,B) %>%
      summarise(freq_AB=n())
    tmp <- tmp %>% left_join(freq_ab, by=c("A", "B"))
    
    # find p(ngram)
    p_ngram <- tmp %>%
      group_by(ngram) %>%
      summarise(freq=n()) %>%
      mutate(total=nrow(tmp),
             p_ngram=freq/total) %>%
      select(ngram, p_ngram)
    tmp <- tmp %>% left_join(p_ngram)
    
    # find freq(A)
    freq_A <- tmp %>%
      group_by(A) %>%
      summarise(freq_A=n(),
                choices=length(unique(B))) %>%
      select(A, freq_A, choices)
    tmp <- tmp %>% left_join(freq_A)  
    
    # find p(b_given_a) and entropy
    tmp <- tmp %>%
      mutate(p_b_given_a=freq_AB/freq_A) %>%
      mutate(log2p = log2(p_b_given_a)) %>%
      mutate(entropy=p_ngram*log2p*-1)
    
    # de-dupe so we have one row per ngram
    tmp <- tmp %>%
      group_by(ngram) %>%
      summarise(choices=first(choices),
                entropy=first(entropy))
    return(data.frame(source=my_source, mode=my_mode, n=n, entropy=sum(tmp$entropy), total_choices=sum(tmp$choices), avg_choices_per_node=mean(tmp$choices), note="Conditional Entropy"))    
  }
}

# 
# ##################################################################
# # debug
# tmp <- readRDS(paste0("clean_data/the_call_of_the_wild_words.Rds")) 
# input <- tmp %>%
#   group_by(line_id) %>%
#   summarise(line=paste(token, collapse= " ")) %>%
#   mutate(source=max(tmp$source)) %>%
#   mutate(mode="words")
# 
# res = calculate_entropy(input, 5)




##################################################################
# save all of our results
entropy_results <- data.frame()

# load all data sources of interest in order
for(item in sources) {
  #for(zipf_mode in c("words","characters")) {
  for(zipf_mode in c("words")) {
    
    # put all tokens on the same line so we can use tidytext for n-grams
    tmp <- readRDS(paste0("clean_data/",item,"_",zipf_mode,".Rds")) 
    input <- tmp %>%
      group_by(line_id) %>%
      summarise(line=paste(token, collapse= " ")) %>%
      mutate(source=max(tmp$source)) %>%
      mutate(mode=zipf_mode)
    
    # iterate until n = 100
    n <- 0
    #while(n < 100) {
    while(n < 10) {
      n = n + 1
      tic()
      entropy_results <- rbind(entropy_results, calculate_entropy(input, n))
      saveRDS(entropy_results, "1_eda/entropy_results.Rds")  
      toc()
    }
  }
}

# save our results
#saveRDS(entropy_results, "1_eda/entropy_results.Rds")



