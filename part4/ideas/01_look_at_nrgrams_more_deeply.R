#########################################################################
# dig deeper into ngrams

#########################################################################
# required libraries
source("../_global_shared_functions.R") 

# lets use the first 10,000 symbols
tmp <- readRDS("../part3/data/clean_wild_first100000.Rds") %>%
  filter(symbol_order <= 10000)
saveRDS(tmp,"data/clean_wild_first10000.Rds")




#########################################################################
# get our ngrams
source("../_global_shared_functions.R") 
symbols <- get_dataframe("clean_wild_first10000")
ngrams <- get_ngrams_iterate(symbols, seq(1,100,1))


#########################################################################
# can we use a "cumulative freq" to extract ngrams of greater length from unknown text?

#--------------------------------------------------------
# step1: get the frequencies of all ngrams
tic()
step1 <- ngrams %>%
  group_by(n, ngram) %>%
  summarise(freq=n()) %>% 
  filter(!str_detect(ngram,"\n"))
toc()

#--------------------------------------------------------
# step2: ngrams are just what they are
ngram_cum_freq <- step1 %>%
  filter(n==1) %>%
  mutate(cum_freq=freq)

#--------------------------------------------------------
# step3: bigrams are "boosted" by matching unigrams
# start with up to 10 levels deep...
tic()
for(i in 1:25) {
  
  # here are the ngrams of interest
  tmp <- step1 %>%
    filter(n==i)
  
  # shorter ngrams are used to "support" the cum freq of the longer ngram
  support <- step1 %>%
    filter(n < i)
  
  # this is criminally ineffecient but easy to follow and debug
  # took at each igram and find cum freq
  for(j in 1:nrow(tmp)) {
    my_ngram <- tmp[j,]$ngram
    print(my_ngram)
    
    # find matching support ngrams
    matches <- support  %>%
      mutate(match=ifelse(str_detect(my_ngram,coll(ngram, ignore_case = FALSE)),1,0)) %>%
      filter(match==1)
    
    # cum freq of this ngram
    row <- data.frame(n=tmp[j,]$n,
                      ngram=my_ngram,
                      freq=tmp[j,]$freq,
                      cum_freq=tmp[j,]$freq + sum(matches$freq))
    ngram_cum_freq <- rbind(ngram_cum_freq,row)
  }
}
toc()


