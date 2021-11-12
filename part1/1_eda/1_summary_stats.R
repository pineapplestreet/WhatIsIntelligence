#########################################################
# settings go here

# include these sources
sources <- c("lorem_ipsum",
             "a_tale_of_two_cities",
             "moby_dick",
             "the_adventures_of_tom_sawyer",
             "the_call_of_the_wild",
             "don_quijote_part1")


#########################################################
# libraries I am using
library(dplyr)
library(tidyr)

# summary stats go here
summary_stats <- data.frame()

# look at all sources
for(source in sources) {

  # get our data
  words <- readRDS(paste0("clean_data/",source,"_words.Rds"))  
  characters <- readRDS(paste0("clean_data/",source,"_characters.Rds"))
  
  # summary stats by character
  row <- data.frame(Source=max(words$source),
                    Tokenized="By Character") %>%
    mutate(`Total Tokens`=nrow(characters)) %>%
    mutate(`Unique Tokens`=length(unique(characters$token))) %>%
    mutate(`Hartley Bits per Token`=log2(`Unique Tokens`)) %>%
    mutate(`Total Information (Hartley Bits)`=`Hartley Bits per Token`*`Total Tokens`)
  summary_stats <- rbind(summary_stats,row)
  
  
  # summary stats by character
  row <- data.frame(Source=max(words$source),
                    Tokenized="By Word") %>%
    mutate(`Total Tokens`=nrow(words)) %>%
    mutate(`Unique Tokens`=length(unique(words$token))) %>%
    mutate(`Hartley Bits per Token`=log2(`Unique Tokens`)) %>%
    mutate(`Total Information (Hartley Bits)`=`Hartley Bits per Token`*`Total Tokens`)
  summary_stats <- rbind(summary_stats,row)
}

characters <- readRDS(paste0("clean_data/",source,"_characters.Rds"))


check <- readRDS(paste0("clean_data/the_adventures_of_tom_sawyer_characters.Rds"))
sort(unique(check$token))

#write.csv(summary_stats,"summary_stats.csv")