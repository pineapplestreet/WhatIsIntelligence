#########################################################################
# required libraries
source("_shared_functions.R") 

#########################################################################
# get summary stats on the data of interest
my_data <- get_combined_dataframe(c("clean_dickens_first100000",
                         "clean_lorem_ipsum_first100000",
                         "clean_moby_dick_first100000",
                         "clean_quijote_first100000",
                         "clean_tom_sawyer_first100000",
                         "clean_wild_first100000"))

# get summary stats
summary_stats <- my_data %>%
  group_by(source) %>%
  summarise(`Total Tokens`=n(),
            `Unique Tokens`=length(unique(symbol))) %>%
  mutate(Source=source,
         Tokenized="by symbol") %>%
  select(Source, Tokenized, `Total Tokens`, `Unique Tokens`) %>%
  mutate(`Hartley Bits per Token`=round(log2(`Unique Tokens`),2)) %>%
  mutate(`Total Information (Hartley Bits)`=`Hartley Bits per Token`*`Total Tokens`) %>%
  mutate(`Total Tokens`=formatC(`Total Tokens`, big.mark=",")) %>%
  mutate(`Total Information (Hartley Bits)`=format(`Total Information (Hartley Bits)`, big.mark=",",scientific=F))