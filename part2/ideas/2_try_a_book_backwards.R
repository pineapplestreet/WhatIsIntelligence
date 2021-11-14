#########################################################################
# can we find a counter-example by simply running a "backwards book"?


#########################################################################
library(dplyr)

# use "A Tale of Two Cities" 
words <- readRDS("data/a_tale_of_two_cities_words.Rds")

"it was the best of times it was the worst of times it was the time "

# keep all words on the same line, just reverse them
backwards <- words %>%
  arrange(section_no, line_no, desc(token_position))