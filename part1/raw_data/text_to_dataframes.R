#########################################################
# settings go here
# 
# # The Adventures of Tom Sawyer
# text_name = "The Call of the Wild"
# text_input_file = "the_call_of_the_wild.txt"
# text_section_name = "Chapter"  # break into sections based on "<<" in text


# # The Adventures of Tom Sawyer
# text_name = "The Adventures of Tom Sawyer"
# text_input_file = "the_adventures_of_tom_sawyer.txt"
# text_section_name = "Chapter"  # break into sections based on "<<" in text


# # Moby Dick
# text_name = "Moby Dick"
# text_input_file = "moby_dick.txt"
# text_section_name = "Chapter"  # break into sections based on "<<" in text
# 
# # A Tale of Two Cities
# text_name = "A Tale of Two Cities"
# text_input_file = "a_tale_of_two_cities.txt"
# text_section_name = "Chapter"  # break into sections based on "<<" in text

# Lorem Ipsum
text_name = "Lorem Ipsum"
text_input_file = "lorem_ipsum.txt"
text_section_name = "Paragraph"  # break into sections based on "<<" in text

# # Don Quijote, Part 1
# text_name = "Don Quijote, Part 1"
# text_input_file = "don_quijote_part1.txt"
# text_section_name = "Chapter"  # break into sections based on "<<" in text

#########################################################
# libraries used
library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(tm)

#########################################################
# function to cleanup each line to play nice with NLP
play_nice_with_nlp <- function(char_vector) {
  
  # turn everything to lowercase
  char_vector <- tolower(char_vector)
  
  # remove any standard punctuation
  char_vector <- str_replace_all(char_vector, "—", " ")  
  char_vector <- removePunctuation(char_vector, preserve_intra_word_contractions=TRUE)  
  
  # deal with weirdness in Don Quijote
  char_vector <- str_replace_all(char_vector, "—", "")
  char_vector <- str_replace_all(char_vector, "¡", "")  
  char_vector <- str_replace_all(char_vector, "¿", "") 
  char_vector <- str_replace_all(char_vector, "«", "") 
  char_vector <- str_replace_all(char_vector, "»", "")   
  
  # deal with weirdness in Tale of Two Cities
  char_vector <- str_replace_all(char_vector, "‘", "")  
  char_vector <- str_replace_all(char_vector, "’", "")  
  char_vector <- str_replace_all(char_vector, "”", "")
  char_vector <- str_replace_all(char_vector, "“", "")  
  
  # return 
  return(char_vector)
}


#########################################################
# load our text file
text_by_line <- readLines(paste0("raw_data/",text_input_file))

# set all initial states
line_counter = 0
section_counter = 0
df <- data.frame()

#########################################################
# cycle through lines and make a dataframe
for(i in 1:length(text_by_line)) {
  
  # ignore any black lines
  if(text_by_line[i]!="") {
    
    # start of a new section
    if(str_detect(text_by_line[i], "<<")) {
      section_counter = section_counter+1
      line_counter = 0
      section_name = paste0(text_section_name," ",section_counter)
    }
    
    # part of a section
    else {
      if(play_nice_with_nlp(text_by_line[i])!="") {
        line_counter = line_counter + 1
        line_id = paste0(section_counter,"_",line_counter)
        df <- rbind(df, data.frame(source=text_name, section_name=section_name, section_no=section_counter, line_no=line_counter, line_id=line_id, clean_text=play_nice_with_nlp(text_by_line[i])))
      }
    }
  }
}

#########################################################
# break into words
words <- df %>%
  unnest_tokens(token, clean_text, token="ngrams", n=1) %>%
  group_by(line_id) %>%
  mutate(token_position=row_number()) %>%
  ungroup()

# save dataframe of words
saveRDS(words,paste0("clean_data/",str_replace(text_input_file,".txt","_words.Rds")))


#########################################################
# break into characters
characters <- df %>%
  mutate(clean_text=str_replace_all(str_trim(clean_text)," ","_")) %>%
  mutate(token = strsplit(clean_text,"")) %>% 
  unnest(token) %>%
  select(-clean_text) %>%
  group_by(line_id) %>%
  mutate(token_position=row_number()) %>%
  ungroup()

# save dataframe of characters
saveRDS(characters,paste0("clean_data/",str_replace(text_input_file,".txt","_characters.Rds")))


# sanity check
sort(unique(characters$token))

# check we got chapters right
check <- words %>%
  filter(line_no==1)
