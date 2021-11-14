#########################################################################
# forget word analysis for now, focus on one or the other

# in the first version we tokenize by line
# this is not very apples-to-apples since books have different structures

# abridge the texts so that they are of equal lengths

# include all punctuation in the character analysis

# take the first X characters **exactly as given** as if they were alien transmissions

# turn our raw text into character based dataframes

# ignore any structure for chapters, but acknowledge intentional "new lines" in the text

#########################################################################
library(dplyr)
library(stringr)
library(tictoc)

#########################################################################
raw_text_to_dataframe <- function(path_to_txt_file, source) {
  tic()
  
  # make a vector of all symbols
  symbols <- c()
  
  # get our text file and loop through it
  text_by_line <- readLines(path_to_txt_file)
  for(i in 1:length(text_by_line)) {
    
    # ignore designation of chapters or paragraphs
    if(!str_detect(text_by_line[i],"<<chapter") & !str_detect(text_by_line[i],"<<Chapter") & !str_detect(text_by_line[i],"<<paragraph")) {
      
      # we have an intentional blank line (two carriage returns)
      if(text_by_line[i]=="") {
        symbols <- c(symbols,"\n")
        symbols <- c(symbols,"\n")        
      }
      
      # we have something else...
      else {
        print(text_by_line[i])
        # take each symbol individually exactly as given
        symbols <- c(symbols,strsplit(text_by_line[i],"")[[1]])
        
        # include the carriage return
        symbols <- c(symbols,"\n")
        
        # stop after 100,000 symbols
        if(length(symbols) > 100000) {break}
      }
    }
  }
  
  # put into a dataframe for later use
  output = data.frame(source=source, 
                      symbol=symbols) %>%
    mutate(symbol_order=row_number()) %>%
    filter(symbol_order <= 100000)
  toc()  
  return(output)
}


# we imagine we have received a mysterious transmission from space 
# including 100,000 symbols we do not recognize...

# save everything as nice clean dataframes
dickens = raw_text_to_dataframe("../part1/raw_data/a_tale_of_two_cities.txt", "Tale of Two Cities")
saveRDS(dickens,"data/clean_dickens_first100000.Rds")

# save everything as nice clean dataframes
quijote = raw_text_to_dataframe("../part1/raw_data/don_quijote_part1.txt", "Don Quijote")
saveRDS(quijote,"data/clean_quijote_first100000.Rds")


# save everything as nice clean dataframes
lorem_ipsum = raw_text_to_dataframe("../part1/raw_data/lorem_ipsum.txt", "Lorem Ipsum")
saveRDS(lorem_ipsum,"data/clean_lorem_ipsum_first100000.Rds")


# save everything as nice clean dataframes
moby_dick = raw_text_to_dataframe("../part1/raw_data/moby_dick.txt", "Moby Dick")
saveRDS(moby_dick,"data/clean_moby_dick_first100000.Rds")


# save everything as nice clean dataframes
tom_sawyer = raw_text_to_dataframe("../part1/raw_data/the_adventures_of_tom_sawyer.txt", "Tom Sawyer")
saveRDS(tom_sawyer,"data/clean_tom_sawyer_first100000.Rds")

# save everything as nice clean dataframes
wild = raw_text_to_dataframe("../part1/raw_data/the_call_of_the_wild.txt", "Call of the Wild")
saveRDS(wild,"data/clean_wild_first100000.Rds")


