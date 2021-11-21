#########################################################################
library(dplyr)
library(stringr)
library(tictoc)

#########################################################################
raw_text_to_dataframe <- function(path_to_txt_file, source, reverse=FALSE) {
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
        
        # reverse the text (to read Hebrew backwards, left-to-right)
        if(reverse) {
          splits <- strsplit(text_by_line[i], "")[[1]]
          reversed <- rev(splits)
          final_result <- paste(reversed, collapse = "")
          text_by_line[i] <- final_result
        }
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

english = raw_text_to_dataframe("raw_text/KJV_english.txt", "English (4,088)")
saveRDS(english,"data2/clean_english.Rds")

chinese = raw_text_to_dataframe("raw_text/KJV_chinese.txt", "Chinese (983)")
saveRDS(chinese,"data2/clean_chinese.Rds")

hebrew = raw_text_to_dataframe("raw_text/KJV_hebrew.txt", "Hebrew (2,136)") 
saveRDS(hebrew,"data2/clean_hebrew.Rds")


hebrew2 = raw_text_to_dataframe("raw_text/KJV_hebrew.txt", "Hebrew, backwards (2,136)", reverse=TRUE) # read left-to-right which is wrong
saveRDS(hebrew2,"data2/clean_hebrew2.Rds")


english2 = raw_text_to_dataframe("raw_text/KJV_english.txt", "English, 0th-order (4,088)")
options <- english2 %>%
  group_by(symbol) %>%
  summarise(freq=n())

english2_random <- sample_n(options, size=4088, replace=TRUE)
english2_sampled <- sample_n(options, size=4088, replace=TRUE, weight=freq)
english2_final <- data.frame(source="English, 0th-order (4,088)", symbol=english2_sampled$symbol) %>%
  mutate(symbol_order=row_number())
saveRDS(english2_final,"data2/clean_english2.Rds")

fix <- readRDS("data2/clean_english2.Rds") %>%
  mutate(source="English, 1st-order (4,088)")
saveRDS(fix,"data2/clean_english2.Rds")



