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
        if(length(symbols) > 10000) {break}
      }
    }
  }
  
  # put into a dataframe for later use
  output = data.frame(source=source, 
                      symbol=symbols) %>%
    mutate(symbol_order=row_number()) %>%
    filter(symbol_order <= 10000)
  toc()  
  return(output)
}

original = raw_text_to_dataframe("data/the_call_of_the_wild.txt", "Call of the Wild")
saveRDS(original,"data2/original.Rds")

models <- c("GPT-NEO-1.3B",
            "GPT-NEO-2.7B",
            "GPT-J-6B")
for(m in models) {
  print(m)
  data = raw_text_to_dataframe(paste0("data/",m,".txt"), m)
  saveRDS(data,paste0("data2/",m,".Rds"))
}
