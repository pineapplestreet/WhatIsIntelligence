#########################################################################
# required libraries
library(corrplot)
library(dplyr)
library(ggplot2)
library(gganimate)
library(gridExtra)
library(Hmisc)
library(scales)
library(stringr)
library(tictoc)
library(tidyr)
library(transformr)
library(zoo)

#########################################################################
# load a single dataframe
get_dataframe <- function(item, folder="data") {
  tmp <- readRDS(paste0(folder,"/",item,".Rds")) 
  return(tmp)
}

#########################################################################
# load up all the needed Rds files as a combined dataframe
get_combined_dataframe <- function(sources, folder="data") {
  print("Global Shared")
  combined_data <- data.frame()
  for(item in sources) {
    print(item)
    print(folder)
    tmp <- get_dataframe(item, folder)
    combined_data <- rbind(combined_data,tmp)
  }
  return(combined_data)
}

#########################################################################
# function to make ngrams
# only include ngrams of length n, ignore the cutoff
get_ngrams <- function(data, n) {
  ngrams <- data %>%
    mutate(
      ngram=sapply(
        1:nrow(.),
        function(x) paste(symbol[pmax(1, x):pmin(x + n-1, nrow(.))], collapse = "")
      )) %>%
    mutate(length=str_length(ngram)) %>%
    filter(length==n) %>%
    mutate(n=length) %>%
    select(source, n, symbol_order, ngram)
}  

#########################################################################
# function to make ngrams of length x to y
get_ngrams_iterate <- function(data, sequence) {
  response <- data.frame()
  for(i in sequence) {
    print(paste0(max(data$source),", n=",i))
    response<- rbind(response,get_ngrams(data, i))
  }
  return(response)
} 


#########################################################################
# get our summary stats
get_summary_stats <- function(my_data) {
  print("Global Shared")
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
  return(summary_stats)
}


#########################################################################
# function to calculate entropies for a window of orders (e.g. 0-100)
calculate_higher_order_entropies <- function(input, starting_n, ending_n) {
  print("Global Shared")
  
  # doesn't make sense
  if(starting_n > ending_n) {return(NULL)}
  
  # hold all of our results
  entropy_results <- data.frame()  
  
  # loop-de-loop
  n <- starting_n
  while(n <= ending_n) {
    entropy_results <- rbind(entropy_results, calculate_entropy(input, n))    
    n <- n+1
  }
  
  # all done
  return(entropy_results)
}


#########################################################################
# function to calculate entropy at each order (n)
calculate_entropy <- function(data, n) {
  print("Global Shared")
  
  # tell us what the script is up to
  tic()
  msg <- paste0(max(data$source)," for n=",n)
  print(msg)
  
  # 0th-order uses Harley Bits
  if(n==0) {
    probs <- data %>%
      group_by(symbol) %>%
      summarise(freq=n()) 
    entropy <- log2(nrow(probs))
    toc()
    return(data.frame(source=max(data$source), n=n, entropy=entropy))
  }
  
  # 1st-order uses Shanon's Entropy
  if(n==1) {
    probs <- data %>%
      group_by(symbol) %>%
      summarise(freq=n()) %>%
      mutate(total=nrow(data)) %>%
      mutate(p = freq/total,
             log2p = log2(p),
             entropy = p*log2p) 
    entropy <- sum(probs$entropy)*-1
    toc()
    return(data.frame(source=max(data$source), n=n, entropy=entropy))
  }
  
  # nth-order uses Conditional Entropy
  if(n > 1) {
    # Stamp gives us the the formula
    # P(abc) log(P(c | ab))
    # example: for n = 1 we want P(ab)*log(P(b|a)) => p_ngram * log2(p_b_given_a)
    # example: for n = 2 we want P(abc)*log(P(c|ab)) => p_ngram * log2(p_b_given_a)
    # example: for n = 3 we want P(abcd)*log(P(d|abc)) => p_ngram * log2(p_b_given_a)
    
    # break into ngrams of length n
    ngrams <- data %>%
      mutate(
        ngram=sapply(
          1:nrow(.),
          function(x) paste(symbol[pmax(1, x):pmin(x + n, nrow(.))], collapse = "")
        ),
        length=str_length(ngram),
        A = substr(ngram, 1, n),
        B = substr(ngram, n+1, length(ngram))) %>%
      filter(length > n) # ignore all ngrams when we run out of words
    
    # get freq(B|A)
    freq_ab <- ngrams %>%
      group_by(A,B) %>%
      summarise(freq_AB=n())
    ngrams <- ngrams %>% left_join(freq_ab, by=c("A", "B"))
    
    # find p(ngram)
    p_ngrams <- ngrams %>%
      group_by(ngram) %>%
      summarise(freq=n()) %>%
      mutate(total=nrow(ngrams),
             p_ngram=freq/total) %>%
      select(ngram, p_ngram)
    ngrams <- ngrams %>% left_join(p_ngrams)
    
    # find freq(A)
    freq_A <- ngrams %>%
      group_by(A) %>%
      summarise(freq_A=n(),
                choices=length(unique(B))) %>%
      select(A, freq_A, choices)
    ngrams <- ngrams %>% left_join(freq_A) 
    
    # find p(b_given_a) and entropy
    ngrams <- ngrams %>%
      mutate(p_b_given_a=freq_AB/freq_A) %>%
      mutate(log2p = log2(p_b_given_a)) %>%
      mutate(entropy=p_ngram*log2p*-1)
    
    # de-dupe so we have one row per ngram
    ngrams <- ngrams %>%
      group_by(ngram) %>%
      summarise(choices=first(choices),
                entropy=first(entropy))
    
    # entropy is our sum per-ngram
    entropy = sum(ngrams$entropy)
    toc()
    return(data.frame(source=max(data$source), n=n, entropy=entropy))
  }  
}




#########################################################################
# Condition #1 - Zipf
# returns a list of [scatter plot, bar plot, table for bar plot]
run_condition1_analysis <- function(data, max_rank, color_key) {
  
  print("Global Shared")
  
  # find our most common symbols
  zipf_results <- data %>%
    group_by(source, symbol) %>%
    summarise(freq=n()) %>%
    mutate(rank=min_rank(desc(freq)),
           log_freq=log10(freq),
           log_rank=log10(rank))
  
  # limit to a given max rank
  if(max_rank > 0) {
    zipf_results <- zipf_results %>% filter(rank<=max_rank)
    my_title <- paste0("Zipf Slopes: Top ",max_rank," Symbols")
  } else {
    my_title <- paste0("Zipf Slopes: All Symbols")
  }
  
  # avg intercept
  tmp <- zipf_results %>%
    filter(rank==1)
  avg_intercepts = mean(tmp$log_freq)
  
  
  # make a plot of Zipf Slopes
  p1 <- ggplot(zipf_results,aes(x=log_rank, y=log_freq, color=source)) +
    geom_point() +
    ggtitle(my_title) +
    labs(x="log10 Rank", y="log10 Frequency") +
    geom_smooth(method="lm", se=FALSE) +
    geom_abline(slope=-1,intercept=avg_intercepts, color="black", lty=2) +
    theme_bw() +
    theme(legend.position="none") +
    facet_wrap(vars(source), ncol=3) +
    scale_colour_manual(name=" ",
                        values = color_key)
  
  p1_ggdata <- zipf_results %>%
    mutate(max_rank=max_rank)
  
  # determine best fit slope by linear regression
  lm_results <- data.frame()
  for(my_source in unique(zipf_results$source)) {
    tmp <- zipf_results %>% 
      filter(source==my_source)
    model <- lm(data=tmp, log_freq~log_rank)
    lm_results <- rbind(lm_results, data.frame(max_rank=max_rank,source=my_source,slope=model$coefficients[2],r2=summary(model)$r.squared))
  }
  p2_table <- lm_results %>%
    arrange(slope)
  
  # clean up the data for ggplot
  ggdata <- p2_table %>%
    mutate(abs_difference=abs(-1-slope)) %>%
    mutate(my_label=paste0(round(slope,2)," slope, ",round(r2,2)," r2, ",round(abs_difference,2)," abs error")) %>%  
    arrange(abs_difference)
  p2_ggdata <- ggdata
  
  # make bar plot of Zipfyness
  p2 <- ggplot(ggdata,aes(x=reorder(source,-abs_difference), y=slope, fill=source, label=my_label)) +
    geom_bar(stat="identity", color="black") +
    ggtitle(my_title) +
    geom_text(size=3, position = position_stack(vjust = 0.5))  +
    geom_hline(yintercept=-1, lty=2) +
    labs(x=" ", y="Slope") +
    theme_bw() +
    theme(legend.position="none") +
    coord_flip() +
    scale_fill_manual(name=" ",
                      values = color_key)  
  
  return(list(p1, p2, p2_table, p2_ggdata, p1_ggdata))
}


#########################################################################
# Condition #2 - Higher-Order Entropy
# plot our higher-order entropy results
plot_condition2 <- function(data, color_key, hide_legend=FALSE) {
  print("Global Shared")
  p1 <- ggplot(data, aes(x=n, y=log10(entropy), color=source)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    ggtitle("Higher-Order Entropy") +
    labs(x="n", y="log10 nth-order entropy") +
    scale_colour_manual(name=" ",
                        values = color_key)
  if(hide_legend) {
    p1 <- p1 + theme(legend.position = "none")
  }
  return(p1)
}


#######################################################################
# 3rd condition: 0th to 5th order entropy linearity
# returns list: [all slopes, ]
run_condition3_analysis <- function(entropy_data, color_key) {
  print("Global Shared")
  
  # look at first 5 orders only
  input_data <- entropy_data %>% 
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
  out1 <- data.window
  
  
  # look at average slope and variance for each source
  summary <- data.window %>%
    group_by(source) %>%
    summarise(avg_slope=mean(slope),
              variance=var(slope))
  out2 <- summary 
  
  # plot the rank by entropy
  p1 <- ggplot(out2, aes(x=reorder(source,-variance), y=variance, fill=source, label=round(variance,4))) +
    geom_bar(stat="identity", color="black") +
    geom_text(size=3, position = position_stack(vjust = 0.5))  +
    theme_bw() +
    scale_fill_manual(name=" ",
                      values = color_key) +
    theme(legend.position="none") +
    coord_flip() +
    labs(x=" ", y="Slope Variance") +
    ggtitle("Entropy Slope Variance, 0th-5th") +
    geom_hline(yintercept = 0.01, lty=2)
  
  # return list of output
  return(list(out1, out2, p1))
}
