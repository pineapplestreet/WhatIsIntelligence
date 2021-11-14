#########################################################################
# required libraries
library(dplyr)
library(ggplot2)
library(gganimate)
library(gridExtra)
library(scales)
library(stringr)
library(tidyr)

#########################################################################
# function to calculate entropy at each order (n)
calculate_entropy <- function(data, n) {
  
  # tell us what the script is up to
  tic()
  msg <- paste0(max(data$source)," for n=",n)
  print(msg)
  
  # 0th-order uses Shanon's Entropy
  if(n==0) {
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
  if(n > 0) {
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
# function to calculate entropies for a window of orders (e.g. 0-100)
calculate_higher_order_entropies <- function(input, starting_n, ending_n) {
  
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
get_combined_dataframe <- function(sources) {
  combined_data <- data.frame()
  for(item in sources) {
    tmp <- readRDS(paste0("data/",item,".Rds")) 
    combined_data <- rbind(combined_data,tmp)
  }
  return(combined_data)
}

#########################################################################
# returns a list of [scatter plot, bar plot, table for bar plot]
run_zipf_analysis <- function(data, max_rank, color_key) {
  
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
    facet_wrap(vars(source)) +
    scale_colour_manual(name=" ",
                        values = color_key)
  
  
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
  
  return(list(p1, p2, p2_table, p2_ggdata))
}



#########################################################################
# plot our higher-order entropy results
plot_entropy_results <- function(data, color_key) {
  p1 <- ggplot(data, aes(x=n, y=log10(entropy), color=source)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    ggtitle("Higher-Order Entropy") +
    labs(x="n", y="log10 nth-order entropy") +
    scale_colour_manual(name=" ",
                        values = color_key)
  return(p1)
}

#########################################################################
# plot an animated zipf bar plot
plot_animated_zipf_bar_plot <- function(data, sequence, color_key) {
  
  # run analysis for desired range of max_rank
  zipf_bar_data <- data.frame()
  for(i in sequence) {
    res = run_zipf_analysis(my_data, i, c("Lorem Ipsum"="#F8766D",
                                          "Don Quijote"="#B79F00",
                                          "Tale of Two Cities"="#00BA38",
                                          "Moby Dick"="#00BFC4",
                                          "Tom Sawyer"="#619CFF",
                                          "Call of the Wild"="#C77CFF"))
    zipf_bar_data <- rbind(zipf_bar_data,res[[4]])
  }
  #return(zipf_bar_data)
  
  
  # make bar plot of Zipfyness
  p2 <- ggplot(zipf_bar_data,aes(x=reorder(source,-abs_difference), y=slope, fill=source, label=my_label)) +
    geom_bar(stat="identity", color="black") +
    geom_text(size=3, position = position_stack(vjust = 0.5))  +
    geom_hline(yintercept=-1, lty=2) +
    labs(x=" ", y="Slope") +
    theme_bw() +
    theme(legend.position="none") +
    coord_flip() +
    scale_fill_manual(name=" ",
                      values = color_key) 
  
  p2 <- p2 +
    transition_time(max_rank) +
    labs(title = "Zipf Slopes: Top {frame_time} Symbols")
  
  return(p2)
}
