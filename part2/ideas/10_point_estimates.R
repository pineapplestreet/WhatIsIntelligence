#########################################################################
# can we invent a point estimate?

#########################################################################
# required libraries
source("_shared_functions.R") 


#########################################################################
#########################################################################
# visualize entropy
my_data <- get_combined_dataframe(c(
  "entropy_results_dickens_first100000",
  "entropy_results_lorem_ipsum_first100000",
  "entropy_results_101_to_200_lorem_ipsum_first100000",  
  "entropy_results_moby_dick_first100000",
  "entropy_results_quijote_first100000",
  "entropy_results_tom_sawyer_first100000",
  "entropy_results_wild_first100000",
  "entropy_results_dickens_first100000_random",
  "entropy_results_dickens_first100000_chaos",
  #"entropy_results_dickens_first100000_vocab6",
  "entropy_results_dickens_first100000_vocab7",
  "entropy_results_dickens_first100000_bigram7",
  "entropy_results_101_to_200_dickens_first100000_bigram7",  
  "entropy_results_dickens_first100000_trigram7",
  "entropy_results_101_to_200_dickens_first100000_trigram7"
  #"entropy_results_dickens_first100000_vocab8"  
  # "entropy_results_dickens_first100000_vocab25",
  # "entropy_results_dickens_first100000_vocab50",
  # "entropy_results_dickens_first100000_vocab100",
  # "entropy_results_dickens_first100000_vocab200"
)) %>%
  filter(entropy>0) %>%
  mutate(log10_entropy=log10(entropy))

# use the sum of log10_entropy (ignoring -Inf for 0 entropy)
results <- data.frame()
for(i in seq(0,160,5)) {
  print(i)
  tmp <- my_data %>%
    filter(n <= i) %>%
    group_by(source) %>%
    summarise(sum_log10_entropy=sum(log10_entropy)) %>%
    mutate(n=i) %>%
    mutate(rank=rank(desc(sum_log10_entropy))) %>%
    mutate(new_label=paste0(source,": ",round(sum_log10_entropy,2))) %>%
    mutate(jojo=n)
  results <- rbind(results,tmp)
}


# make our animated plot
p2 <- ggplot(results) +
  geom_col(aes(x=rank, y=sum_log10_entropy,
               group=source, fill=source),
           color="black",
           width=0.4) +
  geom_text(aes(x=rank, y=0,
                label=new_label, group=source),
            hjust=1.25) +
  geom_hline(yintercept=-1, lty=2) +
  theme_minimal() + ylab('Cumulative Higher Order Entopy') +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position="none",
        plot.margin = unit(c(2,2,2,2),
                           'lines')) +
  scale_fill_manual(name=" ",
                    values = c("Lorem Ipsum"="#bf534b",
                               "Don Quijote"="#00BA38",
                               "Tale of Two Cities"="#00BA38",
                               "Moby Dick"="#00BA38",
                               "Tom Sawyer"="#00BA38",
                               "Call of the Wild"="#00BA38",
                               "Tale of Two Cities (random)"="#bf534b",
                               "Tale of Two Cities (chaos)"="#bf534b" ,                                     
                               "Tale of Two Cities (vocab 7)"="#bf534b",
                               "Tale of Two Cities (bigram 7)"="#bf534b",
                               "Tale of Two Cities (trigram 7)"="#bf534b")) +
  coord_flip(clip='off') + 
  ggtitle('Cumulative Higher-Order Entopy: Through {closest_state}th-Order') +             # title with the timestamp period
  transition_states(jojo,
                    transition_length = 2,
                    state_length = 0) +
  exit_fly(x_loc = 0, y_loc = 0) +         # chart exit animation params
  enter_fly(x_loc = 0, y_loc = 0) +
  ease_aes('cubic-in-out')
p2
anim_save("animated_plot_sum_log_entropy.gif")
