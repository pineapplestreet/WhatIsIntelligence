#########################################################################
# required libraries
source("../_global_shared_functions.R") 

#########################################################################
# get all of the data of interest
my_data <- get_combined_dataframe(sources=c(
  "Original",
  "LSTM-Overfit",
  "LSTM-1",
  "LSTM-1000",
  "GPT-NEO-1.3B",
  "GPT-NEO-2.7B",
  "GPT-J-6B",
  "Tom Sawyer",
  "Don Quijote",
  "Moby Dick",
  "Lorem Ipsum",
  "Tale of two Cities"), 
  folder="data2")

# order factor
my_data$source <- factor(my_data$source, levels=c(
  "LSTM-1",
  "LSTM-1000",
  "LSTM-Overfit",    
  "GPT-NEO-1.3B",
  "GPT-NEO-2.7B",
  "GPT-J-6B",  
  "Call of the Wild",  
  "Don Quijote",
  "Lorem Ipsum",
  "Moby Dick",
  "Tale of two Cities",
  "Tom Sawyer"))

# map colors
map_colors <- c(
  "LSTM-1"="#482878",
  "LSTM-1000"="#B4DE2C",
  "LSTM-Overfit"="#FDE725",  
  "GPT-NEO-1.3B"="#945319",
  "GPT-NEO-2.7B"="#cc6910",
  "GPT-J-6B"="#fc9803",
  "Call of the Wild"="#C77CFF",  
  "Don Quijote"="#B79F00",
  "Lorem Ipsum"="#F8766D",  
  "Moby Dick"="#00BFC4",
  "Tale of two Cities"="#00BA38",  
  "Tom Sawyer"="#619CFF"
)

#########################################################################
# get summary stats
summary_stats <- get_summary_stats(my_data) %>% arrange(`Unique Tokens`)


#########################################################################
# Condition #1

# use 0 for ALL tokens
# or set a MAX RANK to cut off the right tail
cond1 = run_condition1_analysis(my_data, 0, map_colors)
grid.arrange(cond1[[1]],cond1[[2]],widths = c(3, 2), nrow=1)

# 
# #########################################################################
# # calculate higher-order entropy
# my_data_eval <- my_data %>% filter(source %in% c(
#   "Tom Sawyer",
#   "Moby Dick",
#   "Lorem Ipsum",
#   "Don Quijote",
#   "Tale of two Cities")) 
# for(my_source in unique(my_data_eval$source)) {
#   print(my_source)
#   tmp = my_data %>% filter(source==my_source)
#   results = calculate_higher_order_entropies(tmp,0,100)
#   saveRDS(results,paste0("data3/entropy_",my_source,".Rds"))
# }

#########################################################################
# Condition #2
entropy_data <- get_combined_dataframe(sources=c(
  "entropy_LSTM-Overfit",
  "entropy_LSTM-1000",
  "entropy_LSTM-1",
  "entropy_GPT-NEO-1.3B",
  "entropy_GPT-NEO-2.7B",
  "entropy_GPT-J-6B",
  "entropy_Call of the Wild",
  "entropy_Tom Sawyer",
  "entropy_Don Quijote",
  "entropy_Moby Dick",
  "entropy_Lorem Ipsum",
  "entropy_Tale of two Cities"
), 
folder="data3") 

entropy_data$source <- factor(entropy_data$source, levels=c(
  "LSTM-1",
  "LSTM-1000",
  "LSTM-Overfit",    
  "GPT-NEO-1.3B",
  "GPT-NEO-2.7B",
  "GPT-J-6B",  
  "Call of the Wild",  
  "Don Quijote",
  "Lorem Ipsum",
  "Moby Dick",
  "Tale of two Cities",
  "Tom Sawyer"))

plot <- plot_condition2(entropy_data %>% filter(n<40),map_colors)# + facet_wrap(vars(source), ncol=3) + theme(legend.position="none")
plot

tmp_map_colors <- c(
  "Call of the Wild"="#C77CFF",
  "LSTM-Overfit"="#FDE725",
  "LSTM-1000"="#B4DE2C",
  "GPT-NEO-1.3B"="#945319",
  "GPT-NEO-2.7B"="#cc6910",
  "GPT-J-6B"="#fc9803"
)
tmp <- entropy_data %>%
  filter(source %in% c("Call of the Wild",
                       "LSTM-Overfit",
                       "LSTM-1000",
                       "GPT-NEO-1.3B",
                       "GPT-NEO-2.7B",
                       "GPT-J-6B")) %>%
  filter(n < 50)
plot <- plot_condition2(tmp,tmp_map_colors) 
plot

plot2 <- plot_condition2(entropy_data %>% filter(n < 6),map_colors, TRUE)
plot3 <- plot_condition2(entropy_data %>% filter(n < 30),map_colors)
grid.arrange(plot2,plot3,widths = c(2, 3), nrow=1)

# checking long-tail entropy
check <- entropy_data %>% filter(source=="LSTM-50")
my_data <- get_combined_dataframe(sources=c(
  "LSTM-Overfit"), 
  folder="data2")

# break into ngrams of length n
n = 100
ngrams <- my_data %>%
  mutate(
    ngram=sapply(
      1:nrow(.),
      function(x) paste(symbol[pmax(1, x):pmin(x + n, nrow(.))], collapse = "")
    ),
    length=str_length(ngram),
    A = substr(ngram, 1, n),
    B = substr(ngram, n+1, length(ngram))) %>%
  filter(length > n) %>%
  group_by(ngram) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

# find our max nth order entropy
max_order_entropy <- entropy_data %>%
  filter(entropy > 0) %>%
  group_by(source) %>%
  summarise(max_n=max(n))

plot_special <- ggplot(max_order_entropy, aes(x=reorder(source,max_n), y=max_n, fill=source, label=max_n)) +
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(name=" ",
                      values = map_colors) +
  ggtitle("Maximum nth-Order Entropy") +
  labs(x=" ", y="Max n") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") +
  #geom_text(hjust=2, size=3) +
  geom_text(vjust=2, size=3) 
  #coord_flip()
plot_special

#########################################################################
# Condition #3
plot2 <- plot_condition2(entropy_data %>% filter(n < 6),map_colors, TRUE)
cond3 = run_condition3_analysis(entropy_data,map_colors)
grid.arrange(plot2,cond3[[3]],widths = c(2, 3), nrow=1)


#########################################################################
# summarized

# map colors
map_colors_summary <- c(
  "Call of the Wild"="#619CFF",
  "LSTM-Overfit"="#F8766D",
  "LSTM-1000"="#fc9803",
  "LSTM-1"="#fc9803",  
  "GPT-NEO-1.3B"="#88d190",
  "GPT-NEO-2.7B"="#88d190",
  "GPT-J-6B"="#88d190",
  "Don Quijote"="#619CFF",
  "Lorem Ipsum"="#F8766D",  
  "Moby Dick"="#619CFF",
  "Tale of two Cities"="#619CFF",  
  "Tom Sawyer"="#619CFF"
)
cond1 = run_condition1_analysis(my_data, 0, map_colors_summary)

plot_special <- ggplot(max_order_entropy, aes(x=reorder(source,-max_n), y=max_n, fill=source, label=max_n)) +
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(name=" ",
                    values = map_colors_summary) +
  ggtitle("Maximum nth-Order Entropy") +
  labs(x=" ", y="Max n") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") +
  geom_text(hjust=2, size=3) +
  coord_flip()
plot_special

#########################################################################
# Condition #3
plot2 <- plot_condition2(entropy_data %>% filter(n < 6),map_colors_summary, TRUE)
cond3 = run_condition3_analysis(entropy_data,map_colors_summary)
grid.arrange(plot2,cond3[[3]],widths = c(2, 3), nrow=1)

grid.arrange(cond1[[2]] + ggtitle("Condition #1") + labs(x=" ", y="Zipf Slope"),
             plot_special + ggtitle("Condition #2"),
             cond3[[3]] + ggtitle("Condition #3"), nrow=1)

