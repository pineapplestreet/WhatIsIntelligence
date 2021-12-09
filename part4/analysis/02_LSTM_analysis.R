#########################################################################
# required libraries
source("../_global_shared_functions.R") 

#########################################################################
# get all of the data of interest
my_data <- get_combined_dataframe(sources=c(
  "LSTM-1",
  "LSTM-10",
  "LSTM-20",
  "LSTM-30",
  "LSTM-40",
  "LSTM-50",
  "LSTM-100",
  "LSTM-1000",
  "LSTM-Overfit",
  "Original"), 
  folder="data2")

# order factor
my_data$source <- factor(my_data$source, levels=c(
                                                "LSTM-1",
                                                "LSTM-10",
                                                "LSTM-20",
                                                "LSTM-30",
                                                "LSTM-40",
                                                "LSTM-50",
                                                "LSTM-100",
                                                "LSTM-1000",
                                                "LSTM-Overfit",
                                                "Call of the Wild"))

# map colors
map_colors <- c(
  "LSTM-Overfit"="#FDE725",
  "LSTM-1000"="#B4DE2C",
  "LSTM-100"="#6DCD59",
  "LSTM-50"="#35B779",
  "LSTM-40"="#1F9E89",
  "LSTM-30"="#26828E",
  "LSTM-20"="#31688E",
  "LSTM-10"="#3E4A89",
  "LSTM-1"="#482878",  
  "Call of the Wild"="#C77CFF"
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

#########################################################################
# calculate higher-order entropy
# for(my_source in unique(my_data$source)) {
#   print(my_source)
#   tmp = my_data %>% filter(source==my_source)
#   results = calculate_higher_order_entropies(tmp,0,100)  
#   saveRDS(results,paste0("data3/entropy_",my_source,".Rds"))
# }

#########################################################################
# Condition #2
entropy_data <- get_combined_dataframe(sources=c(
  "entropy_LSTM-1",
  "entropy_LSTM-10",
  "entropy_LSTM-20",
  "entropy_LSTM-30",
  "entropy_LSTM-40",
  "entropy_LSTM-50",
  "entropy_LSTM-100",
  "entropy_LSTM-1000",
  "entropy_LSTM-Overfit",
  "entropy_Call of the Wild"
), 
folder="data3") 
plot <- plot_condition2(entropy_data,map_colors)
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


#########################################################################
# Condition #3
plot2 <- plot_condition2(entropy_data %>% filter(n < 6),map_colors, TRUE)
cond3 = run_condition3_analysis(entropy_data,map_colors)
grid.arrange(plot2,cond3[[3]],widths = c(2, 3), nrow=1)


#########################################################################
#########################################################################
#########################################################################
#########################################################################
# digging into correlation

# dataframe of training loss
df.loss <- data.frame(Model="LSTM-1", Mode="Loss", Value=3.7427)
df.loss <- rbind(df.loss,data.frame(Model="LSTM-1", Mode="Validation Loss", Value=3.7219))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-10", Mode="Loss", Value=3.2670))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-10", Mode="Validation Loss", Value=3.2565))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-20", Mode="Loss", Value=2.7291))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-20", Mode="Validation Loss", Value=2.6907))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-30", Mode="Loss", Value=2.4966))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-30", Mode="Validation Loss", Value=2.4593))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-40", Mode="Loss", Value=2.3612))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-40", Mode="Validation Loss", Value=2.3402))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-50", Mode="Loss", Value=2.2741))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-50", Mode="Validation Loss", Value=2.2444))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-100", Mode="Loss", Value=2.0395))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-100", Mode="Validation Loss", Value=2.0437))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-1000", Mode="Loss", Value=1.6553))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-1000", Mode="Validation Loss", Value=1.9250))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-Overfit", Mode="Loss", Value=0.0669))
df.loss <- rbind(df.loss,data.frame(Model="LSTM-Overfit", Mode="Validation Loss", Value=3.8998))
df.loss.wide <- df.loss %>% 
  mutate(source=Model) %>%
  select(source, Mode, Value) %>%
  spread(Mode, Value)

#########################################################################
# Correlation?
combined_results <- cond1[[3]] %>%
  mutate(`Zipf Slope`=slope) %>%
  select(source, `Zipf Slope`) %>%
  left_join(
    cond3[[2]] %>%
      mutate(`Entropy Slope Variance`=variance) %>%
      select(source, `Entropy Slope Variance`)
  ) %>%
  left_join(df.loss.wide) %>%
  filter(!source %in% c("LSTM-Overfit","Call of the Wild"))

# corr w/ p-values
res2 <- rcorr(as.matrix(combined_results %>% select(-source)), type="pearson")
res2$r
res2$P

corrplot(res2$r, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# format for ggplot
item1 = c("Zipf Slope","Entropy Slope Variance","Loss","Validation Loss")
ggdata = data.frame(item1="Zipf Slope", value=res2$P[,1]) %>% tibble::rownames_to_column("item2") %>% select(item1, item2, value)
ggdata = rbind(ggdata,data.frame(item1="Entropy Slope Variance", value=res2$P[,2]) %>% tibble::rownames_to_column("item2") %>% select(item1, item2, value))
ggdata = rbind(ggdata,data.frame(item1="Loss", value=res2$P[,3]) %>% tibble::rownames_to_column("item2") %>% select(item1, item2, value))
ggdata = rbind(ggdata,data.frame(item1="Validation Loss", value=res2$P[,4]) %>% tibble::rownames_to_column("item2") %>% select(item1, item2, value))
ggdata <- ggdata %>%
  mutate(min=pmin(item1, item2)) %>%
  mutate(max=pmax(item1, item2)) %>%
  mutate(label=paste0(min," ",max)) %>%
  arrange(label, item1) %>%
  group_by(label) %>%
  summarise(item1=first(item1),
            item2=first(item2),
            value=first(value)) %>%
  mutate(`Significance`=ifelse(value<0.05,"p < 0.05", "p >= 0.05"))

# make pretty plot
plot2 <- ggplot(ggdata, aes(x = reorder(item2,desc(item2)), y = reorder(item1,desc(item2)), fill=Significance, label=round(value,6))) +
  geom_tile(show.legend = TRUE, colour = "#000000", size=0.5) +
  theme_bw() +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("p-value") +
  scale_fill_manual(name=" ",
                    values = c("p < 0.05"="#02b848")) +  
  labs(x=" ", y=" ") +
  geom_text(size=3) 
plot2



# format correlation for ggplot
item1 = c("Zipf Slope","Entropy Slope Variance","Loss","Validation Loss")
ggdata = data.frame(item1="Zipf Slope", value=res2$r[,1]) %>% tibble::rownames_to_column("item2") %>% select(item1, item2, value)
ggdata = rbind(ggdata,data.frame(item1="Entropy Slope Variance", value=res2$r[,2]) %>% tibble::rownames_to_column("item2") %>% select(item1, item2, value))
ggdata = rbind(ggdata,data.frame(item1="Loss", value=res2$r[,3]) %>% tibble::rownames_to_column("item2") %>% select(item1, item2, value))
ggdata = rbind(ggdata,data.frame(item1="Validation Loss", value=res2$r[,4]) %>% tibble::rownames_to_column("item2") %>% select(item1, item2, value))
ggdata <- ggdata %>%
  mutate(min=pmin(item1, item2)) %>%
  mutate(max=pmax(item1, item2)) %>%
  mutate(label=paste0(min," ",max)) %>%
  arrange(label, item1) %>%
  group_by(label) %>%
  summarise(item1=first(item1),
            item2=first(item2),
            value=first(value)) %>%
  mutate(`Significance`=ifelse(value<0.05,"p < 0.05", "p >= 0.05"))



# make pretty plot
plot1 <- ggplot(ggdata, aes(x = reorder(item2,desc(item2)), y = reorder(item1,desc(item2)), fill=value, label=round(value,4))) +
  geom_tile(show.legend = TRUE, colour = "#000000", size=0.5) +
  theme_bw() +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Pearson Correlation Coefficient") +
  # scale_fill_manual(name=" ",
  #                   values = c("p < 0.05"="#02b848")) +  
  scale_fill_gradientn(colours=c("red","white","lightblue"),values=rescale(c(-1,0,1)))+ 
  labs(x=" ", y=" ") +
  geom_text(size=3) 
plot1

grid.arrange(plot1,plot2,widths = c(3, 3), nrow=1)



# look at this correlation closer
# map colors
map_colors2 <- c(
  "LSTM-1000"="#B4DE2C",
  "LSTM-100"="#6DCD59",
  "LSTM-50"="#35B779",
  "LSTM-40"="#1F9E89",
  "LSTM-30"="#26828E",
  "LSTM-20"="#31688E",
  "LSTM-10"="#3E4A89",
  "LSTM-1"="#482878"
)
plot1 <- ggplot(combined_results, aes(x=`Loss`, size=`Loss`, y=`Entropy Slope Variance`, color=source)) +
  geom_point() +
  scale_colour_manual(name=" ",
                    values = map_colors2) +
  theme_bw() +
  theme(legend.position = "none")
plot1

plot2 <- ggplot(combined_results, aes(x=`Validation Loss`, size=`Validation Loss`, y=`Entropy Slope Variance`, color=source)) +
  geom_point() +
  theme_bw() +
  scale_colour_manual(name=" ",
                      values = map_colors2)  
plot2

grid.arrange(plot1,plot2,widths = c(2, 3), nrow=1)
