#########################################################
# libraries I am using
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

# get our data
entropy_results <- readRDS("1_eda/entropy_results_backup.Rds") %>%
  filter(mode=="words") %>%
  mutate(source=paste0(source," (words)")) %>%
  filter(n <= 10) %>%
  mutate(n=n-1) %>%
  select(source,n,entropy)


# add McCowan, et all data points
dolphins <- data.frame(source="Infant dolphin whistles (McCowan, et all)",
                       n=c(0,1,2,3),
                       entropy=c(4.32,2.38,1.68,1.11))
monkeys <- data.frame(source="Subadult squirrel monkey (McCowan, et all)",
                       n=c(0,1,2),
                       entropy=c(2.58, 1.92, 0.91))

entropy_results <- rbind(entropy_results, dolphins,monkeys) %>%
  mutate(entropy=log10(entropy))

ggplot(entropy_results, aes(x=n, y=entropy, color=source)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ggtitle("Higher-Order Entropy (words)") +
  labs(x="n", y="log10 nth-order entropy") +
  scale_colour_manual(name=" ",
                      values = c("Lorem Ipsum (words)"="#F8766D",
                                 "Don Quijote, Part 1 (words)"="#B79F00",
                                 "A Tale of Two Cities (words)"="#00BA38",
                                 "Moby Dick (words)"="#00BFC4",
                                 "The Adventures of Tom Sawyer (words)"="#619CFF",
                                 "The Call of the Wild (words)"="#C77CFF",
                                 "Infant dolphin whistles (McCowan, et all)"="#000000",
                                 "Subadult squirrel monkey (McCowan, et all)"="#999999"))


# explore entropy
table <- readRDS("1_eda/entropy_results_backup.Rds") %>%
  filter(mode=="words") %>%
  mutate(n=n-1) %>%
  mutate(source=paste0(source," (words)")) %>%
  filter(n <= 10) %>%
  select(source, n, entropy)
  
  
  # add McCowan, et all data points
  dolphins <- data.frame(source="Infant dolphin whistles (McCowan, et all)",
                         n=c(0,1,2,3),
                         entropy=c(4.32,2.38,1.68,1.11))
  monkeys <- data.frame(source="Subadult squirrel monkey (McCowan, et all)",
                        n=c(0,1,2),
                        entropy=c(2.58, 1.92, 0.91))
  
  table <- rbind(table, dolphins,monkeys) %>%
  spread(n,entropy)
