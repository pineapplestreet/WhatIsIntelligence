#########################################################
# settings go here

# include these sources
sources <- c("lorem_ipsum",
             "a_tale_of_two_cities",
             "moby_dick",
             "the_adventures_of_tom_sawyer",
             "the_call_of_the_wild",
             "don_quijote_part1")

# words or characters
zipf_mode <- "words"

#########################################################
# libraries I am using
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

# save all of our results
zipf_results <- data.frame()

# look at zipf for all sources
for(item in sources) {
  
  # get our data
  data <- readRDS(paste0("clean_data/",item,"_",zipf_mode,".Rds")) %>% 
    group_by(source,token) %>%
    summarise(freq=n()) %>%
    mutate(rank=min_rank(desc(freq)),
           log_freq=log10(freq),
           log_rank=log10(rank))
  zipf_results <- rbind(zipf_results, data)
}  


#########################################################
# how many tokens deep do we want to go?
zipf_results_filtered <- zipf_results %>%
  filter(rank <= 100)

# which intercept do we use?
jojo <- zipf_results_filtered %>% 
  filter(rank==1) %>%
  filter(!(source %in% c("Lorem Ipsum","Don Quijote, Part 1")))
mean(jojo$log_freq)

# make a plot
ggplot(zipf_results_filtered,aes(x=log_rank, y=log_freq, color=source)) +
  geom_point() +
  ggtitle("Zipf Slopes: Top 100 Words") +
  labs(x="log10 Rank", y="log10 Frequency") +
  geom_smooth(method="lm", se=FALSE) +
  geom_abline(slope=-1,intercept=3.74476, color="black", lty=2) +
  theme_bw() +
  theme(legend.position="none") +
  #facet_grid(rows = vars(source))
  facet_wrap(vars(source)) +
  scale_colour_manual(name=" ",
                      values = c("Lorem Ipsum"="#F8766D",
                                 "Don Quijote, Part 1"="#B79F00",
                                 "A Tale of Two Cities"="#00BA38",
                                 "Moby Dick"="#00BFC4",
                                 "The Adventures of Tom Sawyer"="#619CFF",
                                 "The Call of the Wild"="#C77CFF"))



# characters
# determine best fit slope by linear regression
lm_results <- data.frame()
for(my_source in unique(zipf_results_filtered$source)) {
  tmp <- zipf_results_filtered %>% 
    filter(source==my_source)
  model <- lm(data=tmp, log_freq~log_rank)
  lm_results <- rbind(lm_results, data.frame(method="Top 100 Words",source=my_source,slope=model$coefficients[2],r2=summary(model)$r.squared))
}

ggdata <- lm_results %>%
  mutate(source=paste0(source, " (words)"))

# add McCowan, et all data points
ggdata <- rbind(ggdata,data.frame(method="special",
                                  source="Infant dolphin whistles (McCowan, et all)",
                                  slope=-1.03,
                                  r2=0.91))
# add McCowan, et all data points
ggdata <- rbind(ggdata,data.frame(method="special",
                                  source="Subadult squirrel monkey (McCowan, et all)",
                                  slope=-0.84,
                                  r2=0.98))
# clean er up
ggdata <- ggdata %>%
  mutate(abs_difference=abs(-1-slope)) %>%
  mutate(my_label=paste0(round(slope,2)," slope, ",round(r2,2)," r2, ",round(abs_difference,2)," abs error")) %>%  
  arrange(abs_difference)
                  
# plot of relative zipfness
ggplot(ggdata,aes(x=reorder(source,-abs_difference), y=slope, fill=source, label=my_label)) +
  geom_bar(stat="identity", color="black") +
  ggtitle("Zipf Slopes, Top 100 Words") +
  geom_text(size=3, position = position_stack(vjust = 0.5))  +
  geom_hline(yintercept=-1, lty=2) +
  labs(x=" ", y="Slope") +
  theme_bw() +
  theme(legend.position="none") +
  coord_flip() +
  scale_fill_manual(name=" ",
                      values = c("Lorem Ipsum (words)"="#F8766D",
                                 "Don Quijote, Part 1 (words)"="#B79F00",
                                 "A Tale of Two Cities (words)"="#00BA38",
                                 "Moby Dick (words)"="#00BFC4",
                                 "The Adventures of Tom Sawyer (words)"="#619CFF",
                                 "The Call of the Wild (words)"="#C77CFF"))
  

