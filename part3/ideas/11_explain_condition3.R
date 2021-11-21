#########################################################################
# found a bug in the calculate entropy code
# it was confusing 1st order for 0th order
# fixed!
# need to re-run all of these. 

#########################################################################
# required libraries
source("_shared_functions.R") 


#########################################################################
my_data3 <- get_combined_dataframe(sources=c(
  "entropy_results_english",
  "entropy_results_english2",
  "entropy_results_hebrew",
  "entropy_results_hebrew2",
  "entropy_results_chinese",  
  "entropy_results_lorem_ipsum_first4088",
  "entropy_results_moby_dick_first4088",
  "entropy_results_quijote_first4088",
  "entropy_results_wild_first4088",
  "entropy_results_tom_sawyer_first4088",
  "entropy_results_dickens_first4088"
), 
folder="data2") %>%
  mutate(log10_entropy=log10(entropy)) %>%
  filter(n <= 5)



#########################################################################
# animate the lines
line_data <- data.frame()
for(i in seq(0,4,1)) {
  tmp <- my_data3 %>%
    filter(n %in% c(i,i+1)) %>%
    mutate(iteration=i)
  line_data <- rbind(line_data, tmp)
}

line_data$source <- factor(line_data$source, levels=c(
  "English (4,088)",
  "English, 1st-order (4,088)",
  "Lorem Ipsum (4,088)",
  "Hebrew (2,136)",
  "Hebrew, backwards (2,136)",
  "Chinese (983)"
))


my_data3$source <- factor(my_data3$source, levels=c(
  "English (4,088)",
  "English, 1st-order (4,088)",
  "Lorem Ipsum (4,088)",
  "Hebrew (2,136)",
  "Hebrew, backwards (2,136)",
  "Chinese (983)"
))

#########################################################################
p1 <- ggplot(line_data, aes(x=n, y=log10_entropy, color=source)) +
  geom_point(data=my_data3, aes(x=n, y=log10_entropy, color=source)) +
  geom_smooth(method="lm", se=FALSE) +
  facet_wrap(vars(source)) +
  scale_colour_manual(name=" ",
                      values = c( 
                        "English (4,088)"="#457ed9",
                        "English, 1st-order (4,088)"="#7b91b5",
                        "Lorem Ipsum (4,088)"="#F8766D",
                        "Hebrew (2,136)"="#19913d",
                        "Hebrew, backwards (2,136)"="#869c8c",
                        "Chinese (983)"="#8b2c9c")) +
  labs(x="nth-order",y="log10 entropy") +
  theme_bw() +
  theme(legend.position="none") +
  transition_states(iteration, 
                    transition_length = 1, 
                    state_length = 0) +
  ease_aes('cubic-in-out') 

p1

anim_save("animated_condition3_plot.gif")




jojo <- analyze_third_condition(my_data3, color_key=c(
  "English (4,088)"="#457ed9",
  "English, 1st-order (4,088)"="#7b91b5",
  "Lorem Ipsum (4,088)"="#F8766D",
  "Hebrew (2,136)"="#19913d",
  "Hebrew, backwards (2,136)"="#869c8c",
  "Chinese (983)"="#8b2c9c",
  "Don Quijote (4,088)"="#B79F00",
  "Tale of Two Cities (4,088)"="#00BA38",
  "Moby Dick (4,088)"="#00BFC4",
  "Tom Sawyer (4,088)"="#619CFF",
  "Call of the Wild (4,088)"="#C77CFF"))

p1 <- jojo[[3]]
grid.arrange(p1,p2, nrow=1)

