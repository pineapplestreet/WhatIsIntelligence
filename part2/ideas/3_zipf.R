#########################################################################
# required libraries
source("_shared_functions.R") 

my_data <- get_combined_dataframe(c("clean_dickens_first100000",
                                    "clean_lorem_ipsum_first100000",
                                    "clean_moby_dick_first100000",
                                    "clean_quijote_first100000",
                                    "clean_tom_sawyer_first100000",
                                    "clean_wild_first100000"))

# use 0 for ALL tokens
# or set a MAX RANK to cut off the right tail
res = run_zipf_analysis(my_data, 20, c("Lorem Ipsum"="#F8766D",
                                        "Don Quijote"="#B79F00",
                                        "Tale of Two Cities"="#00BA38",
                                        "Moby Dick"="#00BFC4",
                                        "Tom Sawyer"="#619CFF",
                                        "Call of the Wild"="#C77CFF"))


grid.arrange(res[[1]],res[[2]],widths = c(3, 2), nrow=1)
res[[3]]


#########################################################################
# animate our Zipf bar plot
source("_shared_functions.R") 
res = plot_animated_zipf_bar_plot(my_data, seq(2,77), c("Lorem Ipsum"="#F8766D",
                                                           "Don Quijote"="#B79F00",
                                                           "Tale of Two Cities"="#00BA38",
                                                           "Moby Dick"="#00BFC4",
                                                           "Tom Sawyer"="#619CFF",
                                                           "Call of the Wild"="#C77CFF"))   
res
anim_save("animated_zipf_bar_plot.gif")