#########################################################################
# required libraries
source("../_global_shared_functions.R") 

# dataframe of loss
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

df.loss$Model <- factor(df.loss$Model, levels=c("LSTM-Overfit",
                                          "LSTM-1000",
                                          "LSTM-100",
                                          "LSTM-50",
                                          "LSTM-40",
                                          "LSTM-30",
                                          "LSTM-20",
                                          "LSTM-10",
                                          "LSTM-1"))
# plot the loss
ggplot(df.loss, aes(x=Model, y=Value, fill=Mode)) +
  geom_bar(stat="identity",position="dodge", color="black") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name=" ",
                      values = c(
                        "Loss"="#ed0c0c",
                        "Validation Loss"="#ed800c"))
  
