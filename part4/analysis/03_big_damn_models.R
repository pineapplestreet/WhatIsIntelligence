#########################################################################
# required libraries
source("../_global_shared_functions.R") 

# illustrate how damn big these models are
df.models <- data.frame(Model="LSTM-1000",Parameters=166862)
df.models <- rbind(df.models,data.frame(Model="GPT-NEO-1.3B",Parameters=1300000000))
df.models <- rbind(df.models,data.frame(Model="GPT-NEO-2.7B",Parameters=2700000000))
df.models <- rbind(df.models,data.frame(Model="GPT-J-6B",Parameters=6053381344))
df.models <- df.models %>%
  mutate(My_Label=paste0(format(round(Parameters/1000000,2), nsmall=1, big.mark=",")," Million")) %>%
  mutate(My_Label=ifelse(Model=="LSTM-1000",paste0("-------------",My_Label),My_Label))
# plot
ggplot(df.models, aes(x=reorder(Model,-Parameters), y=Parameters, fill=Model, label=My_Label)) +
  geom_bar(stat="identity", color="black") +
  coord_flip() +
  ggtitle("Number of Parameters per Model") +
  labs(x="Model", y="Number of Parameters") +
  geom_text(size=3, position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_manual(name=" ",
                      values = c(
                        "GPT-NEO-1.3B"="#945319",
                        "GPT-NEO-2.7B"="#cc6910",
                        "GPT-J-6B"="#fc9803",
                        "LSTM-1000"="#B4DE2C"))
  
