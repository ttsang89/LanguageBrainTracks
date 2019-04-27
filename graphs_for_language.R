library(ggplot2)
getwd()
setwd("/Users/tawnytsang/Desktop/Projects/imaging/Native Language/6wk/spreadsheets")

require(xlsx)
data_2 <- read_excel("05-03-18_PE_language-laterality_checks.xlsx", sheet = "LONG")

data_2$RX <- factor(data_2$RX, levels = c(0,1), labels = c("LR", "HR"))
data_2$Language <- factor(data_2$Language, levels=c(0,1), labels=c("English","Japanese"))

data_2$scatter_adjusted[data_2$RX == "LR"] <- 0
data_2$scatter_adjusted[data_2$RX == "HR"] <- 1

ggplot(data_2, aes(scatter_adjusted,ENG_LeftvsR)) +
  geom_point(size=2,aes(shape=Language, color=RX)) + 
  scale_shape_manual(breaks=c("English","Japanese"),values=c(16,24)) + 
  scale_color_manual(breaks=c("LR","HR"),values=c("blue","red")) + 
  stat_summary(fun.ymin=function(Value)(mean(Value)-sd(Value)), fun.ymax=function(Value)(mean(Value)+sd(Value)), geom="errorbar",width=0.1,color="black") +
  stat_summary(fun.y=mean, geom="point", shape=22,color="black", size=6) +
  scale_fill_manual(breaks = c("LR","HR"),values=c("blue","red")) + 
  xlab("Group") + 
  ylab("Parameter Estimate") + 
  theme(panel.background=element_rect(fill='white',colour='black'))
