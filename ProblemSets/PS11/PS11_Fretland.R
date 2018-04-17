#########

# Erik Fretland
# PS11
# 4/16/18

#######

# Library



library(ggplot2)







setwd("C:/Users/User/Documents/Spring 2018/ECON DS/Final Project")

draft <- read.csv("DraftData2.csv")
draft <- as.data.frame(draft)
attach(draft)
summary(draft$Pos)

ggplot(draft, aes(x= AVPerGm)) + geom_histogram(binwidth = 0.02) 
ggplot(draft, aes(x= Pick, y = AVPerGm)) + geom_point() + geom_smooth() + 
  geom_vline(xintercept = 180) + geom_vline(xintercept = 130)