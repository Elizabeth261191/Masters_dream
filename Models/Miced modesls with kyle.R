#load necessary packages
library(lattice)
library(lme4)

library(readxl)
Babies_height_final_FINAL <- read_excel("C:/Users/telfo/Desktop/Babies height final FINAL.xlsx")
There were 50 or more warnings (use warnings() to see the first 50)
View(Babies_height_final_FINAL)
Height.data<-Babies_height_final_FINAL

summary(Height.data)
#most things look OK, pot needs to be converted to a factor
Height.data$Pot <- as.factor(Height.data$Pot)
summary(Height.data$Pot)


#one issue with looking at growth over time within individuals is that not all individuals are present at all time points
#one option, a conservative one is to focus only on those individuals present at the last time point

#one issue to sort is getting a unique identifier for each individual
Height.data$IndividualID <- as.factor(paste(as.character(Height.data$Tag),as.character(Height.data$Pot),sep="_")) 
summary(Height.data)

Height.data_H3 <- subset(Height.data,Harvest=="3")
tmp <- subset(Height.data,Harvest!="3")
sort(Height.data_H3$IndividualID[which(Height.data_H3$Pot%in%tmp$IndividualID)])

#now that we have data for individuals that were present at all time points, we can make a simple mixed model to see if growth over time varies by species or treatment
#first, let's just check if growth is roughly linear
xyplot(Height~Week|IndividualID,data=Height.data_H3,type=c("p","r"))
#it looks like rest showed more or less linear growth
#we are going to pretend that bad individuals have already been removed to show how to code mixed model

#lets start with simple linear model if growth varies by species
lmem0 <- lmer(Height~1 + (1|IndividualID),data=Height.data_H3)

lmem1 <- lmer(Height ~ Species + (1|IndividualID), data=Height.data_H3)
summary(lmem1)

anova(lmem0,lmem1)

lmem4 <- lmer(Height ~ Species + Treatment + Week + Treatment:Week + Species:Treatment + Species:Week + (1|IndividualID),data=Height.data_H3)
lmem5 <- lmer(Height ~ Species*Treatment*Week + (1|IndividualID),data=Height.data_H3)
anova(lmem4,lmem5)

