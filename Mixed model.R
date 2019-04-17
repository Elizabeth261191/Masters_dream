#Mixed model graphs 

library(readr)
library(ggplot2)
library(lme4)
library(xtable)
library(stargazer)
Height <- read_csv("C:/Users/s1014831/Desktop/Statistics/Height.csv")

Height <- read_csv("Statistics/Height.csv")

theme.clean <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12, face = "plain"),             
          axis.title.y = element_text(size = 12, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.title = element_blank(),                              
          legend.position = c(0.1, 0.8))
}

#one issue to sort is getting a unique identifier for each individual
Height$IndividualID <- as.factor(paste(as.character(Height$Tag),as.character(Height$Pot),sep="_")) 
summary(Height)

Height$Treatment <- factor(Height$Treatment, levels=c('4%','8%','16%'))
Height$Species <- factor(Height$Species, levels=c('VS','VE'))
H3<-subset(Height, Harvest == "3")
VS<-subset(H3, Species=="VS")
VE<-subset(H3, Species=="VE")

Week <- lmer(Height ~ Week + (1|IndividualID),data= VS)
Week+Treatment <- lmer(Height ~ Treatment + Week + (1|IndividualID),data= VS)


stargazer(Week+Treatment, type = "html",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")


p<-ggplot(Height,aes(Week, Height, group=Treatment, col=Treatment, shape= Treatment))  +geom_point() + geom_smooth(method=lm,  aes(fill=Treatment))+theme.clean()+ labs(x = "Week", y = "Height (mm)")
p+ scale_x_continuous(breaks = seq(1,15 , by = 1) + facet_grid(Species~ .) )



A5<-anova(lmem5,lmem4)
t6<-print(xtable(A5), type = "html", head= "VS Height (mm)")


Height_future <- as.data.frame(t(apply(ranef(lmem5)$IndividualID,1,function(x) fixef(lmem5) + x)))
Height_pred<- melt(apply(Height_future,1,function(x) x[1] + x[2]*0:9),value.name = "Height")
names(Height_pred)[1:2] <- c("Week","IndividualID")
Height_pred$Week <- Height_pred$Week - 1
Height_pred$IndividualID <- as.factor(Height_pred$IndividualID)

#plot with actual data
p<-ggplot(Height_pred,aes(x=Week,y=Height,color=IndividualID))+
  geom_line()+
  geom_point(data=Height,aes(x=Week,y=Height))+facet_wrap(~IndividualID,nrow=4)+theme(legend.position = "none")

VS_Week <- lmer(Height ~ Week + (1|IndividualID),data= VS)
VS_Week_Treatment <- lmer(Height ~ Treatment + Week + (1|IndividualID),data= VS)

stargazer(VS_Week_Treatment, type = "html",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

A5<-anova(VS_Week,VS_Week_Treatment)
t6<-print(xtable(A5), type = "html", head= "VS Height (mm)")

#VE
VE_Week<-lmer(Height~Week+ (1|IndividualID),data= VE)
VE_Week_Treatment<- lmer(Height ~ Treatment+Week + (1|IndividualID),data= VE)

stargazer(lmem5, type = "html",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

#plot with growth for both species
p<-ggplot(H3,aes(Week, Height, group=Treatment, col=Treatment, shape= Treatment))+  
 geom_point() + geom_smooth(method=lm,  aes(fill=Treatment))+theme.clean()+labs(x = "Week", y = "Height (mm)")
p+ scale_x_continuous(breaks = seq(1,15 , by = 1))+facet_grid(.~ Species)+theme(strip.background = element_rect(colour="black", fill="white",))




A5<-anova(lmem5,lme4)
t6<-print(xtable(A5), type = "html", head= "VE Height (mm)")

