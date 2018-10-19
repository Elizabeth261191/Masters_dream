#converting character or numeric data to a factor for analysis
LabHarvest$Treatment <- as.factor(LabHarvest$Treatment)

#subsetting data to focus on one harvest, could do for one species, etc.
LabHarvest_H3 <- subset(LabHarvest,Harvest=="3")

#making a boxplot
plot(Height~Treatment,data=LabHarvest_H3_VE)

summary(LabHarvest)
#look at the class of data 
class(LabHarvest$Harvest)

LabHarvest$Harvest <- as.factor(LabHarvest$Harvest)

class(LabHarvest$Harvest)
#what type of data 

LabHarvest$Harvest

summary(as.factor(LabHarvest$Pot))

LabHarvest$Species <- as.factor(LabHarvest$Species)plot
summary(LabHarvest)

LabHarvest$Tag <- as.factor(LabHarvest$Tag
sort(unique(LabHarvest$`Nodules W`))

plot(Height~Species,data=LabHarvest_H3)
plot(Height~Treatment,data=LabHarvest_H3)
LabHarvest$Treatment <- as.factor(LabHarvest$Treatment)
plot(Height~Treatment,data=LabHarvest_H3)
LabHarvest_H3 <- subset(LabHarvest,Harvest=="3")
plot(Height~Treatment,data=LabHarvest_H3)
LabHarvest_H3_VE <- subset(LabHarvest_H3,Species=="VE")
plot(Height~Treatment,data=LabHarvest_H3_VE)

#converting character or numeric data to a factor for analysis
LabHarvest$Treatment <- as.factor(LabHarvest$Treatment)
LabHarvest$Harvest<-as.factor(LabHarvest$Harvest)
LabHarvest$Tag<-as.factor(LabHarvest$Tag)
#converting character or numeric data to a factor for analysis
#subsetting data to focus on one harvest, could do for one species, etc.
LabHarvest_H3 <- subset(LabHarvest,Harvest=="3")

#doing a linear model
plot(Height~Stem_DW,data=LabHarvest)
HvN_lm <- lm(Height~Nodule,data=LabHarvest)
summary(HvN_lm)

#code for subsampling harvest and species
LabHarvest_H2 <- subset(LabHarvest,Harvest=="2")
plot(Height~Treatment,data=LabHarvest_H2)
LabHarvest_H2_VE <- subset(LabHarvest_H2,Species=="VE")
plot(Height~Treatment,data=LabHarvest_H2_VE)

LabHarvest_H1 <- subset(LabHarvest,Harvest=="1")
LabHarvest_H1_VS <- subset(LabHarvest_H1,Species=="VS")
plot(Height~Treatment,data=LabHarvest_H1_VS, main="H1 VS")

LabHarvest_H2 <- subset(LabHarvest,Harvest=="2")
LabHarvest_H2_VS <- subset(LabHarvest_H2,Species=="VS")
plot(Height~Treatment,data=LabHarvest_H2_VS, main="H2 VS")
     

LabHarvest_H3_VS <- subset(LabHarvest_H3,Species=="VS")
plot(Height~Treatment,data=LabHarvest_H3_VS, main="H3 VS")

#doing a biomial distribution 
LabHarvest_H2 <- subset(LabHarvest,Harvest=="2")
LabHarvest_H2_VS <- subset(LabHarvest_H2,Species=="VS")
LabHarvest_H2_VS.m<-glm(Nodules_Present~Treatment, family=binomial, data=LabHarvest_H2_VS)
summary(LabHarvest_H2_VS.m)

#doing a linear model 
LabHarvest_H2 <- subset(LabHarvest,Harvest=="2")
LabHarvest_H2_N.m<-lm(AG_DW~Treatment+Nodules_W+Species, data= LabHarvest_H2)
summary(LabHarvest_H2_N.m)
LabHarvest_H2_VS.m<-glm(Nodules_Present~Treatment, family=binomial, data=LabHarvest_H2_VS)
summary(LabHarvest_H2_VS.m))
shapiro.test(LabHarevst_H2.resid)
bartlett.test(Height~Species, data= LabHarvest_H2)
LabHarvest_H2_VS.m<-glm(Nodules_Present~Treatment, family=binomial, data=LabHarvest_H2_VS)
summary(LabHarevst_H2_VS.m)

LabHarvest.m<-lm(BG_DW~Treatment+Species+Harvest, data=LabHarvest)
summary(LabHarvest.m)

