library(ggplot2)
stable_iso_area <- read_csv("C:/Users/s1014831/Desktop/Statistics/stable_iso_area.csv")
library(xtable)
library(car)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
library(stargazer)

LabHarvestCSV <- read_csv("C:/Users/s1014831/Desktop/LabHarvestCSV.csv")
LabHarvest<-LabHarvestCSV
LabHarvest$Treatment <- factor(LabHarvest$Treatment, levels=c('4%','8%','16%'))
LabHarvest$Species<-factor(LabHarvest$Species, levels=c('VS','VE'))
LabHarvest$Harvest<-as.factor(LabHarvest$Harvest)
LabHarvest$Tag<-as.factor(LabHarvest$Tag)
H3<-subset(LabHarvest, Harvest=="H3")
VS_H3<-subset(H3, Species=="VS")
LabHarvest_VS<-subset(LabHarvest, Species=="VS")
LabHarvest_VE<-subset(LabHarvest, Species=="VE")


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
          legend.position = c(0.9, 0.9))
}

LabHarvest$Treatment <- factor(LabHarvest$Treatment, levels=c('4%','8%','16%'))
VS<-subset(LabHarvest, Species == 'VS')

VS<-subset(stable_iso_area, Species == 'VS')
VE<-subset(stable_iso_area, Species == 'VE')
iso<-stable_iso_area


#using an ANOVA for H3 SLA 
hist(iso$SLA)
#left skewed
log_SLA<-log2(iso$SLA)
hist(log_SLA)
SLA.aov <- aov(log_SLA ~ Treatment+Species , data = iso)
SLA.aov <- lm(log_SLA ~ Treatment+Species , data = iso)
summary(SLA.aov)
TukeyHSD(SLA.aov)
model.tables(SLA.aov, "means")
plot(SLA.aov, 1)
#3 outliers
leveneTest(log_SLA~ Treatment+Species, data = iso)
# p-value <0.05 no evidence  that the variance across groups is statistically significant
xtable(SLA.aov)


#using an ANOVA for H3 15N 
hist(VS$delta15N)
#normal distribution
res.aov <- aov(delta15N ~ Treatment , data = VS)
summary(res.aov)
model.tables(res.aov, "means")
TukeyHSD(res.aov)
plot(res.aov, 1)
#3 outliers
leveneTest(delta15N~ Treatment , data = VS)
# p-value <0.05 no evidence  that the variance across groups is statistically significant


#using an ANOVA for H3 13C
hist(VS$delta13C)
#left skewed
#cannot log as the data is negative 
res.aov <- aov(delta13C ~ Treatment , data = VS)
summary(res.aov)
model.tables(res.aov, "means")
TukeyHSD(res.aov)
plot(res.aov, 1)
#3 outliers
leveneTest(delta13C~ Treatment , data = VS)
# p-value <0.05 no evidence  that the variance across groups is statistically significant

#using an ANOVA for H3 N
hist(VS$N)
#left skewed
#cannot log as the data is negative 
res.aov <- aov(N ~ Treatment , data = VS)
summary(res.aov)
model.tables(res.aov, "means")
TukeyHSD(res.aov)
plot(res.aov, 1)
#3 outliers
leveneTest(delta13C~ Treatment , data = model.tables(res.aov, "means"))
# p-value <0.05 no evidence  that the variance across groups is statistically significant