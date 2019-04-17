library(ggplot2)
library(agridat)
library(ggpubr)
library(stargazer)
library(dcolumn)
library(pastecs)
library(readr)
library(ggpubr)
library(devtools)
library(dplyr) 

Wits_Biomass <- read_csv("C:/Users/s1014831/Desktop/Statistics/Wits data/Wits_Biomass.csv")
Wits_biomass<-Wits_Biomass
Wits_biomass$species<- factor(Wits_biomass$species, levels=c('VS','VEX','SN'))
Wits_biomass$nodule_wt<-as.numeric(Wits_biomass$nodule_wt)
Wits_biomass$nodule_count<-as.numeric(Wits_biomass$nodule_count)


theme.clean <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 8, angle = 0, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12, face = "plain"),             
          axis.title.y = element_text(size = 12, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 8, face = "italic"),          
          legend.title = element_blank(),                              
          legend.position = c(0.9, 0.9))
}

Log_R<-log(Wits_biomass$AG_wt)
Log_B<-log(Wits_biomass$BG_wt)
Log_N_w<-log(Wits_biomass$nodule_wt)
Log_N_c<-log(Wits_biomass$nodule_count)
Log_T<-log(Wits_biomass$Total_wt)
Log_RS<-log(Wits_biomass$ratio)


(p1 <- ggplot(Wits_biomass, aes(x=treatment, y=Log_N_c, fill=treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Treatment", y = "Nodule count log") +  facet_grid(.~ species) + theme(strip.background = element_rect(colour="black", fill="white",)))
(p5 <- ggplot(Wits_biomass, aes (x = Log_N_w, y = Log_N_c, colour = treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(y = "Nodule count log", x = "Nodule weight log (grams)")+geom_smooth(method=lm, aes(fill=treatment))+facet_grid(.~ species)+theme(legend.position = "none") )                          
ggarrange( p1, p5, labels = c("A", "B"))


(p14 <- ggplot(Wits_biomass, aes(x=treatment, y=Log_T, fill=treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Treatment", y = "Whole biomass log (grams)") +  facet_grid(.~ species) + theme(strip.background = element_rect(colour="black", fill="white",)))
(p15 <- ggplot(Wits_biomass, aes(x=treatment, y=Log_RS, fill=treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Treatment", y = "Root: shoot ratio log") +  facet_grid(.~ species) + theme(strip.background = element_rect(colour="black", fill="white",)))
ggarrange( p1, p5, p14, p15, labels = c("A", "B","A", "B"))

(p6<-ggplot(Wits_biomass, aes (x = Log_T, y = Log_N_c, colour = treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(y = "Total biomass log (grams)" , x = "Nodule count log")+geom_smooth(method=lm, aes(fill=treatment))+facet_grid(.~ species)+theme(legend.position = "none"))
(p7<-ggplot(Wits_biomass, aes (x = Log_T, y = Log_N_w, colour = treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(y = "Total biomass log (grams)" , x = "Nodule weight log (grams)")+geom_smooth(method=lm, aes(fill=treatment))+facet_grid(.~ species)+theme(legend.position = "none"))




##Isotope box plots

wits_iso <- read_csv("C:/Users/s1014831/Desktop/Statistics/Wits data/wits_iso.csv")
summary(wits_iso)
wits_iso$Species<- factor(wits_iso$Species, levels=c('VS','VEX','SN'))


(p18 <- ggplot(wits_iso, aes(Treatment, N,fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Leaf N content (per 200 um)") 
  +   facet_wrap(.~Species) +   theme(strip.background = element_rect(colour="black", fill="white",)))

(p19 <- ggplot(wits_iso, aes(Treatment, delta15N,fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Leaf 15N content (per 200 um)") 
  +   facet_wrap(.~Species) +   theme(strip.background = element_rect(colour="black", fill="white",)))

(p20 <- ggplot(wits_iso, aes(Treatment, C,fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Leaf C content (per 200 um)") 
  +   facet_wrap(.~Species) +   theme(strip.background = element_rect(colour="black", fill="white",)))

(p21 <- ggplot(wits_iso, aes(Treatment, delta13C, fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Leaf 13C content (per 200 um)") 
  +   facet_wrap(.~Species)+theme(strip.background = element_rect(colour="black", fill="white",)))

(p22 <- ggplot(wits_iso, aes(Treatment, CN_ratio, fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "C:N ratio") 
  +   facet_wrap(.~Species)+theme(strip.background = element_rect(colour="black", fill="white",)))

ggarrange(p18, p22, p19, p22,labels = c("A", "B", "C", "D"))

hist(Wits_biomass$AG_wt)
log_N<-log10(Wits_biomass$AG_wt)
hist(log_N)
R.aov <- aov(log_N~ treatment*species , data = Wits_biomass)
summary(R.aov)
TukeyHSD(R.aov)
model.tables(R.aov, "means")
plot(R.aov, 1)
leveneTest(AG_wt  ~  treatment*species, data = Wits_biomass)

hist(Wits_Biomass$BG_wt)
log_N<-log10(Wits_Biomass$BG_wt)
hist(log_N)
R.aov <- aov(log_N~ treatment*species , data = Wits_Biomass)
summary(R.aov)
TukeyHSD(R.aov)
model.tables(R.aov, "means")
plot(R.aov, 1)
leveneTest(BG_wt  ~  treatment*species, data = Wits_Biomass)

hist(Wits_Biomass$ratio)
log_N<-log10(Wits_Biomass$ratio)
hist(log_N)
R.aov <- aov(log_N~ treatment*species , data = Wits_Biomass)
summary(R.aov)
TukeyHSD(R.aov)
model.tables(R.aov, "means")
plot(R.aov, 1)
leveneTest(ratio  ~  treatment*species, data = Wits_Biomass)

hist(Wits_Biomass$fine_root_weight)
log_N<-sqrt(Wits_Biomass$fine_root_weight)
hist(log_N)
R.aov <- aov(log_N~ treatment*species , data = Wits_Biomass)
summary(R.aov)
TukeyHSD(R.aov)
model.tables(R.aov, "means")
plot(R.aov, 1)
leveneTest(fine_root_weight  ~  treatment*species, data = Wits_Biomass)