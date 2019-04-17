# all bar charts

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

stable_iso_area <- read_csv("C:/Users/s1014831/Desktop/Statistics/stable_iso_area.csv")
View(stable_iso_area)
stable_iso_area$Treatment <- factor(stable_iso_area$Treatment, levels=c('4%','8%','16%'))
stable_iso_area$Species <- factor(stable_iso_area$Species, levels=c('VS','VE'))
iso_VE<-subset(stable_iso_area, Species=="VE")
iso_VS<-subset(stable_iso_area, Species=="VS")
stable_iso_area<-na.omit(stable_iso_area)

Height <- read_csv("C:/Users/s1014831/Desktop/Statistics/Height.csv")
Height$IndividualID <- as.factor(paste(as.character(Height$Tag),as.character(Height$Pot),sep="_")) 
Height$Treatment <- factor(Height$Treatment, levels=c('4%','8%','16%'))
Height$Species <- factor(Height$Species, levels=c('VS','VE'))
H3<-subset(Height, Harvest == "3")
VS<-subset(H3, Species=="VS")
VE<-subset(H3, Species=="VE")

LICOR <- read_csv("C:/Users/s1014831/Desktop/Statistics/LICOR.csv")
LICOR$Treatment <- factor(LICOR$Treatment, levels=c('4%','8%','16%'))
LICOR$Species <- factor(LICOR$Species, levels=c('VS','VE'))
LICOR<-na.omit(LICOR)

#Total DW barchart 

(p1 <- ggplot(LabHarvest, aes(x=Harvest, y=Height, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Harvest", y = "Height (mm)") +  facet_grid(Species~ Treatment) 
  +   theme(strip.background = element_rect(colour="black", fill="white",)))

(p2 <- ggplot(LabHarvest, aes(x=Harvest, y=BG_DW, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Harvest", y = "Below ground dry weight (grams)") +  facet_grid(Species~ Treatment) 
  +   theme(strip.background = element_rect(colour="black", fill="white",)))

ggarrange(p1, p2,labels = c("A", "B"))

#All species stable isotope boxplots 

(p18 <- ggplot(stable_iso_area, aes(Treatment, N,fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Leaf N content (per 250 um)") 
  +   facet_wrap(.~Species) +   theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%")))

(p19 <- ggplot(stable_iso_area, aes(Treatment, delta15N,fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Leaf 15N content (per 200 um)") 
  +   facet_wrap(.~Species) +   theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%")))

(p20 <- ggplot(stable_iso_area, aes(Treatment, C,fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Leaf C content (per 200 um)") 
  +   facet_wrap(.~Species) +   theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%")))

(p21 <- ggplot(stable_iso_area, aes(Treatment, delta13C, fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Leaf 13C content (per 200 um)") 
  +   facet_wrap(.~Species)+theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%")))

ggarrange(p18, p19, p20, p21,labels = c("A", "B", "C", "D"))

#LMA stable isotope boxplots - VS

(p18 <- ggplot(iso_VS, aes(Treatment, SLA, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Treatment", y = "Leaf Mass per Area ")  +   theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%")))
(p16 <- ggplot(iso_VS, aes (x = SLA, y = C, colour = Treatment,shape= Treatment)) +geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+ theme(legend.position = "none")+labs(y = "Leaf C content", x = "Leaf Mass per Area ")+geom_smooth(method=lm,aes(fill=Treatment))+stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE))                                                                                                                      
(p17 <- ggplot(iso_VS, aes (x = SLA, y = N, colour = Treatment,shape= Treatment)) + geom_point()  + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+ theme(legend.position = "none")+labs(y = "Leaf N content", x = "Leaf Mass per Area ")+geom_smooth(method=lm, aes(fill=Treatment)))                            
(p26<-ggplot(iso_VS, aes (x = SLA, y = delta15N, colour = Treatment,shape= Treatment))  +geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+theme(legend.position = "none")+labs(y = "Leaf 15N content ", x = "Leaf Mass per Area ")+geom_smooth(method=lm,aes(fill=Treatment)))
(p27<-ggplot(iso_VS, aes (x = SLA, y = delta13C, colour = Treatment,shape= Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+theme(legend.position = "none")+labs(y = "Leaf 13C content", x = "Leaf Mass per Area ")+geom_smooth(method=lm,aes(fill=Treatment)))
(p28<-ggplot(iso_VS, aes (x = SLA, y = CN_ratio, colour = Treatment,shape= Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(y = "Leaf C:N", x = "Leaf Mass per Area ")+geom_smooth(method=lm,aes(fill=Treatment)))
ggarrange( p18, p16, p17,p26,p27,p28, labels = c("A", "B", "C", "D", "E", "F"))

#LMA stable isotope boxplots -VE 

(p18 <- ggplot(iso_VE, aes(Treatment, SLA, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Treatment", y = "Leaf Mass per Area ")  +   theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%")))
(p16 <- ggplot(iso_VE, aes (x = SLA, y = C, colour = Treatment,shape= Treatment)) +geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+ theme(legend.position = "none")+labs(y = "Leaf C content", x = "Leaf Mass per Area ")+geom_smooth(method=lm,aes(fill=Treatment)))                                                                                                                      
(p17 <- ggplot(iso_VE, aes (x = SLA, y = N, colour = Treatment,shape= Treatment)) + geom_point()  + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+ theme(legend.position = "none")+labs(y = "Leaf N content", x = "Leaf Mass per Area ")+geom_smooth(method=lm, aes(fill=Treatment)))                            
(p26<-ggplot(iso_VE, aes (x = SLA, y = delta15N, colour = Treatment,shape= Treatment))  +geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+theme(legend.position = "none")+labs(y = "Leaf 15N content ", x = "Leaf Mass per Area ")+geom_smooth(method=lm,aes(fill=Treatment)))
(p27<-ggplot(iso_VE, aes (x = SLA, y = delta13C, colour = Treatment,shape= Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+theme(legend.position = "none")+labs(y = "Leaf 13C content", x = "Leaf Mass per Area ")+geom_smooth(method=lm,aes(fill=Treatment)))
(p28<-ggplot(iso_VE, aes (x = SLA, y = CN_ratio, colour = Treatment,shape= Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(y = "Leaf C:N", x = "Leaf Mass per Area ")+geom_smooth(method=lm,aes(fill=Treatment)))
ggarrange( p18, p16, p17,p26,p27,p28, labels = c("A", "B", "C", "D", "E", "F"))

#Nodules 

(p7 <- ggplot(LabHarvest_VS, aes (x = Nodule, y = Nodules_W, colour = Treatment, shape=Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(y = "Nodule Weight (grams)", x = "Nodule count")+geom_smooth(method=lm, aes(fill=Treatment)))
(p5 <- ggplot(VS_H3, aes (x = Average_SM, y = Nodule)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(y = "Nodule Count", x = "Soil Moisture m3/m3")+geom_smooth(method=lm))
(p5 <- ggplot(VS_H3, aes (x = Average_SM, y = Nodule)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(y = "Nodule Count", x = "Soil Moisture m3/m3")+geom_smooth(method=lm))
(p10 <- ggplot(LabHarvest_VS, aes(Harvest, Nodule, fill = Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Harvest", y = "Nodule Count")+   facet_grid(. ~ Treatment) +theme(strip.background = element_rect(colour="black", fill="white",)))
(p11 <- ggplot(LabHarvest_VS, aes(Harvest, Nodules_W, fill = Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Harvest", y = "Nodule Weight (grams)")+   facet_grid(. ~ Treatment)+theme(strip.background = element_rect(colour="black", fill="white",)))
ggarrange(p5, p10, p11,p7,labels = c("A", "B", "C", "D"))

(p5 <- ggplot(VS_H3, aes (x = Average_SM, y = BG_DW)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(y = "Nodule Weight (grams)", x = "Soil Moisture m3/m3")+geom_smooth(method=lm))

#15N & Nodules

(p5 <- ggplot(iso_VS, aes (x =delta15N , y = Nodule, colour = Treatment,shape=Treatment)) +geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+ theme(legend.position = "none")+labs(y = "Nodule count", x = "Leaf 15N content (per 200um)")+geom_smooth(method=lm, aes(fill=Treatment)))                                                                                                                      
(p6 <- ggplot(iso_VS, aes (x = delta15N, y = Nodules_W, colour = Treatment,shape=Treatment)) + geom_point()  + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+ theme(legend.position = "none")+labs(y = "Nodule weight (grams)", x = " Leaf 15N content (per 200um)")+geom_smooth(method=lm,aes(fill=Treatment)))                            
(p7<-ggplot(iso_VS, aes (x = SLA, y = delta15N, colour = Treatment,shape=Treatment))  +geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+theme(legend.position = "none")+labs(x = " Leaf Mass per Area (LMA)", y = "Leaf 15N content")+geom_smooth(method=lm,aes(fill=Treatment)))
(p8<-ggplot(iso_VS, aes (x = SLA, y = Nodule, colour = Treatment,shape=Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+theme(legend.position = "none")+labs(x = "Leaf Mass per Area (LMA)", y = "Nodule count ")+geom_smooth(method=lm, aes(fill=Treatment)))
(p9<-ggplot(iso_VS, aes (x = Nodule, y = Nodules_W, colour = Treatment,shape=Treatment))  +geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+theme(legend.position = "none")+labs(x = "Nodule count", y = "Nodule weight (grams)")+geom_smooth(method=lm, aes(fill=Treatment)))
(p10<-ggplot(iso_VS, aes (x = SLA, y = Nodules_W, colour = Treatment,shape=Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(x = "Leaf Mass per Area (LMA)", y = "Nodule weight (grams) ")+geom_smooth(method=lm, aes(fill=Treatment)))
ggarrange(p5, p6, p7, p8,p10,labels = c("A", "B", "C", "D", "E"))

#BNF

(p18 <- ggplot(iso_VS, aes(Treatment, Nf, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Nitrogen Fixation") +   theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%")))
(p22<- ggplot(iso_VS, aes (x =Nf, y=Nodule, colour = Treatment,shape=Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(x = "Nitrogen fixation", y = "Nodule count")+geom_smooth(method=lm,aes(fill=Treatment))+theme(legend.position = "none"))
(p23<- ggplot(iso_VS, aes (x =Nf, y=Nodules_W, colour = Treatment,shape=Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(x = "Nitrogen fixation", y = "Nodule weight (grams)")+geom_smooth(method=lm,aes(fill=Treatment))+theme(legend.position = "none"))
(p24<- ggplot(iso_VS, aes (x =Nf, y=SLA, colour = Treatment,shape=Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(x = "Nitrogen fixation", y = "Leaf Mass per Area")+geom_smooth(method=lm,aes(fill=Treatment)))
ggarrange(p18,p22,p23,p24, labels = c ("A", "B", "C", "D"))

#Height - mixed model 

p<-ggplot(VS,aes(Week, Height, group=Treatment, col=Treatment, shape= Treatment))  +geom_point() + geom_smooth(method=lm,  aes(fill=Treatment))+theme.clean()+ labs(x = "Week", y = "Height (mm)")
p+ scale_x_continuous(breaks = seq(1,15 , by = 1))

p<-ggplot(VE,aes(Week, Height, group=Treatment, col=Treatment, shape= Treatment))+  geom_point() + geom_smooth(method=lm,  aes(fill=Treatment))+theme.clean()+labs(x = "Week", y = "Height (mm)")
p+ scale_x_continuous(breaks = seq(1,15 , by = 1))



#LICOR Data

(p18 <- ggplot(LICOR, aes(Harvest, Ci, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Harvest", y = "Ci (umol mol)") 
  +   theme(strip.background = element_rect(colour="black", fill="white",))   + facet_grid(Species ~ Treatment))

(p19 <- ggplot(LICOR, aes(Harvest, Amax, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Harvest", y = "Amax(µmolm-²s-¹)") 
  +   theme(strip.background = element_rect(colour="black", fill="white",))   + facet_grid(Species ~ Treatment))

(p20 <- ggplot(LICOR, aes(Harvest, Leaf_Area, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Harvest", y = "LSA (mm2)") 
  +   theme(strip.background = element_rect(colour="black", fill="white",))   + facet_grid(Species ~ Treatment))

(p21 <- ggplot(LICOR, aes(Harvest, Ci_Ca, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Harvest", y = "Ci:Ca") 
  +   theme(strip.background = element_rect(colour="black", fill="white",))   + facet_grid(Species ~ Treatment))

(p27<-ggplot(LICOR, aes (x = Leaf_Area, y = Ci_Ca, colour = Treatment ,shape= Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+theme(legend.position = "none")+labs(y = "Ci:Ca", x = "LSA (mm2) ")+geom_smooth(method=lm,aes(fill=Treatment))+   facet_grid( ~ Species))
(p28<-ggplot(LICOR, aes (x = Leaf_Area, y = Amax, colour = Treatment,shape= Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(y = "Amax (µmolm-²s-¹)", x = "LSA (mm2) ")+geom_smooth(method=lm,aes(fill=Treatment))+   facet_grid( ~ Species)+stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)) 

ggarrange(p21 ,p19,p27,p28, labels =c ("A", "B", "C","D"))

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


fit1 <- lm(Leaf_Area ~ Ci_Ca, data = LICOR)
ggplotRegression(fit1)
