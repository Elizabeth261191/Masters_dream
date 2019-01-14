library(readxl)
Babies_height_final_FINAL <- read_excel("C:/Users/telfo/Desktop/Babies height final FINAL.xlsx")
View(Babies_height_final_FINAL)
Height.data<-Babies_height_final_FINAL

Height.data$Treatment <- as.factor(Height.data$Treatment)
Height.data$Harvest<-as.factor(Height.data$Harvest)
Height.data$Species<-as.factor(Height.data$Species)

Height_H1 <- subset(Height.data,Harvest=="1")
Height_H1_VS <- subset(Height_H1,Species=="VS")
Height_H1_VE <- subset(Height_H1,Species=="VE")

Height_H2 <- subset(Height.data,Harvest=="2")
Height_H2_VS <- subset(Height_H2,Species=="VS")
Height_H2_VE <- subset(Height_H2,Species=="VE")

Height_H3 <- subset(Height.data,Harvest=="3")
Height_H3_VS <- subset(Height_H3,Species=="VS")
Height_H3_VE <- subset(Height_H3,Species=="VE")

Height_VS<-subset(Height.data, Species=="VS")
Height_VE<-subset(Height.data, Species=="VE")