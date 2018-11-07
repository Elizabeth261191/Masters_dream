library(readxl)
LabHarveste <- read_excel("C:/Users/telfo/Desktop/LabHarveste.xlsx")
View(LabHarveste)
LabHarvest<-LabHarveste
library(ggplot2)

#setting factors
LabHarvest$Treatment <- as.factor(LabHarvest$Treatment)
LabHarvest$Harvest<-as.factor(LabHarvest$Harvest)
LabHarvest$Tag<-as.factor(LabHarvest$Tag)

#subsetting 
LabHarvest_H1 <- subset(LabHarvest,Harvest=="1")
LabHarvest_H1_VS <- subset(LabHarvest_H1,Species=="VS")
LabHarvest_H1 <- subset(LabHarvest,Harvest=="1")
LabHarvest_H1_VE <- subset(LabHarvest_H1,Species=="VE")
LabHarvest_H2 <- subset(LabHarvest,Harvest=="2")
LabHarvest_H2_VS <- subset(LabHarvest_H2,Species=="VS")
LabHarvest_H2 <- subset(LabHarvest,Harvest=="2")
LabHarvest_H2_VE <- subset(LabHarvest_H2,Species=="VE")
LabHarvest_H3 <- subset(LabHarvest,Harvest=="3")
LabHarvest_H3_VS <- subset(LabHarvest_H3,Species=="VS")
LabHarvest_H3 <- subset(LabHarvest,Harvest=="3")
LabHarvest_H3_VE <- subset(LabHarvest_H3,Species=="VE")

LabHarvest_VS<-subset(LabHarvest, Species=="VS")
LabHarvest_VE<-subset(LabHarvest, Species=="VE")

install.packages("agridat")
library(agridat)
theme.clean <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
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

(H2_AGDW.p <- ggplot(LabHarvest_H2, aes(Tag, AG_DW)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    theme.clean() +  
    theme(axis.text.x = element_text(size = 12, angle = 0)) +
    labs(x = "Species & Treatment (%)", y = "AboveGround Dry Weight (grams)"))

#Total dry Weight
LabHarvest_H2_VS_TDW.m<-lm(Total_DW~Treatment, data= LabHarvest_H2_VS)
summary(LabHarvest_H2_VS_TDW.m)


LabHarvest_H2_VE_TDW.m<-lm(Total_DW~Treatment, data= LabHarvest_H2_VE)
summary(LabHarvest_H2_VE_TDW.m)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.33477 -0.08281 -0.00450  0.07747  0.45868 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   0.2215557  0.0324056   6.837 3.15e-09 ***
  Treatment0.08 0.1088885  0.0422891   2.575  0.01228 *  
  Treatment0.16 0.0003333  0.0412584   0.008  0.99358    
SpeciesVS     0.1128794  0.0344713   3.275  0.00169 ** 
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.144 on 66 degrees of freedom
Multiple R-squared:  0.2303,	Adjusted R-squared:  0.1953 
F-statistic: 6.581 on 3 and 66 DF,  p-value: 0.000585
#treatment has significant effect on VS total but it does on VE 
#no signifcant effect when grouped in a model 

#root weight vs treatment 
(H2_BGDW.p <- ggplot(LabHarvest_H2, aes(Tag, BG_DW)) +
    +  geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    +  theme.clean() +  
    + theme(axis.text.x = element_text(size = 12, angle = 0)) +
    + labs(x = "Species & Treatment (%)", y = "Root Dry Weight (grams)"))
#no signifant lm value when species are grouped, or VS or VE

#Aboveg weight vs treatment 
(H2_AGDW.p <- ggplot(LabHarvest_H2, aes(Tag, AG_DW)) +
    +  geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    +  theme.clean() +  
    + theme(axis.text.x = element_text(size = 12, angle = 0)) +
    + labs(x = "Species & Treatment (%)", y = "Aboveground Dry Weight (grams)"))
LabHarvest_H2_AGDW.m<-lm(AG_DW~Treatment, data= LabHarvest_H2)
summary(LabHarvest_H2_AGDW.m)

Call:
  lm(formula = AG_DW ~ Treatment, data = LabHarvest_H2)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.28078 -0.08898 -0.03165  0.06722  0.51243 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   0.273654   0.030210   9.058 3.01e-13 ***
  Treatment0.08 0.115918   0.045195   2.565   0.0126 *  
  Treatment0.16 0.007129   0.044095   0.162   0.8721    
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.154 on 67 degrees of freedom
Multiple R-squared:  0.1052,	Adjusted R-squared:  0.07848 
F-statistic: 3.938 on 2 and 67 DF,  p-value: 0.02415

#leaves Vs treatment 
(LabHarvest_H2_leaf.p <- ggplot(LabHarvest_H2, aes(Tag, Leaves_DW)) + geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0)) + labs(x = "Species & Treatment (%)", y = "Leaf Dry weight (grams)"))
LabHarvest_H2_leafDW.m<-lm(Leaves_DW~Treatment + Species , data= LabHarvest_H2)
summary(LabHarvest_H2_leafDW.m)

Call:
  lm(formula = Leaves_DW ~ Treatment + Species, data = LabHarvest_H2)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.16412 -0.04116 -0.00210  0.03606  0.17499 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)    0.12519    0.01559   8.029 2.35e-11 ***
  Treatment0.08  0.06540    0.02035   3.214  0.00203 ** 
  Treatment0.16  0.01550    0.01985   0.781  0.43765    
SpeciesVS      0.02343    0.01659   1.413  0.16244    
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.06926 on 66 degrees of freedom
Multiple R-squared:  0.1683,	Adjusted R-squared:  0.1305 
F-statistic: 4.453 on 3 and 66 DF,  p-value: 0.006554

#height vs treatment 
(LabHarvest_H2_Height.p <- ggplot(LabHarvest_H2, aes(Tag, Height)) + geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0)) + labs(x = "Species & Treatment (%)", y = "Height (mm)"))
LabHarvest_H2_Height.m<-lm(Height~Treatment + Species , data= LabHarvest_H2)
summary(LabHarvest_H2_Height.m)
#grouped together doesnt have a significant p value or per species 





