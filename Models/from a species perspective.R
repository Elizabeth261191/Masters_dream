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

#looking at Harvests grouped together 

LabHarvest_VS<-subset(LabHarvest, Species=="VS")
LabHarvest_VE<-subset(LabHarvest, Species=="VE")

#no significant values 

(LabHarvest_VS.p <- ggplot(LabHarvest_VS, aes(Tag, AG_DW)) + geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0)) + labs(x = "Species & Treatment (%)", y = "Aboveground Dry weight (grams)"))(LabHarvest_VE.p <- ggplot(LabHarvest_VE, aes(Tag, AG_DW)) + geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0)) + labs(x = "Species & Treatment (%)", y = "Aboveground Dry weight (grams)"))


(LabHarvest_VE_TDW.p <- ggplot(LabHarvest_VE, aes(Tag, Total_DW)) + geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0)) + labs(x = "Species & Treatment (%)", y = "Total Dry weight (grams)"))

#VE and total biomass 
LabHarvest_VE_TDW.m<-lm(Total_DW~ Treatment+Harvest , data= LabHarvest_VE)summary(LabHarvest_VE_TDW.m)
Call:
  lm(formula = Total_DW ~ Treatment + Harvest, data = LabHarvest_VE)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.36344 -0.05772  0.01026  0.07086  0.39156 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)    0.14356    0.02876   4.992 2.60e-06 ***
  Treatment0.08  0.09316    0.03138   2.969  0.00376 ** 
  Treatment0.16  0.09970    0.03086   3.231  0.00168 ** 
  Harvest2       0.13500    0.03154   4.281 4.34e-05 ***
  Harvest3       0.37818    0.03149  12.011  < 2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.1297 on 98 degrees of freedom
Multiple R-squared:  0.6241,	Adjusted R-squared:  0.6087 
F-statistic: 40.67 on 4 and 98 DF,  p-value: < 2.2e-16

#VE and Height 
LabHarvest_VE_Height.m<-lm(Height~ Treatment+Harvest , data= LabHarvest_VE)
summary(LabHarvest_VE_Height.m)

Call:
  lm(formula = Height ~ Treatment + Harvest, data = LabHarvest_VE)

Residuals:
  Min      1Q  Median      3Q     Max 
-65.486 -19.090  -1.693  14.009  79.307 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)     73.934      5.919  12.491  < 2e-16 ***
  Treatment0.08   25.196      6.458   3.902 0.000175 ***
  Treatment0.16   24.366      6.351   3.836 0.000221 ***
  Harvest2        54.552      6.491   8.405 3.45e-13 ***
  Harvest3        79.393      6.480  12.251  < 2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 26.7 on 98 degrees of freedom
Multiple R-squared:  0.6401,	Adjusted R-squared:  0.6254 
F-statistic: 43.58 on 4 and 98 DF,  p-value: < 2.2e-16

#VE and total above ground biomass 
LabHarvest_VE_AGDW.m<-lm(AG_DW~ Treatment+Harvest , data= LabHarvest_VE)
summary(LabHarvest_VE_AGDW.m)

Call:
  lm(formula = AG_DW ~ Treatment + Harvest, data = LabHarvest_VE)

Residuals:
  Min        1Q    Median        3Q       Max 
-0.227195 -0.043085  0.002024  0.051974  0.244805 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)    0.13206    0.02034   6.493 3.48e-09 ***
  Treatment0.08  0.06527    0.02219   2.942  0.00407 ** 
  Treatment0.16  0.07047    0.02182   3.229  0.00169 ** 
  Harvest2       0.07992    0.02230   3.583  0.00053 ***
  Harvest3       0.22267    0.02227  10.000  < 2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.09176 on 98 degrees of freedom
Multiple R-squared:  0.5435,	Adjusted R-squared:  0.5248 
F-statistic: 29.17 on 4 and 98 DF,  p-value: 5.692e-16

#aboveground biomass and VS
LabHarvest_VS_AGDW.m<-lm(AG_DW~ Treatment+Harvest , data= LabHarvest_VS)
summary(LabHarvest_VS_AGDW.m)

Call:
  lm(formula = AG_DW ~ Treatment + Harvest, data = LabHarvest_VS)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.38970 -0.08714 -0.00688  0.06536  0.53930 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)    0.16373    0.03734   4.385 2.74e-05 ***
  Treatment0.08 -0.00885    0.03933  -0.225    0.822    
Treatment0.16 -0.04961    0.03961  -1.252    0.213    
Harvest2       0.22483    0.04131   5.442 3.42e-07 ***
  Harvest3       0.36282    0.03941   9.205 3.45e-15 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.1702 on 106 degrees of freedom
Multiple R-squared:  0.4522,	Adjusted R-squared:  0.4315 
F-statistic: 21.88 on 4 and 106 DF,  p-value: 3.5e-13

#Height and VS
LabHarvest_VS_Height.m<-lm(Height~ Treatment+Harvest , data= LabHarvest_VS)
summary(LabHarvest_VS_Height.m)

Call:
  lm(formula = Height ~ Treatment + Harvest, data = LabHarvest_VS)

Residuals:
  Min       1Q   Median       3Q      Max 
-181.506  -26.823    4.657   25.960  101.562 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)    121.804     10.100  12.060  < 2e-16 ***
  Treatment0.08    4.933     10.638   0.464    0.644    
Treatment0.16  -14.163     10.715  -1.322    0.189    
Harvest2        91.702     11.176   8.205 5.87e-13 ***
  Harvest3       124.269     10.662  11.655  < 2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 46.05 on 106 degrees of freedom
Multiple R-squared:  0.5783,	Adjusted R-squared:  0.5624 
F-statistic: 36.34 on 4 and 106 DF,  p-value: < 2.2e-16

#Total weight VS
LabHarvest_VS_TDW.m<-lm(Total_DW~ Treatment+Harvest , data= LabHarvest_VS)
summary(LabHarvest_VS_TDW.m)

Call:
  lm(formula = Total_DW ~ Treatment + Harvest, data = LabHarvest_VS)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.77148 -0.15615 -0.00086  0.12337  1.08452 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)    0.27443    0.07156   3.835 0.000213 ***
  Treatment0.08 -0.06983    0.07537  -0.926 0.356331    
Treatment0.16 -0.14147    0.07592  -1.863 0.065178 .  
Harvest2       0.41390    0.07919   5.227  8.7e-07 ***
  Harvest3       0.76004    0.07554  10.061  < 2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.3263 on 106 degrees of freedom
Multiple R-squared:  0.4993,	Adjusted R-squared:  0.4805 
F-statistic: 26.43 on 4 and 106 DF,  p-value: 3.268e-15


#dry weight VS
(LabHarvest_VS.p <- ggplot(LabHarvest_VS, aes(Tag, Total_DW)) + geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0)) + labs(x = "Species & Treatment (%)", y = "Total Dry weight (grams)"))
(LabHarvest_VS_NOD.p <- ggplot(LabHarvest_VS, aes(Tag, Nodule)) + geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0)) + labs(x = "Species & Treatment (%)", y = " Nodule"))
LabHarvest_VS_NOD.m<-lm(Nodule~ Treatment+Harvest , data= LabHarvest_VS)

summary(LabHarvest_VS_NOD.m)

Call:
  lm(formula = Nodule ~ Treatment + Harvest, data = LabHarvest_VS)

Residuals:
  Min      1Q  Median      3Q     Max 
-46.540  -9.283  -0.677   8.324  93.066 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)     -8.324      4.331  -1.922 0.057298 .  
Treatment0.08    9.001      4.562   1.973 0.051088 .  
Treatment0.16   17.607      4.595   3.832 0.000216 ***
  Harvest2        21.373      4.793   4.459 2.05e-05 ***
  Harvest3        37.256      4.572   8.148 7.85e-13 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 19.75 on 106 degrees of freedom
Multiple R-squared:  0.4292,	Adjusted R-squared:  0.4077 
F-statistic: 19.93 on 4 and 106 DF,  p-value: 2.94e-12


#nodule Weight
LabHarvest_VS_NODW.m<-lm(Nodules_W~ Treatment+Harvest , data= LabHarvest_VS)
summary(LabHarvest_VS_NODW.m)

Call:
  lm(formula = Nodules_W ~ Treatment + Harvest, data = LabHarvest_VS)

Residuals:
  Min        1Q    Median        3Q       Max 
-0.019986 -0.005017  0.000268  0.001681  0.060268 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   -0.0013820  0.0025906  -0.533   0.5948    
Treatment0.08  0.0007185  0.0027288   0.263   0.7928    
Treatment0.16  0.0039730  0.0027486   1.445   0.1513    
Harvest2       0.0069655  0.0028668   2.430   0.0168 *  
  Harvest3       0.0173954  0.0027349   6.360 5.21e-09 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.01181 on 106 degrees of freedom
Multiple R-squared:  0.2917,	Adjusted R-squared:  0.2649 
F-statistic: 10.91 on 4 and 106 DF,  p-value: 1.9e-07

