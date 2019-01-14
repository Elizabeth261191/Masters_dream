#how is the environment effects by drought 
#drought~Temp+ Precipitation 
#choosing variables in very important two variables together
#ouverfitting variables- adapted to that situations finate amount of variance. 
#model to converge- 


library(readr)
toolik_plants <- read_csv("CC-model-design/toolik_plants.csv)
View(toolik_plants)

#Question 1: How has plant species richness changed over time at Toolik Lake?
  #Hypothesis 1: Plant species richness has increased over time at Toolik Lake
  #Null Hypothesis: Plant species richness has not changed over time at Toolik Lake.
  #Hypothesis 2: Plant species richness has decreased over time at Toolik Lake.
#Question 2: How does mean annual temperature influence plant species richness?
  #Hypothesis 1: Higher temperatures correspond with higher species richness.

library(dplyr)  # for data manipulation
library(ggplot2)  # for data visualisation
library(lme4)  # for models
library(sjPlot)  # to visualise model outputs
library(ggeffects)  # to visualise model predictions
library(MCMCglmm)  # for models
 library(MCMCvis)  # to visualise model outputs
library(brms)  # for models
library(stargazer)  # for tables of model outputs

str(toolik_plants)
#plots are numbers so they need to be transformed into a catergorical value 
toolik_plants$Plot <- as.factor(as.character(toolik_plants$Plot))

#look at the unique plot names 
unique(toolik_plants$Site)
length(unique(toolik_plants$Site))

# Group the data frame by Site to see the number of blocks per site
toolik_plants %>% group_by(Site) %>%
                            summarise(block.n = length(unique(Block)))

unique(toolik_plants$Site)
length(unique(toolik_plants$Site))
                            
First, we have five sites (06MAT, DH, MAT, MNT and SAG).
                            
  # Group the data frame by Site to see the number of blocks per site
toolik_plants %>% group_by(Site) %>%
 summarise(block.n = length(unique(Block)))
                            
#Within each site, there are different numbers of blocks - some sites have three sample blocks, others have four or five.
                            
toolik_plants %>% group_by(Block) %>%
 summarise(plot.n = length(unique(Plot)))
                            
#Within each block, there are eight smaller plots.
unique(toolik_plants$Year)

length(unique(toolik_plants$Species))
unique(toolik_plants$Species)

# We use ! to say that we want to exclude
# all records that meet the criteria
                            
 # We use %in% as a shortcut - we are filtering by many criteria
# but they all refer to the same column - Species
toolik_plants <- toolik_plants %>%
filter(!Species %in% c("Woody cover", "Tube",
                            "Hole", "Vole trail",
                            "removed", "vole turds",
                            "Mushrooms", "Water",
                            "Caribou poop", "Rocks",
                            "mushroom", "caribou poop",
                            "animal litter", "vole poop",
                            "Vole poop", "Unk?"))

# A much longer way to achieve the same purpose is:
# toolik_plants <- toolik_plants %>%
#  filter(Species != "Woody cover" &
#	       Species != "Tube" &
#         Species != "Hole"&
#				 Species != "Vole trail"....))
# But you can see how that involves unnecessary repetition.
length(unique(toolik_plants$Species))
# Calculate species richness
toolik_plants <- toolik_plants %>%
                            group_by(Year, Site, Block, Plot) %>%
                            mutate(Richness = length(unique(Species)))

#making a histogram 
# To both make and plot the histogram, we surround the whole
# code chunk with ()
                            (hist <- ggplot(toolik_plants, aes(x = Richness)) +
                            geom_histogram() +
                            theme_classic())

(hist2 <- ggplot(toolik_plants, aes(x = Relative.Cover)) +
+     geom_histogram() +
                            +     theme_classic())
#linear model, no random effects and asumptions are met
plant_m <- lm(Richness ~ I(Year-2007), data = toolik_plants)
 summary(plant_m)
#look for assumptions
plot(plant_m)

#examining the site as a random factor 
plant_m_plot <- lmer(Richness ~ I(Year-2007) + (1|Site), data = toolik_plants)
summary(plant_m_plot)

#examining site and block
plant_m_plot2 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block), data = toolik_plants)
summary(plant_m_plot2)

#examining site, block and plot
plant_m_plot3 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block/Plot), data = toolik_plants)
summary(plant_m_plot3)

#setting a theme for graphs
set_theme(base = theme_bw())

# Visualises random effects 
(re.effects <- plot_model(plant_m_plot3, type = "re", show.values = TRUE))
                            save_plot(filename = "model_re.png",
                            height = 11, width = 9)
                            
# To see the estimate for our fixed effect (default), Year
                            (fe.effects <- plot_model(plant_m_plot3, show.values = TRUE))
                            save_plot(filename = "model_fe.png",
                            height = 11, width = 9)  

#  taking into consideration year as a random effect                           
plant_m_temp <- lmer(Richness ~ Mean.Temp + (1|Site/Block/Plot) + (1|Year),
                      data = toolik_plants)
                            summary(plant_m_temp)

# Visualise the random effect terms
(temp.re.effects <- plot_model(plant_m_temp, type = "re", show.values = TRUE))
                            save_plot(filename = "model_temp_re.png",
                            height = 11, width = 9)
                            
# Visualise the fixed effect
(temp.fe.effects <- plot_model(plant_m_temp, show.values = TRUE))
                            save_plot(filename = "model_temp_fe.png",
                            height = 11, width = 9)

#random slopes versus random intersects eg how each sp has a diff relationship with tempreture 
plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Site/Block/Plot) + (1|Year),
                 data = toolik_plants)
                            

summary(plant_m_rs)

#simplifying the model structure
plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Plot) + (1|Year),
                 data = toolik_plants)
summary(plant_m_rs)

#visualising the results 
(plant.re.effects <- plot_model(plant_m_rs, type = "re", show.values = TRUE))
save_plot(filename = "model_plant_re.png",
                            height = 17, width = 15)
                            
                            (plant.fe.effects <- plot_model(plant_m_rs, show.values = TRUE))
                            save_plot(filename = "model_plant_fe.png",
                            height = 14, width = 9)
#predicting the data 
ggpredict(plant_m_rs, terms = c("Mean.Temp")) %>% plot()
save_plot(filename = "model_temp_richness.png",
                            height = 9, width = 9)
                            
ggpredict(plant_m_rs, terms = c("Mean.Temp", "Plot"), type = "re") %>% plot()
save_plot(filename = "model_temp_richness_rs_ri.png",
                            height = 9, width = 9)

# Overall predictions - note that we have specified just mean temperature as a term
predictions <- ggpredict(plant_m_rs, terms = c("Mean.Temp"))
                            
(pred_plot1 <- ggplot(predictions, aes(x, predicted)) +
geom_line() +
geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
scale_y_continuous(limits = c(0, 22)) +
labs(x = "Predicted species richness\n", y = "\nMean annual temperature"))
                            
ggsave(pred_plot1, filename = "overall_predictions.png",
 height = 5, width = 5)


# Predictions for each grouping level (here plot which is a random effect)
# re stands for random effect
    predictions_rs_ri <- ggpredict(plant_m_rs, terms = c("Mean.Temp", "Plot"), type = "re")
                            
 (pred_plot2 <- ggplot(predictions_rs_ri, aes(x = x, y = predicted, colour = group)) +
stat_smooth(method = "lm", se = FALSE)  +
scale_y_continuous(limits = c(0, 22)) +
 labs(x = "Predicted species richness\n", y = "\nMean annual temperature"))
                            
(pred_plot3 <- ggplot(predictions_rs_ri, aes(x = x, y = predicted, colour = group)) +
+     stat_smooth(method = "lm", se = FALSE)  +
                            +     labs(x = "Predicted species richness\n", y = "\nMean annual temperature"))

#heirachrhical models 
plant_mcmc <- MCMCglmm(Richness ~ I(Year - 2007), random = ~Site,
                       family = "poisson",  data = toolik_plants)
plant_mcmc <- MCMCglmm(Richness ~ I(Year-2007), random = ~Block + Plot,
                       family = "poisson", data = toolik_plants)

#Set weakly informative priors
prior2 <- list(R = list(V = 1, nu = 0.002),
                            G = list(G1 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000),
                            G2 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000),
                            G3 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000)))
                            
                            # Extract just the Betula nana data
                            betula <- filter(toolik_plants, Species == "Bet nan")
                            
                            betula_m <- MCMCglmm(round(Relative.Cover*100) ~ Year, random = ~Site + Block + Plot,
                            family = "poisson", prior = prior2, data = betula)
                            
                            summary(betula_m)
                            plot(betula_m$VCV)
                            plot(betula_m$Sol)