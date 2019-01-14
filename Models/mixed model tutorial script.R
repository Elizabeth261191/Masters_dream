#code for mixed effect models 

load("dragons.RData")
head(dragons)

hist(dragons$testScore)

dragons$bodyLength2 <- scale(dragons$bodyLength)

basic.lm <- lm(testScore ~ bodyLength2, data = dragons)
summary(basic.lm)

library(ggplot2)  # load the package
ggplot(dragons, aes(x = bodyLength, y = testScore)) +
  geom_point() +
  geom_smooth(method = "lm") 

plot(basic.lm, which = 1)

plot(basic.lm, which = 2)

boxplot(testScore ~ mountainRange, data = dragons

ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange)) +
  geom_point(size = 2) +
  theme_classic() +
  theme(legend.position = "none")

ggplot(aes(bodyLength, testScore), data = dragons) + 
  geom_point() + 
  facet_wrap(~ mountainRange) + 
  xlab("length") + 
  ylab("test score")


mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)

library(lme4)
mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)

#variance for mountainRange = 339.7
#Mountain ranges are clearly important
#We can take the variance for the mountainRange and divide it by the total variance

339.7/(339.7 + 223.8)

#the differences between mountains explains 60% of the variance 

#examining nesting and implicait nesting 
head(dragons)  # we have site and mountainRange
str(dragons)  # we took samples from three sites per mountain range and eight mountain ranges in total

#the sites could be correlated so that should be taken into consideration 
dragons <- within(dragons, sample <- factor(mountainRange:site))

#the second mixed effects model 
mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons) 
#treats the two randoms effects as if they are crossed 


#Is there an association between body length and intelligence in dragons after controlling for variation in mountain ranges and sites within mountain ranges?
mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)  # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer2)

#account for all the mountain-range-level and all the site-level influences
ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
  +     facet_wrap(~mountainRange, nrow=3) +
  +     geom_point() +
  +     theme_classic() +
  +     geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred)) +
  +     theme(legend.position = "none")

#table presentation
library(stargazer)
stargazer(mixed.lmer2, type = "text",
          +           digits = 3,
          +           star.cutoffs = c(0.05, 0.01, 0.001),
          +           digit.separator = "")

#using an anova 
full.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), 
                  data = dragons, REML = FALSE)
reduced.lmer <- lmer(testScore ~ 1 + (1|mountainRange) + (1|sample), 
                     data = dragons, REML = FALSE)

#comparing the anovas 
anova(reduced.lmer, full.lmer)
#I think the variance is about the same 



