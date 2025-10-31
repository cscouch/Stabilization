


# Bayesian ----------------------------------------------------------------

# 
# #Bayesian 2 way anova #Rhat and ESS 1st pass at diagnostics- did models converge reead up on additional dianotics
# #Remove reference site data and calculate abudance/plot
# col.tot<-as.data.frame(colony.new %>% 
#                          #filter(Treatment!="Reference")   %>%
#                          #filter(Survey_Period %in% c("T0_Post_Installation","T1_6mo_preoutplant")) %>%
#                          group_by(Survey_Period, Treatment,Plot_ID) %>% 
#                          summarise(n = n()))
# 
# 
# mod1<-brm(n~Treatment*Survey_Period + (1|Plot_ID),data=col.tot)
# #plot(conditional_effects(mod1)) #plots with effect sizes, plot raw data in the background with low alpha
# #try log transformation
# #model slection 
# #loo(mod1, mod2) - report elpd_loo
# #plot density plot of each treatment- look in tidybayes
# 
# ranef(mod1)#plot effects
# #modify the map to color by intercept
# #standardize all response variables on z scores to be able to compare across metrics 
# #standardized effect size for each metrics in one plot with standardized data then 

#Stats
# library(car)
# 
# col.tot$sqrt_abun<-sqrt(col.tot$n)
# mod <- aov(sqrt_abun ~ Survey_Period * Treatment, data = col.tot)
# plot(mod, which = 2)
# qqPlot(mod$residuals,id = FALSE)
# hist(mod$residuals)
# leveneTest(mod)
# 
# mod <- aov(sqrt_abun ~ Survey_Period * Treatment, data = col.tot)
# 
# library(lme4)
# mod1<-glmer(n~Survey_Period * Treatment+(1|Plot_ID), family="poisson",data=col.tot)
# nullmod<-glmer(n~Survey_Period * Treatment+(1|Plot_ID), family="poisson",data=col.tot)
# anova(mod1,nullmod)
# 
# mod1<-glmer(n~Survey_Period * Treatment+(1|Plot_ID), family="poisson",data=col.tot)
# mod2<-glmer(n~Survey_Period+(1|Plot_ID), family="poisson",data=col.tot)
# anova(mod1,mod2)
# 
# mod1<-glmer(n~Survey_Period * Treatment+(1|Plot_ID), family="poisson",data=col.tot)
# mod3<-glmer(n~Treatment+(1|Plot_ID), family="poisson",data=col.tot)
# anova(mod1,mod3)
# 
# col.tot<-subset(col.tot, Treatment=="Boulder")
# mod1<-glmer(n~Survey_Period +(1|Plot_ID), family="poisson",data=col.tot)
# 
# library(emmeans)
# emmeans(mod1, list(pairwise ~ Survey_Period), adjust = "tukey")
# 
# 
# 1-logLik(mod1)/logLik(nullmod) # Calculate McFadden's R2 =0.3081
