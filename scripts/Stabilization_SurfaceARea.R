#Script summarizes plots boulder pile surface area estimates
#manually restart R
library(tidyverse)
library(here)
library(ggridges)
library(brms)
library(tidybayes)


#LOAD DATA
sa<-read_csv(here("data","Stablization_SA_T0-6monthPO.csv")) 

sa<-as.data.frame(sa %>% 
                  mutate(Survey_Period = recode(Survey_Period, T0_Post_Installation = 'T0 Post Installation', 
                                                T1_6mo_postoutplant =  'T1 (6months post-outplant)',T1_6mo_preoutplant =  'T1 (6months pre-outplant)')))

sa$Survey_Period <- factor(sa$Survey_Period, levels = c("T0 Post Installation","T1 (6months pre-outplant)","T1 (6months post-outplant)"))


#Quick plots to see if there are any issues
ggplot(sa,aes(Survey_Period,SArea,fill=Plot_ID))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~Plot_ID)

ggplot(sa,aes(factor(Plot_ID),SArea,fill=Survey_Period))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~Survey_Period)

ggplot(sa,aes(Survey_Period,SArea))+
  geom_boxplot(alpha = 0.7, outlier.shape = NA)+
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.5)
    
  
ggplot(sa,aes(Survey_Period,Rugosity))+
  geom_boxplot(alpha = 0.7, outlier.shape = NA)+
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.5)+
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_blank(),
    legend.position = "none"
  )

ggsave(filename = here("plots", "SA_TO to T1.jpg"),
       plot = last_plot(),                                
       width = 7, height = 6, units = "in", dpi = 300)
