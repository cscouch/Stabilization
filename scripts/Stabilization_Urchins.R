#Script summarizes wild colonies on experimental and reference site
#manually restart R
library(tidyverse)
library(here)
library(ggridges)
library(brms)
library(tidybayes)


#LOAD DATA
urchin<-read_csv(here("data","Stablization_Urchins_T0-6monthPO.csv")) #use this for all ggsave, write
# urchin<- urchin %>% 
#   rename(SPCODE=Species) %>%
#   #mutate(Survey_Period = recode(Survey_Period, T0_Post_Installation = 'T0', Baseline = 'Baseline', T1_6months =  'T1 (6months)'))%>%
#   left_join(lu)

levels(as.factor(urchin$Survey_Period))
levels(as.factor(urchin$Plot_ID))

table(urchin$Survey_Period,urchin$Treatment)

tmp<- subset(urchin,Treatment=="Reference" & Survey_Period=="T1_6mo_preoutplant")
table(tmp$Plot_ID)

tmp2<- subset(urchin,Treatment=="Reference" & Survey_Period=="T1_6mo_postoutplant")
table(tmp2$Plot_ID)


# Use baseline control data for post-installation control
pi.c<-urchin %>% filter((Survey_Period=="Baseline") & (Treatment=="Control"))
pi.c$Survey_Period <-"T0_Post_Installation"

# Use preoutplant control data for postoutplant control
t1pre.c<-urchin %>% filter((Survey_Period=="T1_6mo_preoutplant") & (Treatment=="Control"))
t1pre.c$Survey_Period <-"T1_6mo_postoutplant"

#Combine into dataframe
urchin.new<-rbind(pi.c,t1pre.c,urchin)

table(urchin.new$Survey_Period,urchin.new$Treatment)

#Remove reference site data and calculate abudance/plot
col.tot<-as.data.frame(urchin.new %>% 
                         #filter(Treatment!="Reference")   %>%                      
                         group_by(Survey_Period, Treatment,Plot_ID) %>% 
                         summarise(n = n()))

View(col.tot)



#Plotting Urchin abundance for first 2 time points

col.tot$Treatment <- factor(col.tot$Treatment, levels = c("Control", "Mesh", "Boulder","Reference"))
col.tot$Survey_Period <- factor(col.tot$Survey_Period, levels = c("Baseline","T0_Post_Installation","T1_6mo_preoutplant","T1_6mo_postoutplant"))
col.tot$T_SP<-as.factor(paste(col.tot$Survey_Period,col.tot$Treatment,sep="_"))
  
col.tot %>%
  filter(Survey_Period %in% c("Baseline","T1_6mo_preoutplant")) %>%
  mutate(Survey_Period = recode(Survey_Period, Baseline = 'Baseline', T1_6mo_preoutplant =  'T1 (6months)'))%>%
  filter(T_SP !="Baseline_Reference") %>%
ggplot(., aes(x = Treatment, y = n, fill = Survey_Period)) +
  geom_boxplot(position = position_dodge(width = 0.6), outlier.shape = NA) +
  geom_jitter(color = "#4D4D4D",
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6),
              size = 1, alpha = 0.8) +
  labs(x = "Treatment", y = "Colony Abundance") +
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
    legend.position = "bottom"
  )

###By Genus
col.gen<-as.data.frame(urchin %>% 
                         filter(!is.na(Size)) %>%
                         group_by(Survey_Period, Treatment,Plot_ID,Urchin_Code) %>% 
                         summarise(n = n()))

View(col.gen)

#Calculate average abundance/plot
ave.urchin.gen<-as.data.frame(col.gen %>% 
                         group_by(Survey_Period, Treatment,Urchin_Code) %>% 
                         summarise(ave.abun = mean(n)))

urchin_pct <- col.gen %>%
  group_by(Survey_Period,Treatment, Urchin_Code) %>%
  summarise(ave_abun = mean(n, na.rm = TRUE), .groups = "drop") %>%
  group_by(Survey_Period,Treatment) %>%
  mutate(total_abun = sum(ave_abun),
         percent = (ave_abun / total_abun) * 100)
urchin_pct

#col.gen<-col.gen %>% filter(Treatment!="Reference")

col.gen$Treatment <- factor(col.gen$Treatment, levels = c("Control", "Mesh", "Boulder","Reference"))
col.gen$T_SP<-as.factor(paste(col.gen$Survey_Period,col.gen$Treatment,sep="_"))

col.gen %>%
  filter(Survey_Period == "T1_6mo_preoutplant") %>%
  filter(Treatment %in% c("Boulder","Reference")) %>%
  filter(T_SP !="Baseline_Reference") %>%
ggplot(.,aes(Treatment,n, fill=Urchin_Code))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Treatment", y = "Urchin Abundance")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),  
        legend.position="bottom") 
