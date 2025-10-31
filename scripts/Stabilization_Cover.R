#Script summarizes wild colonies on experimental and reference site
#manually restart R
library(tidyverse)
library(here)
library(ggridges)
library(brms)
library(tidybayes)


#LOAD DATA & LOOK-UP tables
cover_raw<-read_csv(here("data","Stablization_Cover_T0-6monthPO.csv")) 
lu<-read_csv(here("data","Stabilization_Benthic_Cover_Codes.csv")) 


#Remove bare MESH and CEM from calculations -treating these like we do Tape, wand, shaddow in our normal benthic cover data
cover_new <- cover_raw %>%
  filter(!Benthic_Code %in% c("MESH","CEM"))

#Modify Substrate Type to add unconsolidated
cover_new <- cover_new %>%
  mutate(Substrate_Type2 = case_when(Benthic_Code %in% c("PEB", "SAND") ~ "Unconsolidated",TRUE ~ Substrate_Type),
         Substrate_Type2 = case_when(Substrate_Type2 %in% c("Rubble") ~ "Unconsolidated",TRUE ~ Substrate_Type2))%>%
  rename(CODE = Benthic_Code)%>%
  left_join(lu)
  

head(cover_new)

#calculate % unconsolidated and hard substrate for site descriptors
hard_soft_cover <- cover_new %>%
  group_by(Survey_Period, Treatment,Plot_ID,Substrate_Type2) %>% 
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(total_points = sum(count),percent = (count / total_points) * 100) %>% # total rows per Survey_Period × Treatment
  ungroup()

per_soft<-hard_soft_cover %>%
  filter(Substrate_Type2 =="Unconsolidated", Survey_Period=="Baseline")%>% 
  mutate(Treatment2 = recode(Treatment,
                             Reference = "Reference",
                             Boulder = "Experimental",
                             Mesh = "Experimental",
                             Control = "Experimental")) %>%
  group_by(Treatment2) %>%
  summarise(Mean.cover = mean(percent,na.rm= TRUE),
            se = sd(percent, na.rm = TRUE) / sqrt(n()))

per_soft


#Summarize by Tier 1
tier1_cover <- cover_new %>%
  group_by(Survey_Period, Treatment,Plot_ID,TIER_1) %>% 
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(total_points = sum(count),percent = (count / total_points) * 100) %>% # total rows per Survey_Period × Treatment
  ungroup()

per_tier1<-tier1_cover %>%
  filter(Survey_Period=="Baseline")%>% 
  mutate(Treatment2 = recode(Treatment,
                             Reference = "Reference",
                             Boulder = "Experimental",
                             Mesh = "Experimental",
                             Control = "Experimental")) %>%
  group_by(Treatment2,TIER_1) %>%
  summarise(Mean.cover = mean(percent,na.rm= TRUE),
            se = sd(percent, na.rm = TRUE) / sqrt(n()))

per_tier1



# Use baseline control data for post-installation control
pi.c<-cover_raw %>% filter((Survey_Period=="Baseline") & (Treatment=="Control"))
pi.c$Survey_Period <-"T0_Post_Installation"

# Use preoutplant control data for postoutplant control
t1pre.c<-cover_raw %>% filter((Survey_Period=="T1_6mo_preoutplant") & (Treatment=="Control"))
t1pre.c$Survey_Period <-"T1_6mo_postoutplant"

#Combine into dataframe
cover.new<-rbind(pi.c,t1pre.c,cover_raw)

table(cover.new$Survey_Period,cover.new$Treatment)


#Calculate mean rubble size during baseline surveys
rs.baseline<-cover_raw %>% 
                         filter(Survey_Period=="Baseline")   %>% 
                         mutate(Treatment2 = recode(Treatment,
                                                      Reference = "Reference",
                                                      Boulder = "Experimental",
                                                      Mesh = "Experimental",
                                                     Control = "Experimental")) %>%
                         group_by(Treatment2,Plot_ID) %>% 
                         summarise(mean.size = mean(Rubble_Size,na.rm=TRUE),.groups = "drop") %>%
                           mutate(mean.size = ifelse(is.nan(mean.size), NA, mean.size)) 

#summarize at experimental and reference site
e.r_baseline<- rs.baseline %>%
  group_by(Treatment2) %>%
  summarise(
    Mean_size = mean(mean.size,na.rm=TRUE),
    n_nonmiss = sum(!is.na(mean.size)),
    sd_check = sd(mean.size, na.rm = TRUE),
    se = sd_check/ sqrt(n_nonmiss))


e.r_baseline
#Remove reference site data and calculate abudance/plot
col.tot<-as.data.frame(cover.new %>% 
                         #filter(Treatment!="Reference")   %>%                      
                         group_by(Survey_Period, Treatment,Plot_ID) %>% 
                         summarise(n = n()))

View(col.tot)

#Plotting cover abundance for first 2 time points

col.tot$Treatment <- factor(col.tot$Treatment, levels = c("Control", "Mesh", "Boulder","Reference"))
col.tot$Survey_Period <- factor(col.tot$Survey_Period, levels = c("Baseline","T0_Post_Installation","T1_6mo_preoutplant","T1_6mo_postoutplant"))
col.tot$T_SP<-as.factor(paste(col.tot$Survey_Period,col.tot$Treatment,sep="_"))

col.tot %>%
  filter(Survey_Period %in% c("Baseline","T1_6mo_preoutplant")) %>%
  mutate(Survey_Period = recode(Survey_Period, Baseline = 'Baseline', T1_6mo_preoutplant =  'T1 (6months)'))%>%
  filter(T_SP !="Baseline_Reference") %>%
  ggplot(.,aes(Treatment,n))+
  geom_boxplot(aes(fill=Survey_Period))+
  labs(x = "Treatment", y = "cover Abundance")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank()  ) 


###By Genus
col.gen<-as.data.frame(cover %>% 
                         filter(!is.na(Size)) %>%
                         group_by(Survey_Period, Treatment,Plot_ID,cover_Code) %>% 
                         summarise(n = n()))

View(col.gen)

#col.gen<-col.gen %>% filter(Treatment!="Reference")

col.gen$Treatment <- factor(col.gen$Treatment, levels = c("Control", "Mesh", "Boulder","Reference"))
col.gen$T_SP<-as.factor(paste(col.gen$Survey_Period,col.gen$Treatment,sep="_"))

col.gen %>%
  filter(Survey_Period %in% c("Baseline","T1_6mo_preoutplant")) %>%
  filter(T_SP !="Baseline_Reference") %>%
  ggplot(.,aes(Survey_Period,n, fill=cover_Code))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Treatment)+
  labs(x = "Treatment", y = "Colony Abundance")+
  #theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_blank()  )

