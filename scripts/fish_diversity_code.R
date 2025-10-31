
# Load required packages
library(tidyverse)
library(vegan)
library(ggthemes)
library(here)
library(readr)

# Load survey data
fd6m <- read_csv(here("data","fish_data_Six_month.csv")) #update to file location on your drive


# Update file location and load species info
spp.list <- read_csv(
  here("data", "NCRMP Fish Species List - CLEANer_AAS.csv"),
  na = c("", "NA"),           # readr syntax (not na.strings)
  show_col_types = FALSE
) %>%
  mutate(across(
    c(starts_with("LW_"), LMAX, LENGTH_CONVERSION_FACTOR, SLTLRAT, FLTLRAT),
    ~ suppressWarnings(as.numeric(.))
  ))

# clean up
fd6m <- fd6m %>%
  rename(c(DATE = Date, SURV_PERIOD = `Survey Period`, OBS = Observer,  PLOT_ID = Plot_ID, TREAT = Treatment, 
           SPECIES = Species_Code, SIZE_cm = Size)) %>%
  select(-c(Species, Genus))


# Drop pelagic species and trigger that lives by the wall
fd6m <- fd6m %>%
  filter(!(SPECIES == "DEMA")) %>% # Large school of DEMA on one plot at 6 m - Dama counted and I doubt they were actually using the plot
  filter(!(SPECIES == 'SUFR' & SIZE_cm > 24)) #Mia caught a 30 cm SUFR in adjacent plots 14 & 11, I think i have seen this fish using the wall.


# combine with species list to get taxa specific info
df <- fd6m %>%
  left_join(spp.list, by = c('SPECIES')) %>%
  mutate(BIOMASS_gpm2 = (LW_A * (SIZE_cm * LENGTH_CONVERSION_FACTOR)^LW_B)/1.77) # biomass per m2 for a 1.5 m radius around each plot.


#2 baseline surveys on plot 26 (Jonny and I) one is either miss recorded
# methods called for recording 10 plots in 1.5 m radius from center but we only have 9 plots.
# check raw data sheets; for now assigning JC values to plot 25 and making it a boulder "boulder" treatment

df <- df %>%
  mutate(PLOT_ID = if_else(PLOT_ID == '26' & OBS == 'JC', '25', PLOT_ID),
         TREAT = if_else(PLOT_ID == '25' & SURV_PERIOD == "Baseline", "Boulder", TREAT))


# Diversity
# Get the plots from the rubble field that were surveyed at the baseline 
baseline.plts <- df %>%
  filter(SURV_PERIOD == "Baseline" & TREAT != "Reference") %>%
  distinct(PLOT_ID)


all.plot.numbs <- tibble(PLOT_ID = as.character(1:30)) #Create list of all plots

skipped.baseline.plts <- all.plot.numbs %>%
  filter(!PLOT_ID %in% baseline.plts$PLOT_ID) # Filter out the baseline plots

div.summary <- df %>%
  mutate(COUNT = 1) %>%
  group_by(SURV_PERIOD, PLOT_ID, SPECIES, TREAT) %>%
  summarise(COUNT = sum(COUNT)) %>%
  ungroup() %>%
  complete(PLOT_ID, SURV_PERIOD, SPECIES,  fill = list(COUNT = 0)) %>%
  group_by(PLOT_ID) %>%
  fill(TREAT, .direction = 'downup') %>%
  ungroup() %>%
  filter(!(TREAT == "Baseline" & PLOT_ID == skipped.baseline.plts$PLOT_ID)) %>% # Eliminate baseline plots not surveyed
  filter(!(SURV_PERIOD == "T0_Post_Installation" & TREAT == "Reference")) # eliminate reference plots that were not surveyed


div.sum.wide <- div.summary %>%
  pivot_wider(names_from = SPECIES, values_from = COUNT)

species.matrix <- div.sum.wide[, 4:54] 

div.sum.wide$shannon <- diversity(species.matrix, index = 'shannon')  

# hill #'s
div.sum.wide$hill_q1 <- exp(div.sum.wide$shannon)

# Richness (q = 0)
div.sum.wide$hill_q0 <- specnumber(species.matrix)

# Simpson (q = 2)
simpson_index <- diversity(species.matrix, index = "simpson")  # this is 1 - D
div.sum.wide$hill_q2 <- 1 / simpson_index

# Quick and dirty
ggplot(div.sum.wide, aes(x = TREAT, y = shannon)) +
  geom_boxplot() +
  facet_wrap(~ SURV_PERIOD) +
  theme_minimal()

# Clean it up
hill.plot.df <- div.sum.wide %>%
  select(TREAT, SURV_PERIOD, PLOT_ID, hill_q0, hill_q1, hill_q2) %>%
  pivot_longer(cols = starts_with('hill_q'),
               values_to = 'hill_number',
               names_to = 'q_order') %>%
  mutate(q_order = recode(q_order,
                          hill_q0 = 'q = 0 (Richness)',
                          hill_q1 = 'q = 1 (Shannon)',
                          hill_q2 = 'q = 2 (Simpson)'))

# cleaner plot, short cutting panel 1
hill.plot.df.quick <- hill.plot.df %>%
  mutate(TREAT = case_when(SURV_PERIOD == 'Baseline' & TREAT == "Boulder" ~ "Control",
                           SURV_PERIOD == 'Baseline' & TREAT == "Mesh" ~ "Control",
                           TRUE ~ TREAT),
         hill_number = ifelse(is.infinite(hill_number), NA, hill_number)
  )



ggplot(subset(hill.plot.df.quick, SURV_PERIOD != 'T0_Post_Installation'),
       aes(x = TREAT, y = hill_number, fill = TREAT)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.5) +
  facet_grid(q_order ~ SURV_PERIOD, scales = "free_y") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    y = "Effective Number of Species",
    x = "Treatment",
    title = "Hill Numbers (q = 0, 1, 2) by Treatment"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines")
  )


# Simplified plots for stakeholders - only plot shannon diversity ---------

shannon.df <- hill.plot.df %>%
  filter(q_order == "q = 1 (Shannon)")


# cleaner plot, short cutting panel 1
shannon.df.quick <- shannon.df %>%
  mutate(TREAT = case_when(SURV_PERIOD == 'Baseline' & TREAT == "Boulder" ~ "Control",
                           SURV_PERIOD == 'Baseline' & TREAT == "Mesh" ~ "Control",
                           TRUE ~ TREAT),
         hill_number = ifelse(is.infinite(hill_number), NA, hill_number)
  )

shannon.df.quick$TREAT <- factor(shannon.df.quick$TREAT, levels = c("Control", "Mesh", "Boulder","Reference"))

ggplot(subset(shannon.df.quick, SURV_PERIOD != 'T0_Post_Installation'),
       aes(x = TREAT, y = hill_number, fill = TREAT)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.5) +
  facet_grid(q_order ~ SURV_PERIOD,scales = "free_y", labeller = labeller(
      SURV_PERIOD = c("Baseline" = "Baseline",
        "T1_6mo_preoutplant" = "6 months (pre-outplanting)"))) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Effective Number of Fish Species", x = "Treatment") +

theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    strip.text.x = element_text(size = 14, face = "bold"),  # keep top labels
    strip.text.y = element_blank(),              # remove right labels
    legend.title = element_blank(),
    legend.position = "none"
  )


ggsave(filename = here("plots", "Fish_Shannon_Diversity.jpg"),
  plot = last_plot(),                                
  width = 8.5, height = 6, units = "in", dpi = 300)

