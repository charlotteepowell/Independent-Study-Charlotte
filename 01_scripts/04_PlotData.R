#############################################################################################

# Plot summarized Data from 02_ManipulateData.r

# written by Christine Clarke, September 2025

# for living data project course on Productivity and Reproducibility in Ecology and Evolution

# Read packages ---------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)

# Load Data -------------------------------------------------------------------

vert_data_summary = read.csv("02_outdata/vertebrates_summary_long.csv")

# Format Data -------------------------------------------------------------------

vert_data_summary_neat = vert_data_summary  %>% 
  mutate(section = case_when(section == "CC" ~ "Clear Cut",
                             section == "OG" ~ "Old Growth"))

# Plot data -------------------------------------------------------------------

# number of individuals, by species and forest type 

countPlot = ggplot(vert_data_summary_neat)+geom_boxplot(aes(section,count, fill = section))+
  xlab("Forest Type")+
  ylab("Number of Individuals")+
  theme_classic()+
  facet_wrap(~species)

# mean weight, by species and forest type 

lengthPlot = ggplot(vert_data_summary_neat)+geom_boxplot(aes(section,mean.length, fill = section))+
  xlab("Forest Type")+
  ylab("Weight (g)")+
  theme_classic()+
  facet_wrap(~species)

# mean weight, by species and forest type 

weightPlot = ggplot(vert_data_summary_neat)+geom_boxplot(aes(section,mean.weight, fill = section))+
  xlab("Forest Type")+
  ylab("Length 1 (mm)")+
  theme_classic()+
  facet_wrap(~species)

# Save Plots -------------------------------------------------------------------

ggsave("03_figures/CountBoxplot_v1.png",countPlot)
ggsave("03_figures/LengthBoxplot_v1.png",lengthPlot)
ggsave("03_figures/WeightBoxplot_v1.png",weightPlot)
