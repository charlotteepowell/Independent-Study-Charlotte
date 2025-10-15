
#############################################################################################

# Prepare file for analysis, from file from lterdatasampler

# written by Christine Clarke, September 2025

# for living data project course on Productivity and Reproducibility in Ecology and Evolution

# Read packages ---------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)

# Load Data -------------------------------------------------------------------

vert_data = read.csv("00_rawdata/and_vertebrates.csv")

# Manipulate Data -------------------------------------------------------------------

#For each combination of site type (clear cut or old growth), site reach (upper, middle, lower) and year, calculate:
# - total number of individuals of each species observed (ind_count)
# - mean length for each species
# - mean weight for each species

vert_data_summary = vert_data %>% 
  filter(is.na(species)==F) %>% 
  group_by(section,reach,year,species) %>% 
  summarise(count = n(),
            mean.length = mean(length_1_mm, na.rm = T),
            mean.weight = mean(weight_g, na.rm = T)) %>% 
  ungroup()

# pivot wider, to create columns for each response and species combination

vert_data_summary_wide = vert_data_summary %>% 
  mutate(species = gsub(" ",".",species)) %>% 
  pivot_wider(id_cols = c(reach,year), names_from = c(section,species), values_from = c(count, mean.length, mean.weight))

# Validate Data -------------------------------------------------------------------

# Length and width values that are 10x greater than the next largest value will be excluded, based on the assumption that this is not biologically feasible.

vert_data_summary %>% ggplot()+geom_point(aes(mean.length,mean.weight))+facet_wrap(~species)

# no data violate this cut-off, so no data removed.

# check for normality and equal sample sizes

vert_data_summary %>% count(species,section)  # within species, section counts are similar

vert_data_summary %>% ggplot()+geom_histogram(aes(mean.length))+facet_grid(section~species, scales = "free_x")
vert_data_summary %>% ggplot()+geom_histogram(aes(mean.weight))+facet_grid(section~species, scales = "free_x")
vert_data_summary %>% ggplot()+geom_histogram(aes(count))+facet_grid(section~species, scales = "free_x")

# some evidence of non-normal distributions


# Save Data -------------------------------------------------------------------

write.csv(vert_data_summary_wide,"02_outdata/vertebrates_summary_wide.csv",row.names = F)
write.csv(vert_data_summary,"02_outdata/vertebrates_summary_long.csv",row.names = F)

