#############################################################################################

# Analyse summarized Data from 02_ManipulateData.r

# written by Christine Clarke, September 2025

# for living data project course on Productivity and Reproducibility in Ecology and Evolution

# Read packages ---------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)

# Load Data -------------------------------------------------------------------

vert_data_summary_wide = read.csv("02_outdata/vertebrates_summary_wide.csv")

# Analyse Data ---------------------------------------------------------------

#Since, upon inspection (see 02_ManipulateData.R), the data are not normal, I will use a Mann-Whitney U test (Wilcoxon rank sum test) instead of a t-test.

# One-sided paired tests, treating old growth as the control and clear cut as the treatment
# For each species, I will test for an effect of clear cutting on three response variables: count, mean length, and mean weight

## Coastal giant salamander ------------------------

### Count ------------------------

wilcox.test(paired = TRUE, alternative = "greater", mu = 0,
              vert_data_summary_wide$count_OG_Coastal.giant.salamander,
              vert_data_summary_wide$count_CC_Coastal.giant.salamander) 

### Length ------------------------

wilcox.test(paired = TRUE, alternative = "greater", mu = 0,
            vert_data_summary_wide$mean.length_OG_Coastal.giant.salamander,
            vert_data_summary_wide$mean.length_CC_Coastal.giant.salamander) 

### Weight -----------------------------

wilcox.test(paired = TRUE, alternative = "greater", mu = 0,
            vert_data_summary_wide$mean.weight_OG_Coastal.giant.salamander,
            vert_data_summary_wide$mean.weight_CC_Coastal.giant.salamander) 

## Cascade torrent salamander ------------------------

### Count ------------------------

# skipped, because no rows with values for each of count_OG_Cascade.torrent.salamander and count_CC_Cascade.torrent.salamander

### Length ------------------------

# skipped, because no rows with values for each of mean.length_OG_Cascade.torrent.salamander and mean.length_CC_Cascade.torrent.salamander

### Weight -----------------------------

# skipped, because no rows with values for each of weight.length_OG_Cascade.torrent.salamander and weight.length_CC_Cascade.torrent.salamander

## Cutthroat trout ------------------------

### Count ------------------------

wilcox.test(paired = TRUE, alternative = "greater", mu = 0,
            vert_data_summary_wide$count_OG_Cutthroat.trout,
            vert_data_summary_wide$count_CC_Cutthroat.trout) 

### Length ------------------------

wilcox.test(paired = TRUE, alternative = "greater", mu = 0,
            vert_data_summary_wide$mean.length_OG_Cutthroat.trout,
            vert_data_summary_wide$mean.length_CC_Cutthroat.trout) 

### Weight -----------------------------

wilcox.test(paired = TRUE, alternative = "greater", mu = 0,
            vert_data_summary_wide$mean.weight_OG_Cutthroat.trout,
            vert_data_summary_wide$mean.weight_CC_Cutthroat.trout) 

# I will use a Bonferroni method to adjust my statistical significance level based on the number of tests performed; in this case, six. 
# Thus, I will use p\<0.00833 (0.05/6) as my criteria for determining if the Mann-Whitney U tests suggest that the results are significantly different from those expected if the null hypothesis were correct.

# none of the above calculated p-values are less than this value.
