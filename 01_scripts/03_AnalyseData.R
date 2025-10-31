#############################################################################################

# Analyse summarized Data from 02_ConsolidateDataTypes.r

# written by ...

# for ...

# Read packages ---------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)
library(glmmTMB)

# Load Data -------------------------------------------------------------------

vert_data_summary_wide = read.csv("02_outdata/vertebrates_summary_wide.csv")

# Analyse Data ---------------------------------------------------------------

## fit a GLMM with a random effect on the intercept
## i.e. thermal limits vary by Class
mod1 <- glmmTMB(thermal_limit ~ absolute_latitude * realm + 
               realm * metric + (1 | Class), 
               data = max_ectos,
               family=beta_family)
summary(mod1)

