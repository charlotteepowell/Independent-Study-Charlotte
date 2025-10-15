#############################################################################################

# Download Data From lterdatasampler

# written by Christine Clarke, September 2025

# for living data project course on Productivity and Reproducibility in Ecology and Evolution

# Read packages ---------------------------------------------------------------

library(tidyr)
library(lterdatasampler)
library(grateful)

# Get Citation Info -----------------------------------------------------------

citation("lterdatasampler")
get_pkgs_info(out.dir = "./04_manuscripts")

# Load Data -------------------------------------------------------------------

# Records for aquatic vertebrates (cutthroat trout and salamanders)
# in Mack Creek, Andrews Experimental Forest, Oregon (1987 - present)

vert_data = and_vertebrates

# Export data -----------------------------------------------------------------

write.csv(vert_data, "./00_rawdata/and_vertebrates.csv", row.names = F)
