#############################################################################################

# Download Data From lterdatasampler

# written by Christine Clarke

# Read packages ---------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

# Load Data -----------------------------------------------------------

## Whale measurements and image metadata -------------------------------------------------

imageData_withID_AE = read.csv("00_rawdata/id_unpooled_clean_processed.csv")

## Sloughing assessments ----------------------------------------------

slough_CC = read.csv("00_rawdata/Skin Sloughing Data_CMKC.csv")

# ** to add: Charlotte's sloughing assessment **

# Manipulate Data ------------------------------------------------------

## Extract individual summaries from measurement data --------------------

measurementsByID = imageData_withID_AE %>% 
  group_by(ID) %>% 
  mutate(ID = str_replace(ID," ","")) %>% 
  summarise(medianTotalLength_m = median(TL.m),
            minTotalLength_m = min(TL.m),
            maxTotalLength_m = max(TL.m),
            medianHeadRatio = median(ratio.HF),
            minHeadRatio = min(ratio.HF),
            maxHeadRatio = max(ratio.HF),
            countMeasurements = n()) %>% 
  ungroup() %>% 
  mutate(assumedClass = case_when(medianTotalLength_m >= 12 & medianHeadRatio >= 0.34 ~ "Mature Male",
                                  medianTotalLength_m >= 15 & is.na(medianHeadRatio) ~ "Mature Male",
                                  medianTotalLength_m >= 12 & is.na(medianHeadRatio) ~ "Unk",
                                  T ~ "Female/Immature Male"),
         TotalLengthRange_m = maxTotalLength_m - minTotalLength_m,
         HeadRatioRange = maxHeadRatio - minHeadRatio)

## Combine different analysts' sloughing data ----------------------------


## Join final IDs (e.g. GAL2023_001) sloughing data ----------------------------


## Join sex-class and size info to sloughing data ----------------------------





# Validate Data -------------------------------------------------------------

## Measurements by individual ------------------------------------------------

# Length and ratio by assigned age-sex class
ggplot(measurementsByID)+
  geom_point(aes(medianTotalLength_m,medianHeadRatio,col=assumedClass))

# Length by assigned age-sex class
ggplot(measurementsByID)+
  geom_histogram(aes(medianTotalLength_m, fill = assumedClass))+facet_wrap(~assumedClass,ncol=1, scales = "free_y")
measurementsByID %>% count(assumedClass)

# ** To confirm with Ana: are we missing any mature males? #s don't line up with your paper **

## Cross-check image name and whale ID ------------------------------------------------

slough_CC %>% select(videoName,imageName,indID) %>% 
  left_join(imageData_withID_AE %>% 
              select(imageName,ID),
            by = "imageName") %>% 
  View()

