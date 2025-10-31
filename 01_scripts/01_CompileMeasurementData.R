#############################################################################################

# Calculate whale measurements based on Morphometrix outputs

# Written by Ana Eguiguren

# Modified by Christine Clarke

# Purpose: 
# use .csv files exported from 'morphometrix' software to calculate and compile measurements of whales from aerial images that were annotated in morphometrix. 
# also applies correction (from Eguiguren et al. 2025) to altitudes from drone flight logs.


# Read source file and additional packages ---------------------------------------------------------

source("01_scripts/functions/measuring_whales_functions.R")
library(magick)  # to extract image widths

# Read in and compile morphometrix data -------------------------------------------------

# set path to files

# on Whitehead Lab NAS - a copy of Ana's data
ROOTfolderpath <- "W:/Galapagos/01_PROCESSED DATA/Drone/2023/Snapshots" 

# on Christine's hard drive - a copy of Ana's measurements of David's whales
ROOTfolderpathDavid <- "E:/2024_Snapshots"                                

# run function (written by Ana Eguiguren) to compile and process data

morpho.output <- getMorphoMetrix(ROOTfolderpath)
morpho.output.david <- getMorphoMetrix(ROOTfolderpathDavid)


# Save compiled morphometrix data -------------------------------------------------

write.csv(morpho.output, "02_outdata/morpho-output_AEguiguren_GAL2023.csv", row.names = F)
write.csv(morpho.output.david, "02_outdata/morpho-output_AEguiguren_GullyAndArctic2024.csv", row.names = F)

# Read compiled morphometrix data -------------------------------------------------

morpho.output <- read.csv("02_outdata/morpho-output_AEguiguren_GAL2023.csv", header = T) 
morpho.output.david <- read.csv("02_outdata/morpho-output_AEguiguren_GullyAndArctic2024.csv", header = T)

morpho.output = morpho.output %>% rbind(morpho.output.david) %>% 
  mutate(Location = case_when(grepl("Arctic",imagePath)~"Arctic",
                              grepl("Gully",imagePath)~"Gully",
                              grepl("Galapagos",imagePath)~"Galapagos",
                              T~NA))


# Get altitude and gimble data from flight log files -------------------------------------------------

# run function to get altitude from flight logs (joins based on time from file name)

FlightLogPath = "00_rawdata/Gal2023_FlightReader_Flight_logs.csv"  # file provided by Ana Eguiguren (>100 MB so 'git ignored')

morpho.output <- getSrtAltitude(data = morpho.output, FlightLogPath = FlightLogPath)

# Get Image Widths from images --------------------------------------

# list all .png files and extract file name
allPNGs <- ROOTfolderpath %>%
  list.files(., full.names = TRUE, recursive = TRUE, pattern = ".png") %>% 
  as_tibble(allPNGs) %>% rename(updatedPath = value) %>% mutate(imageName = basename(Path))

morpho.output = morpho.output %>% 
  
  left_join(allPNGs) %>% 
  
  mutate(
    
    # extract image width from image meta-data
    imageWidth = NA_integer_)
      

for(i in c(237:nrow(morpho.output.2))){
  
  if(is.na(morpho.output.2$updatedPath[i])==F){
    
    #skip corrupt files
    if(!(morpho.output.2$updatedPath[i] %in% c( 
       "W:/Galapagos/01_PROCESSED DATA/Drone/2023/Snapshots/20230323/Gal2023_DJIMini2_20230323_091201/Gal2023_DJIMini2_20230323_091201_255.922.png",
       "W:/Galapagos/01_PROCESSED DATA/Drone/2023/Snapshots/20230323/Gal2023_DJIMini2_20230323_091201/Gal2023_DJIMini2_20230323_091201_262.596.png",
       "W:/Galapagos/01_PROCESSED DATA/Drone/2023/Snapshots/20230323/Gal2023_DJIMini2_20230323_091201/Gal2023_DJIMini2_20230323_091201_272.439.png"))){
      morpho.output.2$imageWidth[i] = image_read(morpho.output.2$updatedPath[i]) %>% image_info() %>% pull(width)
    }
  }
}


# Get length and nose ratio estimates -------------------------------------------------

morpho.output.2<- morpho.output %>% 
  
  # only include NADIR images
  filter((mean_GIMBAL.pitch <= -87 | is.na(mean_GIMBAL.pitch))) %>% 
  
  mutate(
  
  #correct to true altitude above sea-level (ASL)
  altitude_ASL = altitudeASL(altitude.raw = mean_OSD_height_m), 
  
  #estimate length in meters
  totalLength_m = measureWhales(image.width = imageWidth, altitude = altitude_ASL, length.pixels = totalLength_px), 
  
  headFinLength_m = measureWhales(image.width = imageWidth, altitude= altitude_ASL, length.pixels = headFinLength_px),
  
  # nose to body ratio
  ratio.HF = headFinLength_m/totalLength_m, 
  
  # assign assumed age-sex class based on rule of thumb
  assumedClass = case_when(totalLength_m >= 12 & ratio.HF >= 0.34 ~ "Mature Male",
                           Location %in% c("Gully","Arctic") ~"Mature Male",         # ** Manually set high lat as males for now **
                           T ~ "Female/Immature Male")
)


write.csv(morpho.output.2, "02_outdata/morpho-output_All_withSex.csv", row.names = F)

morpho.output.2 = read.csv("02_outdata/morpho-output_All_withSex.csv")


# Quality Control -----------------------------------------------------------

# compare to Ana's data

cleanMeasurements_byID_AE = read.csv("00_rawdata/id_unpooled_clean_processed.csv")
rawMeasurments_CC = read.csv("02_outdata/morpho-output_All_withSex.csv")

test = left_join(cleanMeasurements_byID_AE,rawMeasurments_CC, by = c("imageName","notes","TL.px"="totalLength_px"))

test %>% ggplot()+geom_point(aes(image_width,imageWidth))  # same image widths
test %>% ggplot()+geom_point(aes(altitude.c,altitude_ASL)) # variation in altitude (maybe b/c I took the average for each second?)
test %>% ggplot()+geom_point(aes(TL.m,totalLength_m))      # so some variation in measurements in m
test %>% ggplot()+geom_histogram(aes(TL.m-totalLength_m))  # basically with +/-1m   
test %>% ggplot()+geom_point(aes(ratio.HF.x,ratio.HF.y))   # but not in ratios    


# some long lengths for 'females' - maybe measurement error?
ggplot(rawMeasurments_CC)+geom_point(aes(totalLength_m,ratio.HF, col = assumedClass))
ggplot(rawMeasurments_CC)+geom_histogram(aes(totalLength_m, fill = assumedClass))+facet_wrap(~assumedClass,ncol=1, scales = "free_y")



