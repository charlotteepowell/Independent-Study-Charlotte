# Custom-made functions for the project

#0. Load packages ----
library(tidyverse) # for data manipulation and visualization
library(stringr) # for string manipulation


# 1. Get data from Morphometrix -----
getMorphoMetrix  <- function(ROOTfolderpath){
  require(dbplyr); require(dplyr)
#' @title compMorphometrix
#' @description compile the csv outputs from Morphometrix folder 
#' @param ROOTfolderpath 	a character vector of full path names to the folder where the csv outputs are located  


  #remove / from ROOTFfolderpath
  if (substring(ROOTfolderpath, nchar(ROOTfolderpath), nchar(ROOTfolderpath)) == 
     "/") {
   ROOTfolderpath = substring(ROOTfolderpath, 1, nchar(ROOTfolderpath) - 
                               1)
  }

  #folder not found
  if(isFALSE(dir.exists(ROOTfolderpath)))
    stop(paste(ROOTfolderpath, "doesn't exist", sep = ""))

  #get list of files
   tmp.files <- list.files(path = ROOTfolderpath, pattern = "*.csv", 
                        full.names = TRUE, recursive = TRUE)
   
   #make empty data table to populate
   #empty flightlog
   morpho.output <- data.frame(imagePath = character(), 
                               pixelDimension = character(),
                               totalLength_px = character(),
                               headFinLength_px = character(),
                               notes = character()
                               )
   
   # go through all files
   for (i in seq_along(tmp.files)){
     
     #prints progress bar
     cat(paste("\r", round(100 * (i/length(tmp.files)), 1), 
               "% processing, file", i, ":", (tmp.files[i])))
     
     #read in each file
     
     tmp.table <- read.csv(tmp.files[i], header = T) %>% 
       as_tibble() %>% 
       filter(Object %in% c("Image Path","Pixel Dimension","Notes","TL","HF"),
              Value_unit != "Meters") %>% 
       select(-Value_unit) %>% 
       mutate(Object = case_when(Object == "Image Path" ~ "imagePath",
                                 Object == "Pixel Dimension" ~ "pixelDimension",
                                 Object == "Notes" ~ "notes",
                                 Object == "TL" ~ "totalLength_px",
                                 Object == "HF" ~ "headFinLength_px",
                                 T~Object
       )) %>% 
       pivot_wider(names_from = Object, values_from = Value)
   
     morpho.output <- bind_rows(morpho.output, tmp.table)

   }
   
   morpho.output <- morpho.output %>% 
     mutate(pixelDimension = as.numeric(pixelDimension),
            totalLength_px = as.numeric(totalLength_px),
            headFinLength_px = as.numeric(headFinLength_px)
     )
   return(morpho.output)
   
}

# 2. getSRTAltitude -----
getSrtAltitude <- function(data,FlightLogPath){
  
  require(stringr)
  
  #read in all flight srt data
  drone_srt_files <- read.csv(FlightLogPath, header = T)
  
  data <- data %>% 
    
    mutate(imageName = basename(imagePath),
           videoFile = paste0(substr(imageName, 1, 32), ".MP4"),
           
           # identify vlc and other file snapshot types
           imageType = ifelse(str_detect(imageName, "vlc"), "vlc", "boris"), 
           
           # extract time of frame (in seconds, relative to start of video)
           ss_sec = ifelse(imageType=="vlc",
                           yes = as.numeric(substr(imageName, 41, 42)) *60 + as.numeric(substr(imageName, 44,45)), 
                           
                           no = str_extract(imageName, "_([0-9\\.]+)(ns)?\\.png$") %>%
                             str_remove_all("_|ns|\\.png")%>%
                             as.numeric())%>%
             floor(),  # round down to nearest second
           
           # extract datetime from video file name
           dateTime_videoStart = str_replace(videoFile, "Gal2023_DJIMini2_", "") %>% 
             str_replace(".MP4", "") %>% ymd_hms(),
           
           dateTime_stillImage = dateTime_videoStart + ss_sec) %>% 
    
    # Join data with drone flight log to get altitude based on mp4_file & time match
    left_join(drone_srt_files %>% select(datetime_utc6,OSD.height..m.,GIMBAL.pitch) %>% 
                mutate(datetime_utc6 = ymd_hms(datetime_utc6)) %>% 
                group_by(datetime_utc6) %>% 
                summarise(mean_OSD_height_m = mean(OSD.height..m.),
                          mean_GIMBAL.pitch = mean(GIMBAL.pitch)) %>% 
                ungroup(), 
              by = c("dateTime_stillImage" = "datetime_utc6"),
              
              # allow many-to-one b/c some still images have multiple rows for multiple measured whales
              relationship = "many-to-one")
  
  # View result
  return(data)
  
}


# 3. corrects subtitle altitude to true altitude above sea-level (ASL) based on launch height and correction model----
#model used is non-hierarchical
altitudeASL <- function(boat_height =  1.03 - 0.24, 
                        launch.chest = 1.4, 
                        camera.height = 0.045, 
                        altitude.raw){
  altitude.fix = altitude.raw+launch.chest+camera.height+boat_height
  altitude.c = 1.40 + altitude.fix*1.017
  return(altitude.c)
  
}

# 4. estimates whale length in meters------

measureWhales<- function(image.width, altitude, length.pixels){
  alpha = ifelse(image.width == 3840, yes = 0.000328, no = 
                   ifelse(image.width == 1920, yes = 0.000656, no = NA))
  length = alpha * altitude * length.pixels
  return(length)
}
