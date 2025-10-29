

# Measure and graph whales

# Written by Ana Eguiguren

# Modified by Christine Clarke

# Purpose: use .csv files exported from 'morphometrix' software to calculate and compile measurements of whales from aerial images that were annotated in morphometrix


# Read source file ---------------------------------------------------------

source("01_scripts/functions/measuring_whales_functions.R")

# Read in and compile morphometrix data -------------------------------------------------

# set path to files (on Whitehead Lab NAS - a copy of Ana's data)

ROOTfolderpath <- "W:/Galapagos/01_PROCESSED DATA/Drone/2023/Snapshots"

# run function (written by Ana Eguiguren) to compile and process data

morpho.output <- getMorphoMetrix(ROOTfolderpath)

# Save compiled morphometrix data -------------------------------------------------

write.csv(morpho.output, "02_outdata/morpho-output_AEguiguren_GAL2023_2025-10-28.csv", row.names = F)

# Read compiled morphometrix data -------------------------------------------------

morpho.output <- read.csv("02_outdata/morpho-output_AEguiguren_GAL2023_2025-10-28.csv", header = T)

# Get altitude and gimble data from flight log files -------------------------------------------------

# run function to get altitude from flight logs (joins based on time from file name)

FlightLogPath = "00_rawdata/Gal2023_FlightReader_Flight_logs.csv"  # file provided by Ana Eguiguren (>100 MB so 'git ignored')

morpho.output <- getSrtAltitude(data = morpho.output, FlightLogPath = FlightLogPath)


# Get length estimates -------------------------------------------------

# ** not yet worked through **

# ~~~~b. get length estimates-----
morpho.output<- morpho.output %>% mutate(
  altitude.c = altitudeASL(altitude.raw = droneAltitude), #add launch height to barometer altitude:
  TL.m = measureWhales(image.width = image_width, altitude = altitudeASL(altitude.raw = droneAltitude),length.pixels = TL.px), #estimate length in meters
  HD.m = measureWhales(image.width = image_width, altitude = altitudeASL(altitude.raw = droneAltitude),length.pixels = HD.px), #estimate length in meters
  HF.m = measureWhales(image.width = image_width, altitude = altitudeASL(altitude.raw = droneAltitude),length.pixels = HF.px),
  ratio.HD = HD.px/TL.px, # ratio using nose-dorsal fin measure
  ratio.HF = HF.px/TL.px, # ratio using nose- flipper measure
  ratio.DF = HF.px/HD.px # ratio nose-flipper to nose DF
)

