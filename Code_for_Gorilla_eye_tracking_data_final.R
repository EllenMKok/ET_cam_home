########################################################
# Version 1 (August 3, 2023)
#
# This script can be used to assess data quality and run AOI analyses based on data gathered in Gorilla
# you need to download 'advanced' eye tracking data (check manual on how to get this data)
# see https://support.gorilla.sc/support/troubleshooting-and-technical/localisation#eyetracking2 for more information
# We are not affiliated with Gorilla and this script is not checked
# Released under the MIT License 

# When using this script, please cite as
# Kok. E.M., van den Broek, G.S.E., Lyford, A. (2023). ET_cam_home. A script for assessing webcam-based eye-tracking data quality, 
# running AOI analyses and visualizing data. Available from https://github.com/EllenMKok/ET_cam_home


# Please check the code for comments with 'TO DO' to adapt the code to your current situation.
# This code comes without warranty. 
#########################################################
#Libraries used in the script
#install.packages("plyr")
library(plyr)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("stringdist")
library(stringdist)
#install.packages("readxl")
library(readxl)
#install.packages("devtools")
#devtools::install_github("jwdink/eyetrackingR")
#install.packages("eyetrackingR")
library("eyetrackingR")
#install.packages('jpeg')
library('jpeg')
#install.packages("writexl")
library("writexl")
#install.packages("gridExtra")
library(gridExtra)
#install.packages("tidyr")
library(tidyr)
#install.packages("readr")
library(readr)
#install.packages("ggplot2")
library(ggplot2)
#########################################################
# Step 1:
# Making a path to the folder for data processing
path <- "X:/XXxxxx/XXxx/etc/"
path <- "C:/Users/kok00044/OneDrive - Universiteit Utrecht/Desktop/Gorilla code/"

#########################################################
# Step 2.
# This script uses (and makes)three subfolders:
# "RawData", "ProcessedData", "FilesForDataPreparation"
# Save this code in "FilesForDataPreparation" 
# in the folder RawData, it makes two additional folders: "AdvancedET" and "CalibrationData".
# After creating those folders, you separate the data from the Gorilla folder 'uploads' into those two folders.
# You can recognize the advanced calibration data because it has 'calibration' in the file name. 
# The other files go in the AdvancedET folder.
dir.create(paste0(path, "/RawData"))
dir.create(paste0(path, "/ProcessedData"))
dir.create(paste0(path, "/FilesForDataPreparation"))
dir.create(paste0(path, "/RawData/AdvancedET"))
dir.create(paste0(path, "/RawData/CalibrationData"))



RawDataPath <- paste(path, "RawData",sep="")
AdvETDataPath<- paste(RawDataPath, "AdvancedET",sep="/")
CalibrationDataPath<- paste(RawDataPath, "CalibrationData",sep="/")

OutputPath <- paste(path, "ProcessedData",sep="")
FilesPath <- paste(path, "FilesForDataPreparation",sep="")


#########################################################
# Step 3:
# Making a file that holds data-quality information.
# refer to Holmqvist, K., Örbom, S.L., Hooge, I.T.C. et al. Eye tracking: empirical foundations for a minimal reporting guideline. 
# Behav Res 55, 364–416 (2023). https://doi.org/10.3758/s13428-021-01762-8
# for more information about data quality reporting

# Three aspects of data quality should be reported.
# Accuracy: Accuracy refers to the difference between the true gaze position and the gaze position reported by the eye tracker.
# Precision: Precision refers to the reproducibility of a gaze position by the eye tracker when the true gaze position does not change.
# Dataloss: Finally, data loss refers to the amount of data lost in an eye-tracker signal.

# Additionally, Gorilla provides information on how well the algorithm has ran in a measure called face_conf


# We'll first make a file that holds all data-quality information. This comes from two sources, the calibration data 
# and the advanced eye-tracking data.

# First, calibration data. Here, we'll extract accuracy and precision data using a readin function.

# The readin function reads in the file “filename” from your input folder and then saves the 
# rows of the data that report the results of the last calibration round, 
# with the name of the file added in a column called “file”. 
# TO DO: The number of relevant rows depends on the number of calibration points that you used in your experiment. 
# The default value at the moment is 5 but you need to replace this with the number of calibration points used in your experiment 
# (change code at “numberofcalibrationpoints = 5”)
CustomReadin <- function(filename, folder, numberofcalibrationpoints = 5){
  interimdataframe <- read_excel(paste(CalibrationDataPath,filename,sep="/"), 
                                 col_names = TRUE)
  interimdataframe$file <- paste(filename)
  print(filename)
  if (nrow(interimdataframe)>=(numberofcalibrationpoints+2)){
    print("readin")
    interimdataframe <- interimdataframe[(nrow(interimdataframe)-((numberofcalibrationpoints+2))):nrow(interimdataframe),]
    return(interimdataframe)
  }
}

AllUploadFiles <- list.files(CalibrationDataPath, pattern=glob2rx("*.xlsx"))
AllUploadFiles <- as.data.frame(AllUploadFiles)
names(AllUploadFiles)[1] <- "V1"


Data <- do.call(rbind.fill,
                lapply(
                  AllUploadFiles$V1,
                  CustomReadin,
                  folder=CalibrationDataPath 
                )
)

# We'll only need the rows that provide the accuracy of the calibration (cf validation). 
# Select rows in which type = 'accuracy'.
CalibrationUploadData_01 <- Data%>%
  filter(type == "accuracy")
# Next, let's remove all columns (variables) we don't need.
CalibrationUploadData_02 <- CalibrationUploadData_01 %>%
  select(participant_id, time_stamp, type, point_x_normalised, point_y_normalised,
         mean_centroid_x_normalised, mean_centroid_y_normalised, SD_centroid_x_normalised, SD_centroid_y_normalised, file)

# The variable 'file' in the Calibration upload data holds the file name. 
# In the gorilla version that this code was written for, the filenames contain a unique code that identifies the task. 
# We use a regular expression to extract this task code and save this in the column "taskname". 
# This can be used to identify different conditions, for example.
CalibrationUploadData_02$taskname <- str_extract(CalibrationUploadData_02$file, "(?<=task-)[^-]+")

# We will now calculate average accuracy per participant per file.
# The accuracy is the absolute distance between the calibration point and the mean of the samples for that calibration point.
# Precision is the SD of the samples for that calibration point.
# This is first some code to build a function that calculates accuracy and precision using Pythagoras theorem.
accuracy_calculation <- function(point_x_normalised,mean_centroid_x_normalised, point_y_normalised, mean_centroid_y_normalised) {
  accuracy <- sqrt((point_x_normalised - mean_centroid_x_normalised)^2 + (point_y_normalised - mean_centroid_y_normalised)^2)
  return(accuracy)
}

precision_calculation <-function(SD_centroid_x_normalised,SD_centroid_y_normalised){
  precision <-sqrt(SD_centroid_x_normalised^2 + SD_centroid_y_normalised^2)
  return(precision)
}
# We now apply it to the data.
CalibrationUploadData_02$accuracy <-mapply(accuracy_calculation, CalibrationUploadData_02$point_x_normalised,CalibrationUploadData_02$mean_centroid_x_normalised, CalibrationUploadData_02$point_y_normalised, CalibrationUploadData_02$mean_centroid_y_normalised)
CalibrationUploadData_02$precision <-mapply(precision_calculation,CalibrationUploadData_02$SD_centroid_x_normalised, CalibrationUploadData_02$SD_centroid_y_normalised)

Data_quality <- CalibrationUploadData_02 %>% 
  dplyr::group_by(participant_id,taskname) %>%
  dplyr::summarise(accuracy = mean(accuracy),
                   precision = mean(precision)) 

# This makes a wider version with a column for each task
Data_quality_wide <- Data_quality %>% 
  pivot_wider(id_cols = participant_id,
              names_from = taskname,
              values_from = c(accuracy, precision))

# Now we continue to reading in the advanced eye tracking data.
# This is the data-quality information from the advanced eye-tracking data files. 
# This includes resolution en data loss  
# and face_conf, which is a measure of how certain the algorithm was that it correctly found a face.

AllUploadFiles <- list.files(AdvETDataPath, pattern=glob2rx("*.xlsx"))
AllUploadFiles <- as.data.frame(AllUploadFiles)
names(AllUploadFiles)[1] <- "V1"

AdvETUploadData <- do.call(rbind.fill,
                           lapply(
                             AllUploadFiles$V1, 
                             function(x) read_excel(paste(AdvETDataPath,x,sep="/"), 
                                                    col_names = TRUE)))

# Let's keep only the rows that provide actual eye-tracking data points. 
# Select rows in which type = 'prediction'.
AdvETUploadData_01 <- AdvETUploadData%>%
  filter(type == "prediction")
# Select relevant columns
AdvETUploadData_02 <- AdvETUploadData_01 %>%
  select(file=filename,participant_id, time_elapsed, x_pred_normalised, y_pred_normalised,face_conf)

# In order to calculate data loss, we'd have to know the sample frequency of the eye tracker 
# (= software plus hardware!)
# We don't have that (we can't know the sampling frequency of the participant's hardware).
# Thus, instead, we calculate the actual frequency as a measure of data quality.
# Frequency (Hz) is the number of samples per second.
# We also take the mean of the face_conf measure.

Data_quality_2 <- AdvETUploadData_02 %>% 
  dplyr::group_by(participant_id, file) %>%
  dplyr::summarise(face_conf = mean(face_conf), 
                   Samples = length(time_elapsed), 
                   Time = max(time_elapsed)/1000, 
                   Hz = length(time_elapsed)/(max(time_elapsed/1000)))

# This is to make a wider version if you have more than one stimulus.
Data_quality_2_wide <- Data_quality_2 %>% 
  pivot_wider(id_cols = participant_id,
              names_from = file,
              values_from = c(face_conf, Samples, Time, Hz))

# And this is to merge that with the accuracy and precision.
Data_quality_wide_complete <- merge(Data_quality_wide, 
                                    Data_quality_2_wide, 
                                    all = TRUE)

# You can then write this data to an excel file. 
write_xlsx(Data_quality_wide_complete, paste0(OutputPath,"\\dataquality.xlsx"))

# Accuracy: Accuracy refers to the difference between the true gaze position and the gaze position reported by the eye tracker.
# It is measured as a proportion of the screen size.
# Precision: Precision refers to the reproducibility of a gaze position by the eye tracker when the true gaze position does not change.
# Our measure for precision is the standard deviation of the gaze position during validation. 
# It is measured as a proportion of the screen size.
# Faceconf: Faceconf is a measure provided by Gorilla of how certain the algorithm was that it correctly found a face.
# Dataloss: Typically, eye tracking papers include a measure of data loss.
# Data loss refers to the amount of data lost in an eye-tracker signal.
# For this type of webcam-based eye-tracking data, we cannot calculate the data loss,
# because we cannot know the true sampling frequency.
# As an alternative, we suggest reporting the realized frequency in Hz.
# This is calculated from the number of samples and the time elapsed (=trial duration).

#########################################################
# Step 4:
# AOI Analyses
# Note that we refrain from conducting event detection (parsing the data into fixations and saccades) 
# and instead work with the raw data.
# Because we believe the quality of the data does not warrant event detection.

# AOI analysis can be conducted using the eyetrackingR package (Dink & Ferguson, 2015). 
# Dink, J. W., & Ferguson, B. (2015). 
# eyetrackingR: An R Library for Eye-tracking Data Analysis. Retrieved from http://www.eyetrackingr.com.  

# TO DO: define the location of each AOI and make an csv file 
# see http://www.eyetracking-r.com/vignettes/preparing_your_data

# Next, we read the location of each AOI for each stimulus from csv files
# This is an example with four AOI's plus one AOI for the full screen.
AOI1 <-read.csv(paste(FilesPath,"/AOI1.csv", sep=""),sep=";")
AOI2 <-read.csv(paste(FilesPath,"/AOI2.csv", sep=""),sep=";")
AOI3 <-read.csv(paste(FilesPath,"/AOI3.csv", sep=""),sep=";")
AOI4 <-read.csv(paste(FilesPath,"/AOI4.csv", sep=""),sep=";")
AOIScreen <-read.csv(paste(FilesPath,"/AOIScreen.csv", sep=""),sep=";")

# Next, for each sample, we decide whether it belongs to an AOI. We do that for every individual AOI.
AdvETUploadDataCombined <- AdvETUploadData_02 %>%
  add_aoi(aoi_dataframe = AOI1, 
          x_col = "x_pred_normalised", y_col = "y_pred_normalised", 
          aoi_name = "AOI1",
          x_min_col = "Left", x_max_col = "Right", y_max_col = "Top", y_min_col = "Bottom") %>%
  add_aoi(aoi_dataframe = AOI2, 
          x_col = "x_pred_normalised", y_col = "y_pred_normalised", 
          aoi_name = "AOI2",
          x_min_col = "Left", x_max_col = "Right", y_max_col = "Top", y_min_col = "Bottom") %>%
  add_aoi(aoi_dataframe = AOI3, 
          x_col = "x_pred_normalised", y_col = "y_pred_normalised", 
          aoi_name = "AOI3",
          x_min_col = "Left", x_max_col = "Right", y_max_col = "Top", y_min_col = "Bottom") %>%
  add_aoi(aoi_dataframe = AOI4, 
          x_col = "x_pred_normalised", y_col = "y_pred_normalised", 
          aoi_name = "AOI4",
          x_min_col = "Left", x_max_col = "Right", y_max_col = "Top", y_min_col = "Bottom") %>%
  add_aoi(aoi_dataframe = AOIScreen, 
          x_col = "x_pred_normalised", y_col = "y_pred_normalised", 
          aoi_name = "AOIScreen",
          x_min_col = "Left", x_max_col = "Right", y_max_col = "Top", y_min_col = "Bottom")

# You can now calculate the number of samples in each AOI based on that.
AOI_summary_data <- AdvETUploadDataCombined %>% 
  dplyr::group_by(participant_id, file) %>%
  dplyr::summarise(ET_aoi1 = sum(AOI1), 
                   ET_aoi2 = sum(AOI2),
                   ET_aoi3= sum(AOI3), 
                   ET_aoi4 = sum(AOI4), 
                   sumAOIscreen = sum(AOIScreen))

# You could also work with the proportion of the samples. 
# This is useful if you have trials of unequal length.
# The proportion is calculated in relation to the total number of samples directed at the screen:
# Not all gazes/samples are on the screen. If AOIScreen = FALSE, that gaze falls outside of the screen where Gorilla presents the data.
# This could be caused by either the participant looking away from the screen, 
# or by inaccuracy/imprecision causing the sample to be measured outside of the screen area.
# Either way, we take only the samples that fall inside the screen area
# and use that to calculate the proportion of samples that falls within an AOI.
# If your AOIs together cover the whole screen,
# the proportions should sum up to 1.
AOI_summary_data_proportions <- AdvETUploadDataCombined %>% 
  dplyr::group_by(participant_id, file) %>%
  dplyr::summarise(ET_aoi1_prop = sum(AOI1)/sum(AOIScreen), 
                   ET_aoi2_prop = sum(AOI2)/sum(AOIScreen),
                   ET_aoi3_prop = sum(AOI3)/sum(AOIScreen),
                   ET_aoi4_prop = sum(AOI4)/sum(AOIScreen))

# You can finally write this to another file
write_xlsx(AOI_summary_data, paste0(OutputPath,"\\aoi_complete_final.xlsx"))
write_xlsx(AOI_summary_data_proportions, paste0(OutputPath,"\\aoi_proportions.xlsx"))

#########################################################
# Step 5:
# Visualisations
# Those are just a couple of different types of visualisations that you can make.

# The 'for' loop allows you to automatically make a visualization for each participant separately.
# The code fragment "facet_wrap(~file)" means that you will make a separate sub-image for each trial.

# You can plug one of the examples into the 'for' loop.
# The example plot is just plotting the gaze locations over time.
# Time is colour-coded (color = time_elapsed), so darker colors are the beginning of the trial
# and lighter colors towards the end of the trial.

# For loop
List_ppn = unique(AdvETUploadData_02$participant_id)#make a list of participants

#Create directory for images
dir.create(paste0(OutputPath, "/Images"))

for(i in List_ppn)     {    # for each participant....
  
  pdfname = paste(OutputPath,"/Images", sep = "") #location of the pdf
  
    AdvETUploadData_00 <- AdvETUploadData_02 %>%
    filter(participant_id == i)
  
    # This section can be changed depending on the plot you'd like to make
  # This is a raw data plot (one point per sample) with dots colored to represent the time.
  AdvETUploadData_00 %>%
    ggplot(aes(x_pred_normalised, y_pred_normalised, xmin = -0.2, xmax = 1.2, ymin = -0.2, ymax = 1.2, color = time_elapsed))  + #note that xmax and ymax are specified to give some space around the box with good data. If values are higher, they aren't shown
    geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = 'white', color = 'darkgrey')+ #This draws a rectangle the size of the screen
    theme_classic()+
    geom_point()+
    scale_x_continuous(name="x") + 
    scale_y_continuous(name="y") +coord_fixed() + 
    facet_wrap(~file)+
    theme(text = element_text(size = 20))
  # This is the end of the section can be changed depending on the plot you'd like to make
  
  ggsave(filename = paste0("img_", i, ".pdf"),
         path = pdfname, 
         device = "pdf")
  
}


#######################################additional plots (plug into above)

# This is a raw data plot (one point per sample) without color-coding time.
AdvETUploadData_00 %>%
  ggplot(aes(x_pred_normalised, y_pred_normalised, xmin = -0.2, xmax = 1.2, ymin = -0.2, ymax = 1.2))  + #note that xmax and ymax are specified to give some space around the box with good data. If values are higher, they aren't shown
  geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = 'white', color = 'darkgrey')+ #This draws a rectangle the size of the screen
  theme_classic()+
  geom_point()+
  geom_point(color = "darkblue", size = 1.0) +
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +coord_fixed() + 
  facet_wrap(~file)+
  theme(text = element_text(size = 20)) +
  ggtitle(i)

# X-over time plot for 5 seconds
AdvETUploadData_00 %>%
  ggplot(aes(time_elapsed, x_pred_normalised)) +geom_line()+ 
  xlim(000,5000)+
  facet_wrap(~file)

# Y-over time plot for 5 seconds
AdvETUploadData_00 %>%
  ggplot(aes(time_elapsed, y_pred_normalised)) +geom_line()+ 
  xlim(000,5000)+
  facet_wrap(~file)

# Basic heatmap.
AdvETUploadData_00 %>%
  ggplot(aes(x_pred_normalised, y_pred_normalised, xmin = -0.2, xmax = 1.2, ymin = -0.2, ymax = 1.2)) +
  theme_classic()+
   geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = 'white', color = 'darkgrey')+ #screen
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +coord_fixed() + 
  scale_fill_distiller(palette= "Spectral") 