library(rstatix)
library(reshape)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)

# import data set, selecting sheet 3 within workbook
slu <- read_excel("C:/Users/kyler/OneDrive/Documents/ISU/Projects/Stats-2018 Slu Cluster Meta Data.xlsx", sheet = 3, col_names = TRUE)

# Calculate means and std deviations of velocity and elbow torque
velo5_summ <- slu %>%
  get_summary_stats("5oz Hand Velocity", type= "mean_sd")
velo7_summ <- slu %>%
  get_summary_stats("7oz Hand Velocity", type= "mean_sd")
velo9_summ <- slu %>%
  get_summary_stats("9oz Hand Velocity", type= "mean_sd")
varus5_summ <- slu %>%
  get_summary_stats("Peak 5oz Moment", type= "mean_sd")
varus7_summ <- slu %>%
  get_summary_stats("Peak 7oz Moment", type= "mean_sd")
varus9_summ <- slu %>%
  get_summary_stats("Peak 9oz Moment", type= "mean_sd")
comp5_summ <- slu %>%
  get_summary_stats("Peak Z ELB Force 5", type= "mean_sd")
comp7_summ <- slu %>%
  get_summary_stats("Peak Z ELB Force 7", type= "mean_sd")
comp9_summ <- slu %>%
  get_summary_stats("Peak Z ELB Force 9", type= "mean_sd")
med5_summ <- slu %>%
  get_summary_stats("Peak Medial ELB Force 5oz", type= "mean_sd")
med7_summ <- slu %>%
  get_summary_stats("Peak Medial ELB Force 7oz", type= "mean_sd")
med9_summ <- slu %>%
  get_summary_stats("Peak Medial ELB Force 9oz", type= "mean_sd")
lat5_summ <- slu %>%
  get_summary_stats("Peak Lateral ELB Force 5", type= "mean_sd")
lat7_summ <- slu %>%
  get_summary_stats("Peak Lateral ELB Force 7", type= "mean_sd")
lat9_summ <- slu %>%
  get_summary_stats("Peak Lateral ELB Force 9", type= "mean_sd")
extvel5_summ <- slu %>%
  get_summary_stats("Elbow Extension Velocity 5oz", type= "mean_sd")
extvel7_summ <- slu %>%
  get_summary_stats("Elbow Extension Velocity 7oz", type= "mean_sd")
extvel9_summ <- slu %>%
  get_summary_stats("Elbow Extension Velocity 9oz", type= "mean_sd")
flex5_summ <- slu %>%
  get_summary_stats("Elbow Flexion 5oz", type= "mean_sd")
flex7_summ <- slu %>%
  get_summary_stats("Elbow Flexion 7oz", type= "mean_sd")
flex9_summ <- slu %>%
  get_summary_stats("Elbow Flexion 9oz", type= "mean_sd")

# combine each data frame into a list
velo_list <- list(velo5_summ, velo7_summ, velo9_summ)
var_list <- list(varus5_summ, varus7_summ, varus9_summ)
comp_list <- list(comp5_summ, comp7_summ, comp9_summ)
med_list <- list(med5_summ, med7_summ, med9_summ)
lat_list <- list(lat5_summ, lat7_summ, lat9_summ)
extvel_list <- list(extvel5_summ, extvel7_summ, extvel9_summ)
flex_list <- list(flex5_summ, flex7_summ, flex9_summ)

# merge lists into a single data frame
Velocity <- Reduce(function(x, y) merge(x, y, all=TRUE), velo_list)
Varus <- Reduce(function(x, y) merge(x, y, all=TRUE), var_list)
Compression <- Reduce(function(x, y) merge(x, y, all=TRUE), comp_list)
Medial <- Reduce(function(x, y) merge(x, y, all=TRUE), med_list)
Lateral <- Reduce(function(x, y) merge(x, y, all=TRUE), lat_list)
Extension_Velocity <- Reduce(function(x, y) merge(x, y, all=TRUE), extvel_list)
Elbow_Flexion <- Reduce(function(x, y) merge(x, y, all=TRUE), flex_list)

# change variable names
Velocity$variable <- gsub('5oz Hand Velocity', '5', Varus$variable)
Velocity$variable <- gsub('7oz Hand Velocity', '7', Varus$variable)
Velocity$variable <- gsub('9oz Hand Velocity', '9', Varus$variable)

Varus$variable <- gsub('Peak 5oz Moment', '5', Varus$variable)
Varus$variable <- gsub('Peak 7oz Moment', '7', Varus$variable)
Varus$variable <- gsub('Peak 9oz Moment', '9', Varus$variable)

Compression$variable <- gsub('Peak Z ELB Force 5', '5', Compression$variable)
Compression$variable <- gsub('Peak Z ELB Force 7', '7', Compression$variable)
Compression$variable <- gsub('Peak Z ELB Force 9', '9', Compression$variable)

Medial$variable <- gsub('Peak Medial ELB Force 5oz', '5', Medial$variable)
Medial$variable <- gsub('Peak Medial ELB Force 7oz', '7', Medial$variable)
Medial$variable <- gsub('Peak Medial ELB Force 9oz', '9', Medial$variable)

Lateral$variable <- gsub('Peak Lateral ELB Force 5', '5', Lateral$variable)
Lateral$variable <- gsub('Peak Lateral ELB Force 7', '7', Lateral$variable)
Lateral$variable <- gsub('Peak Lateral ELB Force 9', '9', Lateral$variable)

Extension_Velocity$variable <- gsub('Elbow Extension Velocity 5oz', '5', Extension_Velocity$variable)
Extension_Velocity$variable <- gsub('Elbow Extension Velocity 7oz', '7', Extension_Velocity$variable)
Extension_Velocity$variable <- gsub('Elbow Extension Velocity 9oz', '9', Extension_Velocity$variable)

Elbow_Flexion$variable <- gsub('Elbow Flexion 5oz', '5', Elbow_Flexion$variable)
Elbow_Flexion$variable <- gsub('Elbow Flexion 7oz', '7', Elbow_Flexion$variable)
Elbow_Flexion$variable <- gsub('Elbow Flexion 9oz', '9', Elbow_Flexion$variable)

# bar plots to viz differences in hand velocity and elbow torque
ggplot(Velocity, aes(x = variable, y = mean)) + geom_col(fill="red") + 
  labs(title = "Hand Velocity Differences by Ball Weight", x = "Ball Weight (oz)", y = "Hand Velocity (m/s)")

ggplot(Varus,  aes(x = variable, y = mean)) + geom_col(fill="purple") + 
  labs(title = "Peak Elbow Varus Torque Differences by Ball Weight", x = "Ball Weight (oz)", y = "Elbow Varus Torque (Nm)")

ggplot(Compression,  aes(x = variable, y = mean)) + geom_col(fill="blue") + 
  labs(title = "Peak Elbow Compression Force Differences by Ball Weight", x = "Ball Weight (oz)", y = "Peak Compression Force (N)")

ggplot(Medial,  aes(x = variable, y = mean)) + geom_col(fill="violet") + 
  labs(title = "Peak Elbow Medial Force Differences by Ball Weight", x = "Ball Weight (oz)", y = "Peak Medial Force (N)")

ggplot(Lateral,  aes(x = variable, y = mean)) + geom_col(fill="green") + 
  labs(title = "Peak Elbow Lateral Force Differences by Ball Weight", x = "Ball Weight (oz)", y = "Peak Compression Force (N)")

ggplot(Extension_Velocity,  aes(x = variable, y = mean)) + geom_col(fill="yellowgreen") + 
  labs(title = "Peak Elbow Extension Velocity Differences by Ball Weight", x = "Ball Weight (oz)", y = "Peak Elbow Extension Velocity (dg/s)")

ggplot(Elbow_Flexion,  aes(x = variable, y = mean)) + geom_col(fill="orange") + 
  labs(title = "Elbow Flexion Angle at MER Differences by Ball Weight", x = "Ball Weight (oz)", y = "Peak Elbow Flexion Angle (dg)")

