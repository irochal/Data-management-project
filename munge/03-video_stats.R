library(dplyr)
library(tidyverse)

# RUN 3
# RUN 4
# # Create a new data frame that contains the % engagement over each video
video_df4 = data.frame(cyber.security.4_video.stats$step_position, cyber.security.4_video.stats[,9:15]) 
colnames(video_df4) = c("Step position",5,10,25,50,75,95,100)
all_video_4 = pivot_longer(video_df4, cols = !starts_with("Step"))

# Separate the videos by weeks

ndf4_video_week1 = all_video_4[1:35,]
ndf4_video_week2 = all_video_4[36:63,]
ndf4_video_week3 = all_video_4[64:91,]

# RUN 5
# # Create a new data frame that contains the % engagement over each video 
video_df5 = data.frame(cyber.security.5_video.stats$step_position, cyber.security.5_video.stats[,9:15]) 
colnames(video_df5) = c("Step position",5,10,25,50,75,95,100)
all_video_5 = pivot_longer(video_df5, cols = !starts_with("Step"))

# Separate the videos by weeks

ndf5_video_week1 = all_video_5[1:35,]
ndf5_video_week2 = all_video_5[36:63,]
ndf5_video_week3 = all_video_5[64:91,]

# RUN 6
# # Create a new data frame that contains the % engagement over each video
video_df6 = data.frame(cyber.security.6_video.stats$step_position, cyber.security.6_video.stats[,9:15]) 
colnames(video_df6) = c("Step position",5,10,25,50,75,95,100)
all_video_6 = pivot_longer(video_df6, cols = !starts_with("Step"))

# Separate the videos by weeks

ndf6_video_week1 = all_video_6[1:35,]
ndf6_video_week2 = all_video_6[36:63,]
ndf6_video_week3 = all_video_6[64:91,]

# RUN 7 
# # Create a new data frame that contains the % engagement over each video
video_df7 = data.frame(cyber.security.7_video.stats$step_position, cyber.security.7_video.stats[,9:15]) 
colnames(video_df7) = c("Step position",5,10,25,50,75,95,100)
all_video_7 = pivot_longer(video_df7, cols = !starts_with("Step"))

# Separate the videos by weeks

ndf7_video_week1 = all_video_7[1:35,]
ndf7_video_week2 = all_video_7[36:63,]
ndf7_video_week3 = all_video_7[64:91,]


# Create new data frames that include the video number and the percentage of people who left before
#watching up to 95%  of each of these videos 
# This is done for runs 4-7
# RUN 4
video_eng = cyber.security.4_video.stats$viewed_five_percent - cyber.security.4_video.stats$viewed_ninetyfive_percent
d4f = data.frame(step = cyber.security.4_video.stats$step_position , left = video_eng)  

# RUN 5
video_eng = cyber.security.5_video.stats$viewed_five_percent - cyber.security.5_video.stats$viewed_ninetyfive_percent
d5f = data.frame(step = cyber.security.5_video.stats$step_position , left = video_eng)  

# RUN 6 
video_eng = cyber.security.6_video.stats$viewed_five_percent - cyber.security.6_video.stats$viewed_ninetyfive_percent
d6f = data.frame(step = cyber.security.6_video.stats$step_position , left = video_eng)  

# RUN 7
video_eng = cyber.security.7_video.stats$viewed_five_percent - cyber.security.7_video.stats$viewed_ninetyfive_percent
d7f = data.frame(step = cyber.security.7_video.stats$step_position , left = video_eng)  













