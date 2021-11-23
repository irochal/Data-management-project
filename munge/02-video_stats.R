library(dplyr)
library(tidyverse)


cyber_security_3_video_stats <- 
  read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-3_video-stats.csv")

cyber_security_4_video_stats <- 
  read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-4_video-stats.csv")

cyber_security_5_video_stats <- 
  read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-5_video-stats.csv")

cyber_security_6_video_stats <- 
  read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-6_video-stats.csv")

cyber_security_7_video_stats <- 
  read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-7_video-stats.csv")


# RUN 3
# Create a new data frame that contains the % engagement over each step 
video_df3 = data.frame(cyber_security_3_video_stats$step_position, cyber_security_3_video_stats[,9:15]) 
colnames(video_df3) = c("Step position",5,10,25,50,75,95,100)
all_video_3 = pivot_longer(video_df3, cols = !starts_with("Step"))
view(all_video_3)



plot(cyber_security_3_video_stats$viewed_onehundred_percent, cyber_security_3_video_stats$video_duration,
     xlab = "% of people who watched 100% of the video", ylab = "Video duration in minutes",
     main = "Plot of video duration vs engagement")
abline(lm(cyber_security_3_video_stats$video_duration ~ cyber_security_3_video_stats$viewed_onehundred_percent),
       lty = 2 , col = "red")

cor(cyber_security_3_video_stats$viewed_onehundred_percent, cyber_security_3_video_stats$video_duration)

# We observe that there is some negative correlation between engagement and duration. So in general
# when the duration is higher, less people tend to watch 100% of the videos. 

ndf3_video_week1 = all_video_3[1:35,]
ndf3_video_week2 = all_video_3[36:63,]
ndf3_video_week3 = all_video_3[64:91,]

video_eng = cyber_security_3_video_stats$viewed_five_percent - cyber_security_3_video_stats$viewed_ninetyfive_percent
d3f = data.frame(step = cyber_security_3_video_stats$step_position , left = video_eng)  
d3f
# RUN 4
# # Create a new data frame that contains the % engagement over each step 
video_df4 = data.frame(cyber_security_4_video_stats$step_position, cyber_security_4_video_stats[,9:15]) 
colnames(video_df4) = c("Step position",5,10,25,50,75,95,100)
all_video_4 = pivot_longer(video_df4, cols = !starts_with("Step"))
view(all_video_4)

# Separate the videos by weeks

ndf4_video_week1 = all_video_4[1:35,]
ndf4_video_week2 = all_video_4[36:63,]
ndf4_video_week3 = all_video_4[64:91,]

# RUN 5
# # Create a new data frame that contains the % engagement over each step 
video_df5 = data.frame(cyber_security_5_video_stats$step_position, cyber_security_5_video_stats[,9:15]) 
colnames(video_df5) = c("Step position",5,10,25,50,75,95,100)
all_video_5 = pivot_longer(video_df5, cols = !starts_with("Step"))
view(all_video_5)

# Separate the videos by weeks

ndf5_video_week1 = all_video_5[1:35,]
ndf5_video_week2 = all_video_5[36:63,]
ndf5_video_week3 = all_video_5[64:91,]

# RUN 6
# # Create a new data frame that contains the % engagement over each step 
video_df6 = data.frame(cyber_security_6_video_stats$step_position, cyber_security_6_video_stats[,9:15]) 
colnames(video_df6) = c("Step position",5,10,25,50,75,95,100)
all_video_6 = pivot_longer(video_df6, cols = !starts_with("Step"))
view(all_video_6)

# Separate the videos by weeks

ndf6_video_week1 = all_video_6[1:35,]
ndf6_video_week2 = all_video_6[36:63,]
ndf6_video_week3 = all_video_6[64:91,]

# RUN 7 
# # Create a new data frame that contains the % engagement over each step 
video_df7 = data.frame(cyber_security_7_video_stats$step_position, cyber_security_7_video_stats[,9:15]) 
colnames(video_df7) = c("Step position",5,10,25,50,75,95,100)
all_video_7 = pivot_longer(video_df7, cols = !starts_with("Step"))
view(all_video_7)

# Separate the videos by weeks

ndf7_video_week1 = all_video_7[1:35,]
ndf7_video_week2 = all_video_7[36:63,]
ndf7_video_week3 = all_video_7[64:91,]

