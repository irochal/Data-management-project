library(ProjectTemplate)
setwd("/Volumes/KINGSTON")
create.project("Data Management and EDA project")
setwd("Data Management and EDA project")
load.project()

install.packages("ggplot2")
library("ggplot2")


library(readr)
cyber_security_1_enrolments <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-1_enrolments.csv")
head(cyber_security_1_enrolments)
enrol = cyber_security_1_enrolments$learner_id
length(enrol)
nr1 = nrow(cyber_security_1_enrolments)

table(cyber_security_1_enrolments$gender)
table(cyber_security_1_enrolments$age_range)
table(cyber_security_1_enrolments$highest_education_level)
table(cyber_security_1_enrolments$employment_status)
table(cyber_security_1_enrolments$detected_country)
table(cyber_security_1_enrolments$employment_area)

library(gdata)
unknownToNA(cyber_security_1_enrolments$gender, unknown = "Unknown")


barplot(table(cyber_security_1_enrolments$role))

dc = table(cyber_security_1_enrolments$detected_country)
length(dc)
# In general people from 184 countries enrolled in the seminar 
dc_max = max(table(cyber_security_1_enrolments$detected_country))
which.max(dc)
sort(dc)
# Barplot showing the 23 countries from where more people attended 
barplot(tail(sort(dc),23), col = 1:23)

# Barplot showing the gender 
barplot(table(cyber_security_1_enrolments$gender))
table(cyber_security_1_enrolments$gender)

# Find the number of people who enrolled in each graph 
nr2 = nrow(cyber_security_2_enrolments)

nr3 = nrow(cyber_security_3_enrolments)

nr4 = nrow(cyber_security_4_enrolments)

nr5 = nrow(cyber_security_5_enrolments)

nr6 = nrow(cyber_security_6_enrolments)

nr7 = nrow(cyber_security_7_enrolments)


# Create a vector with the number of enrollments in each run 
number_of_enrollments = c(nr1,nr2,nr3,nr4,nr5,nr6,nr7)
number_of_enrollments

par(mfrow = c(1, 2))
plot(number_of_enrollments, xlab = "Run number", ylab = "Number of enrollments", type = "b",
     main = "Plot of enrollments numbers over each run")
# We observe that there is a major declining trend in student's enrollments over the 7 runs 

# Find out how many people actually completed the course in each run 
na.omit(cyber_security_1_enrolments$fully_participated_at)
successful_run1 = (length(na.omit(cyber_security_1_enrolments$fully_participated_at))/nr1) *100

na.omit(cyber_security_2_enrolments$fully_participated_at)
successful_run2 = (length(na.omit(cyber_security_2_enrolments$fully_participated_at))/nr2) *100

na.omit(cyber_security_3_enrolments$fully_participated_at)
successful_run3 = (length(na.omit(cyber_security_3_enrolments$fully_participated_at))/nr3) *100

na.omit(cyber_security_4_enrolments$fully_participated_at)
successful_run4 = (length(na.omit(cyber_security_4_enrolments$fully_participated_at))/ nr4)*100

na.omit(cyber_security_5_enrolments$fully_participated_at)
successful_run5 = (length(na.omit(cyber_security_5_enrolments$fully_participated_at))/ nr5)*100

na.omit(cyber_security_6_enrolments$fully_participated_at)
successful_run6 = (length(na.omit(cyber_security_6_enrolments$fully_participated_at))/ nr6) *100

na.omit(cyber_security_7_enrolments$fully_participated_at)
successful_run7 = (length(na.omit(cyber_security_7_enrolments$fully_participated_at))/ nr7) *100

number_of_sucessfull_completions = c(successful_run1, successful_run2, successful_run3,
                                     successful_run4, successful_run5, successful_run6,
                                     successful_run7)

plot(number_of_sucessfull_completions, xlab = "Run number", ylab = "% of people who fully participated",
     type = "b", main = "Plot of % of people who completed the course")

# We observe that for example in run 2 even though we have more people enrolled compared to runs 
# 3-7, the percentage of people that actually completed the course is the lowest. 


df1 = cyber_security_2_enrolments[, c("learner_id", "fully_participated_at", "gender")]


View(na.omit(df1))

# INCLUDE THAT IN THE REPORT: It does not work bc there are not enough data 


df2 = cyber_security_2_enrolments[, c("learner_id", "fully_participated_at", "age_range")]
View(na.omit(df2))

# Similarly this does not work because we do not have enough data 


df3 = cyber_security_2_enrolments[, c("learner_id", "fully_participated_at", "detected_country")]
f = na.omit(df3)

barplot(table(f$detected_country), ylim = c(0,25), col = ifelse(table(f$detected_country)> 10, "red", "green"))
sort(table(f$detected_country))

df4 = cyber_security_1_enrolments[, c("learner_id", "fully_participated_at", "detected_country")]
d = na.omit(df4)
barplot(table(d$detected_country), ylim = c(0,1000))
sort(table(d$detected_country))

# I may be approaching the end of the first CRISPDM cycle because I have reached some dead
# ends and 

# Let's have a better look on step activity know and how the engagement varies between the  runs 
# RUN 1 
par(mfrow = c(1, 2))

t1 = table(cyber_security_1_step_activity_1$new_step_1)
dft1 = as.data.frame(t1)
se1 = dft1[,2]
steps1 = dft1[,1]

t1_complete = as.data.frame(table(cyber_security_1_step_activity_1$new_step_1[cyber_security_1_step_activity_1$last_completed_at != ""]))

plot(as.numeric(steps1), (t1_complete[,2]/dft1[,2])*100, type = "l", xaxt = "n", 
     ylab = "Percentage of people who completed each step", xlab = "Step number", 
     main = "Plot of percentage of people participating that complited each step (Run 1) ")
axis(1,at = 1:60, labels = steps1)


ddf1 = data.frame(steps1, se1)
plot(as.numeric(steps1), se1, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 1)")
axis(1,at = 1:60, labels = steps1)
lines(t1_complete[,2], col = "red", lty = 2)
legend("topright", legend=c("Participated", "Completed"),
        col=c("black", "red"), lty=1:2, cex=0.8)

# This plots shows a very clear declining trend in students engagement, as the weeks progress. We see
# some points that standout. These are 1.18, 3.18 and 3.21. Now 1.18 is a video, 3.18 is the test and 
# 3.21 is the glossary and referebces. So we see that in general people have interacted with the glosary
# so this may be an important section of the course that needs to stay in. Also for this specific run, it 
#seems that the engagement for the test was unexpectedly high, BUT COMPLETION WAS QUITE LOW . 

# Now let's check if the same trends follows in all of the runs 
# RUN 2

t2 = table(cyber_security_2_step_activity_2$new_step_2)
dft2 = as.data.frame(t2)
se2 = dft2[,2]
steps2 = dft2[,1]

t2_complete = as.data.frame(table(cyber_security_2_step_activity_2$new_step_2[cyber_security_2_step_activity_2$last_completed_at != ""]))

par(mfrow = c(1, 2))

plot(as.numeric(steps2), (t2_complete[,2]/dft2[,2])*100, type = "l", xaxt = "n", 
     ylab = "Percentage of people who completed each step", xlab = "Step number", 
     main = "Plot of percentage of people participating that complited each step (Run 2) ")
axis(1,at = 1:63, labels = steps2)


ddf2 = data.frame(steps2, se2)
plot(as.numeric(steps2), se2, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number in run 2")
axis(1,at = 1:63, labels = steps2)
lines(t2_complete[,2], col = "red", lty = 2)
legend("topright", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8)

# We observe the same declining trend here. Moreover 3.21 seems to be a section that has more engagement
#Also the engagement for 1.19 seems to be a bit higher. We note here that 1.19 is a video, so again we 
# have some evidence that the videos may have more engagement. Finally, for this particular run, the test
# seems to have an extremelly low engagement, which is expected since this course had the lower number
# of completion. So this low completion might no mean that the participants failed the test, but most 
#probably it means that the did not even engage with the test. 

# RUN 3
t3 = table(cyber_security_3_step_activity_3$new_step_3)
dft3 = as.data.frame(t3)
se3= dft3[,2]
steps3 = dft3[,1]

ddf3 = data.frame(steps3, se3)
t3_complete = as.data.frame(table(cyber_security_3_step_activity_3$new_step_3[cyber_security_3_step_activity_3$last_completed_at != ""]))

par(mfrow = c(1, 2))

plot(as.numeric(steps3), (t3_complete[,2]/dft3[,2])*100, type = "l", xaxt = "n", 
     ylab = "Percentage of people who completed each step", xlab = "Step number", 
     main = "Plot of percentage of people participating that complited each step (Run 3) ")
axis(1,at = 1:62, labels = steps3)


plot(as.numeric(steps3), se3, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 3)")
axis(1,at = 1:62, labels = steps3)
lines(t3_complete[,2], col = "red", lty = 2)
legend("topright", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8)

# Again we observe a declining trend, with the same outliers. Here we dont have the glossary. However, 
# this could be a section to keep in. Here engagement follows completion

# RUN 4
t4 = table(cyber_security_4_step_activity_4$new_step_4)
dft4 = as.data.frame(t4)
se4 = dft4[,2]
steps4 = dft4[,1]
coords = paste(steps4,se4,sep=",")

ddf4 = data.frame(steps4, se4)
t4_complete = as.data.frame(table(cyber_security_4_step_activity_4$new_step_4[cyber_security_4_step_activity_4$last_completed_at != ""]))

par(mfrow = c(1, 2))

plot(as.numeric(steps4), (t4_complete[,2]/dft4[,2])*100, type = "l", xaxt = "n", 
     ylab = "Percentage of people who completed each step", xlab = "Step number", 
     main = "Plot of percentage of people participating that complited each step (Run 4) ")
axis(1,at = 1:62, labels = steps4)


plot(as.numeric(steps4), se4, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 4)")
axis(1,at = 1:62, labels = steps4)
lines(t4_complete[,2], col = "red", lty = 2)
legend("topright", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8)

# Again we observe a declining trend, with the same outliers 

# RUN 5 
t5 = table(cyber_security_5_step_activity_5$new_step_5)
dft5 = as.data.frame(t5)
se5 = dft5[,2]
steps5 = dft5[,1]
ddf5 = data.frame(steps5, se5)

t5_complete = as.data.frame(table(cyber_security_5_step_activity_5$new_step_5[cyber_security_5_step_activity_5$last_completed_at != ""]))

par(mfrow = c(1, 2))

plot(as.numeric(steps5), (t5_complete[,2]/dft5[,2])*100, type = "l", xaxt = "n", 
     ylab = "Percentage of people who completed each step", xlab = "Step number", 
     main = "Plot of percentage of people participating that complited each step (Run 5) ")
axis(1,at = 1:62, labels = steps5)


plot(as.numeric(steps5), se5, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 5)")
axis(1,at = 1:62, labels = steps5)
lines(t5_complete[,2], col = "red", lty = 2)
legend("topright", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8)


# Again we observe the same declining trend. Again we have the same outliers, but here for steps
#starting in week 2 we see a stabilisation in engagement compared to steps in week 3

# RUN 6
t6 = table(cyber_security_6_step_activity_6$new_step_6)
dft6 = as.data.frame(t6)
se6 = dft6[,2]
steps6 = dft6[,1]

ddf6 = data.frame(steps6, se6)
t6_complete = as.data.frame(table(cyber_security_6_step_activity_6$new_step_6[cyber_security_6_step_activity_6$last_completed_at != ""]))

par(mfrow = c(1, 2))

plot(as.numeric(steps6), (t6_complete[,2]/dft6[,2])*100, type = "l", xaxt = "n", 
     ylab = "Percentage of people who completed each step", xlab = "Step number", 
     main = "Plot of percentage of people participating that complited each step (Run 6) ")
axis(1,at = 1:62, labels = steps6)


plot(as.numeric(steps6), se6, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 6)")
axis(1,at = 1:62, labels = steps6)
lines(t6_complete[,2], col = "red", lty = 2)
legend("topright", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8)

# Again soe declining trend. However here for step 1.03 we see a big different in starting engagement
# and completion 

# RUN 7 
t7 = table(cyber_security_7_step_activity_7$new_step_7)
dft7 = as.data.frame(t7)
se7 = dft7[,2]
steps7 = dft7[,1]

ddf7 = data.frame(steps7, se7)
t7_complete = as.data.frame(table(cyber_security_7_step_activity_7$new_step_7[cyber_security_7_step_activity_7$last_completed_at != ""]))

par(mfrow = c(1, 2))

plot(as.numeric(steps7), (t7_complete[,2]/dft7[,2])*100, type = "l", xaxt = "n", 
     ylab = "Percentage of people who completed each step", xlab = "Step number", 
     main = "Plot of percentage of people participating that complited each step (Run 7) ")
axis(1,at = 1:62, labels = steps7)


plot(as.numeric(steps7), se4, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 7)")
axis(1,at = 1:62, labels = steps7)
lines(t7_complete[,2], col = "red", lty = 2)
legend("topright", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8)


#Again we observe the same declining trend. However this run is the one with the biggest difference in
# engagement and completion


# step_act = ggplot(all_run_activity_step, aes(x = steps, y = se, colour = factor(run), group = run ))  + geom_line() +
#  xlab("Step number") + ylab("Step Engagement") + ggtitle("Plot of step engagement over the 7 runs") + ylim(0,5000)

complete_step_act = ggplot(all_run_activity_step_complete, aes(x = steps, y = se, colour = factor(run), group = run ))  + geom_line() +
  xlab("Step number") + ylab("Completed Step Engagement") + ggtitle("Plot of completed step engagement over the 7 runs") + ylim(0,5000)
complete_step_act

# We observe that the step activity completion  for run 1 is the highest. This can be explained by the fact
# that the enrollments for this run were the highest. However, when compared to the other 6 runs
# we observe that the engagement declines much faster (the slope is steeper) for this run. 
# Also we observe that 6,7 have the lowest engagement. This is also expected since these were the
# runs that had the lowest number of enrollments. 
# Now it would be interesting to see what happens with run 4 since it was the run which had similar
# enrolments number to these of run 3 and 5, but considerably higher completion rate. We observe 
# that run 4 had very similar engagement rate to that of run 5. So this would be an interesting
# area to investigate further. ( We can have a look at the video stats !!!!!!!!!!!!!!!!!!!)

# Moreover we observe that the engagement in run 2 is similar to that of runs 3,4 and 5. However
# the number of enrollments was almost double. So this low engagement could explain the low
# completion rate

# In general we observe a declining trend in engagement. Also in all the runs we have a second
# further decrease when week 2 starts. However there seems to be some max points for
# run 1. If we look at the individual plot for run one we will observe that these correspond to
# video steps, as well as the glossary. The observation about the glossary also stands out in run
# 2, so the course developers should consider putting it back on the course, since it seemed 
# popular. Also for runs 2-7 we observe a sharp decrease in one point, which is the point representing
# the test. This is expected as the completion rate for these run was much lower. However, from
# the graph before we saw that the completion for run 4 was much higher when compared to runs 3-7
# but here the engagement on the steps is very similar. 

# From the plots before we saw that the numebr of enrollments for 3 and 5 did not differ much, 
# however the completion rate was higher for run 3. From the all activity plot however we see that
# the engagement for run 3 is lower than that of run 5, so that would be an interesting thing to
# look at

# First let's look at the demographics for these specific groups.
tt3 = table(cyber_security_3_enrolments$gender)
gender_percent_3 = round(tt3*100/nrow(cyber_security_3_enrolments),2)

tt5 = table(cyber_security_5_enrolments$gender)
gender_percent_5 = round(tt5*100/nrow(cyber_security_5_enrolments),2)

par(mfrow = c(1, 2))
barplot(gender_percent_3, ylab = "%", main = "Run 3")
barplot(gender_percent_5, ylab = "%", main = "Run 5")
# When it comes to gender we do not really observe any differences 
ttt3 = table(cyber_security_3_enrolments$age_range)
age_percent_3 = round(ttt3*100/nrow(cyber_security_3_enrolments),2)

ttt5 = table(cyber_security_5_enrolments$age_range)
age_percent_5 = round(ttt5*100/nrow(cyber_security_5_enrolments),2)

barplot(age_percent_3, ylab = "%", main = "Run 3", col = "olivedrab4")
barplot(age_percent_5, ylab = "%", main = "Run 5", col = "deepskyblue3")
# Here we observe something interesting. We see that in run 5 we have more people in the age 
# ranges of 56-65 and >65, compared to run 3 that has more people in the age group 18-25 and 
# 26- 35. So that could be an indication that these age groups are more interested in the course.
# However it should be noted that the age range sample is quite small, so our conclusions 
# may not be representative of the data set. 

# Now for runs 3-7 we have some video stats that would be interesting to look, and we could 
# also compare the ones for run 3 and 5: 

# FIRST LOAD THE DATA 


# Mporo na kano plot apo ta video gia na do se pia video upirxe consistency sti parakolouthisi
# oste na boithisei thn etairia na dei pia video kratane to endiaferon
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
# Lets see the engagement for all the videos in run 3
install.packages("gridExtra")
require(gridExtra)

p1_run3 = ggplot(ndf3_video_week1, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 3 (Week 1)") 

p2_run3 = ggplot(ndf3_video_week2, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 3 (Week 2)") 

p3_run3 = ggplot(ndf3_video_week3, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 3 (Week 3)") 


# Here lets see which videos had the biggest turnover
video_eng = cyber_security_3_video_stats$viewed_five_percent - cyber_security_3_video_stats$viewed_ninetyfive_percent
d3f = data.frame(step = cyber_security_3_video_stats$step_position , left = video_eng)  
d3f
d3f_plot = ggplot(d3f, aes(x = as.factor(step), y = left, fill = factor(step))) + geom_bar(stat="identity") + xlab("Step number") +
  ylab("People leaving") + ggtitle("Plot of step number against percentage of people leaving (Run 3)") + ylim(0,27)

grid.arrange(p1_run3, p2_run3, p3_run3,d3f_plot,nrow = 2, ncol=2)


# By looking at the plot we see that all video have a very large decline from 95% to 100%.
# So it is better to draw conclusions by looking up to 95%
# In general for week 1 we observe the most decline in engagement for video 1.5
# For week 2 we observe a steep decline for videos 2.4 and 2.11 and fro week 3 in general we observe quite a 
# constant engagement in the videos, with video 3.14 being the one where more people left 
#However remember from the previous plots that the engagement in the steps
# during week 3 was lower, so probably the people that have stayed through week 3 are interested in the course
video_duration_3 = data.frame(step_position = cyber_security_3_video_stats$step_position,
                              duration = cyber_security_3_video_stats$video_duration)
video_duration_3

# From the graph we observe that when people watch up to 95% of the video then there is a 
# sudden decline in engagement (probably people just don't watch the final minutes)
# (the course developers should look not to include important things on the last minutes)
# So we can take a better look at the engagement up to 95%. 

plot(cyber_security_3_video_stats$viewed_ninetyfive_percent, cyber_security_3_video_stats$video_duration,
     xlab = "% of people who watched 95% of the video", ylab = "Video duration in minutes",
     main = "Plot of video duration vs engagement (Run 3")
abline(lm(cyber_security_3_video_stats$video_duration ~ cyber_security_3_video_stats$viewed_ninetyfive_percent),
       lty = 2 , col = "red")

cor(cyber_security_3_video_stats$viewed_ninetyfive_percent, cyber_security_3_video_stats$video_duration)

# We observe that there is a strong negative correlation (-0.8068828) between engagement and duration. 
# So in general when the duration is higher, less people tend to watch 95% of the videos. 

# From the plot above we see that in general the longer the videos the less the 100% engagement
# From the table, we observe that the videos which had the highest decline in engagement are 
# indeed the ones that are some with longer in duration. So the developers of the course might want to
# create shorter videos

# Now let's look if that happens in the other runs. And then we can also focus on runs 3 and 5. 

# RUN 4 
p1_run4 = ggplot(ndf4_video_week1, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 4 (Week 1)") 

p2_run4 = ggplot(ndf4_video_week2, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 4 (Week 2)") 

p3_run4 = ggplot(ndf4_video_week3, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 4 (Week 3)") 


# Here lets see which videos had the biggest turnover
video_eng = cyber_security_4_video_stats$viewed_five_percent - cyber_security_4_video_stats$viewed_ninetyfive_percent
d4f = data.frame(step = cyber_security_4_video_stats$step_position , left = video_eng)  
d4f
d4f_plot = ggplot(d4f, aes(x = as.factor(step), y = left, fill = factor(step))) + geom_bar(stat="identity") + xlab("Step number") +
  ylab("People leaving") + ggtitle("Plot of step number against percentage of people leaving (Run 4)") + ylim(0,27)

grid.arrange(p1_run4, p2_run4, p3_run4,d4f_plot, nrow = 2, ncol=2)


# Here we observe again a similar declining pattern in engagement. For week 1 we observe the 
# highest decline in 1.5 before. For week two again we observe the biggest drop in engagement for videos  
# 2.11 and 2.4. For week 3 engagement seems pretty constant like before . 

video_duration_4 = data.frame(step_position = cyber_security_4_video_stats$step_position,
                              duration = cyber_security_4_video_stats$video_duration)
video_duration_4

# Now let's look if there is a relationship between duration and engagement here as well

plot(cyber_security_4_video_stats$viewed_ninetyfive_percent, cyber_security_4_video_stats$video_duration,
     xlab = "% of people who watched 100% of the video", ylab = "Video duration in minutes",
     main = "Plot of video duration vs engagement (Run 4)")
abline(lm(cyber_security_4_video_stats$video_duration ~ cyber_security_4_video_stats$viewed_ninetyfive_percent),
       lty = 2 , col = "red")

cor(cyber_security_4_video_stats$viewed_ninetyfive_percent, cyber_security_4_video_stats$video_duration)

# Here we see again a very strong negative correlation between engagement and duration (-0.817063)

# RUN 5 
p1_run5 = ggplot(ndf5_video_week1, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 5 (Week 1)") 

p2_run5 = ggplot(ndf5_video_week2, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 5 (Week 2)") 

p3_run5 = ggplot(ndf5_video_week3, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 5 (Week 3)") 

video_eng = cyber_security_5_video_stats$viewed_five_percent - cyber_security_5_video_stats$viewed_ninetyfive_percent
d5f = data.frame(step = cyber_security_5_video_stats$step_position , left = video_eng)  
d5f
d5f_plot = ggplot(d5f, aes(x = as.factor(step), y = left, fill = factor(step))) + geom_bar(stat="identity") + xlab("Step number") +
  ylab("People leaving") + ggtitle("Plot of step number against percentage of people leaving (Run 5)") + ylim(0,27)

grid.arrange(p1_run5, p2_run5, p3_run5,d5f_plot,nrow = 2, ncol=2)

# Again we observe very similar patterns as before. However in week 1, video 1.19 is more popular when compared
# to the other weeks and in general for week 1 there is a decline in people leaving 

plot(cyber_security_5_video_stats$viewed_ninetyfive_percent, cyber_security_5_video_stats$video_duration,
     xlab = "% of people who watched 100% of the video", ylab = "Video duration in minutes",
     main = "Plot of video duration vs engagement (Run 5)")
abline(lm(cyber_security_5_video_stats$video_duration ~ cyber_security_5_video_stats$viewed_ninetyfive_percent),
       lty = 2 , col = "red")

cor(cyber_security_5_video_stats$viewed_ninetyfive_percent, cyber_security_5_video_stats$video_duration)
# Here we observe less correlation between duration and engagement. Could this mean that younger people
# have longer attention span and do engage more with videos of longer duration? 
# Reamber that in week 1 videos in general were longer and for this run we observe a different pattern 


# RUN 6
p1_run6 = ggplot(ndf6_video_week1, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 6 (Week 1)") 

p2_run6 = ggplot(ndf6_video_week2, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 6 (Week 2)") 

p3_run6 = ggplot(ndf6_video_week3, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 6 (Week 3)") 

video_eng = cyber_security_6_video_stats$viewed_five_percent - cyber_security_6_video_stats$viewed_ninetyfive_percent
d6f = data.frame(step = cyber_security_6_video_stats$step_position , left = video_eng)  
d6f
d6f_plot = ggplot(d6f, aes(x = as.factor(step), y = left, fill = factor(step))) + geom_bar(stat="identity") + xlab("Step number") +
  ylab("People leaving") + ggtitle("Plot of step number against percentage of people leaving (Run 6)") + ylim(0,27)

grid.arrange(p1_run6, p2_run6, p3_run6,d6f_plot,nrow = 2, ncol=2)

# Again we observe very similar patterns as before. However in week 1, video 1.19 is more popular when compared
# to the other weeks and in general for week 1 there is a decline in people leaving ????

plot(cyber_security_6_video_stats$viewed_ninetyfive_percent, cyber_security_6_video_stats$video_duration,
     xlab = "% of people who watched 100% of the video", ylab = "Video duration in minutes",
     main = "Plot of video duration vs engagement (Run 6)")
abline(lm(cyber_security_6_video_stats$video_duration ~ cyber_security_6_video_stats$viewed_ninetyfive_percent),
       lty = 2 , col = "red")

cor(cyber_security_6_video_stats$viewed_ninetyfive_percent, cyber_security_6_video_stats$video_duration)

# Again here there is a strong negative correlation between engagement and duration (-0.7175826)

# RUN 7 
p1_run7 = ggplot(ndf7_video_week1, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 7 (Week 1)") 

p2_run7 = ggplot(ndf7_video_week2, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 7 (Week 2)") 

p3_run7 = ggplot(ndf7_video_week3, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 7 (Week 3)") 

video_eng = cyber_security_7_video_stats$viewed_five_percent - cyber_security_7_video_stats$viewed_ninetyfive_percent
d7f = data.frame(step = cyber_security_7_video_stats$step_position , left = video_eng)  
d7f
d7f_plot = ggplot(d7f, aes(x = as.factor(step), y = left, fill = factor(step))) + geom_bar(stat="identity") + xlab("Step number") +
  ylab("People leaving") + ggtitle("Plot of step number against percentage of people leaving (Run 7)") + ylim(0,27)

grid.arrange(p1_run7, p2_run7, p3_run6,d7f_plot,nrow = 2, ncol=2)

# Again we observe very similar patterns as before. However in week 1, video 1.17 has higher turnover,
# when compared to the other runs

plot(cyber_security_7_video_stats$viewed_ninetyfive_percent, cyber_security_7_video_stats$video_duration,
     xlab = "% of people who watched 100% of the video", ylab = "Video duration in minutes",
     main = "Plot of video duration vs engagement (Run 7)")
abline(lm(cyber_security_7_video_stats$video_duration ~ cyber_security_7_video_stats$viewed_ninetyfive_percent),
       lty = 2 , col = "red")

cor(cyber_security_7_video_stats$viewed_ninetyfive_percent, cyber_security_7_video_stats$video_duration)
# Again very strong negative correlation (-0.8331848)


grid.arrange(d3f_plot, d4f_plot, d5f_plot, d6f_plot, d7f_plot)

# we observe that for all runs video 1.5 (281) has the largest dropout rate among all videos in week 1 and 
# all videos in general. Then for week 2 video 2.11 has the largest dropout (312) and for week 3
#except from run 6, video 3.14 (313) is the one with the highest dropout. 
# Now the videos with the lowest dropouts among all weeks for most? runs are videos 2.1 (37) and 
# 2.17 (92)
# Also what is quite interesting is that video 2.4 has the longest duration, but it is not the one
# with the highest dropout. On the other hand other videos seem to have higher dropout even if they are
# Shorter. So this may be an area that people find interesting. 

