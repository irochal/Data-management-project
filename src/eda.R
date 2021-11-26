library(ProjectTemplate)
setwd("/Volumes/KINGSTON")
create.project("Data Management and EDA project")
setwd("/Volumes/KINGSTON/Data Management and EDA project")
load.project()

install.packages("ggplot2")
install.packages("gridExtra")
library("ggplot2")
library(gridExtra)

library(readr)

library(gdata)
unknownToNA(cyber.security.1_enrolments$gender, unknown = "Unknown")


# Find the number of people who enrolled in each graph 
nr1 = nrow(cyber.security.1_enrolments)
nr2 = nrow(cyber.security.2_enrolments)
nr3 = nrow(cyber.security.3_enrolments)
nr4 = nrow(cyber.security.4_enrolments)
nr5 = nrow(cyber.security.5_enrolments)
nr6 = nrow(cyber.security.6_enrolments)
nr7 = nrow(cyber.security.7_enrolments)


# Create a vector with the number of enrollments in each run 
number_of_enrollments = c(nr1,nr2,nr3,nr4,nr5,nr6,nr7)
number_of_enrollments

par(mfrow = c(1, 2))
plot(number_of_enrollments, xlab = "Run number", ylab = "Number of enrollments", type = "b",
     main = "Plot of enrollments numbers over each run", cex.main = 0.8)
# We observe that there is a major declining trend in student's enrollments over the 7 runs 

# Find out how many people actually completed the course in each run 
fp1 = cyber.security.1_enrolments$fully_participated_at[cyber.security.1_enrolments$fully_participated_at != ""]
successful_run1 = (length(fp1)/nr1) *100

fp2 = cyber.security.2_enrolments$fully_participated_at[cyber.security.2_enrolments$fully_participated_at != ""]
successful_run2 = (length(fp2)/nr2) *100

fp3 = cyber.security.3_enrolments$fully_participated_at[cyber.security.3_enrolments$fully_participated_at != ""]
successful_run3 = (length(fp3)/nr3)*100

fp4 = cyber.security.4_enrolments$fully_participated_at[cyber.security.4_enrolments$fully_participated_at != ""]
successful_run4 = (length(fp4)/ nr4)*100

fp5 = cyber.security.5_enrolments$fully_participated_at[cyber.security.5_enrolments$fully_participated_at != ""]
successful_run5 = (length(fp5)/ nr5)*100

fp6 = cyber.security.6_enrolments$fully_participated_at[cyber.security.6_enrolments$fully_participated_at != ""]
successful_run6 = (length(fp6)/ nr6) *100

fp7 = cyber.security.7_enrolments$fully_participated_at[cyber.security.7_enrolments$fully_participated_at != ""]
successful_run7 = (length(fp7)/ nr7) *100

number_of_sucessfull_completions = c(successful_run1, successful_run2, successful_run3,
                                     successful_run4, successful_run5, successful_run6,
                                     successful_run7)

plot(number_of_sucessfull_completions, xlab = "Run number", ylab = "% of people who fully participated",
     type = "b", main = "Plot of % of people who completed the course", cex.main = 0.8)

# We observe that for example in run 2 even though we have more people enrolled compared to runs 
# 3-7, the percentage of people that actually completed the course is the lowest. 
# In general we obsere a declining trend in participation, which has shown a somewhat stable participation
# during runs 3-5 and then the declining trend continues.
# What is really interesting here is that for runs 4-7 we observe a declining trend in enrolments, but an increasing
# trend in completion. Let,s look further into that 

# First let's look at all the people who participated 
par(mfrow = c(2, 2))
barplot((table(cyber.security.4_enrolments$gender)/nr4)*100, main = "Run 4", col = "darkseagreen 2")
barplot((table(cyber.security.5_enrolments$gender)/nr5)*100, main = "Run 5", col = "salmon")
barplot((table(cyber.security.6_enrolments$gender)/nr6)*100, main = "Run 6", col = "violet")
barplot((table(cyber.security.7_enrolments$gender)/nr7)*100, main = "Run 6", col = "yellow")
# We observe that most people's gender is unkown which makes our conclusions risky. However, for run 4
# which had the highest completion rate, we observe that there more males compared to females. For the 
# other runs we observe very similar numbers of males and females, with run 6 being the only run
# with more females. 

# Now let's look at the people who completed these runs 
par(mfrow = c(2, 2))
df4 = cyber.security.4_enrolments[, c("learner_id", "fully_participated_at", "gender")]
df4_gender = df4[apply(df4 != "", 1, all),]
barplot((table(df4_gender$gender)/nrow(df4_gender))*100, main = "Run 4", xlab = "Gender", ylab = "%", col = "darkseagreen2")

df5 = cyber.security.5_enrolments[, c("learner_id", "fully_participated_at", "gender")]
df5_gender = df5[apply(df5 != "", 1, all),]
barplot((table(df5_gender$gender)/nrow(df5_gender))*100, main = "Run 5", xlab = "Gender", ylab = "%", col = "salmon")

df1_6 = cyber.security.6_enrolments[, c("learner_id", "fully_participated_at", "gender")]
df6_gender = df1_6[apply(df1_6 != "", 1, all),]
barplot((table(df6_gender$gender)/nrow(df6_gender))*100,  ylim = c(0,200), main = "Run 6", xlab = "Gender", ylab = "%", col = "violet")

df1_7 = cyber.security.7_enrolments[, c("learner_id", "fully_participated_at", "gender")]
df7_gender = df1_7[apply(df1_7 != "", 1, all),]
barplot((table(df7_gender$gender)/nrow(df7_gender))*100, ylim = c(0,200), main = "Run 7", xlab = "Gender", ylab = "%", col = "yellow")
# INCLUDE THAT IN THE REPORT: In run 4, 6 and 7 , of those who registered their gender there were more males compared
# to females that completed the course. However the data is not sufficient. Especially for run 4 and 7 which have 
# the highest completion rate amongst the 4, this pattern is more visible. In run 5 again we can see this patter,
# but there were so little people with registered gender, which makes this insight not very useful

# In general there is some evidence that in the runs with higher completion there were more males who completed
# the course, but since there are many unknowns this may not be representative of what happens in reality

# Let's also take a look at the age ranges in these runs
par(mfrow = c(2, 2))
barplot((table(cyber.security.4_enrolments$age_range)/nr4)*100, main = "Run 4", col = "darkseagreen 2")
barplot((table(cyber.security.5_enrolments$age_range)/nr5)*100, main = "Run 5", col = "salmon")
barplot((table(cyber.security.6_enrolments$age_range)/nr6)*100, main = "Run 6", col = "violet")
barplot((table(cyber.security.7_enrolments$age_range)/nr7)*100, main = "Run 6", col = "yellow")
# We observe that again most age ranges are unknown. However it seems like run 5, is the only run 
# were the majority of registered age ranges, lies in the 46-55, 56-65 and >65 age ranges, while in 
# all the ither runs there are younger people

# Now for the ones that completed the course
df4_age = cyber.security.4_enrolments[, c("learner_id", "fully_participated_at", "age_range")]
df4_age_all = df4_age[apply(df4_age != "", 1, all),]
barplot((table(df4_age_all$age_range)/nrow(df4_age_all))*100, col = "darkseagreen2", main = "Run 4", ylab = "%")

df5_age = cyber.security.5_enrolments[, c("learner_id", "fully_participated_at", "age_range")]
df5_age_all = df5_age[apply(df5_age != "", 1, all),]
barplot((table(df5_age_all$age_range)/nrow(df5_age_all))*100, col = "salmon", main = "Run 5", ylab = "%")

df6_age = cyber.security.6_enrolments[, c("learner_id", "fully_participated_at", "age_range")]
df6_age_all = df6_age[apply(df6_age != "", 1, all),]
barplot((table(df6_age_all$age_range)/nrow(df6_age_all))*100, col = "violet", main = "Run 6", ylab = "%")

df7_age = cyber.security.7_enrolments[, c("learner_id", "fully_participated_at", "age_range")]
df7_age_all = df7_age[apply(df7_age != "", 1, all),]
barplot((table(df7_age_all$age_range)/nrow(df7_age_all))*100, col = "yellow", main = "Run 7", ylab = "%")

# We observe in runs 4,6 and 7 more younger people to have completed. However for run 5, the only two poeple,
# who had registered age group were >65. So age may be a factor affecting completion, but again not certain results. 


# I may be approaching the end of the first CRISPDM cycle because I have reached some dead
# ends. 

# Let's have a better look on step activity know and how the engagement varies between the  runs 
# RUN 1 
par(mfrow = c(2, 4))

ddf1 = data.frame(steps1, se1)
plot(as.numeric(steps1), se1, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 1)")
axis(1,at = 1:60, labels = steps1)
lines(t1_complete[,2], col = "red", lty = 2)
legend("topleft", legend=c("Participated", "Completed"),
        col=c("black", "red"), lty=1:2, cex=0.8, bty = "n")

# This plots shows a very clear declining trend in students engagement, as the weeks progress. We see
# some points that standout. These are 1.18, 3.18 and 3.21. Now 1.18 is a video, 3.18 is the test and 
# 3.21 is the glossary and referebces. So we see that in general people have interacted with the glosary
# so this may be an important section of the course that needs to stay in. Also for this specific run, it 
#seems that the engagement for the test was unexpectedly high, BUT COMPLETION WAS QUITE LOW . 
# In general there is a more sudden decline in week 2, and from then on the decline is more gradual.
# Also some oints that seem to have a lag between completion and engagement are 2.8 and 3.11 are both quiz
# sections, so it seems like people engage with those but don't complete them!

# Now let's check if the same trends follows in all of the runs 
# RUN 2

ddf2 = data.frame(steps2, se2)
plot(as.numeric(steps2), se2, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 2)")
axis(1,at = 1:63, labels = steps2)
lines(t2_complete[,2], col = "red", lty = 2)
legend("topleft", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8, bty = "n")

# We observe the same declining trend here. Moreover 3.21 seems to be a section that has more engagement
#Also the engagement for 1.19 seems to be a bit higher. We note here that 1.19 is a video, so again we 
# have some evidence that the videos may have more engagement. Finally, for this particular run, the test
# seems to have an extremelly low engagement, which is expected since this course had the lower number
# of completion. So this low completion might no mean that the participants failed the test, but most 
#probably it means that the did not even engage with the test. Also we observe the same pattern for the quizes

# RUN 3

plot(as.numeric(steps3), se3, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 3)")
axis(1,at = 1:62, labels = steps3)
lines(t3_complete[,2], col = "red", lty = 2)
legend("topleft", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8, bty = "n")

# Again we observe a declining trend, with the same outliers. Here we dont have the glossary. However, 
# this could be a section to keep in. Here engagement follows completion, and same trend for quizes

# RUN 4
plot(as.numeric(steps4), se4, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 4)")
axis(1,at = 1:62, labels = steps4)
lines(t4_complete[,2], col = "red", lty = 2)
legend("topleft", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8, bty = "n")

# Again we observe a declining trend, with the same outliers. Here there is a lag between engagement and completion
# for 3.3 which is a poll. Also same pattern for quizes (1.8, 2.8, 3.11)
# RUN 5 
plot(as.numeric(steps5), se5, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 5)")
axis(1,at = 1:62, labels = steps5)
lines(t5_complete[,2], col = "red", lty = 2)
legend("topleft", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8, bty = "n")

# Again we observe the same declining trend. Again we have the same outliers with quizes , but here for steps
#starting in week 2 we see a stabilisation in engagement compared to steps in week 3

# RUN 6
plot(as.numeric(steps6), se6, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 6)")
axis(1,at = 1:62, labels = steps6)
lines(t6_complete[,2], col = "red", lty = 2)
legend("topleft", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8, bty = "n")

# Again see declining trend. However here for step 1.03 we see a big different in starting engagement
# and completion. This section was an exercise (THE ONLY RUN TO SEE THAT). Also we have the same trend fro quizes

# RUN 7 
plot(as.numeric(steps7), se4, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 7)")
axis(1,at = 1:62, labels = steps7)
lines(t7_complete[,2], col = "red", lty = 2)
legend("topleft", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8, bty = "n")

#Again we observe the same declining trend. However this run is the one with the biggest difference in
# engagement and completion


# step_act = ggplot(all_run_activity_step, aes(x = steps, y = se, colour = factor(run), group = run ))  + geom_line() +
#  xlab("Step number") + ylab("Step Engagement") + ggtitle("Plot of step engagement over the 7 runs") + ylim(0,5000)

complete_step_act = ggplot(all_run_activity_step_complete, aes(x = steps, y = se, colour = factor(run), group = run ))  + geom_line() +
  xlab("Step number") + ylab("Completed Step Engagement") + ggtitle("Plot of completed step engagement over the 7 runs") + 
complete_step_act

# We observe that the step activity completion  for run 1 is the highest. This can be explained by the fact
# that the enrollments for this run were the highest. However, when compared to the other 6 runs
# we observe that the engagement declines much faster (the slope is steeper) for this run. 
# Also we observe that 6,7 have the lowest completion. This is also expected since these were the
# runs that had the lowest number of enrollments. 

# Now it would be interesting to see what happens with run 4 since it was the run which had similar
# enrolments number to that of run  5, but considerably higher completion rate. We observe 
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
# but here the engagement on the steps is very similar. Also step completion for 6 and 7 was quite similar,
# but course completion was higher for run 7. 

# From the plots before we saw that the numebr of enrollments for 4 and 5 did not differ much, 
# however the completion rate was higher for run 4. From the all activity plot however we see that
# the engagement for run 4 is lower than that of run 5, so that would be an interesting thing to
# look at

# Now for runs 3-7 we have some video stats that would be interesting to look. We will exclude run 3, since 
# it does not differ much from 4_7 and is also older:

# FIRST LOAD THE DATA 


# Mporo na kano plot apo ta video gia na do se pia video upirxe consistency sti parakolouthisi
# oste na boithisei thn etairia na dei pia video kratane to endiaferon


# RUN 4 
p1_run4 = ggplot(ndf4_video_week1, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 4 (Week 1)") 

p2_run4 = ggplot(ndf4_video_week2, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 4 (Week 2)") 

p3_run4 = ggplot(ndf4_video_week3, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 4 (Week 3)") 


# Here lets see which videos had the biggest turnover
video_eng = cyber.security.4_video.stats$viewed_five_percent - cyber.security.4_video.stats$viewed_ninetyfive_percent
d4f = data.frame(step = cyber.security.4_video.stats$step_position , left = video_eng)  
d4f
d4f_plot = ggplot(d4f, aes(x = as.factor(step), y = left, fill = factor(step))) + geom_bar(stat="identity") + xlab("Step number") +
  ylab("People leaving") + ggtitle("Plot of step number against percentage of people leaving (Run 4)") + ylim(0,27)

grid.arrange(p1_run4, p2_run4, p3_run4,d4f_plot, nrow = 2, ncol=2)
# By looking at the plot we see that all video have a very large decline from 95% to 100%.
# So it is better to draw conclusions by looking up to 95%
# In general for week 1 we observe the most decline in engagement for video 1.5
# For week 2 we observe a steep decline for videos 2.4 and 2.11 and fro week 3 in general we observe quite a 
# constant engagement in the videos, with video 3.14 being the one where more people left 
#However remember from the previous plots that the engagement in the steps
# during week 3 was lower, so probably the people that have stayed through week 3 are interested in the course

video_duration_4 = data.frame(step_position = cyber.security.4_video.stats$step_position,
                              duration = cyber.security.4_video.stats$video_duration)
video_duration_4

# From the graph we observe that when people watch up to 95% of the video then there is a 
# sudden decline in engagement (probably people just don't watch the final minutes)
# (the course developers should look not to include important things on the last minutes)
# So we can take a better look at the engagement up to 95%. 

# Now let's look if there is a relationship between duration and engagement here as well

plot(cyber.security.4_video.stats$viewed_ninetyfive_percent, cyber.security.4_video.stats$video_duration,
     xlab = "% of people who watched 100% of the video", ylab = "Video duration in minutes",
     main = "Plot of video duration vs engagement (Run 4)")
abline(lm(cyber.security.4_video.stats$video_duration ~ cyber.security.4_video.stats$viewed_ninetyfive_percent),
       lty = 2 , col = "red")

cor(cyber.security.4_video.stats$viewed_ninetyfive_percent, cyber.security.4_video.stats$video_duration)

# Here we see again a very strong negative correlation between engagement and duration (-0.817063)
# From the plot above we see that in general the longer the videos the less the 100% engagement
# From the table, we observe that the videos which had the highest decline in engagement are 
# indeed the ones that are some with longer in duration. So the developers of the course might want to
# create shorter videos

# RUN 5 
p1_run5 = ggplot(ndf5_video_week1, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 5 (Week 1)") 

p2_run5 = ggplot(ndf5_video_week2, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 5 (Week 2)") 

p3_run5 = ggplot(ndf5_video_week3, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 5 (Week 3)") 

video_eng = cyber.security.5_video.stats$viewed_five_percent - cyber.security.5_video.stats$viewed_ninetyfive_percent
d5f = data.frame(step = cyber.security.5_video.stats$step_position , left = video_eng)  
d5f
d5f_plot = ggplot(d5f, aes(x = as.factor(step), y = left, fill = factor(step))) + geom_bar(stat="identity") + xlab("Step number") +
  ylab("People leaving") + ggtitle("Plot of step number against percentage of people leaving (Run 5)") + ylim(0,27)

grid.arrange(p1_run5, p2_run5, p3_run5,d5f_plot,nrow = 2, ncol=2)

# Again we observe very similar patterns as before. However in week 1, video 1.19 is more popular when compared
# to the other weeks and in general for week 1 there is a decline in people leaving 

plot(cyber.security.5_video.stats$viewed_ninetyfive_percent, cyber.security.5_video.stats$video_duration,
     xlab = "% of people who watched 100% of the video", ylab = "Video duration in minutes",
     main = "Plot of video duration vs engagement (Run 5)")
abline(lm(cyber.security.5_video.stats$video_duration ~ cyber.security.5_video.stats$viewed_ninetyfive_percent),
       lty = 2 , col = "red")

cor(cyber.security.5_video.stats$viewed_ninetyfive_percent, cyber.security.5_video.stats$video_duration)
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

video_eng = cyber.security.6_video.stats$viewed_five_percent - cyber.security.6_video.stats$viewed_ninetyfive_percent
d6f = data.frame(step = cyber.security.6_video.stats$step_position , left = video_eng)  
d6f
d6f_plot = ggplot(d6f, aes(x = as.factor(step), y = left, fill = factor(step))) + geom_bar(stat="identity") + xlab("Step number") +
  ylab("People leaving") + ggtitle("Plot of step number against percentage of people leaving (Run 6)") + ylim(0,27)

grid.arrange(p1_run6, p2_run6, p3_run6,d6f_plot,nrow = 2, ncol=2)

# Again we observe very similar patterns as before. However in week 1, video 1.19 is more popular when compared
# to the other weeks and in general for week 1 there is a decline in people leaving ????

plot(cyber.security.6_video.stats$viewed_ninetyfive_percent, cyber.security.6_video.stats$video_duration,
     xlab = "% of people who watched 100% of the video", ylab = "Video duration in minutes",
     main = "Plot of video duration vs engagement (Run 6)")
abline(lm(cyber.security.6_video.stats$video_duration ~ cyber.security.6_video.stats$viewed_ninetyfive_percent),
       lty = 2 , col = "red")

cor(cyber.security.6_video.stats$viewed_ninetyfive_percent, cyber.security.6_video.stats$video_duration)

# Again here there is a strong negative correlation between engagement and duration (-0.7175826)

# RUN 7 
p1_run7 = ggplot(ndf7_video_week1, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 7 (Week 1)") 

p2_run7 = ggplot(ndf7_video_week2, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 7 (Week 2)") 

p3_run7 = ggplot(ndf7_video_week3, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 7 (Week 3)") 

video_eng = cyber.security.7_video.stats$viewed_five_percent - cyber.security.7_video.stats$viewed_ninetyfive_percent
d7f = data.frame(step = cyber.security.7_video.stats$step_position , left = video_eng)  
d7f
d7f_plot = ggplot(d7f, aes(x = as.factor(step), y = left, fill = factor(step))) + geom_bar(stat="identity") + xlab("Step number") +
  ylab("People leaving") + ggtitle("Plot of step number against percentage of people leaving (Run 7)") + ylim(0,27)

grid.arrange(p1_run7, p2_run7, p3_run6,d7f_plot,nrow = 2, ncol=2)

# Again we observe very similar patterns as before. However in week 1, video 1.17 has higher turnover,
# when compared to the other runs

plot(cyber.security.7_video.stats$viewed_ninetyfive_percent, cyber.security.7_video.stats$video_duration,
     xlab = "% of people who watched 100% of the video", ylab = "Video duration in minutes",
     main = "Plot of video duration vs engagement (Run 7)")
abline(lm(cyber.security.7_video.stats$video_duration ~ cyber.security.7_video.stats$viewed_ninetyfive_percent),
       lty = 2 , col = "red")

cor(cyber.security.7_video.stats$viewed_ninetyfive_percent, cyber.security.7_video.stats$video_duration)
# Again very strong negative correlation (-0.8331848)


grid.arrange(d4f_plot, d5f_plot, d6f_plot, d7f_plot)

# we observe that for all runs video 1.5 (281) has the largest dropout rate among all videos in week 1 and 
# all videos in general. Then for week 2 video 2.11 has the largest dropout (312) and for week 3
#except from run 6, video 3.14 (313) is the one with the highest dropout. 
# Now the videos with the lowest dropouts among all weeks for most? runs are videos 2.1 (37) and 
# 2.17 (92)
# Also what is quite interesting is that video 2.4 has the longest duration, but it is not the one
# with the highest dropout. On the other hand other videos seem to have higher dropout even if they are
# Shorter. So this may be an area that people find interesting. 






