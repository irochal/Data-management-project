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

# CRISP DM cycle 1
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
# Create a plot of the number of enrollments for all runs
plot(number_of_enrollments, xlab = "Run number", ylab = "Number of enrollments", type = "b",
     main = "Plot of enrollments numbers over each run", cex.main = 0.8)

# Find out how many people actually completed the course in each run 
successful_run1 = (length(fp1)/nr1) *100
successful_run2 = (length(fp2)/nr2) *100
successful_run3 = (length(fp3)/nr3)*100
successful_run4 = (length(fp4)/ nr4)*100
successful_run5 = (length(fp5)/ nr5)*100
successful_run6 = (length(fp6)/ nr6) *100
successful_run7 = (length(fp7)/ nr7) *100

number_of_sucessfull_completions = c(successful_run1, successful_run2, successful_run3,
                                     successful_run4, successful_run5, successful_run6,
                                     successful_run7)

# Create a plot for percentage of people that completed each run 
plot(number_of_sucessfull_completions, xlab = "Run number", ylab = "% of people who fully participated",
     type = "b", main = "Plot of % of people who completed the course", cex.main = 0.8)

# Create barplots for the % of people of each gender for runs 4-7 (this is for all people that 
# registered for the course)
par(mfrow = c(2, 2))
barplot((table(cyber.security.4_enrolments$gender)/nr4)*100, main = "Run 4", col = "darkseagreen 2", ylab = "%")
barplot((table(cyber.security.5_enrolments$gender)/nr5)*100, main = "Run 5", col = "salmon", ylab = "%")
barplot((table(cyber.security.6_enrolments$gender)/nr6)*100, main = "Run 6", col = "violet", ylab = "%")
barplot((table(cyber.security.7_enrolments$gender)/nr7)*100, main = "Run 7", col = "yellow", ylab = "%")
# In general there is no difference in the gender of people who registered for the course

# Now let's look at the people who completed these runs 
# Create barplots for the % of people of each gender for runs 4-7 (this is only for people that 
# completed the course)
par(mfrow = c(2, 2))
barplot((table(df4_gender$gender)/nrow(df4_gender))*100, main = "Run 4", xlab = "Gender", ylab = "%", col = "darkseagreen2")
barplot((table(df5_gender$gender)/nrow(df5_gender))*100, main = "Run 5", xlab = "Gender", ylab = "%", col = "salmon")
barplot((table(df6_gender$gender)/nrow(df6_gender))*100,  ylim = c(0,200), main = "Run 6", xlab = "Gender", ylab = "%", col = "violet")
barplot((table(df7_gender$gender)/nrow(df7_gender))*100, ylim = c(0,200), main = "Run 7", xlab = "Gender", ylab = "%", col = "yellow")
# In general there is some evidence that in the runs with higher completion there were more males who completed
# the course, but since there are many unknowns this may not be representative of what happens in reality


# Create barplots for the % of people of each age range for runs 4-7 (this is for all people that 
# registered for the course)
par(mfrow = c(2, 2))
barplot((table(cyber.security.4_enrolments$age_range)/nr4)*100, main = "Run 4", col = "darkseagreen 2", ylab = "%")
barplot((table(cyber.security.5_enrolments$age_range)/nr5)*100, main = "Run 5", col = "salmon",ylab = "%")
barplot((table(cyber.security.6_enrolments$age_range)/nr6)*100, main = "Run 6", col = "violet",ylab = "%")
barplot((table(cyber.security.7_enrolments$age_range)/nr7)*100, main = "Run 7", col = "yellow",ylab = "%")


# Now for the ones that completed the course
# Create barplots for the % of people of each age range for runs 4-7 (this is only for people that 
# completed the course)
barplot((table(df4_age_all$age_range)/nrow(df4_age_all))*100, col = "darkseagreen2", main = "Run 4", ylab = "%")
barplot((table(df5_age_all$age_range)/nrow(df5_age_all))*100, col = "salmon", main = "Run 5", ylab = "%")
barplot((table(df6_age_all$age_range)/nrow(df6_age_all))*100, col = "violet", main = "Run 6", ylab = "%")
barplot((table(df7_age_all$age_range)/nrow(df7_age_all))*100, col = "yellow", main = "Run 7", ylab = "%")

# We observe in runs 4,6 and 7 more younger people to have completed. However for run 5, the only two poeple,
# who had registered age group were >65. So age may be a factor affecting completion, but again not certain results. 

# CRISP DM cycle 2 

# Let's have a better look on step activity know and how the engagement varies between the  runs 
par(mfrow = c(2, 4))

# Creating plots for the step activity for poeple who just started each step and for people who 
# finished each step, against all step numbers, for all 7 runs 
# RUN 1
plot(as.numeric(steps1), se1, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 1)")
axis(1,at = 1:60, labels = steps1)
lines(t1_complete[,2], col = "red", lty = 2)
legend("topleft", legend=c("Participated", "Completed"),
        col=c("black", "red"), lty=1:2, cex=0.8, bty = "n")

# RUN 2
plot(as.numeric(steps2), se2, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 2)")
axis(1,at = 1:63, labels = steps2)
lines(t2_complete[,2], col = "red", lty = 2)
legend("topleft", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8, bty = "n")

# RUN 3

plot(as.numeric(steps3), se3, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 3)")
axis(1,at = 1:62, labels = steps3)
lines(t3_complete[,2], col = "red", lty = 2)
legend("topleft", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8, bty = "n")

# RUN 4
plot(as.numeric(steps4), se4, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 4)")
axis(1,at = 1:62, labels = steps4)
lines(t4_complete[,2], col = "red", lty = 2)
legend("topleft", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8, bty = "n")

# RUN 5 
plot(as.numeric(steps5), se5, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 5)")
axis(1,at = 1:62, labels = steps5)
lines(t5_complete[,2], col = "red", lty = 2)
legend("topleft", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8, bty = "n")

# RUN 6
plot(as.numeric(steps6), se6, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 6)")
axis(1,at = 1:62, labels = steps6)
lines(t6_complete[,2], col = "red", lty = 2)
legend("topleft", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8, bty = "n")

# RUN 7 
plot(as.numeric(steps7), se4, type = "l", xaxt = "n", xlab = "Step number", 
     ylab = "People engaging in each step", main = "Plot of step activity against step number (Run 7)")
axis(1,at = 1:62, labels = steps7)
lines(t7_complete[,2], col = "red", lty = 2)
legend("topleft", legend=c("Participated", "Completed"),
       col=c("black", "red"), lty=1:2, cex=0.8, bty = "n")

# Creating a ggplot for people who completed each step, for all runs 
complete_step_act = ggplot(all_run_activity_step_complete, aes(x = steps, y = se, colour = factor(run), group = run ))  + geom_line() +
  xlab("Step number") + ylab("Completed Step Engagement") + ggtitle("Plot of completed step engagement over the 7 runs") 
complete_step_act

# We observe that the step activity completion  for run 1 is the highest. When compared to the other 6 runs
# we observe that the engagement declines much  for this run. We observe 
# that run 4 had very similar engagement rate to that of run 5, but higher completion rate.
# In general we observe a declining trend in engagement. 

# Let's look if quiz length has an effect in engagement 
# RUN 1

par(mfrow = c(2, 4))

# Create a vector that contains the numebr of questions in each quiz/test and e vector that contains
# the percentage of people who did not finish it for all runs
# Then make plots for question length against percentage of people left and find the correlation
# ( this is done for runs 4-7)
question_length_1 = c(6,3,1,3,9)
people_left_1 = c(quiz_1.8_1_left,quiz_2.8_1_left ,quiz_2.20_1_left, quiz_3.11_1_left, test_1_left)
cor(as.numeric(question_length_1), people_left_1)
plot(as.numeric(question_length_1), people_left_1, xlab = "Quiz/Test number of steps", ylab = "% of people not completing",
     main = "Run 1")
abline(lm(people_left_1~question_length_1), col = "red", lty = 2)
text(2, 17, expression(r == 0.9808962))

# RUN 2 
question_length_2 = c(6,3,1,3,9)
people_left_2 = c(quiz_1.8_2_left,quiz_2.8_2_left ,quiz_2.20_2_left, quiz_3.11_2_left, test_2_left)
cor(as.numeric(question_length_2), people_left_2)
plot(as.numeric(question_length_2), people_left_2, xlab = "Quiz/Test number of steps", ylab = "% of people not completing",
     main = "Run 2")
abline(lm(people_left_2~question_length_2), col = "red", lty = 2)
text(2, 11.5, expression(r == 0.09521637))

# RUN 3
question_length_3 = c(6,3,1,3,9)
people_left_3 = c(quiz_1.8_3_left,quiz_2.8_3_left ,quiz_2.20_3_left, quiz_3.11_3_left, test_3_left)
cor(as.numeric(question_length_3), people_left_3)
plot(as.numeric(question_length_3), people_left_3, xlab = "Quiz/Test number of steps", ylab = "% of people not completing",
     main = "Run 3")
abline(lm(people_left_3~question_length_3), col = "red", lty = 2)
text(2, 13.7, expression(r == 0.5491551))

# RUN 4 
question_length_4 = c(6,3,1,3,9)
people_left_4 = c(quiz_1.8_4_left,quiz_2.8_4_left ,quiz_2.20_4_left, quiz_3.11_4_left, test_4_left)
cor(as.numeric(question_length_4), people_left_4)
plot(as.numeric(question_length_4), people_left_4, xlab = "Quiz/Test number of steps", ylab = "% of people not completing",
     main = "Run 4")
abline(lm(people_left_4~question_length_4), col = "red", lty = 2)
text(2, 15, expression(r == 0.5727359))

# RUN 5 
question_length_5 = c(6,3,1,3,9)
people_left_5 = c(quiz_1.8_5_left,quiz_2.8_5_left ,quiz_2.20_5_left, quiz_3.11_5_left, test_5_left)
cor(as.numeric(question_length_5), people_left_5)
dfqp1 = data.frame(question_length_5, people_left_5)
plot(as.numeric(question_length_5), people_left_5, xlab = "Quiz/Test number of steps", ylab = "% of people not completing",
     main = "Run 5")
abline(lm(people_left_5~question_length_5), col = "red", lty = 2)
text(2, 14, expression(r == 0.4587964))


# RUN 6 
question_length_6 = c(6,3,1,3,9)
people_left_6 = c(quiz_1.8_6_left,quiz_2.8_6_left ,quiz_2.20_6_left, quiz_3.11_6_left, test_6_left)
cor(as.numeric(question_length_6), people_left_6)
plot(as.numeric(question_length_6), people_left_6, xlab = "Quiz/Test number of steps", ylab = "% of people not completing",
     main = "Run 6")
abline(lm(people_left_6~question_length_6), col = "red", lty = 2)
text(2, 18.7, expression(r == 0.8231372))

# RUN 7 
question_length_7 = c(6,3,1,3,9)
people_left_7 = c(quiz_1.8_7_left,quiz_2.8_7_left ,quiz_2.20_7_left, quiz_3.11_7_left, test_7_left)
cor(as.numeric(question_length_7), people_left_7)
plot(as.numeric(question_length_7), people_left_7, xlab = "Quiz/Test number of steps", ylab = "% of people not completing",
     main = "Run 7")
abline(lm(people_left_7~question_length_7), col = "red", lty = 2)
text(2, 18, expression(r == 0.5011749))

# In general we observe that there is a moderate negative correlation between length of the quiz 
# and completion for all runs, except run 2 


# CRISP DM cycle 3
# Make plots for video engagement for runs 4-7 
# RUN 4 
p1_run4 = ggplot(ndf4_video_week1, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 4 (Week 1)") 

p2_run4 = ggplot(ndf4_video_week2, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 4 (Week 2)") 

p3_run4 = ggplot(ndf4_video_week3, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 4 (Week 3)") 


# Here lets see which videos had the biggest turnover
# Make barplot for the percentage of people that left before watching up to 95% of the video. Each bar
#represents a video
d4f_plot = ggplot(d4f, aes(x = as.factor(step), y = left, fill = factor(step))) + geom_bar(stat="identity") + xlab("Step number") +
  ylab("% of people leaving") + ggtitle("Plot of step number against percentage of people leaving (Run 4)") + ylim(0,27)

grid.arrange(p1_run4, p2_run4, p3_run4, nrow = 1, ncol=3)
# By looking at the plot we see that all video have a very large decline from 95% to 100%.
# So it is better to draw conclusions by looking up to 95%, and there are some video which have 
# larger decline when compared to others

# Create a data frame that containg the duration of each video 
video_duration_4 = data.frame(step_position = cyber.security.4_video.stats$step_position,
                              duration = cyber.security.4_video.stats$video_duration)
video_duration_4

# Now let's look if there is a relationship between duration and engagement here as well
# Make plot of video number against lentgh of the video and find the correlation 
plot(cyber.security.4_video.stats$viewed_ninetyfive_percent, cyber.security.4_video.stats$video_duration,
     xlab = "% of people who watched 95% of the video", ylab = "Video duration in minutes",
     main = "Plot of video duration vs engagement (Run 4)")
abline(lm(cyber.security.4_video.stats$video_duration ~ cyber.security.4_video.stats$viewed_ninetyfive_percent),
       lty = 2 , col = "red")
text(65, 400, expression(r == -0.817063))

cor(cyber.security.4_video.stats$viewed_ninetyfive_percent, cyber.security.4_video.stats$video_duration)

# Here we see a very strong negative correlation between engagement and duration (-0.817063)
# From the plot above we see that in general the longer the videos the less the 100% engagement


# RUN 5 
p1_run5 = ggplot(ndf5_video_week1, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 5 (Week 1)") 

p2_run5 = ggplot(ndf5_video_week2, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 5 (Week 2)") 

p3_run5 = ggplot(ndf5_video_week3, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 5 (Week 3)") 

d5f_plot = ggplot(d5f, aes(x = as.factor(step), y = left, fill = factor(step))) + geom_bar(stat="identity") + xlab("Step number") +
  ylab("% of people leaving") + ggtitle("Plot of step number against percentage of people leaving (Run 5)") + ylim(0,27)

grid.arrange(p1_run5, p2_run5, p3_run5,nrow = 3, ncol=3)

# Again we observe very similar patterns as before. 

plot(cyber.security.5_video.stats$viewed_ninetyfive_percent, cyber.security.5_video.stats$video_duration,
     xlab = "% of people who watched 95% of the video", ylab = "Video duration in minutes",
     main = "Plot of video duration vs engagement (Run 5)")
abline(lm(cyber.security.5_video.stats$video_duration ~ cyber.security.5_video.stats$viewed_ninetyfive_percent),
       lty = 2 , col = "red")
text(65, 400, expression(r == -0.6827021))

cor(cyber.security.5_video.stats$viewed_ninetyfive_percent, cyber.security.5_video.stats$video_duration)
# Here we observe less correlation between duration and engagement. Could this mean that younger people
# have longer attention span and do engage more with videos of longer duration? 

# RUN 6
p1_run6 = ggplot(ndf6_video_week1, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 6 (Week 1)") 

p2_run6 = ggplot(ndf6_video_week2, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 6 (Week 2)") 

p3_run6 = ggplot(ndf6_video_week3, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 6 (Week 3)") 


d6f_plot = ggplot(d6f, aes(x = as.factor(step), y = left, fill = factor(step))) + geom_bar(stat="identity") + xlab("Step number") +
  ylab("% of people leaving") + ggtitle("Plot of step number against percentage of people leaving (Run 6)") + ylim(0,27)

grid.arrange(p1_run6, p2_run6, p3_run6,nrow = 1, ncol=3)

# Again we observe very similar patterns as before. 

plot(cyber.security.6_video.stats$viewed_ninetyfive_percent, cyber.security.6_video.stats$video_duration,
     xlab = "% of people who watched 95% of the video", ylab = "Video duration in minutes",
     main = "Plot of video duration vs engagement (Run 6)")
abline(lm(cyber.security.6_video.stats$video_duration ~ cyber.security.6_video.stats$viewed_ninetyfive_percent),
       lty = 2 , col = "red")
text(65, 400, expression(r == -0.7175826))

cor(cyber.security.6_video.stats$viewed_ninetyfive_percent, cyber.security.6_video.stats$video_duration)

# Again here there is a strong negative correlation between engagement and duration (-0.7175826)

# RUN 7 
p1_run7 = ggplot(ndf7_video_week1, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 7 (Week 1)") 

p2_run7 = ggplot(ndf7_video_week2, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 7 (Week 2)") 

p3_run7 = ggplot(ndf7_video_week3, aes(x = as.numeric(name), y = value, colour = factor(`Step position`), group =`Step position` ))  + geom_line() +
  xlab("% viewed") + ylab("% of people who viewed the video") + ggtitle("Plot of video engagement over run 7 (Week 3)") 


d7f_plot = ggplot(d7f, aes(x = as.factor(step), y = left, fill = factor(step))) + geom_bar(stat="identity") + xlab("Step number") +
  ylab("% of people leaving") + ggtitle("Plot of step number against percentage of people leaving (Run 7)") + ylim(0,27)

grid.arrange(p1_run7, p2_run7, p3_run6,nrow = 1, ncol=3)

# Again we observe very similar patterns as before. 

plot(cyber.security.7_video.stats$viewed_ninetyfive_percent, cyber.security.7_video.stats$video_duration,
     xlab = "% of people who watched 95% of the video", ylab = "Video duration in minutes",
     main = "Plot of video duration vs engagement (Run 7)")
abline(lm(cyber.security.7_video.stats$video_duration ~ cyber.security.7_video.stats$viewed_ninetyfive_percent),
       lty = 2 , col = "red")
text(65, 400, expression(r == -0.8331848))


cor(cyber.security.7_video.stats$viewed_ninetyfive_percent, cyber.security.7_video.stats$video_duration)
# Again very strong negative correlation (-0.8331848)

# Create a plot that contains all plots from runs 4-7 for the percentage of people leaving each video
# before watching up to 95%
grid.arrange(d4f_plot, d5f_plot, d6f_plot, d7f_plot)

# we observe that for all runs video 1.5 (281) has the largest dropout rate among all videos in week 1 and 
# all videos in general. Then for week 2 video 2.11 has the largest dropout (312) and for week 3
#except from run 6, video 3.14 (313) is the one with the highest dropout. 
# Now the videos with the lowest dropouts among all weeks for most? runs are videos 2.1 (37) and 
# 2.17 (92)
# Also what is quite interesting is that video 2.4 has the longest duration, but it is not the one
# with the highest dropout. On the other hand other videos seem to have higher dropout even if they are
# Shorter. So this may be an area that people find interesting. 






