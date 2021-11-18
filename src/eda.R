library(ProjectTemplate)
setwd("/Volumes/KINGSTON")
create.project("Data Management and EDA project")
setwd("Data Management and EDA project")

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

library(readr)
cyber_security_2_enrolments <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-2_enrolments.csv")
head(cyber_security_2_enrolments)
nr2 = nrow(cyber_security_2_enrolments)

library(readr)
cyber_security_3_enrolments <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-3_enrolments.csv")
head(cyber_security_3_enrolments)
nr3 = nrow(cyber_security_3_enrolments)

library(readr)
cyber_security_4_enrolments <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-4_enrolments.csv")
head(cyber_security_4_enrolments)
nr4 = nrow(cyber_security_4_enrolments)

library(readr)
cyber_security_5_enrolments <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-5_enrolments.csv")
head(cyber_security_5_enrolments)
nr5 = nrow(cyber_security_5_enrolments)

library(readr)
cyber_security_6_enrolments <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-6_enrolments.csv")
head(cyber_security_6_enrolments)
nr6 = nrow(cyber_security_6_enrolments)

library(readr)
cyber_security_7_enrolments <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-7_enrolments.csv")
head(cyber_security_7_enrolments)
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

library(readr)
cyber_security_1_step_activity <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-1_step-activity.csv")
head(cyber_security_1_step_activity)
new_step_1 = (cyber_security_1_step_activity$week_number)*100 + cyber_security_1_step_activity$step_number
cyber_security_1_step_activity_1 = cbind(cyber_security_1_step_activity, new_step_1)
 
 
library(readr)
cyber_security_2_step_activity <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-2_step-activity.csv")
head(cyber_security_2_step_activity)
new_step_2 = (cyber_security_2_step_activity$week_number)*100 + cyber_security_2_step_activity$step_number
cyber_security_2_step_activity_2 = cbind(cyber_security_2_step_activity, new_step_2)


library(readr)
cyber_security_3_step_activity <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-3_step-activity.csv")
head(cyber_security_3_step_activity)
new_step_3 = (cyber_security_3_step_activity$week_number)*100 + cyber_security_3_step_activity$step_number
cyber_security_3_step_activity_3 = cbind(cyber_security_3_step_activity, new_step_3)


library(readr)
cyber_security_4_step_activity <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-4_step-activity.csv")
head(cyber_security_4_step_activity)
new_step_4 = (cyber_security_4_step_activity$week_number)*100 + cyber_security_4_step_activity$step_number
cyber_security_4_step_activity_4 = cbind(cyber_security_4_step_activity, new_step_4)


library(readr)
cyber_security_5_step_activity <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-5_step-activity.csv")
head(cyber_security_5_step_activity)
new_step_5 = (cyber_security_5_step_activity$week_number)*100 + cyber_security_5_step_activity$step_number
cyber_security_5_step_activity_5 = cbind(cyber_security_5_step_activity, new_step_5)


library(readr)
cyber_security_6_step_activity <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-6_step-activity.csv")
head(cyber_security_6_step_activity)
new_step_6 = (cyber_security_6_step_activity$week_number)*100 + cyber_security_6_step_activity$step_number
cyber_security_6_step_activity_6 = cbind(cyber_security_6_step_activity, new_step_6)


library(readr)
cyber_security_7_step_activity <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-7_step-activity.csv")
head(cyber_security_7_step_activity)
new_step_7 = (cyber_security_7_step_activity$week_number)*100 + cyber_security_7_step_activity$step_number
cyber_security_7_step_activity_7 = cbind(cyber_security_7_step_activity, new_step_7)


t1 = table(cyber_security_1_step_activity_1$new_step_1)
dft1 = as.data.frame(t1)
se1 = dft1[,2]
steps1 = dft1[,1]

plot(steps1,se1, xlab = "Step number", ylab = "People engaging in each step", 
     main = "Plot of step activity against step number")
text(2.1, 4750, labels= "2.1")
points(2.1, 4489, col = "red", pch = 19)
text(2.23, 4297, labels = "2.2")
points(2.2, 4297, col = "blue", pch = 19)
abline(lm(se1~steps1), col = "red", lty = 2)

ddf1 = data.frame(steps1, se1)
ggplot(ddf1, aes(x = steps1, y = se1)) + geom_point() + geom_smooth() + xlab("Step number") +
  ylab("People engaging in each step") + ggtitle("Plot of step activity against step number in run 1")


# This plots shows a very clear declining trend in students engagement, as the weeks progress. We see
# some points that standout. These are 1.18, 3.18 and 3.21. Now 1.18 is a video, 3.18 is the test and 
# 3.21 is the glossary and referebces. So we see that in general people have interacted with the glosary
# so this may be an important section of the course that needs to stay in. Also for this specific run, it 
#seems that the engagement for the test was unexpectedly high. 

# Now let's check if the same trends follows in all of the runs 

t2 = table(cyber_security_2_step_activity_2$new_step_2)
dft2 = as.data.frame(t2)
se2 = dft2[,2]
steps2 = dft2[,1]

ddf2 = data.frame(steps2, se2)
ggplot(ddf2, aes(x = steps2, y = se2)) + geom_point() + geom_smooth(col = "red") + xlab("Step number") +
  ylab("People engaging in each step") + ggtitle("Plot of step activity against step number in run 2") 

# We observe the same declining trend here. Moreover 3.21 seems to be a section that has more engagement
#Also the engagement for 1.19 seems to be a bit higher. We note here that 1.19 is a video, so again we 
# have some evidence that the videos may have more engagement. Finally, for this particular run, the test
# seems to have an extremelly low engagement, which is expected since this course had the lower number
# of completion. So this low completion might no mean that the participants failed the test, but most 
#probably it means that the did not even engage with the test. 
  
t3 = table(cyber_security_3_step_activity_3$new_step_3)
dft3 = as.data.frame(t3)
se3= dft3[,2]
steps3 = dft3[,1]

ddf3 = data.frame(steps3, se3)
ggplot(ddf3, aes(x = steps3, y = se3)) + geom_point() + geom_smooth(col = "green") + xlab("Step number") +
  ylab("People engaging in each step") + ggtitle("Plot of step activity against step number in run 3") 


# Again we observe a declining trend, with the same outliers 

t4 = table(cyber_security_4_step_activity_4$new_step_4)
dft4 = as.data.frame(t4)
se4 = dft4[,2]
steps4 = dft4[,1]
coords = paste(steps4,se4,sep=",")

ddf4 = data.frame(steps4, se4)
ggplot(ddf4, aes(x = steps4, y = se4)) + geom_point() + geom_smooth(col = "salmon") + xlab("Step number") +
  ylab("People engaging in each step") + ggtitle("Plot of step activity against step number in run 4")


# Again we observe a declining trend, with the same outliers 

t5 = table(cyber_security_5_step_activity_5$new_step_5)
dft5 = as.data.frame(t5)
se5 = dft5[,2]
steps5 = dft5[,1]

ddf5 = data.frame(steps5, se5)
ggplot(ddf5, aes(x = steps5, y = se5)) + geom_point() + geom_smooth(col = "slateblue1") + xlab("Step number") +
  ylab("People engaging in each step") + ggtitle("Plot of step activity against step number in run 5") 

# Again we observe the same declining trend. Again we have the same outliers, but here for steps
#starting in week 2 we see a stabilisation in engagement compared to steps in week 3


t6 = table(cyber_security_6_step_activity_6$new_step_6)
dft6 = as.data.frame(t6)
se6 = dft6[,2]
steps6 = dft6[,1]

ddf6 = data.frame(steps6, se6)
ggplot(ddf6, aes(x = steps6, y = se6)) + geom_point() + geom_smooth(col = "yellow") + xlab("Step number") +
  ylab("People engaging in each step") + ggtitle("Plot of step activity against step number in run 6") 

# Again soe declining trend. However quite stabilised. 
#We have the same outliers 

t7 = table(cyber_security_7_step_activity_7$new_step_7)
dft7 = as.data.frame(t7)
se7 = dft7[,2]
steps7 = dft7[,1]

ddf7 = data.frame(steps7, se7)
ggplot(ddf7, aes(x = steps7, y = se7)) + geom_point() + geom_smooth(col = "violet") + xlab("Step number") +
  ylab("People engaging in each step") + ggtitle("Plot of step activity against step number in run 7") 


# Again we observe the same trend


# Also what we observe in all runs is that for sections 1.2-1.9 there seems to be in general a 
# much higher engagement, so this could be a section that in general the participants find more 
# interesting throughout


ggplot(all_run_activity_step, aes(x = steps, y = se, colour = factor(run), group = run ))  + geom_smooth() +
  xlab("Step number") + ylab("Step Engagement") + ggtitle("Plot of step engagement over the 7 runs") 

# We observe that the step activity for run 1 is the highest. This can be explained by the fact
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









