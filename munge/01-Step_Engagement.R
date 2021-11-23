# Example preprocessing script.
# Change the step numbering in order to have the correct format and outcome 
cyber_security_1_step_activity <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-1_step-activity.csv")
head(cyber_security_1_step_activity)
new_step_1 = (cyber_security_1_step_activity$week_number)*100 + cyber_security_1_step_activity$step_number
cyber_security_1_step_activity_1 = cbind(cyber_security_1_step_activity, new_step_1)
view(cyber_security_1_step_activity_1)

# Do the same thing for each run 
# RUN 2
cyber_security_2_step_activity <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-2_step-activity.csv")
head(cyber_security_2_step_activity)
new_step_2 = (cyber_security_2_step_activity$week_number)*100 + cyber_security_2_step_activity$step_number
cyber_security_2_step_activity_2 = cbind(cyber_security_2_step_activity, new_step_2)

# RUN 3
cyber_security_3_step_activity <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-3_step-activity.csv")
head(cyber_security_3_step_activity)
new_step_3 = (cyber_security_3_step_activity$week_number)*100 + cyber_security_3_step_activity$step_number
cyber_security_3_step_activity_3 = cbind(cyber_security_3_step_activity, new_step_3)

#RUN 4 
cyber_security_4_step_activity <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-4_step-activity.csv")
head(cyber_security_4_step_activity)
new_step_4 = (cyber_security_4_step_activity$week_number)*100 + cyber_security_4_step_activity$step_number
cyber_security_4_step_activity_4 = cbind(cyber_security_4_step_activity, new_step_4)

#RUN 5
cyber_security_5_step_activity <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-5_step-activity.csv")
head(cyber_security_5_step_activity)
new_step_5 = (cyber_security_5_step_activity$week_number)*100 + cyber_security_5_step_activity$step_number
cyber_security_5_step_activity_5 = cbind(cyber_security_5_step_activity, new_step_5)

#RUN 6
cyber_security_6_step_activity <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-6_step-activity.csv")
head(cyber_security_6_step_activity)
new_step_6 = (cyber_security_6_step_activity$week_number)*100 + cyber_security_6_step_activity$step_number
cyber_security_6_step_activity_6 = cbind(cyber_security_6_step_activity, new_step_6)

# RUN 7
cyber_security_7_step_activity <- read_csv("data/FutureLearn MOOC Dataset (1)/cyber-security-7_step-activity.csv")
head(cyber_security_7_step_activity)
new_step_7 = (cyber_security_7_step_activity$week_number)*100 + cyber_security_7_step_activity$step_number
cyber_security_7_step_activity_7 = cbind(cyber_security_7_step_activity, new_step_7)



# Create a combined data frame 
run1 = rep(1, times = length(t1))
ndf1 = as.data.frame(cbind(steps = steps1, se = se1, run = run1))


run2 = rep(2, times = length(t2))
ndf2 = as.data.frame(cbind(steps = steps2, se = se2, run = run2))


run3 = rep(3, times = length(t3))
ndf3 = as.data.frame(cbind(steps = steps3, se = se3, run = run3))


run4 = rep(4, times = length(t4))
ndf4 = as.data.frame(cbind(steps = steps4, se = se4, run = run4))

run5 = rep(5, times = length(t5))
ndf5 = as.data.frame(cbind(steps = steps5, se = se5, run = run5))

run6 = rep(6, times = length(t6))
ndf6 = as.data.frame(cbind(steps = steps6, se = se6, run = run6))

run7 = rep(7, times = length(t7))
ndf7 = as.data.frame(cbind(steps = steps7, se = se7, run = run7))

library(dplyr)
library(tidyverse)

all_run_activity_step = do.call(rbind, list(ndf1, ndf2, ndf3, ndf4, ndf5, ndf6, ndf7))
class(all_run_activity_step)
all_run_activity_step



# I DO NOT KNOW IF I NEED THAT 
# Create all activity step for the competed steps 
run1_complete = rep(1, times = length(t1_complete))
ndf1_complete = as.data.frame(cbind(steps = steps1, se = t1_complete[,2], run = run1))

run2_complete = rep(1, times = length(t2_complete))
ndf2_complete = as.data.frame(cbind(steps = steps2, se = t2_complete[,2], run = run2))

run3_complete = rep(1, times = length(t3_complete))
ndf3_complete = as.data.frame(cbind(steps = steps3, se = t3_complete[,2], run = run3))

run4_complete = rep(1, times = length(t4_complete))
ndf4_complete = as.data.frame(cbind(steps = steps4, se = t4_complete[,2], run = run4))

run5_complete = rep(1, times = length(t5_complete))
ndf5_complete = as.data.frame(cbind(steps = steps5, se = t5_complete[,2], run = run5))

run6_complete = rep(1, times = length(t6_complete))
ndf6_complete = as.data.frame(cbind(steps = steps6, se = t6_complete[,2], run = run6))

run7_complete = rep(1, times = length(t7_complete))
ndf7_complete = as.data.frame(cbind(steps = steps7, se = t7_complete[,2], run = run7))

all_run_activity_step_complete = do.call(rbind, list(ndf1_complete, ndf2_complete, ndf3_complete, 
            ndf4_complete, ndf5_complete, ndf6_complete, ndf7_complete))
class(all_run_activity_step_complete)
all_run_activity_step_complete




