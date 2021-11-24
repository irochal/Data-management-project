# Example preprocessing script.
# Change the step numbering in order to have the correct format and outcome 
new_step_1 = (cyber.security.1_step.activity$week_number)*100 + cyber.security.1_step.activity$step_number
cyber.security.1_step.activity_1 = cbind(cyber.security.1_step.activity, new_step_1)
view(cyber.security.1_step.activity_1)

# Do the same thing for each run 
# RUN 2
new_step_2 = (cyber.security.2_step.activity$week_number)*100 + cyber.security.2_step.activity$step_number
cyber.security.2_step.activity_2 = cbind(cyber.security.2_step.activity, new_step_2) 

# RUN 3
new_step_3 = (cyber.security.3_step.activity$week_number)*100 + cyber.security.3_step.activity$step_number
cyber.security.3_step.activity_3 = cbind(cyber.security.3_step.activity, new_step_3)

#RUN 4 
new_step_4 = (cyber.security.4_step.activity$week_number)*100 + cyber.security.4_step.activity$step_number
cyber.security.4_step.activity_4 = cbind(cyber.security.4_step.activity, new_step_4)

#RUN 5
new_step_5 = (cyber.security.5_step.activity$week_number)*100 + cyber.security.5_step.activity$step_number
cyber.security.5_step.activity_5 = cbind(cyber.security.5_step.activity, new_step_5)

#RUN 6
new_step_6 = (cyber.security.6_step.activity$week_number)*100 + cyber.security.6_step.activity$step_number
cyber.security.6_step.activity_6 = cbind(cyber.security.6_step.activity, new_step_6)

# RUN 7
new_step_7 = (cyber.security.7_step.activity$week_number)*100 + cyber.security.7_step.activity$step_number
cyber.security.7_step.activity_7 = cbind(cyber.security.7_step.activity, new_step_7)



# Create a combined data frame 
# 
# RUN 1
t1 = table(cyber.security.1_step.activity_1$new_step_1)
dft1 = as.data.frame(t1)
se1 = dft1[,2]
steps1 = dft1[,1]

t1_complete = as.data.frame(table(cyber.security.1_step.activity_1$new_step_1[cyber.security.1_step.activity_1$last_completed_at != ""]))
# RUN 2
t2 = table(cyber.security.2_step.activity_2$new_step_2)
dft2 = as.data.frame(t2)
se2 = dft2[,2]
steps2 = dft2[,1]

t2_complete = as.data.frame(table(cyber.security.2_step.activity_2$new_step_2[cyber.security.2_step.activity_2$last_completed_at != ""]))

# RUN 3
t3 = table(cyber.security.3_step.activity_3$new_step_3)
dft3 = as.data.frame(t3)
se3= dft3[,2]
steps3 = dft3[,1]

ddf3 = data.frame(steps3, se3)
t3_complete = as.data.frame(table(cyber.security.3_step.activity_3$new_step_3[cyber.security.3_step.activity_3$last_completed_at != ""]))

# RUN 4
t4 = table(cyber.security.4_step.activity_4$new_step_4)
dft4 = as.data.frame(t4)
se4 = dft4[,2]
steps4 = dft4[,1]
coords = paste(steps4,se4,sep=",")

ddf4 = data.frame(steps4, se4)
t4_complete = as.data.frame(table(cyber.security.4_step.activity_4$new_step_4[cyber.security.4_step.activity_4$last_completed_at != ""]))

# RUN 5
t5 = table(cyber.security.5_step.activity_5$new_step_5)
dft5 = as.data.frame(t5)
se5 = dft5[,2]
steps5 = dft5[,1]
ddf5 = data.frame(steps5, se5)

t5_complete = as.data.frame(table(cyber.security.5_step.activity_5$new_step_5[cyber.security.5_step.activity_5$last_completed_at != ""]))

# RUN 6 
t6 = table(cyber.security.6_step.activity_6$new_step_6)
dft6 = as.data.frame(t6)
se6 = dft6[,2]
steps6 = dft6[,1]

ddf6 = data.frame(steps6, se6)
t6_complete = as.data.frame(table(cyber.security.6_step.activity_6$new_step_6[cyber.security.6_step.activity_6$last_completed_at != ""]))

# RUN 7 
t7 = table(cyber.security.7_step.activity_7$new_step_7)
dft7 = as.data.frame(t7)
se7 = dft7[,2]
steps7 = dft7[,1]

ddf7 = data.frame(steps7, se7)
t7_complete = as.data.frame(table(cyber.security.7_step.activity_7$new_step_7[cyber.security.7_step.activity_7$last_completed_at != ""]))



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




