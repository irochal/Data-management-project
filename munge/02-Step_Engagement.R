# Example preprocessing script.
# Change the step numbering in order to have the correct format and outcome 
new_step_1 = (cyber.security.1_step.activity$week_number)*100 + cyber.security.1_step.activity$step_number
cyber.security.1_step.activity_1 = cbind(cyber.security.1_step.activity, new_step_1)

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



# Create a combined data frames 

# RUN 1
t1 = table(cyber.security.1_step.activity_1$new_step_1)
dft1 = as.data.frame(t1)
se1 = dft1[,2]
steps1 = dft1[,1]

ddf1 = data.frame(steps1, se1)
t1_complete = as.data.frame(table(cyber.security.1_step.activity_1$new_step_1[cyber.security.1_step.activity_1$last_completed_at != ""]))
# RUN 2
t2 = table(cyber.security.2_step.activity_2$new_step_2)
dft2 = as.data.frame(t2)
se2 = dft2[,2]
steps2 = dft2[,1]

ddf2 = data.frame(steps2, se2)
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


# Assign week numebr to each step for every run 
run1 = rep(1, times = length(t1))

run2 = rep(2, times = length(t2))

run3 = rep(3, times = length(t3))

run4 = rep(4, times = length(t4))

run5 = rep(5, times = length(t5))

run6 = rep(6, times = length(t6))

run7 = rep(7, times = length(t7))

# Create data frame that contains the people who completed each step for all the runs
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


# Find the percentage of people that engagged in quizes and test but did not complete, by subtracting
# the number of people that finished the quiz from the number of people that started the quiz 
# for all runs 
# RUN 1 
dft1 = as.data.frame(t1)
dft1comp = as.data.frame(t1_complete)
quiz_1.8_1_left = ((dft1[8,2] - dft1comp[8,2])/dft1[8,2])*100

quiz_2.8_1_left = ((dft1[26,2] - dft1comp[26,2])/dft1[26,2])*100

quiz_2.20_1_left = ((dft1[38,2] - dft1comp[38,2])/dft1[38,2])*100

quiz_3.11_1_left = ((dft1[50,2] - dft1comp[50,2])/dft1[50,2])*100

test_1_left = ((dft1[57,2] - dft1comp[57,2])/dft1[57,2])*100

# RUN 2 
dft2 = as.data.frame(t2)
dft2comp = as.data.frame(t2_complete)
quiz_1.8_2_left = ((dft2[8,2] - dft2comp[8,2])/dft2[8,2])*100

quiz_2.8_2_left = ((dft2[27,2] - dft2comp[27,2])/dft2[27,2])*100

quiz_2.20_2_left = ((dft2[39,2] - dft2comp[39,2])/dft2[39,2])*100

quiz_3.11_2_left = ((dft2[53,2] - dft2comp[53,2])/dft2[53,2])*100

test_2_left = ((dft2[60,2] - dft2comp[60,2])/dft2[60,2])*100

# RUN 3 
dft3 = as.data.frame(t3)
dft3comp = as.data.frame(t3_complete)
quiz_1.8_3_left = ((dft3[8,2] - dft3comp[8,2])/dft3[8,2])*100

quiz_2.8_3_left = ((dft3[27,2] - dft3comp[27,2])/dft3[27,2])*100

quiz_2.20_3_left = ((dft3[39,2] - dft3comp[39,2])/dft3[39,2])*100

quiz_3.11_3_left = ((dft3[53,2] - dft3comp[53,2])/dft3[53,2])*100

test_3_left = ((dft3[60,2] - dft3comp[60,2])/dft3[60,2])*100

# RUN 4
dft4 = as.data.frame(t4)
dft4comp = as.data.frame(t4_complete)
quiz_1.8_4_left = ((dft4[8,2] - dft4comp[8,2])/dft4[8,2])*100

quiz_2.8_4_left = ((dft4[27,2] - dft4comp[27,2])/dft4[27,2])*100

quiz_2.20_4_left = ((dft4[39,2] - dft4comp[39,2])/dft4[39,2])*100

quiz_3.11_4_left = ((dft4[53,2] - dft4comp[53,2])/dft4[53,2])*100

test_4_left = ((dft4[60,2] - dft4comp[60,2])/dft4[60,2])*100

# RUN 5
dft5 = as.data.frame(t5)
dft5comp = as.data.frame(t5_complete)
quiz_1.8_5_left = ((dft5[8,2] - dft5comp[8,2])/dft5[8,2])*100

quiz_2.8_5_left = ((dft5[27,2] - dft5comp[27,2])/dft5[27,2])*100

quiz_2.20_5_left = ((dft5[39,2] - dft5comp[39,2])/dft5[39,2])*100

quiz_3.11_5_left = ((dft5[53,2] - dft5comp[53,2])/dft5[53,2])*100

test_5_left = ((dft5[60,2] - dft5comp[60,2])/dft5[60,2])*100

# RUN 6 
dft6 = as.data.frame(t6)
dft6comp = as.data.frame(t6_complete)
quiz_1.8_6_left = ((dft6[8,2] - dft6comp[8,2])/dft6[8,2])*100

quiz_2.8_6_left = ((dft6[27,2] - dft6comp[27,2])/dft6[27,2])*100

quiz_2.20_6_left = ((dft6[39,2] - dft6comp[39,2])/dft6[39,2])*100

quiz_3.11_6_left = ((dft6[53,2] - dft6comp[53,2])/dft6[53,2])*100

test_6_left = ((dft6[60,2] - dft6comp[60,2])/dft6[60,2])*100

# RUN 7 
dft7 = as.data.frame(t7)
dft7comp = as.data.frame(t7_complete)
quiz_1.8_7_left = ((dft7[8,2] - dft7comp[8,2])/dft7[8,2])*100

quiz_2.8_7_left = ((dft7[27,2] - dft7comp[27,2])/dft7[27,2])*100

quiz_2.20_7_left = ((dft7[39,2] - dft7comp[39,2])/dft7[39,2])*100

quiz_3.11_7_left = ((dft7[53,2] - dft7comp[53,2])/dft7[53,2])*100

test_7_left = ((dft7[60,2] - dft7comp[60,2])/dft7[60,2])*100





















