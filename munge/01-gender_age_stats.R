# Extracting the fully participated columns for each run and removing the empty rows
# This is done in order to find the number of people who completed the course 
fp1 = cyber.security.1_enrolments$fully_participated_at[cyber.security.1_enrolments$fully_participated_at != ""]

fp2 = cyber.security.2_enrolments$fully_participated_at[cyber.security.2_enrolments$fully_participated_at != ""]

fp3 = cyber.security.3_enrolments$fully_participated_at[cyber.security.3_enrolments$fully_participated_at != ""]

fp4 = cyber.security.4_enrolments$fully_participated_at[cyber.security.4_enrolments$fully_participated_at != ""]

fp5 = cyber.security.5_enrolments$fully_participated_at[cyber.security.5_enrolments$fully_participated_at != ""]

fp6 = cyber.security.6_enrolments$fully_participated_at[cyber.security.6_enrolments$fully_participated_at != ""]

fp7 = cyber.security.7_enrolments$fully_participated_at[cyber.security.7_enrolments$fully_participated_at != ""]

# In order to get the gender statistics for the people that completed the course, we just extract 
#these three columns and then create new data frames where all the missing entries are removed
df4 = cyber.security.4_enrolments[, c("learner_id", "fully_participated_at", "gender")]
df4_gender = df4[apply(df4 != "", 1, all),]

df5 = cyber.security.5_enrolments[, c("learner_id", "fully_participated_at", "gender")]
df5_gender = df5[apply(df5 != "", 1, all),]

df1_6 = cyber.security.6_enrolments[, c("learner_id", "fully_participated_at", "gender")]
df6_gender = df1_6[apply(df1_6 != "", 1, all),]


df1_7 = cyber.security.7_enrolments[, c("learner_id", "fully_participated_at", "gender")]
df7_gender = df1_7[apply(df1_7 != "", 1, all),]

# In order to get the age range statistics for the people that completed the course, we just extract 
#these three columns and then create new data frames where all the missing entries are removed
df4_age = cyber.security.4_enrolments[, c("learner_id", "fully_participated_at", "age_range")]
df4_age_all = df4_age[apply(df4_age != "", 1, all),]

df5_age = cyber.security.5_enrolments[, c("learner_id", "fully_participated_at", "age_range")]
df5_age_all = df5_age[apply(df5_age != "", 1, all),]

df6_age = cyber.security.6_enrolments[, c("learner_id", "fully_participated_at", "age_range")]
df6_age_all = df6_age[apply(df6_age != "", 1, all),]

df7_age = cyber.security.7_enrolments[, c("learner_id", "fully_participated_at", "age_range")]
df7_age_all = df7_age[apply(df7_age != "", 1, all),]


