# PISA2012_FL_part5
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Fryer & Levitt (2004) approach

# Prepared by Elisabeth Sedmik on Wednesday, June 24 2015
# Based on code by Suhas D. Parandekar

# Revised on 07/28/2015

# The following code tries to unravel the secret of Vietnam's outstanding performance on the PISA 2012 assessment. 
# It presents an analytical comparison of possible explanatory factors (as assessed within the PISA 2012 test) 
# of Vietnam's high test score in MATH, comparing 7 other developing countries with Vietnam. The statistical 
# approach taken is a modified dummy variable approach following Fryer and Levitt (2004).

##################################################################################
# PLEASE NOTE THAT THIS IS THE FILE FOR THE MATH REGRESSIONS
# For the Reading and Science regressions please see PISA_2012_FL_part7 onwards
##################################################################################

##################################################################################
# Outline:
# 1. GENERATING DATA SET (MERGING, CLEANING) (in part 1)
# 2. DESCRIPTIVE STATISTICS WITH VIETNAM + 7 DEVELOPING COUNTRIES (in part 1)
# 3. PISA SCORES (in part 1)
# 4. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH (Math: part 2 - part 6)
##################################################################################

# Loading R packages to process PISA data:

# Admin packages
library(foreign)# To import and export data to and from R (eg. txt files)
library(xlsx)# To generate MS-Excel output
library(xtable)# To generate Latex output (in which the research paper is written)
library(epicalc)# For producing descriptives of data
library(tables) # Computes and displays complex tables of summary statistics
library(stargazer)# For latex regression and summary statistics tables

# Modeling packages
library(intsvy)# For PISA (and TIMSS, PIRLS, etc) analysis with Plausible Values (PV) and Balanced Repeated Replication (BRR)
library(TDMR)# For tuned data mining in R - eg. detect column of constants in dataframe
library(gmodels)# For model fitting, contains various R programming tools (eg. PROC FREQ like tables)
library(dplyr)# For varioys data manipulation
library(psych)# For rescaling variables to given mean and sd
library(sm)# for locally smoothed regressions and density estimation
library(lme4)# To run mixed-effects models using Eigen and S4

# Please be aware that many packages (eg. tables, intsvy) require additional packages to run. When trying to load
# the package, R will tell you which ones are missing. Overall you may need to download around 40 packages.

load("DEVCON8a.RDA")

# How big is our initital sample size? 
T0 <- DEVCON8a[, c("VIETNAM")] 
N0<- NROW(na.omit(T0))
N0 

############### 4. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH ################

############# 4.2 Explanatory variables - Students, teachers, pedagogical practices and schools #############

# NON-ROTATED PART:

# 1. Students		
# Background: FEMALE, ST05Q01, REPEAT (indexed ST07), ST08Q01, ST09Q01, ST115Q01, MISCED, HISEI,		
# --------------WEALTH, CULTPOS, HEDRES, ST28Q01		
# Home Support: SC25 (Parent Participation, SC), SC24Q01 (Parental Expectations, SC)		
# Gender Balance: PCGIRLS (Proportion of girls enrolled at school, SC)

# 2. Teachers		
# Quantity: STRATIO, PROPCERT, PROPQUAL, TCSHORT, SMRATIO		
# Quality: TCFOCST, SC30Q01, SC30Q02, SC30Q03, SC30Q04, SC31Q01-Q07 (TCH incentive), SC39Q08, SC35Q02		

# 3. Pedagogical practices		
# General / student-perceived teaching practices: SC40Q01-SC40Q03 (Practices in Maths, SC)		
# Assessment: SC18Q01-Q08		
# Classroom Management: SC39Q07 (Seeking student feedback, SC)	

# 4. Schools		
# Type: SC01Q01 (Public or private school, SC), SC02Q02 (Revenues from student fees, SC), SC03Q01	
# --------------CLSIZE (Class Size based on SC05, SC), SCHSIZE (based on SC07, SC)	
# Resources: RATCMP15 (Availabilit of resources, SC), COMPWEB (PC for learning connected to the internet, SC),		
# --------------SC16Q01-Q11		
# --------------SCMATEDU (Quality of educ. resources, SC), SCMATBUI (Quality of Physical Infrastructure, SC),		
# --------------SC20Q01 (Additional maths lessons offered, SC)		
#  Leadership: LEADCOM (Framing Schools goal and curriculum, SC), LEADINST (Instructional Leadership, SC), 		
# --------------LEADPD (Promoting Development, SC), LEADTCH (Teacher Participation in Leadership, SC),		
# --------------SC19Q01 & SC19Q02 (if Student Achievement data is made available, SC), SCHAUTON (School autonomy, SC), 		
# --------------TCHPARTI (Teacher participation, SC), SC39Q03 (recording of student/teacher/test data, SC)		
# Selectivity: SCHSEL (School Selectivity of students, SC)		
# Climate: STUDCLIM (Student aspects of school climate, SC), TEACCLIM (teacher aspects of school climate, SC), 		
# --------------TCMORALE (Teacher Morale, SC)		

# ROTATED PART 1	

# 1. Students	
#-----Effort: MATWKETH	
#-----Attitude: INSTMOT, INTMAT, SUBNORM, MATHEFF, FAILMAT, MATINTFC, MATBEH, PERSEV, OPENPS 	

# ROTATED PART 2

# 1. Students
#-----Effort: ST55Q02 (Math lessons out of school), ST57Q01-Q06 (dropped for Math)
#-----Preparation: EXAPPLM, EXPUREM, FACMCONC
# 2. Teachers
#-----Quantity: LMINS (minutes of language classes), MMINS (minutes of math classes), SMINS (minutes of science classes)

# ROTATED PART 3

# 1. Students
#-----Background: ST91Q03
#-----Attitude: SCMAT, ANXMAT, BELONG, ATSCHL, ATTLNACT, ST91Q02
# 2. Teachers
#----Quality: MTSUP, STUDREL, ST91Q04
# 3. Pedagogical Practices
#-----General: TCHBEHTD, TCHBEHSO
#-----Assessment: TCHBEHFA
#-----Cognitive Activation: COGACT
#-----Classroom Management: CLSMAN, DISCLIMA

########################## 4.2.1 Explanatory Variables - rotated & non-rotated questions #######################

############################### 4.2.8 Non-rotated & PART 2 rotated questions #############################

# Let's prepare our data set by deleting the missing data for all gap decreasing variables from the non-rotated parts
# AND deleting missing data from all variables we will use from the first (part 1) rotated part

# We will add the rotated variables according to the subsector (students, teachers, etc) they belong to (see our schematic structure)
# and within, as always, in order that they have been asked

# 1. STUDENTS
#-----Effort: ST55Q02 (Math lessons out of school), ST57Q01-Q06
#-----Preparation: EXAPPLM, EXPUREM, FACMCONC

# 2. TEACHERS
#-----Quantity: LMINS (minutes of language classes), MMINS (minutes of math classes), SMINS (minutes of science classes)

# 4. SCHOOLS
#-----Resources: ST72 (not for math but for 'test language' class) WE ARE NOT TAKING THIS FOR MATH

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL","ST55Q02",
                    "ST57Q01","ST57Q02","ST57Q03","ST57Q04","ST57Q05","ST57Q06","EXAPPLM","EXPUREM",
                    "FAMCONC","LMINS","MMINS","SMINS")]
N1 <- NROW(na.omit(T1b)) 
N1 #7602
N0-N1 #40881 NA's
DEVCON8j <- DEVCON8a[complete.cases(T1b),]

# That is a very small sample, let's see which variables have largely missing data, maybe we can decide to drop them.

# Double check the non-rotated parts to be equal in NA's to when we did it in 4.2.5 Non-rotated Questions - all gap decreasing/Sensitivity
# (File: PISA2012_FL_part 2)

T1nonrot <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                       "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                       "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                       "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                       "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL")]
Nnonrot <- NROW(na.omit(T1nonrot)) 
Nnonrot #25612
N0-Nnonrot #22871 NA's

############################# TO SEE THE REGRESSIONS WHERE WE CHOSE TO DROP ST57 PLEASE GO TO LINE 886, ######
############################# TO SEE HOW AND WHY WE CHOSE TO DROP ST57 FROM THE VARIABLES PLEASE SEE #########
############################# LINES 157 TO 885 ###############################################################

# Ok so let' see where the many missing NA's are coming from. We cannot simply look at the absolute number of NA's
# in each variable, as only cumulatively they will tell us, how many are missing overall. For example, if we have 
# 5 observations/rows (row 1, row 2, row 3, row 4, row 5), and we know that variable A is missing 2 rows, variable B 
# is missing 2 rows and variable C is missing 3 rows. If we go by the lowest absolute missing, we might think of 
# picking variable A and variable B. But it could be the case that in variable A, rows 1 and 2 are missing and in 
# variable B rows 3 and 4 are missing; so we end up with only row 5. If we took variable A (rows 1 and 2 missing) 
# and variable C, that could have rows 1, 2 and 3 missing, we ended up with row 4 and row 5 to analyze. Which are more
# then before. That is assuming our objective is to have the least rows missing and we no preference over any specific 
# variable. Our case is not that simple, because we have to strike a balance between a meaningful sample size an finding
# the variables that help us explain Vietnams performance. 

# We will take it step by step and look at the individual variables cumulatively and separately now

T1ST55 <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                       "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                       "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                       "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                       "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL","ST55Q02")]
NST55 <- NROW(na.omit(T1ST55)) 
NST55 #15236
N0-NST55 #33247 NA's

# From 22781 NA's to 33247 NA's is a big step, so we might think about excluding ST55Q02, which asks how many extra
# hours are spent learning outside school for math. Quite an important variable though. 

T1ST57 <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                       "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                       "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                       "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                       "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                       "ST57Q01","ST57Q02","ST57Q03","ST57Q04","ST57Q05","ST57Q06")]
NST57 <- NROW(na.omit(T1ST57)) 
NST57 #9716
N0-NST57 #38767 NA's

# Clearly, we have large missing data in the ST57 questions, so we might think about picking specific subquestions
# and not all. Before that, for consistency, we test the other variables too.

T1ST551 <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                       "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                       "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                       "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                       "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                       "ST55Q02","ST57Q01","ST57Q02","ST57Q03","ST57Q04","ST57Q05","ST57Q06")]
NST551 <- NROW(na.omit(T1ST551)) 
NST551 #9139
N0-NST551 #39344 NA's

# ST57 alone seems to have an immensely large set of missing data. Adding ST55 does not decrease the avaialble data set
# much more, but careful ST55 alone does decrease the available data set from 25612 observations to 15236 observations.

T1EXAPPLM <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                          "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                          "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                          "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                          "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL","EXAPPLM")]
NEXAPPLM <- NROW(na.omit(T1EXAPPLM)) 
NEXAPPLM #16588
N0-NEXAPPLM #31895 NA's

# We want to demonstrate something here. Above we took only EXAPPLM, without ST55/St57. No we inlcude ST57 

T1EXAPPLM1 <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                          "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                          "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                          "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                          "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                          "ST55Q02","EXAPPLM")]
NEXAPPLM1 <- NROW(na.omit(T1EXAPPLM1)) 
NEXAPPLM1 #15040
N0-NEXAPPLM1 #33443 NA's

# EXAPPLM does not look like a big issue when cumulated with the ST57 questions, seems like similar rows were missing.
# Individually however, it does give for a big impact!

# We hope that by now, we have made our point about not just picking variables with the least missing data 
# very clear!

T1EXPUREM <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                          "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                          "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                          "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                          "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                          "ST55Q02","EXAPPLM","EXPUREM")]
NEXPUREM <- NROW(na.omit(T1EXPUREM)) 
NEXPUREM #14994
N0-NEXPUREM #33489 NA's

T1FAMCONC <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                          "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                          "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                          "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                          "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                          "ST55Q02","EXAPPLM","EXPUREM","FAMCONC")]
NFAMCONC <- NROW(na.omit(T1FAMCONC)) 
NFAMCONC #14851
N0-NFAMCONC #33632 NA's

T1MMINS <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                        "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                        "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                        "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                        "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                        "ST55Q02","EXAPPLM","EXPUREM","FAMCONC","MMINS")]
NMMINS <- NROW(na.omit(T1MMINS)) 
NMMINS #12777
N0-NMMINS #35706 NA's

T1MINS <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                       "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                       "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                       "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                       "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                       "ST55Q02","EXAPPLM","EXPUREM","FAMCONC","MMINS","LMINS","SMINS")]
NMINS <- NROW(na.omit(T1MINS)) 
NMINS #11944
N0-NMINS #36539 NA's

T1exclST55ST57 <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                       "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                       "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                       "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                       "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                       "EXAPPLM","EXPUREM","FAMCONC","MMINS","LMINS","SMINS")]
N1exclST55ST57 <- NROW(na.omit(T1exclST55ST57)) 
N1exclST55ST57 #13036
N0-N1exclST55ST57 #35447 NA's

# The big issues are ST55 and ST57 (although careful with that as we saw), but lets try to see how important
# ST55 and ST57 are and if we can work on subsets of these questions to create for less missing data

# ST55
#________________________________________________________________________________________________________

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL","ST55Q02")]
N1 <- NROW(na.omit(T1b)) 
N1 #15236
N0-N1 #33247 NA's
DEVCON8j <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==1] <- 0
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==2] <- 1
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==1]  <- 5
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==2]  <- 15
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==3]  <- 60
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==4]  <- 150
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==5]  <- 350
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==1] <- 1
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==2] <- 0
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8j$SC25Q10[is.na(DEVCON8j$SC25Q10)]  <- 0
DEVCON8j$SC25Q11[is.na(DEVCON8j$SC25Q11)]  <- 0
DEVCON8j$FUNDMOM <- DEVCON8j$SC25Q11
DEVCON8j$COUNCILMOM <- DEVCON8j$SC25Q10

# Now for the teacher-related variables

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8j$TCM_STUASS[DEVCON8j$SC30Q01==1] <- 1
DEVCON8j$TCM_STUASS[DEVCON8j$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8j$TCM_PEER[DEVCON8j$SC30Q02==1] <- 1
DEVCON8j$TCM_PEER[DEVCON8j$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8j <- merge(DEVCON8j,SC31OUT.rda,by="NEWID")
DEVCON8j$TCH_INCENTV <- rescale(DEVCON8j$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Now for the pedagogical practices-related variables

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8j$ASS_PROG[DEVCON8j$SC18Q01==1] <- 1
DEVCON8j$ASS_PROG[DEVCON8j$SC18Q01==2] <- 0

DEVCON8j$ASS_PROM[DEVCON8j$SC18Q02==1] <- 1
DEVCON8j$ASS_PROM[DEVCON8j$SC18Q02==2] <- 0

DEVCON8j$ASS_SCH[DEVCON8j$SC18Q05==1] <- 1
DEVCON8j$ASS_SCH[DEVCON8j$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8j$STU_FEEDB[DEVCON8j$SC39Q07==1] <- 1
DEVCON8j$STU_FEEDB[DEVCON8j$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8j$COMP_USE[DEVCON8j$SC40Q01==1] <- 1
DEVCON8j$COMP_USE[DEVCON8j$SC40Q01==2] <- 0

DEVCON8j$TXT_BOOK[DEVCON8j$SC40Q02==1] <- 1
DEVCON8j$TXT_BOOK[DEVCON8j$SC40Q02==2] <- 0

# Now for the schools-related variables

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8j$DUM_SMLTOWN <- ifelse(DEVCON8j$SC03Q01==2,1,0)
DEVCON8j$DUM_TOWN    <- ifelse(DEVCON8j$SC03Q01==3,1,0)

DEVCON8j$TOWN <- DEVCON8j$DUM_SMLTOWN+DEVCON8j$DUM_TOWN
DEVCON8j$TOWN[DEVCON8j$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8j$EXC2_PLAY[DEVCON8j$SC16Q02==1] <- 1
DEVCON8j$EXC2_PLAY[DEVCON8j$SC16Q02==2] <- 0

DEVCON8j$EXC6_MATHCOMP[DEVCON8j$SC16Q06==1] <- 1
DEVCON8j$EXC6_MATHCOMP[DEVCON8j$SC16Q06==2] <- 0

DEVCON8j$EXC10_SPORT[DEVCON8j$SC16Q10==1] <- 1
DEVCON8j$EXC10_SPORT[DEVCON8j$SC16Q10==2] <- 0

DEVCON8j$EXC11_UNICORN[DEVCON8j$SC16Q11==1] <- 1
DEVCON8j$EXC11_UNICORN[DEVCON8j$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8j$SCL_EXTR_CL[DEVCON8j$SC20Q01==1] <- 1
DEVCON8j$SCL_EXTR_CL[DEVCON8j$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8j$SCORE_PUBLIC[DEVCON8j$SC19Q01==1] <- 1
DEVCON8j$SCORE_PUBLIC[DEVCON8j$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8j$QUAL_RECORD[DEVCON8j$SC39Q03==1] <- 1
DEVCON8j$QUAL_RECORD[DEVCON8j$SC39Q03==2] <- 0

#ST55Q02
#________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8j$OUTMATH_NONE <- ifelse(DEVCON8j$ST55Q02==1,1,0)
DEVCON8j$OUTMATH_LESS2   <- ifelse(DEVCON8j$ST55Q02==2,1,0)
DEVCON8j$OUTMATH_2TO4   <- ifelse(DEVCON8j$ST55Q02==3,1,0)
DEVCON8j$OUTMATH_4TO6   <- ifelse(DEVCON8j$ST55Q02==4,1,0)
DEVCON8j$OUTMATH_MORE6   <- ifelse(DEVCON8j$ST55Q02==5,1,0)

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8j, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8j.rda") 

R117 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R117 # Vietnam: 123.28

R118 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R118 # Vietnam 74.97

R119 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R119 # OUTMATH_NONE decreases
# Vietnam 74.65

R120 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R120 # OUTMATH_NONE decreases, OUTMATH_LESS2 decreases
# Vietnam 73.15 

R121 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2",
                        "OUTMATH_2TO4"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R121 # OUTMATH_NONE decreases, OUTMATH_LESS2 decreases, OUTMATH_2TO4 decreases
# Vietnam 72.25

R122 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2",
                        "OUTMATH_2TO4","OUTMATH_4TO6"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R122 # OUTMATH_NONE decreases, OUTMATH_LESS2 decreases, OUTMATH_2TO4 decreases, OUTMATH_4TO6 decreases
# Vietnam 72.21

R123 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2",
                        "OUTMATH_2TO4","OUTMATH_MORE6"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R123 # OUTMATH_NONE decreases, OUTMATH_LESS2 decreases, OUTMATH_2TO4 decreases, OUTMATH_MORE6 decreases
# Vietnam 72.21
# Since these are mutually exclusive categories, to use OUTMATH_MORE6 we had to delete OUTMATH_2to4. But as you can see,
# since this is teh last category, it gives the same value.

# Lets try these two alone MATHOUT_LESS2 and MATHOUT_2TO4

R124 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_LESS2",
                        "OUTMATH_2TO4"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R124 # VIETNAM 75.17, actually increases

R125 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2",
                        "OUTMATH_2TO4"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R125 # VIETNAM 72.25

# We could try to minimize to OUTMATH_NONE, OUTMATH_LESS2, OUTMATH_2TO4. Now let's look at ST57

# ST57
#________________________________________________________________________________________________________

# Initially we are working with 25612 rows
T1nonrot <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                         "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                         "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                         "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                         "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL")]
Nnonrot <- NROW(na.omit(T1nonrot)) 
Nnonrot #25612
N0-Nnonrot #22871 NA's

# For all ST57 questions we are working with 9.716 rows, not a good number
T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL","ST57Q01",
                    "ST57Q02","ST57Q03","ST57Q04","ST57Q05","ST57Q06")]
N1 <- NROW(na.omit(T1b)) 
N1 #9716
N0-N1 #38767 NA's
DEVCON8j <- DEVCON8a[complete.cases(T1b),]

# For ST57Q01
T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL","ST57Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 #15677
N0-N1 #32806 NA's

#For ST57Q02
T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL","ST57Q02")]
N1 <- NROW(na.omit(T1b)) 
N1 #12424
N0-N1 #36059 NA's

# For ST57Q03
T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL","ST57Q03")]
N1 <- NROW(na.omit(T1b)) 
N1 #13307
N0-N1 #35176 NA's

# For ST57Q04
T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL","ST57Q04")]
N1 <- NROW(na.omit(T1b)) 
N1 #12856
N0-N1 # 35627 NA's

# For ST57Q05
T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL","ST57Q05")]
N1 <- NROW(na.omit(T1b)) 
N1 #13981
N0-N1 # 34502 NA's

# For ST57Q06
T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL","ST57Q06")]
N1 <- NROW(na.omit(T1b)) 
N1 #13887
N0-N1 # 34596 NA's

# Let's prepare the relevant student variables again:

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==1] <- 0
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==2] <- 1
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==1]  <- 5
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==2]  <- 15
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==3]  <- 60
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==4]  <- 150
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==5]  <- 350
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==1] <- 1
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==2] <- 0
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8j$SC25Q10[is.na(DEVCON8j$SC25Q10)]  <- 0
DEVCON8j$SC25Q11[is.na(DEVCON8j$SC25Q11)]  <- 0
DEVCON8j$FUNDMOM <- DEVCON8j$SC25Q11
DEVCON8j$COUNCILMOM <- DEVCON8j$SC25Q10

# Now for the teacher-related variables

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8j$TCM_STUASS[DEVCON8j$SC30Q01==1] <- 1
DEVCON8j$TCM_STUASS[DEVCON8j$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8j$TCM_PEER[DEVCON8j$SC30Q02==1] <- 1
DEVCON8j$TCM_PEER[DEVCON8j$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8j <- merge(DEVCON8j,SC31OUT.rda,by="NEWID")
DEVCON8j$TCH_INCENTV <- rescale(DEVCON8j$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Now for the pedagogical practices-related variables

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8j$ASS_PROG[DEVCON8j$SC18Q01==1] <- 1
DEVCON8j$ASS_PROG[DEVCON8j$SC18Q01==2] <- 0

DEVCON8j$ASS_PROM[DEVCON8j$SC18Q02==1] <- 1
DEVCON8j$ASS_PROM[DEVCON8j$SC18Q02==2] <- 0

DEVCON8j$ASS_SCH[DEVCON8j$SC18Q05==1] <- 1
DEVCON8j$ASS_SCH[DEVCON8j$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8j$STU_FEEDB[DEVCON8j$SC39Q07==1] <- 1
DEVCON8j$STU_FEEDB[DEVCON8j$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8j$COMP_USE[DEVCON8j$SC40Q01==1] <- 1
DEVCON8j$COMP_USE[DEVCON8j$SC40Q01==2] <- 0

DEVCON8j$TXT_BOOK[DEVCON8j$SC40Q02==1] <- 1
DEVCON8j$TXT_BOOK[DEVCON8j$SC40Q02==2] <- 0

# Now for the schools-related variables

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8j$DUM_SMLTOWN <- ifelse(DEVCON8j$SC03Q01==2,1,0)
DEVCON8j$DUM_TOWN    <- ifelse(DEVCON8j$SC03Q01==3,1,0)

DEVCON8j$TOWN <- DEVCON8j$DUM_SMLTOWN+DEVCON8j$DUM_TOWN
DEVCON8j$TOWN[DEVCON8j$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8j$EXC2_PLAY[DEVCON8j$SC16Q02==1] <- 1
DEVCON8j$EXC2_PLAY[DEVCON8j$SC16Q02==2] <- 0

DEVCON8j$EXC6_MATHCOMP[DEVCON8j$SC16Q06==1] <- 1
DEVCON8j$EXC6_MATHCOMP[DEVCON8j$SC16Q06==2] <- 0

DEVCON8j$EXC10_SPORT[DEVCON8j$SC16Q10==1] <- 1
DEVCON8j$EXC10_SPORT[DEVCON8j$SC16Q10==2] <- 0

DEVCON8j$EXC11_UNICORN[DEVCON8j$SC16Q11==1] <- 1
DEVCON8j$EXC11_UNICORN[DEVCON8j$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8j$SCL_EXTR_CL[DEVCON8j$SC20Q01==1] <- 1
DEVCON8j$SCL_EXTR_CL[DEVCON8j$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8j$SCORE_PUBLIC[DEVCON8j$SC19Q01==1] <- 1
DEVCON8j$SCORE_PUBLIC[DEVCON8j$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8j$QUAL_RECORD[DEVCON8j$SC39Q03==1] <- 1
DEVCON8j$QUAL_RECORD[DEVCON8j$SC39Q03==2] <- 0

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8j, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8j.rda") 

R126 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R126 # Vietnam: 119.11 

R127 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R127 # Vietnam 72.90

R128 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","ST57Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R128 # ST57Q01 increases, 
# Vietnam 73.78

R129 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","ST57Q01","ST57Q02"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R129 # ST57Q01 increases, ST57Q02 increases
# Vietnam 74.28

R130 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","ST57Q01","ST57Q02","ST57Q03"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R130 # ST57Q01 increases, ST57Q02 increases, ST57Q03 increases
# Vietnam 73.70

R131 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","ST57Q01","ST57Q02","ST57Q03",
                        "ST57Q04"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R131 # ST57Q01 increases, ST57Q02 increases, ST57Q03 increases, ST57Q04 decreases drastically
# Vietnam 68.86

R132 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","ST57Q01","ST57Q02","ST57Q03",
                        "ST57Q04","ST57Q05"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R132 # ST57Q01 increases, ST57Q02 increases, ST57Q03 increases, ST57Q04 decreases drastically, ST57Q05 decreases
# Vietnam 68.27

R133 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","ST57Q01","ST57Q02","ST57Q03",
                        "ST57Q04","ST57Q05","ST57Q06"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R133 # ST57Q01 increases, ST57Q02 increases, ST57Q03 increases, ST57Q04 decreases drastically, ST57Q05 decreases
# ST57Q06 decreases
# Vietnam 67.28 

# ST55 and/or ST57
#____________________________________________________________________________________________________________________

# Ok so let's try to get the sample size bigger by only using OUTMATH_NONE, OUTMATH_LESS2, OUTMATH_2TO4 (from ST55Q02)
# and ST57Q04, ST57Q05 and ST57Q06

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL","ST55Q02",
                    "ST57Q04","ST57Q05","ST57Q06")]
N1 <- NROW(na.omit(T1b)) 
N1 # 11544
N0-N1 # 36939
DEVCON8j <- DEVCON8a[complete.cases(T1b),]

# Still, 11544 is not a satisfactorily big sample. If we look at the content of the questions, we will see that ST55
# asks for 'Math lessons' (and potentially other subjects) outside school; ST57 asks 'Thinking about all school subjects: 
# on average, how many hours do you spend each week on the following?' and gives detailed options (tutor, study with parent,...).
# Ideally we would filter for math, since we are testing specifically for it, but there is value in asking for the other
# subjects as well (children who do well in subject A might also be keen in subject B, and other theories). Given that,
# it would be also valuable what system of out of school activities (learning with parent, tutoring, etc) actually has an 
# effect on our Vietnam dummy. So let's see how the sample size looks liek if we just use ST57Q04, ST57Q05, ST57Q06

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                    "ST57Q04","ST57Q05","ST57Q06","EXAPPLM","EXPUREM",
                    "FAMCONC","LMINS","MMINS","SMINS")]
N1 <- NROW(na.omit(T1b)) 
N1 # 9961
N0-N1 #38522 NA's
DEVCON8j <- DEVCON8a[complete.cases(T1b),]

################################# FOR OUR REGRESSION ANALYSIS START HERE ########################################

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                    "ST55Q02","EXAPPLM","EXPUREM","FAMCONC","LMINS","MMINS","SMINS")]
N1 <- NROW(na.omit(T1b)) 
N1 # 11944
N0-N1 #36539 NA's
DEVCON8j <- DEVCON8a[complete.cases(T1b),]

# Ok' lets work with ST55 then

# Let's prepare the relevant student variables again:

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==1] <- 0
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==2] <- 1
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==1]  <- 5
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==2]  <- 15
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==3]  <- 60
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==4]  <- 150
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==5]  <- 350
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==1] <- 1
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==2] <- 0
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8j$SC25Q10[is.na(DEVCON8j$SC25Q10)]  <- 0
DEVCON8j$SC25Q11[is.na(DEVCON8j$SC25Q11)]  <- 0
DEVCON8j$FUNDMOM <- DEVCON8j$SC25Q11
DEVCON8j$COUNCILMOM <- DEVCON8j$SC25Q10

# Now for the teacher-related variables

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8j$TCM_STUASS[DEVCON8j$SC30Q01==1] <- 1
DEVCON8j$TCM_STUASS[DEVCON8j$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8j$TCM_PEER[DEVCON8j$SC30Q02==1] <- 1
DEVCON8j$TCM_PEER[DEVCON8j$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8j <- merge(DEVCON8j,SC31OUT.rda,by="NEWID")
DEVCON8j$TCH_INCENTV <- rescale(DEVCON8j$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Now for the pedagogical practices-related variables

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8j$ASS_PROG[DEVCON8j$SC18Q01==1] <- 1
DEVCON8j$ASS_PROG[DEVCON8j$SC18Q01==2] <- 0

DEVCON8j$ASS_PROM[DEVCON8j$SC18Q02==1] <- 1
DEVCON8j$ASS_PROM[DEVCON8j$SC18Q02==2] <- 0

DEVCON8j$ASS_SCH[DEVCON8j$SC18Q05==1] <- 1
DEVCON8j$ASS_SCH[DEVCON8j$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8j$STU_FEEDB[DEVCON8j$SC39Q07==1] <- 1
DEVCON8j$STU_FEEDB[DEVCON8j$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8j$COMP_USE[DEVCON8j$SC40Q01==1] <- 1
DEVCON8j$COMP_USE[DEVCON8j$SC40Q01==2] <- 0

DEVCON8j$TXT_BOOK[DEVCON8j$SC40Q02==1] <- 1
DEVCON8j$TXT_BOOK[DEVCON8j$SC40Q02==2] <- 0

# Now for the schools-related variables

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8j$DUM_SMLTOWN <- ifelse(DEVCON8j$SC03Q01==2,1,0)
DEVCON8j$DUM_TOWN    <- ifelse(DEVCON8j$SC03Q01==3,1,0)

DEVCON8j$TOWN <- DEVCON8j$DUM_SMLTOWN+DEVCON8j$DUM_TOWN
DEVCON8j$TOWN[DEVCON8j$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8j$EXC2_PLAY[DEVCON8j$SC16Q02==1] <- 1
DEVCON8j$EXC2_PLAY[DEVCON8j$SC16Q02==2] <- 0

DEVCON8j$EXC6_MATHCOMP[DEVCON8j$SC16Q06==1] <- 1
DEVCON8j$EXC6_MATHCOMP[DEVCON8j$SC16Q06==2] <- 0

DEVCON8j$EXC10_SPORT[DEVCON8j$SC16Q10==1] <- 1
DEVCON8j$EXC10_SPORT[DEVCON8j$SC16Q10==2] <- 0

DEVCON8j$EXC11_UNICORN[DEVCON8j$SC16Q11==1] <- 1
DEVCON8j$EXC11_UNICORN[DEVCON8j$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8j$SCL_EXTR_CL[DEVCON8j$SC20Q01==1] <- 1
DEVCON8j$SCL_EXTR_CL[DEVCON8j$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8j$SCORE_PUBLIC[DEVCON8j$SC19Q01==1] <- 1
DEVCON8j$SCORE_PUBLIC[DEVCON8j$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8j$QUAL_RECORD[DEVCON8j$SC39Q03==1] <- 1
DEVCON8j$QUAL_RECORD[DEVCON8j$SC39Q03==2] <- 0

#ST55Q02
#________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8j$OUTMATH_NONE <- ifelse(DEVCON8j$ST55Q02==1,1,0)
DEVCON8j$OUTMATH_LESS2   <- ifelse(DEVCON8j$ST55Q02==2,1,0)
DEVCON8j$OUTMATH_2TO4   <- ifelse(DEVCON8j$ST55Q02==3,1,0)
DEVCON8j$OUTMATH_4TO6   <- ifelse(DEVCON8j$ST55Q02==4,1,0)

# LMINS, MMINS, SMINS
#________________________________________________________________________________________________________
DEVCON8j$SHRS <- (DEVCON8j$SMINS)/60
DEVCON8j$MHRS <- (DEVCON8j$MMINS)/60
DEVCON8j$LHRS <- (DEVCON8j$LMINS)/60

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8j, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8j.rda") 

R134 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R134
#Estimate Std. Error t value
#(Intercept)   398.53       3.20  124.37
#VIETNAM       119.49       6.92   17.27
#R-squared      24.92       2.56    9.73

R135 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R135 # Vietnam 72.62

R136 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R136 # OUTMATH_NONE decreases
# Vietnam 72.18 

R137 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R137 # OUTMATH_NONE decreases, OUTMATH_LESS2 decreases
# Vietnam 70.73 

R138 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2",
                        "OUTMATH_2TO4"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R138 # OUTMATH_NONE decreases, OUTMATH_LESS2 decreases, OUTMATH_2TO4 decreases
# Vietnam 69.99 

R139 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2",
                        "OUTMATH_2TO4","OUTMATH_4TO6"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R139 # OUTMATH_NONE decreases, OUTMATH_LESS2 decreases, OUTMATH_2TO4 decreases, OUTMATH_4TO6 decreases
# Vietnam 69.96

R140 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2",
                        "OUTMATH_2TO4","OUTMATH_4TO6","EXAPPLM"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R140 # OUTMATH_NONE decreases, OUTMATH_LESS2 decreases, OUTMATH_2TO4 decreases, OUTMATH_4TO6 decreases, EXAPPLM increases
# Vietnam 71.50

R141 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2",
                        "OUTMATH_2TO4","OUTMATH_4TO6","EXAPPLM","EXPUREM"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R141 # OUTMATH_NONE decreases, OUTMATH_LESS2 decreases, OUTMATH_2TO4 decreases, OUTMATH_4TO6 decreases, EXAPPLM increases,
# EXPUREM decreases
# Vietnam 67.35

R142 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2",
                        "OUTMATH_2TO4","OUTMATH_4TO6","EXAPPLM","EXPUREM","FAMCONC"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R142 # OUTMATH_NONE decreases, OUTMATH_LESS2 decreases, OUTMATH_2TO4 decreases, OUTMATH_4TO6 decreases, EXAPPLM increases,
# EXPUREM decreases, FAMCONC decreases drastically (-32%)
# Vietnam 45.93

# Student Effort & Preparation testing all gap decreasing variables

R142a <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("VIETNAM",
                         "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                         "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                         "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                         "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                         "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                         "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2",
                         "OUTMATH_2TO4","OUTMATH_4TO6","EXPUREM","FAMCONC"),
                     weight="W_FSTUWT",
                     data=DEVCON8j,export=FALSE)
R142a # VIETNAM 46.65

# Student Effort & Preparation testing all gap increasing variables (EXAPPLM)

R142b <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("VIETNAM",
                         "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                         "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                         "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                         "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                         "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                         "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","EXAPPLM"),
                     weight="W_FSTUWT",
                     data=DEVCON8j,export=FALSE)
R142b # VIETNAM 74.16


R143 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2",
                        "OUTMATH_2TO4","OUTMATH_4TO6","EXAPPLM","EXPUREM","FAMCONC","LHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R143 # OUTMATH_NONE decreases, OUTMATH_LESS2 decreases, OUTMATH_2TO4 decreases, OUTMATH_4TO6 decreases, EXAPPLM increases,
# EXPUREM decreases, FAMCONC decreases drastically, LHRS increases
# Vietnam 45.96

R144 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2",
                        "OUTMATH_2TO4","OUTMATH_4TO6","EXAPPLM","EXPUREM","FAMCONC","LHRS","MHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R144 # OUTMATH_NONE decreases, OUTMATH_LESS2 decreases, OUTMATH_2TO4 decreases, OUTMATH_4TO6 decreases, EXAPPLM increases,
# EXPUREM decreases, FAMCONC decreases drastically, LHRS increases, MHRS increases 
# Vietnam 51.23 

R145 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2",
                        "OUTMATH_2TO4","OUTMATH_4TO6","EXAPPLM","EXPUREM","FAMCONC","LHRS","MHRS","SHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R145 # OUTMATH_NONE decreases, OUTMATH_LESS2 decreases, OUTMATH_2TO4 decreases, OUTMATH_4TO6 decreases, EXAPPLM increases,
# EXPUREM decreases, FAMCONC decreases drastically, LHRS increases, MHRS increases, SHRS increases
# Vietnam 56.17 

# Now we are testing all the 6 gap decreasing variables

R146 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH_NONE","OUTMATH_LESS2",
                        "OUTMATH_2TO4","OUTMATH_4TO6","EXPUREM","FAMCONC"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R146
#Estimate Std. Error t value
#(Intercept)     408.34      20.64   19.78
#VIETNAM          46.65       7.33    6.37
#PRESCHOOL        20.30       4.26    4.77
#REPEAT          -24.38       3.26   -7.47
#ST08Q01          -7.32       1.69   -4.33
#ST115Q01         -3.85       2.39   -1.61
#BOOK_N            0.05       0.01    3.07
#PARPRESSURE       9.55       4.31    2.22
#PCGIRLS          -4.96      15.09   -0.33
#FUNDMOM           0.15       0.07    2.33
#COUNCILMOM       -0.14       0.06   -2.35
#PROPCERT         10.55       6.91    1.53
#SMRATIO          -0.01       0.01   -1.25
#TCSHORT           2.70       1.68    1.61
#TCFOCST          -0.44       2.05   -0.21
#TCM_STUASS       -4.69       9.16   -0.51
#TCM_PEER         -6.57       5.33   -1.23
#TCH_INCENTV      -3.20       2.65   -1.21
#ASS_PROG        -30.73       8.19   -3.75
#ASS_PROM          8.69       6.04    1.44
#ASS_SCH          -3.27       8.07   -0.41
#STU_FEEDB         4.59       5.55    0.83
#COMP_USE          1.27       5.17    0.25
#TXT_BOOK         -6.46       7.44   -0.87
#TOWN             -6.32       3.46   -1.83
#CLSIZE            0.67       0.24    2.79
#COMPWEB          12.76       6.37    2.00
#SCMATEDU          4.42       2.93    1.51
#SCMATBUI          2.91       2.48    1.18
#EXC2_PLAY         7.89       4.25    1.86
#EXC6_MATHCOMP    -0.44       5.24   -0.08
#EXC10_SPORT      -7.73       8.25   -0.94
#EXC11_UNICORN     6.10       5.80    1.05
#SCL_EXTR_CL       8.19       5.08    1.61
#SCORE_PUBLIC      9.35       4.93    1.90
#QUAL_RECORD       9.80       7.70    1.27
#SCHSEL            2.38       3.24    0.73
#OUTMATH_NONE    -10.17       4.16   -2.44
#OUTMATH_LESS2   -16.23       3.88   -4.18
#OUTMATH_2TO4     -9.13       4.28   -2.13
#OUTMATH_4TO6     -2.39       4.90   -0.49
#EXPUREM          12.53       0.99   12.68
#FAMCONC          24.43       1.59   15.37
#R-squared        47.04       2.28   20.66

# Interestingly, when we did the one-by-one regression and had EXAPPLM (which we identified as gap increasing) included,
# the dummy stood at 45.93 after including FAMCONC. But again we have to pay attention to the direction and not the absolute
# values of the coefficients

# Now we are testing all the 4 gap increasing variables

R147 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","EXAPPLM","LHRS","MHRS","SHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R147 
#Estimate Std. Error t value
#(Intercept)     381.30      21.75   17.53
#VIETNAM          82.17       7.40   11.10
#PRESCHOOL        21.94       4.44    4.95
#REPEAT          -27.78       3.44   -8.08
#ST08Q01          -7.04       1.62   -4.35
#ST115Q01         -6.59       2.65   -2.49
#BOOK_N            0.05       0.02    3.26
#PARPRESSURE       9.44       4.36    2.17
#PCGIRLS           3.95      14.59    0.27
#FUNDMOM           0.15       0.07    2.22
#COUNCILMOM       -0.14       0.06   -2.23
#PROPCERT          3.78       7.45    0.51
#SMRATIO          -0.01       0.01   -0.97
#TCSHORT           2.69       1.76    1.53
#TCFOCST          -1.38       2.10   -0.65
#TCM_STUASS       -5.93       9.76   -0.61
#TCM_PEER         -4.97       5.61   -0.89
#TCH_INCENTV      -3.72       2.79   -1.33
#ASS_PROG        -36.13       7.92   -4.56
#ASS_PROM         13.05       6.49    2.01
#ASS_SCH          -2.74       8.39   -0.33
#STU_FEEDB         5.73       5.29    1.08
#COMP_USE         -2.39       5.42   -0.44
#TXT_BOOK         -7.42       7.89   -0.94
#TOWN             -8.76       3.61   -2.43
#CLSIZE            0.62       0.26    2.37
#COMPWEB          12.49       6.54    1.91
#SCMATEDU          4.20       3.01    1.40
#SCMATBUI          4.26       2.57    1.66
#EXC2_PLAY         6.14       4.32    1.42
#EXC6_MATHCOMP     1.37       5.33    0.26
#EXC10_SPORT      -9.49       8.49   -1.12
#EXC11_UNICORN     7.50       5.92    1.27
#SCL_EXTR_CL       6.76       5.35    1.26
#SCORE_PUBLIC      8.21       5.06    1.62
#QUAL_RECORD       1.52       8.77    0.17
#SCHSEL            1.13       3.50    0.32
#EXAPPLM           1.87       0.98    1.91
#LHRS             -7.63       1.39   -5.49
#MHRS              4.81       1.20    4.02
#SHRS              6.52       0.69    9.50
#R-squared        45.12       2.35   19.20

# Since FAMCONC really did the trick here, let us try one more thing and leave ST55 out, so as to have a 
# better sample size and just work with the other variables

########### Regressions without ST55/LMINS/SMINS/MMINS, to see how FAMCONC works in a larger set

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                    "EXAPPLM","EXPUREM","FAMCONC")]
N1 <- NROW(na.omit(T1b)) 
N1 # 16324
N0-N1 #32159 NA's
DEVCON8j <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==1] <- 0
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==2] <- 1
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==1]  <- 5
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==2]  <- 15
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==3]  <- 60
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==4]  <- 150
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==5]  <- 350
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==1] <- 1
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==2] <- 0
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8j$SC25Q10[is.na(DEVCON8j$SC25Q10)]  <- 0
DEVCON8j$SC25Q11[is.na(DEVCON8j$SC25Q11)]  <- 0
DEVCON8j$FUNDMOM <- DEVCON8j$SC25Q11
DEVCON8j$COUNCILMOM <- DEVCON8j$SC25Q10

# Now for the teacher-related variables

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8j$TCM_STUASS[DEVCON8j$SC30Q01==1] <- 1
DEVCON8j$TCM_STUASS[DEVCON8j$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8j$TCM_PEER[DEVCON8j$SC30Q02==1] <- 1
DEVCON8j$TCM_PEER[DEVCON8j$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8j <- merge(DEVCON8j,SC31OUT.rda,by="NEWID")
DEVCON8j$TCH_INCENTV <- rescale(DEVCON8j$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Now for the pedagogical practices-related variables

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8j$ASS_PROG[DEVCON8j$SC18Q01==1] <- 1
DEVCON8j$ASS_PROG[DEVCON8j$SC18Q01==2] <- 0

DEVCON8j$ASS_PROM[DEVCON8j$SC18Q02==1] <- 1
DEVCON8j$ASS_PROM[DEVCON8j$SC18Q02==2] <- 0

DEVCON8j$ASS_SCH[DEVCON8j$SC18Q05==1] <- 1
DEVCON8j$ASS_SCH[DEVCON8j$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8j$STU_FEEDB[DEVCON8j$SC39Q07==1] <- 1
DEVCON8j$STU_FEEDB[DEVCON8j$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8j$COMP_USE[DEVCON8j$SC40Q01==1] <- 1
DEVCON8j$COMP_USE[DEVCON8j$SC40Q01==2] <- 0

DEVCON8j$TXT_BOOK[DEVCON8j$SC40Q02==1] <- 1
DEVCON8j$TXT_BOOK[DEVCON8j$SC40Q02==2] <- 0

# Now for the schools-related variables

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8j$DUM_SMLTOWN <- ifelse(DEVCON8j$SC03Q01==2,1,0)
DEVCON8j$DUM_TOWN    <- ifelse(DEVCON8j$SC03Q01==3,1,0)

DEVCON8j$TOWN <- DEVCON8j$DUM_SMLTOWN+DEVCON8j$DUM_TOWN
DEVCON8j$TOWN[DEVCON8j$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8j$EXC2_PLAY[DEVCON8j$SC16Q02==1] <- 1
DEVCON8j$EXC2_PLAY[DEVCON8j$SC16Q02==2] <- 0

DEVCON8j$EXC6_MATHCOMP[DEVCON8j$SC16Q06==1] <- 1
DEVCON8j$EXC6_MATHCOMP[DEVCON8j$SC16Q06==2] <- 0

DEVCON8j$EXC10_SPORT[DEVCON8j$SC16Q10==1] <- 1
DEVCON8j$EXC10_SPORT[DEVCON8j$SC16Q10==2] <- 0

DEVCON8j$EXC11_UNICORN[DEVCON8j$SC16Q11==1] <- 1
DEVCON8j$EXC11_UNICORN[DEVCON8j$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8j$SCL_EXTR_CL[DEVCON8j$SC20Q01==1] <- 1
DEVCON8j$SCL_EXTR_CL[DEVCON8j$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8j$SCORE_PUBLIC[DEVCON8j$SC19Q01==1] <- 1
DEVCON8j$SCORE_PUBLIC[DEVCON8j$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8j$QUAL_RECORD[DEVCON8j$SC39Q03==1] <- 1
DEVCON8j$QUAL_RECORD[DEVCON8j$SC39Q03==2] <- 0

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8j, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8j.rda") 

R148 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R148
#Estimate Std. Error t value
#(Intercept)   392.71       3.01  130.50
#VIETNAM       124.85       6.62   18.86
#R-squared      27.95       2.49   11.23

R149 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R149 # Vietnam 77.18

R150 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","EXAPPLM"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R150 # EXAPPLM increases
# Vietnam 78.71

R151 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","EXAPPLM","EXPUREM"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R151 # EXAPPLM increases, EXPUREM decreases
# Vietnam 73.93

R152 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","EXAPPLM","EXPUREM","FAMCONC"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R152  # EXAPPLM increases, EXPUREM decreases, FAMCONC decreases (29%)
# Vietnam 52.51

# Only the two gap decreasing (EXPUREM and FAMCONC)

R153 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","EXPUREM","FAMCONC"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R153
#               Estimate Std. Error t value
#(Intercept)     379.21      19.92   19.03
#VIETNAM          53.09       7.07    7.50
#PRESCHOOL        20.21       3.84    5.26
#REPEAT          -28.36       3.05   -9.29
#ST08Q01          -7.94       1.48   -5.35
#ST115Q01         -3.12       2.18   -1.43
#BOOK_N            0.05       0.01    4.37
#PARPRESSURE       9.49       4.17    2.27
#PCGIRLS          -0.19      14.22   -0.01
#FUNDMOM           0.15       0.06    2.42
#COUNCILMOM       -0.12       0.06   -2.13
#PROPCERT         13.86       6.21    2.23
#SMRATIO          -0.02       0.01   -1.41
#TCSHORT           1.98       1.64    1.21
#TCFOCST          -1.20       1.88   -0.64
#TCM_STUASS       -2.51       7.48   -0.34
#TCM_PEER         -5.16       4.74   -1.09
#TCH_INCENTV      -2.83       2.57   -1.10
#ASS_PROG        -28.27       8.63   -3.27
#ASS_PROM          8.56       5.55    1.54
#ASS_SCH           1.75       7.29    0.24
#STU_FEEDB         2.95       4.90    0.60
#COMP_USE         -0.33       4.79   -0.07
#TXT_BOOK         -7.24       6.79   -1.07
#TOWN             -6.09       3.44   -1.77
#CLSIZE            0.67       0.23    2.98
#COMPWEB          14.29       5.76    2.48
#SCMATEDU          4.68       2.73    1.71
#SCMATBUI          2.69       2.27    1.19
#EXC2_PLAY         5.39       3.89    1.38
#EXC6_MATHCOMP    -0.83       4.91   -0.17
#EXC10_SPORT      -5.59       8.20   -0.68
#EXC11_UNICORN     6.32       5.42    1.16
#SCL_EXTR_CL       9.27       4.67    1.98
#SCORE_PUBLIC      8.00       4.60    1.74
#QUAL_RECORD      10.66       6.51    1.64
#SCHSEL            2.91       2.99    0.97
#EXPUREM          12.05       0.96   12.53
#FAMCONC          23.93       1.43   16.71
#R-squared        48.72       2.17   22.48

# Only EXAPPLM (the one gap increasing variable, same regression as R150)

R154 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","EXAPPLM"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R154 
#Estimate Std. Error t value
#(Intercept)     355.97      20.96   16.98
#VIETNAM          78.71       7.78   10.11
#PRESCHOOL        23.78       4.15    5.73
#REPEAT          -35.46       3.37  -10.52
#ST08Q01          -8.82       1.57   -5.60
#ST115Q01         -6.08       2.35   -2.58
#BOOK_N            0.07       0.01    4.59
#PARPRESSURE      10.08       4.56    2.21
#PCGIRLS          13.52      15.43    0.88
#FUNDMOM           0.18       0.07    2.63
#COUNCILMOM       -0.12       0.06   -1.83
#PROPCERT         14.03       6.86    2.04
#SMRATIO          -0.02       0.01   -1.98
#TCSHORT           2.35       1.79    1.31
#TCFOCST          -1.63       2.05   -0.80
#TCM_STUASS       -2.21       7.76   -0.28
#TCM_PEER         -5.73       5.37   -1.07
#TCH_INCENTV      -3.15       2.96   -1.07
#ASS_PROG        -28.43       8.26   -3.44
#ASS_PROM         11.36       5.80    1.96
#ASS_SCH           0.12       7.78    0.02
#STU_FEEDB         2.01       5.00    0.40
#COMP_USE         -1.70       5.44   -0.31
#TXT_BOOK        -10.10       7.50   -1.35
#TOWN             -8.15       3.69   -2.21
#CLSIZE            0.81       0.24    3.41
#COMPWEB          17.88       6.25    2.86
#SCMATEDU          4.97       2.98    1.67
#SCMATBUI          3.76       2.56    1.47
#EXC2_PLAY         6.72       4.25    1.58
#EXC6_MATHCOMP     0.34       5.40    0.06
#EXC10_SPORT      -5.08       8.83   -0.58
#EXC11_UNICORN     6.90       5.96    1.16
#SCL_EXTR_CL       9.99       5.05    1.98
#SCORE_PUBLIC      9.19       5.05    1.82
#QUAL_RECORD       5.90       6.85    0.86
#SCHSEL            2.26       3.25    0.70
#EXAPPLM           2.63       1.02    2.58
#R-squared        43.31       2.44   17.75

################################ One last check: just FAMCONC

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                    "FAMCONC")]
N1 <- NROW(na.omit(T1b)) 
N1 # 16452
N0-N1 #32031 NA's
DEVCON8j <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==1] <- 0
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==2] <- 1
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==1]  <- 5
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==2]  <- 15
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==3]  <- 60
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==4]  <- 150
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==5]  <- 350
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==1] <- 1
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==2] <- 0
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8j$SC25Q10[is.na(DEVCON8j$SC25Q10)]  <- 0
DEVCON8j$SC25Q11[is.na(DEVCON8j$SC25Q11)]  <- 0
DEVCON8j$FUNDMOM <- DEVCON8j$SC25Q11
DEVCON8j$COUNCILMOM <- DEVCON8j$SC25Q10

# Now for the teacher-related variables

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8j$TCM_STUASS[DEVCON8j$SC30Q01==1] <- 1
DEVCON8j$TCM_STUASS[DEVCON8j$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8j$TCM_PEER[DEVCON8j$SC30Q02==1] <- 1
DEVCON8j$TCM_PEER[DEVCON8j$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8j <- merge(DEVCON8j,SC31OUT.rda,by="NEWID")
DEVCON8j$TCH_INCENTV <- rescale(DEVCON8j$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Now for the pedagogical practices-related variables

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8j$ASS_PROG[DEVCON8j$SC18Q01==1] <- 1
DEVCON8j$ASS_PROG[DEVCON8j$SC18Q01==2] <- 0

DEVCON8j$ASS_PROM[DEVCON8j$SC18Q02==1] <- 1
DEVCON8j$ASS_PROM[DEVCON8j$SC18Q02==2] <- 0

DEVCON8j$ASS_SCH[DEVCON8j$SC18Q05==1] <- 1
DEVCON8j$ASS_SCH[DEVCON8j$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8j$STU_FEEDB[DEVCON8j$SC39Q07==1] <- 1
DEVCON8j$STU_FEEDB[DEVCON8j$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8j$COMP_USE[DEVCON8j$SC40Q01==1] <- 1
DEVCON8j$COMP_USE[DEVCON8j$SC40Q01==2] <- 0

DEVCON8j$TXT_BOOK[DEVCON8j$SC40Q02==1] <- 1
DEVCON8j$TXT_BOOK[DEVCON8j$SC40Q02==2] <- 0

# Now for the schools-related variables

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8j$DUM_SMLTOWN <- ifelse(DEVCON8j$SC03Q01==2,1,0)
DEVCON8j$DUM_TOWN    <- ifelse(DEVCON8j$SC03Q01==3,1,0)

DEVCON8j$TOWN <- DEVCON8j$DUM_SMLTOWN+DEVCON8j$DUM_TOWN
DEVCON8j$TOWN[DEVCON8j$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8j$EXC2_PLAY[DEVCON8j$SC16Q02==1] <- 1
DEVCON8j$EXC2_PLAY[DEVCON8j$SC16Q02==2] <- 0

DEVCON8j$EXC6_MATHCOMP[DEVCON8j$SC16Q06==1] <- 1
DEVCON8j$EXC6_MATHCOMP[DEVCON8j$SC16Q06==2] <- 0

DEVCON8j$EXC10_SPORT[DEVCON8j$SC16Q10==1] <- 1
DEVCON8j$EXC10_SPORT[DEVCON8j$SC16Q10==2] <- 0

DEVCON8j$EXC11_UNICORN[DEVCON8j$SC16Q11==1] <- 1
DEVCON8j$EXC11_UNICORN[DEVCON8j$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8j$SCL_EXTR_CL[DEVCON8j$SC20Q01==1] <- 1
DEVCON8j$SCL_EXTR_CL[DEVCON8j$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8j$SCORE_PUBLIC[DEVCON8j$SC19Q01==1] <- 1
DEVCON8j$SCORE_PUBLIC[DEVCON8j$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8j$QUAL_RECORD[DEVCON8j$SC39Q03==1] <- 1
DEVCON8j$QUAL_RECORD[DEVCON8j$SC39Q03==2] <- 0

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8j, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8j.rda") 

R155 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R155
#Estimate Std. Error t value
#(Intercept)   392.40       3.01  130.51
#VIETNAM       125.07       6.61   18.92
#R-squared      27.93       2.48   11.27

R156 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R156 # Vietnam 77.15

R157 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","FAMCONC"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R157
#Estimate Std. Error t value
#(Intercept)     375.28      20.25   18.53
#VIETNAM          53.02       7.27    7.30
#PRESCHOOL        21.51       3.89    5.53
#REPEAT          -32.22       3.17  -10.17
#ST08Q01          -8.42       1.49   -5.67
#ST115Q01         -4.91       2.14   -2.29
#BOOK_N            0.06       0.01    4.64
#PARPRESSURE      10.08       4.22    2.39
#PCGIRLS           4.11      14.25    0.29
#FUNDMOM           0.15       0.06    2.47
#COUNCILMOM       -0.12       0.06   -2.09
#PROPCERT         16.00       6.30    2.54
#SMRATIO          -0.02       0.01   -1.70
#TCSHORT           2.00       1.65    1.21
#TCFOCST          -1.27       1.91   -0.66
#TCM_STUASS       -2.33       7.97   -0.29
#TCM_PEER         -5.17       4.92   -1.05
#TCH_INCENTV      -3.02       2.64   -1.14
#ASS_PROG        -26.73       8.69   -3.08
#ASS_PROM          9.39       5.52    1.70
#ASS_SCH           2.10       7.17    0.29
#STU_FEEDB         2.28       4.85    0.47
#COMP_USE         -0.66       4.89   -0.13
#TXT_BOOK         -7.61       6.90   -1.10
#TOWN             -6.20       3.48   -1.78
#CLSIZE            0.70       0.23    3.04
#COMPWEB          14.10       5.91    2.39
#SCMATEDU          5.02       2.79    1.80
#SCMATBUI          3.06       2.33    1.31
#EXC2_PLAY         5.20       3.98    1.30
#EXC6_MATHCOMP    -0.48       4.95   -0.10
#EXC10_SPORT      -5.33       8.23   -0.65
#EXC11_UNICORN     6.04       5.54    1.09
#SCL_EXTR_CL      10.57       4.87    2.17
#SCORE_PUBLIC      7.43       4.72    1.57
#QUAL_RECORD       9.81       6.65    1.47
#SCHSEL            2.80       3.05    0.92
#FAMCONC          24.89       1.48   16.77
#R-squared        47.31       2.24   21.16

# So in each modification of the sample, when we account for FAMCONC, the Vietnam dummy decreases by 32%, 29% 
# and 31% respectively. That is very significant!








###################################### JUST FOR NOTES / FOR LATER  #############################################

T1exclST57 <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                           "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                           "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                           "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                           "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                           "ST55Q02","EXAPPLM","EXPUREM","FAMCONC","LMINS","MMINS","SMINS")]
N1exclST57 <- NROW(na.omit(T1exclST57)) 
N1exclST57 #11944
N0-N1exclST57 #36539 NA's

T1ST57Q01 <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                          "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                          "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                          "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                          "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                          "ST55Q02","EXAPPLM","EXPUREM","FAMCONC","LMINS","MMINS","SMINS","ST57Q01")]
NST57Q01 <- NROW(na.omit(T1ST57Q01)) 
NST57Q01 #11371
N0-NST57Q01 #37112 NA's

T1ST57Q01Q02 <- DEVCON8a[, c("VIETNAM","ST55Q02","EXAPPLM","EXPUREM",
                          "FAMCONC","LMINS","MMINS","SMINS","ST57Q01","ST57Q02")]
NST57Q01Q02 <- NROW(na.omit(T1ST57Q01Q02)) 
NST57Q01Q02 #15355
N0-NST57Q01Q02 # 33128 NA's

T1ST57Q01Q02Q03 <- DEVCON8a[, c("VIETNAM","ST55Q02","EXAPPLM","EXPUREM",
                             "FAMCONC","LMINS","MMINS","SMINS","ST57Q01","ST57Q02","ST57Q03")]
NST57Q01Q02Q03 <- NROW(na.omit(T1ST57Q01Q02Q03)) 
NST57Q01Q02Q03 #13472
N0-NST57Q01Q02Q03 # 35011 NA's

T1ST57Q01Q02Q03Q04 <- DEVCON8a[, c("VIETNAM","ST55Q02","EXAPPLM","EXPUREM",
                                "FAMCONC","LMINS","MMINS","SMINS","ST57Q01","ST57Q02","ST57Q03","ST57Q04")]
NST57Q01Q02Q03Q04 <- NROW(na.omit(T1ST57Q01Q02Q03Q04)) 
NST57Q01Q02Q03Q04 #12835
N0-NST57Q01Q02Q03Q04 #35648 NA's

T1ST57Q01Q02Q03Q04Q05 <- DEVCON8a[, c("VIETNAM","ST55Q02","EXAPPLM","EXPUREM",
                                   "FAMCONC","LMINS","MMINS","SMINS","ST57Q01","ST57Q02","ST57Q03","ST57Q04",
                                   "ST57Q05")]
NST57Q01Q02Q03Q04Q05 <- NROW(na.omit(T1ST57Q01Q02Q03Q04Q05)) 
NST57Q01Q02Q03Q04Q05 #12703
N0-NST57Q01Q02Q03Q04Q05 #35780 NA's

T1ST57Q01Q02Q03Q04Q05Q06 <- DEVCON8a[, c("VIETNAM","ST55Q02","EXAPPLM","EXPUREM",
                                      "FAMCONC","LMINS","MMINS","SMINS","ST57Q01","ST57Q02","ST57Q03","ST57Q04",
                                      "ST57Q05","ST57Q06")]
NST57Q01Q02Q03Q04Q05Q06 <- NROW(na.omit(T1ST57Q01Q02Q03Q04Q05Q06)) 
NST57Q01Q02Q03Q04Q05Q06 #12608
N0-NST57Q01Q02Q03Q04Q05Q06 #35875 NA's

####################################### ABOVE FOR LATER ########################




