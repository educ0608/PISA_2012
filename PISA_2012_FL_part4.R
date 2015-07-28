# PISA2012_FL_part4
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Fryer & Levitt (2004) approach

# Prepared by Elisabeth Sedmik on Wednesday, June 24 2015
# Based on code by Suhas D. Parandekar

# Revised on 07/28/2015

# The following code tries to unravel the secret of Vietnam's outstanding performance on the PISA 2012 assessment. 
# It presents an analytical comparison of possible explanatory factors (as assessed within the PISA 2012 test) 
# of Vietnam's high test score in MATH, comparing 7 other developing countries with Vietnam. The statistical 
# approach taken is a modified dummy variable approach following Fryer and Levitt (2004).

##################################################################################
# Outline:
# 1. GENERATING DATA SET (MERGING, CLEANING) (in part 1)
# 2. DESCRIPTIVE STATISTICS WITH VIETNAM + 7 DEVELOPING COUNTRIES (in part 1)
# 3. PISA SCORES (in part 1)
# 4. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH (see also part 2)
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

# 1. STUDENTS
# 1.0 Background: First set: FEMALE, ST05Q01, REPEAT (indexed ST07), ST08Q01, ST09Q01, ST115Q01, Second set: MISCED, HISEI,
# --------------WEALTH, CULTPOS, HEDRES, ST28Q01, ST91Q03 (Problems at home prevent from being good at school, rotated)
# 1.1 Effort: MATWKETH (Math work ethics, rotated), ST55Q02 (Out-of-school lessons in math, rotated), ST57Q01-Q06
# --------------(rotated, recommend to use individual rankings and not PISA 'OUTHOURS' index)
# 1.2 Attitude: PERSEV (rotated), OPENPS (rotated), some of the following from 'Attitudes towards Math': INTMAT, 
# --------------INSTMOT, SUBNORM, MATHEFF, ANXMAT, SCMAT, FAILMAT, MATWKETH, MATINTFC, MATBEH (all rotated), from 
# --------------'Attitudes towards school': ATSCHL (Attitudes towards school: learning outcome, rotated), 
# --------------ATTLNACT (Attitude towards School: learning activities, rotated), BELONG (Sense of belonging to school, rotated)
# 1.3 Preparation/Content: EXAPPLM (Experience with applied math at school, rotated), EXPUREM (Experience with pure
# --------------Math tasks at school, rotated), FAMCONC (same as FAMCON-FOIL, rotated)
# 1.4 Home Support: SC25 (Parent Participation, SC), SC24Q01 (Parental Expectations, SC)
# 1.5 Gender Balance: PCGIRLS (Proportion of girls enrolled at school, SC)

# 2. TEACHERS
# 2.1 Quantity: LMINS (rotated), MMINS (rotated), SMINS (rotated), STRATIO (Teacher-Student ratio, SC), PROPCERT 
# --------------(Proportion of fully certified Teachers, SC), PROPQUAL (Proportion of teachers with an ISCED 5A qualification, SC),
# --------------TCSHORT (Shortage of Teaching Staff, SC), SMRATIO (Student - Mathematics Teacher Ratio, SC)
# 2.2 Quality: STUDREL (Teacher-Student Relations, rotated), 
# --------------TEACHSUP or MTSUP (Teacher support in Math, rotated both in Form 3), SC35Q02 (Professional development on Maths for Math teachers, SC), 
# --------------SC30Q04 (Teacher Monitoring through Inspector observation, SC), SC30Q02 (Teacher monitoring through peer review, SC), 
# --------------SC30Q03 (Teacher Monitoring through staff, SC), SC30Q01 (Teacher practice measured through stdt achvmnt, SC), 
# --------------SC31Q01-Q07 (Teacher incentives through appraisal), ST91Q04 (If had different teacher would try harder, rotated), 
# --------------TCFOCST (Teacher Focus in Math, SC), SC39Q08 (Teacher Mentoring, SC)

# 3. PEDAGOGICAL PRACTICES
# 3.0 General / student-perceived teaching practices: TCHBEHTD (Teacher-directed Instruction, rotated),
# --------------TCHBEHSO (Student Orientation, rotated), SC40Q01-SC40Q03 (Practices in Maths, SC)
# 3.1 Assessment: TCHBEHFA (Formative Assessment, rotated), ASSESS (SC, see p. 309) or better SC18Q01-Q08
# 3.2 Cognitive Activation: COGACT (Cognitive Activation, rotated)
# 3.3 Classroom Management: CLSMAN (Classroom Management, rotated), DISCLIMA (Disciplinary Climante, rotated),
# --------------SC39Q07 (Seeking student feedback, SC)

# 4. SCHOOLS
# 4.0 Type: SC01Q01 (Public or private school, SC), SC02Q02 (Revenues from student fees, SC), SC03Q01 (SC), SCHSIZE (SC)
# 4.1 Resources: RATCMP15 (Availabilit of resources, SC), COMPWEB (PC for learning connected to the internet, SC),
# --------------CLSIZE (Class Size based on SC05, SC) and/or ST72 (Students asked about class size, rotated), 
# --------------SC16Q01-SC16Q11 (SC), 
# --------------SCMATEDU (Quality of educ. resources, SC), SCMATBUI (Quality of Physical Infrastructure, SC),
# --------------SC20Q01 (Additional maths lessons offered, SC)
# 4.2 Leadership: LEADCOM (Framing Schools goal and curriculum, SC), LEADINST (Instructional Leadership, SC), 
# --------------LEADPD (Promoting Development, SC), LEADTCH (Teacher Participation in Leadership, SC),
# --------------SC19Q01 & SC19Q02 (if Student Achievement data is made available, SC), SCHAUTON (School autonomy, SC), 
# --------------TCHPARTI (Teacher participation, SC), SC39Q03 (recording of student/teacher/test data, SC)
# 4.3 Selectivity: SCHSEL (School Selectivity of students, SC) or SC32Q01-SC32Q07 (School selectivity - individual items, SC),
# 4.4 Climate: STUDCLIM (Student aspects of school climate, SC), TEACCLIM (teacher aspects of school climate, SC), 
# --------------TCMORALE (Teacher Morale, SC)

########################## 4.2.1 Explanatory Variables - rotated & non-rotated questions #######################

############################### 4.2.7 Non-rotated & PART 1 rotated questions #############################

# Let's prepare our data set by deleting the missing data for all gap decreasing variables from the non-rotated parts
# AND deleting missing data from all variables we will use from the first (part 1) rotated part

# We will add the rotated variables according to the subsector (students, teachers, etc) they belong to (see our schematic structure)
# and within, as always, in order that they have been asked

# 1. STUDENTS
#-----Effort: MATWKETH
#-----Attitude: INSTMOT, INTMAT, SUBNORM, MATHEFF, FAILMAT, MATINTFC, MATBEH, PERSEV, OPENPS 

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","LEADINST","SC39Q03",
                    "SCHSEL", "MATWKETH","PERSEV","OPENPS","INTMAT","INSTMOT","SUBNORM","MATHEFF","FAILMAT",
                    "MATINTFC","MATBEH")]
N1 <- NROW(na.omit(T1b)) 
N1 #15616
N0-N1 #32867 NA's
DEVCON8i <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==1] <- 0
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==2] <- 1
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==1]  <- 5
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==2]  <- 15
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==3]  <- 60
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==4]  <- 150
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==5]  <- 350
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==1] <- 1
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==2] <- 0
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8i$SC25Q10[is.na(DEVCON8i$SC25Q10)]  <- 0
DEVCON8i$SC25Q11[is.na(DEVCON8i$SC25Q11)]  <- 0
DEVCON8i$FUNDMOM <- DEVCON8i$SC25Q11
DEVCON8i$COUNCILMOM <- DEVCON8i$SC25Q10

# Now for the teacher-related variables

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8i$TCM_STUASS[DEVCON8i$SC30Q01==1] <- 1
DEVCON8i$TCM_STUASS[DEVCON8i$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8i$TCM_PEER[DEVCON8i$SC30Q02==1] <- 1
DEVCON8i$TCM_PEER[DEVCON8i$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8i <- merge(DEVCON8i,SC31OUT.rda,by="NEWID")
DEVCON8i$TCH_INCENTV <- rescale(DEVCON8i$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Now for the pedagogical practices-related variables

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8i$ASS_PROG[DEVCON8i$SC18Q01==1] <- 1
DEVCON8i$ASS_PROG[DEVCON8i$SC18Q01==2] <- 0

DEVCON8i$ASS_PROM[DEVCON8i$SC18Q02==1] <- 1
DEVCON8i$ASS_PROM[DEVCON8i$SC18Q02==2] <- 0

DEVCON8i$ASS_SCH[DEVCON8i$SC18Q05==1] <- 1
DEVCON8i$ASS_SCH[DEVCON8i$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8i$STU_FEEDB[DEVCON8i$SC39Q07==1] <- 1
DEVCON8i$STU_FEEDB[DEVCON8i$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8i$COMP_USE[DEVCON8i$SC40Q01==1] <- 1
DEVCON8i$COMP_USE[DEVCON8i$SC40Q01==2] <- 0

DEVCON8i$TXT_BOOK[DEVCON8i$SC40Q02==1] <- 1
DEVCON8i$TXT_BOOK[DEVCON8i$SC40Q02==2] <- 0

# Now for the schools-related variables

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8i$DUM_SMLTOWN <- ifelse(DEVCON8i$SC03Q01==2,1,0)
DEVCON8i$DUM_TOWN    <- ifelse(DEVCON8i$SC03Q01==3,1,0)

DEVCON8i$TOWN <- DEVCON8i$DUM_SMLTOWN+DEVCON8i$DUM_TOWN
DEVCON8i$TOWN[DEVCON8i$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8i$EXC2_PLAY[DEVCON8i$SC16Q02==1] <- 1
DEVCON8i$EXC2_PLAY[DEVCON8i$SC16Q02==2] <- 0

DEVCON8i$EXC6_MATHCOMP[DEVCON8i$SC16Q06==1] <- 1
DEVCON8i$EXC6_MATHCOMP[DEVCON8i$SC16Q06==2] <- 0

DEVCON8i$EXC10_SPORT[DEVCON8i$SC16Q10==1] <- 1
DEVCON8i$EXC10_SPORT[DEVCON8i$SC16Q10==2] <- 0

DEVCON8i$EXC11_UNICORN[DEVCON8i$SC16Q11==1] <- 1
DEVCON8i$EXC11_UNICORN[DEVCON8i$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8i$SCL_EXTR_CL[DEVCON8i$SC20Q01==1] <- 1
DEVCON8i$SCL_EXTR_CL[DEVCON8i$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8i$SCORE_PUBLIC[DEVCON8i$SC19Q01==1] <- 1
DEVCON8i$SCORE_PUBLIC[DEVCON8i$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8i$QUAL_RECORD[DEVCON8i$SC39Q03==1] <- 1
DEVCON8i$QUAL_RECORD[DEVCON8i$SC39Q03==2] <- 0

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8i, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8i.rda") 

# First, remember, we have a smaller data set (15616 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R103 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM"),
                   weight="W_FSTUWT",
                   data=DEVCON8i,export=FALSE)
R103
#Estimate Std. Error t value
#(Intercept)   396.09       2.92  135.43
#VIETNAM       119.94       6.77   17.72
#R-squared      27.27       2.54   10.75

# Let's try our regression with all gap decreasing variables before we add the rotated parts

R104 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                       "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                       "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                   weight="W_FSTUWT",
                   data=DEVCON8i,export=FALSE)
R104
#Estimate Std. Error t value
#(Intercept)     354.09      20.94   16.91
#VIETNAM          73.32       7.76    9.45
#PRESCHOOL        27.40       3.73    7.35
#REPEAT          -34.74       3.85   -9.02
#ST08Q01          -7.35       1.57   -4.69
#ST115Q01         -4.91       2.03   -2.42
#BOOK_N            0.07       0.01    5.05
#PARPRESSURE      10.20       4.29    2.38
#PCGIRLS           8.33      14.22    0.59
#FUNDMOM           0.17       0.07    2.58
#COUNCILMOM       -0.14       0.06   -2.30
#PROPCERT         17.08       6.75    2.53
#SMRATIO          -0.03       0.01   -2.23
#TCSHORT           3.58       1.79    2.00
#TCFOCST          -1.43       1.93   -0.74
#TCM_STUASS       -1.46       7.72   -0.19
#TCM_PEER         -5.46       5.65   -0.97
#TCH_INCENTV      -3.13       2.58   -1.21
#ASS_PROG        -24.68       7.69   -3.21
#ASS_PROM         13.79       5.86    2.35
#ASS_SCH          -2.48       7.94   -0.31
#STU_FEEDB         2.33       5.01    0.47
#COMP_USE         -0.40       5.38   -0.07
#TXT_BOOK        -10.51       7.03   -1.49
#TOWN             -8.26       3.42   -2.41
#CLSIZE            0.77       0.23    3.34
#COMPWEB          11.32       6.25    1.81
#SCMATEDU          5.86       3.06    1.92
#SCMATBUI          4.40       2.50    1.76
#EXC2_PLAY         8.22       3.97    2.07
#EXC6_MATHCOMP     0.39       5.32    0.07
#EXC10_SPORT      -4.21       9.91   -0.43
#EXC11_UNICORN     6.90       5.56    1.24
#SCL_EXTR_CL      11.32       5.22    2.17
#SCORE_PUBLIC     10.26       4.86    2.11
#QUAL_RECORD       8.49       6.40    1.33
#SCHSEL            0.63       3.10    0.20
#R-squared        43.03       2.33   18.49

# So with all the gap decreasing non rotated variables, the Vietnam dummy goes down to 73.32

R105 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R105 # MATWKETH decreases
#VIETNAM: 73.05

R106 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R106 # MATWKETH decreases, INSTMOT decreases
#VIETNAM: 71.87

R107 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R107 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases
#VIETNAM: 71.26

R108 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R108 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases, SUBNORM decreases (drastically!)
#VIETNAM: 64.09

R109 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM","MATHEFF"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R109 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases, SUBNORM decreases (drastically!), MATHEF decreases
#VIETNAM: 61.70

R110 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM","MATHEFF","FAILMAT"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R110 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases, SUBNORM decreases (drastically!), MATHEF decreases
# FAILMAT increases
#VIETNAM: 63.22

R111 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM","MATHEFF","FAILMAT","MATINTFC"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R111 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases, SUBNORM decreases (drastically!), MATHEF decreases
# FAILMAT increases, MATINTFC decreases
#VIETNAM: 61.86

R112 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM","MATHEFF","FAILMAT","MATINTFC","MATBEH"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R112 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases, SUBNORM decreases (drastically!), MATHEF decreases
# FAILMAT increases, MATINTFC decreases, MATBEH increases
#VIETNAM: 62.17

R113 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM","MATHEFF","FAILMAT","MATINTFC","MATBEH","PERSEV"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R113 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases, SUBNORM decreases (drastically!), MATHEF decreases
# FAILMAT increases, MATINTFC decreases, MATBEH increases, PERSEV decreases
#VIETNAM: 61.57

R114 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM","MATHEFF","FAILMAT","MATINTFC","MATBEH","PERSEV","OPENPS"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R114 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases, SUBNORM decreases (drastically!), MATHEF decreases
# FAILMAT increases, MATINTFC decreases, MATBEH increases, PERSEV decreases, OPENPS increases
#VIETNAM: 64.40

# Let's check all the gap-decreasing variables together:

R115 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM","MATHEFF","MATINTFC","PERSEV"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R115
#Estimate Std. Error t value
#(Intercept)     360.09      19.38   18.58
#VIETNAM          59.91       6.92    8.66
#PRESCHOOL        22.54       3.48    6.48
#REPEAT          -30.69       3.67   -8.37
#ST08Q01          -5.88       1.60   -3.68
#ST115Q01         -4.28       1.90   -2.25
#BOOK_N            0.05       0.01    4.14
#PARPRESSURE       9.13       3.97    2.30
#PCGIRLS           7.59      13.09    0.58
#FUNDMOM           0.16       0.06    2.76
#COUNCILMOM       -0.14       0.05   -2.64
#PROPCERT         16.31       6.18    2.64
#SMRATIO          -0.02       0.01   -2.22
#TCSHORT           2.14       1.63    1.31
#TCFOCST          -0.32       1.72   -0.19
#TCM_STUASS       -1.38       7.32   -0.19
#TCM_PEER         -4.89       5.28   -0.93
#TCH_INCENTV      -2.97       2.24   -1.33
#ASS_PROG        -16.29       7.75   -2.10
#ASS_PROM         10.38       5.49    1.89
#ASS_SCH           1.22       7.61    0.16
#STU_FEEDB         2.07       4.50    0.46
#COMP_USE         -0.65       5.04   -0.13
#TXT_BOOK         -7.22       6.31   -1.14
#TOWN             -5.99       3.18   -1.88
#CLSIZE            0.69       0.21    3.26
#COMPWEB          12.97       5.71    2.27
#SCMATEDU          4.75       2.73    1.74
#SCMATBUI          3.17       2.27    1.40
#EXC2_PLAY         5.87       3.72    1.58
#EXC6_MATHCOMP    -0.82       4.81   -0.17
#EXC10_SPORT      -4.65       9.15   -0.51
#EXC11_UNICORN     7.06       4.82    1.46
#SCL_EXTR_CL      11.19       4.99    2.24
#SCORE_PUBLIC     10.74       4.37    2.45
#QUAL_RECORD      11.97       6.21    1.93
#SCHSEL            1.64       2.88    0.57
#MATWKETH         -9.89       1.58   -6.25
#INSTMOT           5.76       1.29    4.48
#INTMAT           -3.37       1.88   -1.79
#SUBNORM         -12.14       0.90  -13.54
#MATHEFF          29.90       2.05   14.58
#MATINTFC          8.44       0.85    9.94
#PERSEV            4.38       1.10    3.96
#R-squared        49.61       2.08   23.89

# Let's check all gap increasing variables together (of course with the gap decreasing non-rotated part, as always):

R116 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","FAILMAT","MATBEH","OPENPS"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R116
#Estimate Std. Error t value
#(Intercept)     353.47      21.36   16.55
#VIETNAM          80.05       7.71   10.38
#PRESCHOOL        26.53       3.69    7.18
#REPEAT          -34.06       3.92   -8.68
#ST08Q01          -6.97       1.57   -4.44
#ST115Q01         -3.94       2.03   -1.94
#BOOK_N            0.07       0.01    4.80
#PARPRESSURE       9.77       4.29    2.28
#PCGIRLS           7.39      14.55    0.51
#FUNDMOM           0.17       0.07    2.61
#COUNCILMOM       -0.15       0.06   -2.45
#PROPCERT         17.96       6.73    2.67
#SMRATIO          -0.02       0.01   -1.98
#TCSHORT           3.71       1.78    2.08
#TCFOCST          -1.46       1.92   -0.76
#TCM_STUASS       -1.77       7.75   -0.23
#TCM_PEER         -4.25       5.61   -0.76
#TCH_INCENTV      -3.18       2.56   -1.24
#ASS_PROG        -25.33       7.81   -3.24
#ASS_PROM         13.98       5.99    2.34
#ASS_SCH          -2.61       8.19   -0.32
#STU_FEEDB         2.64       4.97    0.53
#COMP_USE         -0.30       5.41   -0.06
#TXT_BOOK         -9.45       7.19   -1.31
#TOWN             -7.80       3.42   -2.28
#CLSIZE            0.75       0.23    3.27
#COMPWEB          11.54       6.21    1.86
#SCMATEDU          6.16       3.06    2.01
#SCMATBUI          4.01       2.53    1.58
#EXC2_PLAY         7.79       3.96    1.97
#EXC6_MATHCOMP    -0.50       5.33   -0.09
#EXC10_SPORT      -3.82       9.99   -0.38
#EXC11_UNICORN     6.61       5.62    1.18
#SCL_EXTR_CL      11.41       5.22    2.19
#SCORE_PUBLIC     10.42       4.91    2.12
#QUAL_RECORD       9.82       6.34    1.55
#SCHSEL            0.92       3.14    0.29
#FAILMAT          -7.11       1.13   -6.29
#MATBEH           -4.20       1.67   -2.52
#OPENPS            9.38       1.25    7.49
#R-squared        44.16       2.30   19.20


