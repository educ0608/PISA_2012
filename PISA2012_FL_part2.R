# PISA2012_FL_part2
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Fryer & Levitt (2004) approach

# Prepared by Elisabeth Sedmik on Wednesday, June 24 2015
# Based on code by Suhas D. Parandekar

# Revised on 07/21/2015

# The following code tries to unravel the secret of Vietnam's outstanding performance on the PISA 2012 assessment. 
# It presents an analytical comparison of possible explanatory factors (as assessed within the PISA 2012 test) 
# of Vietnam's high test score in MATH, comparing 7 other developing countries with Vietnam. The statistical 
# approach taken is a modified dummy variable approach following Fryer and Levitt (2004).

##################################################################################
# Outline:
# 1. GENERATING DATA SET (MERGING, CLEANING) (in part 1)
# 2. DESCRIPTIVE STATISTICS WITH VIETNAM + 7 DEVELOPING COUNTRIES (in part 1)
# 3. PISA SCORES 
# 4. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH 
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

############### 4. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH ################

################################## 4.1 Introduction & baseline regressions ##################################

# The Main idea is quite simple: We build a regression on PISA test scores of 1) a dummy representing Vietnam 
# (which we already created) and 2) a vector of other covariates (which we will create in this section). 
# We start with the dummy variable as the only regressor. Covariates are added to the regression subsequently. 
# In each turn, we analyze how and with which specific addition the (Vietnam) dummy variable decreases/becomes 
# insignificant in association with the test scores. 

# See: Roland Fryer and Steven Levitt 'Understanding the Black-White test score gap in the first two years of school', The Review of Economics and Statistics, 2004, Vol 86, 447-464
# Available to download here: http://www.mitpressjournals.org/doi/pdf/10.1162/003465304323031049 (July 2015)

############# 4.2 Explanatory variables - Students, teachers, pedagogical practices and schools #############

# Useful tip: Take a moment and think about the specific regressors you want to choose (eg. Teaching variables,
# Student Attitutde, Parent Involvement, etc.) that best fit with your conjecture. Since our target is to 'unravel
# a secret', namely why Vietnam did so well in PISA 2012, it requires quite a rigorous and holistic approach, so
# we analyze many potential sources.

# Please see our conceptual scheme. We have arranged possible explanatory variables into four sets of factors and
# working through the codebooks, questionnaires and Technical Manual, carefully decided which variables (or indices)
# to use to proxy these factors: 

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
# --------------TCHBEHSO (Student Orientation, rotated), SC40Q03 (Standardized Math curriculum per school, SC)
# 3.1 Assessment: TCHBEHFA (Formative Assessment, rotated), ASSESS (SC, see p. 309) or better SC18Q01-Q08
# 3.2 Cognitive Activation: COGACT (Cognitive Activation, rotated)
# 3.3 Classroom Management: CLSMAN (Classroom Management, rotated), DISCLIMA (Disciplinary Climante, rotated),
# --------------SC39Q07 (Seeking student feedback, SC)

# 4. SCHOOLS
# 4.0 Type: SC01Q01 (Public or private school, SC), SC02Q02 (Revenues from student fees, SC), SCHSIZE (SC)
# 4.1 Resources: RATCMP15 (Availabilit of resources, SC), COMPWEB (PC for learning connected to the internet, SC),
# --------------CLSIZE (Class Size based on SC05, SC) and/or ST72 (Students asked about class size, rotated), 
# --------------SC16Q01-SC16Q11 (SC) or also MACTIV (Extracurricular Math activities at school, SC else SC16 & SC21) 
# --------------and CREACTIV (Creative extra-curricular activities at school, SC else SC16Q02/06/10/11), 
# --------------SCMATEDU (Quality of educ. resources, SC), SCMATBUI (Quality of Physical Infrastructure, SC),
# --------------SC20Q01 (Additional maths lessons offered, SC)
# 4.2 Leadership: LEADCOM (Framing Schools goal and curriculum, SC), LEADINST (Instructional Leadership, SC), 
# --------------LEADPD (Promoting Development, SC), LEADTCH (Teacher Participation in Leadership, SC),
# --------------SC19Q01 & SC19Q02 (if Student Achievement data is made available, SC), SCHAUTON (School autonomy, SC), 
# --------------TCHPARTI (Teacher participation, SC), SC39Q03 (recording of student/teacher/test data, SC)
# 4.3 Selectivity: SCHSEL (School Selectivity of students, SC) or SC32Q01-SC32Q07 (School selectivity - individual items, SC),
# --------------ABGMATH? (Ability of grouping students between Math classes, SC)
# 4.4 Climate: STUDCLIM (Student aspects of school climate, SC), TEACCLIM (teacher aspects of school climate, SC), 
# --------------TCMORALE (Teacher Morale, SC)

# Our approach follows Fryer and Levitt (2004), in that we are creating a Vietnam dummy variable in the regression,
# that, as you saw in the regressions for Math, Science and Reading above, adds quite substantially to the Vietnamese
# test scores. By regressing on other explanatory variables, we want to ideally have this gap to the other 7 countries reduced;
# ie. explained by certain factors, that will shed light on what is so different in Vietnam vis-a-vis the other countries. 
# We therefore start with a first set of factors, see which variables reduce the gap to the other countries (ie decrease
# the Vietnam dummy), keep those in our regression and move to the next set. After working through all sets cumulatively,
# we hope to find the gap to the other countries substantially decreased. 


########################## 4.2.1 Explanatory Variables - rotated & non-rotated questions #######################

# The student questionnaires administered to students contains a common part and three rotated sections (two of
# which were given to each student). For example, Student Ann would answer the common part, rotation question set 1
# and rotation question set 2, Student Bert would answer the common part, rotation question set 3 and question set 1
# and so on, so that all rotation parts are answered by 2/3 of the sampled students. 
# For more details please read: PISA 2012 technical report, p. 58-61 and p. 376-386

# The school questionnaire administered to school administration does not contain rotated parts. 

# We follow our conceptual scheme, ie first student variables, then teacher variables, and so on; we will first look at the 
# questions in the non-rotated parts (ie. questions that were administered to all students) and the school questionnaire.
# In a later step we will look at the rotated questions. 

############################### 4.2.2 Non-rotated Questions - Student variables #############################

# As we regress on the independent variables from the questionnaires (eg. hours spent learning outside school, etc.), we 
# need to first make sure that we are not faced with too many missing cases per independent variable; otherwise we cannot
# analyze it. We therefore create intermediary files with the specific variables, see how many cases are missing, drop
# the missing cases and add it back to the main file. In the process we will loose quite a few cases and our sensitivity
# analysis later on will work in a reverse order of dropping missing variables to ensure that our findings are coherent.
# For an overview of how to handle missing data in R we recommend: http://www.statmethods.net/input/missingdata.html

# How big is our initital sample size? Two ways to check: 
T0 <- DEVCON8a[, c("VIETNAM")] # create a vector only of "VIETNAM" or any other variable you want to look at
N0<- NROW(na.omit(T0)) # tell R to delete any rows with missing variables 
N0 # display, we find that we have 48483 data points, which is a good sample size to start with

# OR (if we know there are no missing variables, you can simply look at the length of the vector: 
length(DEVCON8a$VIETNAM) # 48483 data points. Carefull, though, this is the length of the vector! so...

length(DEVCON8a$SC40Q03) # will give you 48483 "data points", but ...
TSC40 <- DEVCON8a[, c("SC40Q03")]
NSC40 <- NROW(na.omit(TSC40))
NSC40 # deleting all NA's will give you "only" 47504 data points

# Let's get started with the non-rotated student variables:

# How many non-missing values for all non-rotated student variables (excl SC25)?
T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST09Q01","ST115Q01", "HISEI", "HISCED", "MISCED", "FISCED",
                    "WEALTH", "CULTPOS", "HEDRES", "ST28Q01", "SC24Q01", "PCGIRLS")]
N1 <- NROW(na.omit(T1b)) 
N1 # 34405
N0-N1 # 14078 NAs

# And now we are completing the missing cases, to work on a dataframe of non-missing variabels for these regressors
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

# Some of the PISA items were designed to be used in analyses as single items (for example, gender). However, most questionnaire
# items were designed to be combined in some way in order to measure latent constructs that cannot be observed directly.
# As you can and will see, we are working mostly with these incdices as regressors; however we still need to prepare some
# variables (eg. ST05Q01, ST28Q01, SC25) to be meaningfully used as regressors. Let's quickly do that:

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8b$FEMALE[DEVCON8b$ST04Q01==1] <- 1
DEVCON8b$FEMALE[DEVCON8b$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
# We change three levels into Yes or No, to create a pre-school dummy variable
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==1] <- 0
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==2] <- 1
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==3] <- 1

#ST08Q01
#_______________________________________________________________________________________________________________
# leave as is

#ST09Q01
#_______________________________________________________________________________________________________________
# leave as is 

# ST115Q01 
#______________________________________________________________________________________________________________
# leave as is

#ST28Q01
#______________________________________________________________________________________________________________
# We want do not want to work with categories (1-6) but with actual range of number of books, hence we assign 
# each category its average number of books (please see the student coding file p. 95) 
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==1]  <- 5
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==2]  <- 15
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==3]  <- 60
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==4]  <- 150
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==5]  <- 350
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
# This question asked principal to tick if there is constant, little or largely absent pressure from parents on them to 
# set high academic standards AND the expectation of parents to achieve them. We think this gives a good proxy to measure
# how much pressure 'from home' also rests on the students and hence included it in the student variables.
# We create a dummy variable, whether parental achievement pressure is observed amongst many parents or few/nearly none
DEVCON8b$PARPRESSURE[DEVCON8b$SC24Q01==1] <- 1
DEVCON8b$PARPRESSURE[DEVCON8b$SC24Q01==2] <- 0
DEVCON8b$PARPRESSURE[DEVCON8b$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
# We intentionally did not delete missing variables from the 'SC25' (Parent Participation) variable. In this specific case,
# we replace N/A's with 0's, principles were asked to indicate the percetnage of parents that fall in each category. 
# This may generate some spurious data, but we still find it safe to replace N/A with 0's, indicating that no parents 
# fall in this category. 
DEVCON8b$SC25Q01[is.na(DEVCON8b$SC25Q01)]  <- 0
DEVCON8b$SC25Q02[is.na(DEVCON8b$SC25Q02)]  <- 0
DEVCON8b$SC25Q03[is.na(DEVCON8b$SC25Q03)]  <- 0
DEVCON8b$SC25Q04[is.na(DEVCON8b$SC25Q04)]  <- 0
DEVCON8b$SC25Q05[is.na(DEVCON8b$SC25Q05)]  <- 0
DEVCON8b$SC25Q06[is.na(DEVCON8b$SC25Q06)]  <- 0
DEVCON8b$SC25Q07[is.na(DEVCON8b$SC25Q07)]  <- 0
DEVCON8b$SC25Q08[is.na(DEVCON8b$SC25Q08)]  <- 0
DEVCON8b$SC25Q09[is.na(DEVCON8b$SC25Q09)]  <- 0
DEVCON8b$SC25Q10[is.na(DEVCON8b$SC25Q10)]  <- 0
DEVCON8b$SC25Q11[is.na(DEVCON8b$SC25Q11)]  <- 0
DEVCON8b$SC25Q12[is.na(DEVCON8b$SC25Q12)]  <- 0

# SC25Q01 is quite rich in information, so we create sub-variables
#TIGERMOM
DEVCON8b$TIGERMOM  <- DEVCON8b$SC25Q01+DEVCON8b$SC25Q03
DEVCON8b$TIGERMOM[DEVCON8b$TIGERMOM>100] <- 100 

# Since asking for students behaviour and students progress can be viewed complementary, we add them up (and do not average them).
# We then account for the fact that not more than 100% of parents can logically be a 'TIGERMOM'. You can do that differently, 
# as long as you are consistent with the other created out of the 'SC25' questions.

#VOLUMOM
DEVCON8b$VOLUMOM <- DEVCON8b$SC25Q05+DEVCON8b$SC25Q06+DEVCON8b$SC25Q07+DEVCON8b$SC25Q09+DEVCON8b$SC25Q12
DEVCON8b$VOLUMOM[DEVCON8b$VOLUMOM>100] <- 100 # censoring at 100 should look familiar now

#TEACHMOM
DEVCON8b$TEACHMOM <- DEVCON8b$SC25Q08

#FUNDMOM
DEVCON8b$FUNDMOM <-  DEVCON8b$SC25Q11

#COUNCILMOM
DEVCON8b$COUNCILMOM <- DEVCON8b$SC25Q10

# As you might have noticed, we did not include SCQ02 and SCQ04, which resembles Q01 and Q03 but from the teachers inititative,
# not parents initiative. Theoretically, these could be seen as complementary; ie. progress/behaviour is either discussed on
# the parents initiative or not at all/on the teachers initiative (if deemed necessary), given that there is no formal process
# in place of discussing students behaviour/progress. In fact, if you add these two together, the mean is 80%, which seems
# satisfactory to only focus on parent initiated discussions.

# Now that we have all independent variables ready, let's check the means for each variable in the group of 7 and for Vietnam

mean1A <- t(sapply(DEVCON8b[c("FEMALE", "PRESCHOOL","REPEAT","ST08Q01","ST09Q01","ST115Q01", "HISEI", "MISCED",
                              "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS", "TIGERMOM", "VOLUMOM",
                              "TEACHMOM", "FUNDMOM", "COUNCILMOM")], function(x) 
                                unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# You can see that, on average, students in Vietnam have more pre-schooling (at least one year, PRESCHOOL), 
# higher parental pressure (on schools and student achievement, PARPRESSURE), higher ratio of girls in school (PCGIRLS),
# much higher percentage of parents initiative to discuss students behaviour and/or progress (TIGERMOM), higher parent
# participation in volunterring activities (VOLUMOM), much higher parent participation in assisting in teaching (TEACHMOM),
# much higher assistance of parents in fundraising (FUNDMOM).

# Please refer to the file of "descriptive statistics" to see means and NA's for each variable individually

# Regardless, these are very interesting findings, so let's see how well they explain Vietnam's high performance (and decrease the gap, ie the
# Vietnam dummy variable) along the way:

# First, remember, we have a smaller data set (34405 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R0 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R0

# That gives us ...
#            Estimate Std. Error t value
#(Intercept)   389.97       2.78  140.44
#VIETNAM       124.14       5.69   21.80
#R-squared      28.19       2.42   11.62

# So let's start with the regression of the student related non-rotated variables

R1 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R1 # FEMALE increases the gap
#VIETNAM: 124.22

R2 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R2 # FEMALE increases, PRESCHOOL decreases
#VIETNAM: 114.16

R3 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R3 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases
#VIETNAM: 109.49

R4 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R4 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases
#VIETNAM: 107.08

R5 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R5 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases
#VIETNAM: 107.10

R6 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R6 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases
#VIETNAM: 106.83

R7 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R7 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases
#VIETNAM: 114.88

R8 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                      "MISCED"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R8 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases
#VIETNAM: 115.83

R9 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                      "MISCED", "WEALTH"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R9 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases
#VIETNAM: 116.24

R10 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                      "MISCED", "WEALTH", "CULTPOS"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R10 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases
#VIETNAM: 116.34

R11 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R11 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases
#VIETNAM: 116.50

R12 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R12 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases
#VIETNAM: 116.37

R13 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R13 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases
#VIETNAM: 115.21

R14 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R14 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases
#VIETNAM: 114.19

R15 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS",
                       "TIGERMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R15 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases
#VIETNAM: 114.45

R16 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS",
                       "TIGERMOM", "VOLUMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R16 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases
# VOLUMOM increases
#VIETNAM: 114.66 

R17 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS",
                       "TIGERMOM", "VOLUMOM", "TEACHMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R17 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases
# VOLUMOM increases, TEACHMOM increases
#VIETNAM: 115.83

R18 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS",
                       "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R18 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases
# VOLUMOM increases, TEACHMOM increases, FUNDMOM decreases
#VIETNAM: 110.92

R19 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS",
                       "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R19 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases
# VOLUMOM increases, TEACHMOM increases, FUNDMOM decreases, COUNCILMOM decreases
#VIETNAM: 107.22

# Now testing all 9 variables that decreased the gap

R20 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R20
#Estimate Std. Error t value
#(Intercept)   366.69       7.16   51.25
#VIETNAM        94.52       5.41   17.49
#PRESCHOOL      42.56       4.18   10.17
#REPEAT        -48.57       2.87  -16.89
#ST08Q01        -8.61       1.23   -6.98
#ST115Q01       -4.60       1.73   -2.65
#BOOK_N          0.09       0.01    6.95
#PARPRESSURE    10.92       5.27    2.07
#PCGIRLS        22.79      10.98    2.08
#FUNDMOM         0.26       0.06    4.26
#COUNCILMOM     -0.16       0.06   -2.82
#R-squared      40.97       1.90   21.56

# Now testing all 10 variables that increased the gap

R21 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES",
                       "TIGERMOM","VOLUMOM","TEACHMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R21
#Estimate Std. Error t value
#(Intercept)   424.48       5.44   78.09
#VIETNAM       131.25       4.54   28.91
#FEMALE         -8.32       1.97   -4.23
#ST09Q01       -19.93       2.12   -9.39
#HISEI           0.46       0.05    8.78
#MISCED          2.53       0.64    3.96
#WEALTH          9.89       1.26    7.86
#CULTPOS        -3.89       0.84   -4.64
#HEDRES         12.89       0.97   13.34
#TIGERMOM       -0.02       0.06   -0.32
#VOLUMOM         0.10       0.08    1.18
#TEACHMOM       -0.06       0.07   -0.89
#R-squared      40.64       1.95   20.86

########################## 4.2.2 Non-rotated Questions - student & teacher variables ##########################

# 9 gap decreasing student-related variables: 
# PRESCHOOL, REPEAT, ST08Q01, ST115Q01, BOOK_N, PARPRESSURE, PCGIRLS, FUNDMOM, COUNCILMOM     

# Teacher related variables: 
# Quantity: STRATIO, PROPCERT, PROPQUAL, TCSHORT, SMRATIO
# Quality: TCFOCST, SC30Q01, SC30Q02, SC30Q03, SC30Q04, SC31Q01-Q07 (TCH incentive), SC39Q08, SC35Q02

# How many non-missing values for all non-rotated teacher variables and all gap-decreasing student variables?
T1b <- DEVCON8a[, c("VIETNAM","STRATIO","PROPCERT","PROPQUAL","TCSHORT","SMRATIO","TCFOCST", 
                    "SC30Q01", "SC30Q02", "SC30Q03", "SC30Q04", "SC35Q02", "SC39Q08","SC31Q01", 
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07",
                    "ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS")]
N1 <- NROW(na.omit(T1b)) 
N1 #27447
N0-N1 #21036 NA's
DEVCON8d <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8d$PRESCHOOL[DEVCON8d$ST05Q01==1] <- 0
DEVCON8d$PRESCHOOL[DEVCON8d$ST05Q01==2] <- 1
DEVCON8d$PRESCHOOL[DEVCON8d$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8d$BOOK_N[DEVCON8d$ST28Q01==1]  <- 5
DEVCON8d$BOOK_N[DEVCON8d$ST28Q01==2]  <- 15
DEVCON8d$BOOK_N[DEVCON8d$ST28Q01==3]  <- 60
DEVCON8d$BOOK_N[DEVCON8d$ST28Q01==4]  <- 150
DEVCON8d$BOOK_N[DEVCON8d$ST28Q01==5]  <- 350
DEVCON8d$BOOK_N[DEVCON8d$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8d$PARPRESSURE[DEVCON8d$SC24Q01==1] <- 1
DEVCON8d$PARPRESSURE[DEVCON8d$SC24Q01==2] <- 0
DEVCON8d$PARPRESSURE[DEVCON8d$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8d$SC25Q10[is.na(DEVCON8d$SC25Q10)]  <- 0
DEVCON8d$SC25Q11[is.na(DEVCON8d$SC25Q11)]  <- 0
DEVCON8d$FUNDMOM <-  DEVCON8d$SC25Q11
DEVCON8d$COUNCILMOM <- DEVCON8d$SC25Q10

#SC30Q01, SC30Q02, SC30Q03, SC30Q04
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8d$TCM_STUASS[DEVCON8d$SC30Q01==1] <- 1
DEVCON8d$TCM_STUASS[DEVCON8d$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8d$TCM_PEER[DEVCON8d$SC30Q02==1] <- 1
DEVCON8d$TCM_PEER[DEVCON8d$SC30Q02==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Principal or Senior observation (OBSER)
DEVCON8d$TCM_OBSER[DEVCON8d$SC30Q03==1] <- 1
DEVCON8d$TCM_OBSER[DEVCON8d$SC30Q03==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Inspector/external observer (INSPE)
DEVCON8d$TCM_INSPE[DEVCON8d$SC30Q04==1] <- 1
DEVCON8d$TCM_INSPE[DEVCON8d$SC30Q04==2] <- 0
                    
#SC39Q08
#________________________________________________________________________________________________________________
# Convert into 0 1 variable Quality assurance through teacher mentoring 
DEVCON8d$Teacherment[DEVCON8d$SC39Q08==1] <- 1
DEVCON8d$Teacherment[DEVCON8d$SC39Q08==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
# We will generate an OECD style rasch index that measures incentives - High incentives means high value on this WMLE measure
SC31DAT <- DEVCON8d[,c("NEWID","W_FSCHWT","W_FSTUWT","SC31Q01", "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07")]
write.csv(SC31DAT, "SC31DAT.csv")
# Generated Winsteps output using Winsteps control+data file SC31a.txt
# Person file Output read back into R
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
# We merge back to the PISA data, except now I have to give it a c suffix.
# Merge school and student datasets 
DEVCON8d <- merge(DEVCON8d,SC31OUT.rda,by="NEWID")
DEVCON8d$TCH_INCENTV <- rescale(DEVCON8d$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# First, remember, we have a smaller data set (27477 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R22 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8d,export=FALSE)
R22
# Estimate Std. Error t value
# (Intercept)   386.85       2.83  136.88
# VIETNAM       126.59       6.20   20.41
# R-squared      30.79       2.34   13.13

# We start with the regression of all gap decreasing student variables and then add teacher variables one by one:

R23 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R23
# Estimate Std. Error t value
# (Intercept)   356.78       9.36   38.14
# VIETNAM        96.50       5.31   18.17
# PRESCHOOL      40.57       4.15    9.77
# REPEAT        -45.89       3.46  -13.28
# ST08Q01        -7.14       1.32   -5.41
# ST115Q01       -5.65       1.76   -3.20
# BOOK_N          0.08       0.01    6.54
# PARPRESSURE    13.76       4.36    3.16
# PCGIRLS        40.15      16.04    2.50
# FUNDMOM         0.26       0.06    4.54
# COUNCILMOM     -0.22       0.06   -3.86
# R-squared      43.50       2.10   20.72

R24 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R24 # STRATIO increases gap
#VIETNAM: 96.53

R25 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO", "PROPCERT"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R25 # STRATIO increases, PROPCERT decreases
#VIETNAM: 95.17

R26 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO", "PROPCERT", "PROPQUAL"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R26 # STRATIO increases, PROPCERT decreases, PROPQUAL increases
#VIETNAM:  95.18







































############### OLD REGRESSIONS (with HISCED, FISCED) ####################

R1 <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("VIETNAM",
                         "PRESCHOOL"),
                     weight="W_FSTUWT",
                     data=DEVCON8b,export=FALSE)
R1 # PRESCHOOL decreases the gap
#VIETNAM: 114.10

R2 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R2 # PRESCHOOL, REPEAT decreases the gap
#VIETNAM: 109.49

R3 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R3 # PRESCHOOL, REPEAT, ST08Q01 decreases the gap
#VIETNAM: 107.19 

R4 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R4 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases
#VIETNAM: 107.23 

R5 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R5 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases
#VIETNAM: 107.02

R6 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R6 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases
#VIETNAM: 115.12

R7 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R7 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases,
# HISCED increases
#VIETNAM: 116.07

R8 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                      "MISCED"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R8 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases,
# HISCED increases, MISCED increases
#VIETNAM: 116.20

R9 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                      "MISCED", "FISCED"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R9 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases,
# HISCED increases,MISCED increases, FISCED decreases (slightly)
#VIETNAM: 116.11

R10 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                      "MISCED", "FISCED", "WEALTH"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R10 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases
#VIETNAM: 116.46

R11 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R11 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases
#VIETNAM: 116.57

R12 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R12 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases
#VIETNAM: 116.71

R13 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R13 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases
#VIETNAM: 116.59

R14 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R14 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases
#VIETNAM: 115.46

R15 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                       "PCGIRLS"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R15 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases
#VIETNAM: 114.98

R16 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "TIGERMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R16 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases
#VIETNAM: 115.26

R17 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "TIGERMOM", "VOLUMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R17 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases,
# VOLUMOM increases
#VIETNAM: 115.46

R18 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "TIGERMOM", "VOLUMOM", "TEACHMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R18 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases,
# VOLUMOM increases, TEACHMOM increases
#VIETNAM: 116.62

R19 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R19 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases,
# VOLUMOM increases, TEACHMOM increases, FUNDMOM decreases
#VIETNAM: 111.74

R20 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R20 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases,
# VOLUMOM increases, TEACHMOM increases, FUNDMOM decreases, COUNCILMOM decreases
#VIETNAM: 108.20

# Now testing all 10 variables that decreased the gap

R21 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","FISCED","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R21
#Estimate Std. Error t value
#(Intercept)   355.78       7.35   48.38
#VIETNAM        98.75       5.02   19.67
#PRESCHOOL      36.36       3.94    9.22
#REPEAT        -46.48       2.90  -16.04
#ST08Q01        -9.67       1.19   -8.11
#ST115Q01       -4.92       1.70   -2.90
#FISCED          7.12       0.73    9.69
#BOOK_N          0.07       0.01    6.11
#PARPRESSURE    10.10       5.02    2.01
#PCGIRLS        18.44      10.77    1.71
#FUNDMOM         0.26       0.06    4.51
#COUNCILMOM     -0.16       0.05   -3.09
#R-squared      42.86       1.84   23.32

# Now testing all 10 variables that increased the gap

R22 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "ST09Q01", "HISEI", "HISCED", "MISCED", "WEALTH", "CULTPOS", "HEDRES",
                       "TIGERMOM", "VOLUMOM", "TEACHMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R22
#Estimate Std. Error t value
#(Intercept)   419.25       5.20   80.55
#VIETNAM       131.30       4.51   29.10
#ST09Q01       -18.99       2.10   -9.04
#HISEI           0.47       0.05    8.94
#HISCED         -0.74       0.71   -1.04
#MISCED          3.20       0.74    4.34
#WEALTH          9.93       1.27    7.83
#CULTPOS        -3.87       0.85   -4.57
#HEDRES         12.68       0.94   13.46
#TIGERMOM       -0.02       0.06   -0.35
#VOLUMOM         0.10       0.08    1.19
#TEACHMOM       -0.07       0.07   -0.94
#R-squared      40.46       1.96   20.67
















################ FOR LATER / NOTES ###############################################

# So let"s do our first regression on the student related non-rotated variables

R1 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL","REPEAT","ST08Q01","ST09Q01","ST115Q01","HISEI", "HISCED", "MISCED","FISCED",
                      "WEALTH","CULTPOS","HEDRES","BOOK_N", "PARPRESSURE", "PCGIRLS","TIGERMOM","VOLUMOM",
                      "TEACHMOM", "FUNDMOM", "COUNCILMOM"), 
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R1



# As opposed to our original regression of ...

MATH0 <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("VIETNAM"),
                     weight="W_FSTUWT",
                     data=DEVCON8,export=FALSE)
MATH0

# that gives us ...
# Estimate Std. Error t value
# (Intercept)   383.29       2.50  153.26
# VIETNAM       128.05       5.65   22.68
# R-squared      27.21       2.25   12.07















########################################################### JUST TESTS #################################

length(DEVCON8a$REPEAT)
T1test <- DEVCON8a[, c("VIETNAM", "ST07Q01")]
N1test <- NROW(na.omit(T1test))
N1test

# 47359

T1b <- DEVCON8a[, c("VIETNAM","ST05Q01","ST07Q01","ST08Q01","ST09Q01","ST115Q01","HISEI","MISCED","FISCED", 
                    "WEALTH","CULTPOS","HEDRES","ST28Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 33,500
N0-N1 # 14983 NAs

# to test for the complementary of SC25Q01 and SC25Q02
T1test <- DEVCON8b[, c("VIETNAM", "SC25Q01","SC25Q02", "SC25Q03", "SC25Q04")]
T1test

T1test$FIRST <- T1test$SC25Q01+T1test$SC25Q02
T1test$SECOND <- T1test$SC25Q03+T1test$SC25Q04

mean(T1test$FIRST)
mean(T1test$SECOND)

############ THIS IS JUST NOTES or tests or for later use ##############

# Some of the PISA items were designed to be used in analyses as single items (for example, gender). However, most questionnaire
# items were designed to be combined in some way in order to measure latent constructs that cannot be observed directly.
# We will work closely with these indices as regressors; to see on which questionnaire items they are based on, please
# consult the PISA 2012 Technical Manual, Ch. 16

# We group the rotated questions not according to our conceptual scheme but in which  

TP2 <- DEVCON8a[, c("ANXMAT", "SCMAT", "ATSCHL", "ATTLNACT", "BELONG")]
NP2 <- NROW(na.omit(TP2))
NP2 # 29384

TP3 <- DEVCON8a[, c("ST55Q02", "ST57Q01", "ST57Q02", "ST57Q03", "ST57Q04", "ST57Q05", "ST57Q06", "EXAPPLM", "EXPUREM", "FAMCONC")]
NP3 <- NROW(na.omit(TP3))
NP3 # 15311

TP1 <- DEVCON8a[, c("MATWKETH", "PERSEV", "OPENPS", "INTMAT", "INSTMOT", "SUBNORM", "MATHEFF", "FAILMAT", "MATINTFC", "MATBEH")]
NP1 <- NROW(na.omit(TP1))
NP1 # 28364



############ THIS IS JUST NOTES or tests or for later use ##############

# ST01 - GRADE
# We omit as an analytical variable as we already capture that aspect through Preschool and Age variable


Tanne <- DEVCON8a[, c("NEWID")]
Nanne <- NROW(na.omit(T0))
Nanne


DEVCON8a$SC47Q01
length(DEVCON8a$SC47Q01)
class(DEVCON8a$SC47Q01)

T1b <- DEVCON8a[, c("SC47Q01")]
N1 <- NROW(na.omit(T1b)) # removes all NA's
N1 # 0
N0-N1 # 48483 NAs ---> no Financial Education data for (at least one of these) countries, lets leave it



## FOR LATER:

# Some of the PISA items were designed to be used in analyses as single items (for example, gender). However, most questionnaire
# items were designed to be combined in some way in order to measure latent constructs that cannot be observed directly.
# We will work closely with these indices as regressors; to see on which questionnaire items they are based on, please
# consult the PISA 2012 Technical Manual, Ch. 16


### JUST for TESTING out: with and without deleting NA's ####################

R0 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM", 
                      "COGACT",
                      "CULTPOS",
                      "HOMEPOS",
                      "INTMAT",
                      "INSTMOT"),
                  weight="W_FSTUWT",
                  data=DEVCON8a,export=FALSE)
R0
# Estimate Std. Error t value
# (Intercept)   430.10       4.56   94.34
# VIETNAM       128.94       5.14   25.10
# COGACT          5.02       1.30    3.86
# CULTPOS        -9.98       1.24   -8.03
# HOMEPOS        26.12       1.70   15.34
# INTMAT         -2.52       2.09   -1.20
# INSTMOT         2.84       1.95    1.46
# R-squared      37.46       2.07   18.07

T1b <- DEVCON8a[, c("VIETNAM","COGACT","CULTPOS", "HOMEPOS", "INTMAT", "INSTMOT")]
N1 <- NROW(na.omit(T1b)) 
N1 #15035
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

R0 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM", 
                      "COGACT",
                      "CULTPOS",
                      "HOMEPOS",
                      "INTMAT",
                      "INSTMOT"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R0

# Estimate Std. Error t value
# (Intercept)   430.10       4.56   94.34
# VIETNAM       128.94       5.14   25.10
# COGACT          5.02       1.30    3.86
# CULTPOS        -9.98       1.24   -8.03
# HOMEPOS        26.12       1.70   15.34
# INTMAT         -2.52       2.09   -1.20
# INSTMOT         2.84       1.95    1.46
# R-squared      37.46       2.07   18.07

mean1A <- t(sapply(DEVCON8a[c("COGACT","CULTPOS","HOMEPOS", "INTMAT", "INSTMOT")], function(x) 
  unlist(t.test(x~DEVCON8a$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# COGACT                 0.2961395               -0.3261360  0.000000e+00   47.778674
# CULTPOS               -0.1461614               -0.2364387  3.840693e-09    5.899749
# HOMEPOS               -1.3157310               -1.8168381 3.646975e-208   31.925199
# INTMAT                 0.7187600                0.6919566  3.412589e-02    2.119206
# INSTMOT                0.4258199                0.3689303  3.473342e-05    4.144245

mean1B <- t(sapply(DEVCON8b[c("COGACT","CULTPOS","HOMEPOS", "INTMAT", "INSTMOT")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1B

# estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# COGACT                 0.2515972               -0.3329091 6.802506e-176   30.778643
# CULTPOS               -0.1498809               -0.2633874  2.148253e-05    4.259201
# HOMEPOS               -1.2993032               -1.8394074  4.382984e-77   19.358243
# INTMAT                 0.7369364                0.6680100  1.401528e-04    3.814131
# INSTMOT                0.4056477                0.3180724  3.931470e-06    4.626535

###############################################################################


DEVCON8a$FAILMAT
DEVCON8a

T1b <- DEVCON8a[, c("VIETNAM","COGACT","CULTPOS","HOMEPOS","FAILMAT","EXAPPLM")]
N1 <- NROW(na.omit(T1b)) 
N1 # 0
N0-N1 # 48483
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

R0 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM", 
                      "COGACT",
                      "CULTPOS",
                      "HOMEPOS",
                      "FAILMAT"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R0

# I work on dataframe of non-missing
DEVCON8b <- DEVCON8a[complete.cases(T1b),]





length(DEVCON8$ST74Q01)

is.na(DEVCON8$ST74Q01)

# How many cases ?
T0 <- DEVCON8a[, c("VIETNAM")]
N0<- NROW(na.omit(T0)) # 48483 data points - we have scores on these students for sure

T1b <- DEVCON8a[, c("VIETNAM","ST05Q01","ST07Q01","ST08Q01","ST09Q01","ST115Q01")]
N1 <- NROW(na.omit(T1b)) # removes all NA's
N1 # 43,626
N0-N1 # 4857 NAs

T1b[complete.cases(T1b),]
length(T1b)
T1c <- na.omit(T1b)
length(T1c$VIETNAM) # 43626

# I work on dataframe of non-missing
DEVCON8test <- DEVCON8a[complete.cases(T1b),]
length(DEVCON8test$SC22Q13)


T10 <- DEVCON8a[, c("VIETNAM")]
N0 <- NROW(na.omit(T10))
N0

# Not applicable anymore, see page 376 is.na(DEVCON8a$PERSEV)
# T1z <- DEVCON8a[, c("VIETNAM", "PERSEV")]
# N1 <- NROW(na.omit)
# N1

#How many missing variables? 
# length(which(is.na(DEVCON8a$PERSEV)))



