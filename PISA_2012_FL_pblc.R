# PISA2012_FL
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Fryer & Levitt (2004) approach

# Prepared by Elisabeth Sedmik on Wednesday, June 24 2015
# Based on code by Suhas D. Parandekar

# Revised on 07/13/2015

# The following code tries to unravel the secret of Vietnam's outstanding performance on the PISA 2012 assessment. 
# It presents an analytical comparison of possible explanatory factors (as assessed within the PISA 2012 test) 
# of Vietnam's high test score in MATH, comparing 7 other developing countries with Vietnam. The statistical 
# approach taken is a modified dummy variable approach following Fryer and Levitt (2004).

##################################################################################
# Outline:
# 1. GENERATING DATA SET (MERGING, CLEANING) 
# 2. DESCRIPTIVE STATISTICS WITH VIETNAM + 7 DEVELOPING COUNTRIES
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


################################ 1. GENERATING DATA SET (MERGING, CLEANING) #################################

# We generate new, merged data sets based on the student data set and school data set for the 8 countries
# that have a Per Capita GDP below $10,000 in PPP for 2010, based on OECD-PISA Table Table IV.3.2 
# OECD-PISA Report: "PISA 2012 Results: What Makes a School Successful?" (Volume IV, Chapter 3, Table IV.3.2)
# Original data downloaded from: http://pisa2012.acer.edu.au/downloads.php (txt files, spss or sas)

student.rda <- read.dta("C:/Users/xxx/Desktop/PISAlatestversions/RFiles/PISA_2012/stu.dta")
school.rda <- read.dta("C:/Users/xxx/Desktop/PISAlatestversions/RFiles/PISA_2012/sch.dta")

# Please refer to the questionnaires, codebooks and the technical manual (pages 396-398) about the specific
# variables included in the data files and their abbreviations  
# for an overview of included variables see: http://www.oecd.org/pisa/pisaproducts/pisa2012technicalreport.htm
# and for abbreviations see the student and school codebooks: http://pisa2012.acer.edu.au/downloads.php

# We will filter for the 8 developing countries per country by school id and merge into one file ("DEVCON8"):

# Albania 

ALB_T <- filter(student.rda, cnt == "ALB") # filter by "cnt" and create a new student file just for 'Albania'
ALB_S <- filter(school.rda, cnt == "ALB") # filter by "cnt" and create a new school file just for 'Albania'
ALB_P <- merge(ALB_T, ALB_S, by = "schoolid") # merge both files into one file just for 'Albania'
ALB_P$cnt <- ALB_P$cnt.x # we duplicate "cnt.x" as "cnt" (it is only called "cnt.x" since we merged ALB_S and ALB_T, not that in ALB_S and ALB_T it was called "cnt")
ALB_P$cnt.x <- NULL # we delete the column "cnt.x"
ALB_P$subnatio <- ALB_P$subnatio.x # we duplicate "subnatio.x" as "subnatio" (to have the same nomenclature as in the original data!)
ALB_P$subnatio.x <- NULL # we delete the column "subnatio.x"
ALB_P$stratum <- ALB_P$stratum.x # same as above
ALB_P$stratum.x <- NULL # same as above
ALB_P$oecd <- ALB_P$oecd.x # same as above
ALB_P$oecd.x <- NULL # same as above
ALB_P$nc <- ALB_P$nc.x  # same as above
ALB_P$nc.x <- NULL # same as above

# We want to have a unique identifier number per student per school per country. Note, that for all countries, numbering of school and student id's
# starts at 1; so we introduce "NEWID" to create for a cumulative list spanning all 8 countries:

ALB_P$COUNTRY <- 1 # We numerate each country alphabetically, starting with Albania = 1, Colombia = 2, etc.
ALB_P$NEWID <- (ALB_P$COUNTRY*10000000)+((as.numeric(ALB_P$schoolid))*10000)+(as.numeric(ALB_P$stidstd))

# Useful tip: these are very large data files. to double check if the new columns have been created, do not load 
# the new file ALB_P into the editor window, as it will only show up until a certain column or take very long to load. 
# Instead, type in "ALB_P" into the Console to see all columns (and the first 10 rows per column). 

# Now we repeat the same process for the remaining 7 countries:

# Colombia

COL_T <- filter(student.rda, cnt == "COL")
COL_S <- filter(school.rda, cnt == "COL")
COL_P <- merge(COL_T, COL_S, by = "schoolid")
COL_P$cnt <- COL_P$cnt.x
COL_P$cnt.x <- NULL
COL_P$subnatio <- COL_P$subnatio.x
COL_P$subnatio.x <- NULL
COL_P$stratum <- COL_P$stratum.x
COL_P$stratum.x <- NULL
COL_P$oecd <- COL_P$oecd.x
COL_P$oecd.x <- NULL
COL_P$nc <- COL_P$nc.x
COL_P$nc.x <- NULL
COL_P$COUNTRY <-2
COL_P$NEWID <- (COL_P$COUNTRY*10000000)+((as.numeric(COL_P$schoolid))*10000)+(as.numeric(COL_P$stidstd)) 

# The command 'as.numeric' converts individual columns to numeric variables 
# Useful tip: to test for the class of variables in the column 'schoolid', type in 'class(COL_P$schoolid)'

# Indonesia

IDN_T <- filter(student.rda, cnt == "IDN")
IDN_S <- filter(school.rda, cnt == "IDN")
IDN_P <- merge(IDN_T, IDN_S, by = "schoolid")
IDN_P$cnt <- IDN_P$cnt.x
IDN_P$cnt.x <- NULL
IDN_P$subnatio <- IDN_P$subnatio.x
IDN_P$subnatio.x <- NULL
IDN_P$stratum <- IDN_P$stratum.x
IDN_P$stratum.x <- NULL
IDN_P$oecd <- IDN_P$oecd.x
IDN_P$oecd.x <- NULL
IDN_P$nc <- IDN_P$nc.x
IDN_P$nc.x <- NULL
IDN_P$COUNTRY <-3
IDN_P$NEWID <- (IDN_P$COUNTRY*10000000)+((as.numeric(IDN_P$schoolid))*10000)+(as.numeric(IDN_P$stidstd))

# Jordan

JOR_T <- filter(student.rda, cnt == "JOR")
JOR_S <- filter(school.rda, cnt == "JOR")
JOR_P <- merge(JOR_T, JOR_S, by = "schoolid")
JOR_P$cnt <- JOR_P$cnt.x
JOR_P$cnt.x <- NULL
JOR_P$subnatio <- JOR_P$subnatio.x
JOR_P$subnatio.x <- NULL
JOR_P$stratum <- JOR_P$stratum.x
JOR_P$stratum.x <- NULL
JOR_P$oecd <- JOR_P$oecd.x
JOR_P$oecd.x <- NULL
JOR_P$nc <- JOR_P$nc.x
JOR_P$nc.x <- NULL
JOR_P$COUNTRY <-4
JOR_P$NEWID <- (JOR_P$COUNTRY*10000000)+((as.numeric(JOR_P$schoolid))*10000)+(as.numeric(JOR_P$stidstd))

# Peru

PER_T <- filter(student.rda, cnt == "PER")
PER_S <- filter(school.rda, cnt == "PER")
PER_P <- merge(PER_T, PER_S, by = "schoolid")
PER_P$cnt <- PER_P$cnt.x
PER_P$cnt.x <- NULL
PER_P$subnatio <- PER_P$subnatio.x
PER_P$subnatio.x <- NULL
PER_P$stratum <- PER_P$stratum.x
PER_P$stratum.x <- NULL
PER_P$oecd <- PER_P$oecd.x
PER_P$oecd.x <- NULL
PER_P$nc <- PER_P$nc.x
PER_P$nc.x <- NULL
PER_P$COUNTRY <-5
PER_P$NEWID <- (PER_P$COUNTRY*10000000)+((as.numeric(PER_P$schoolid))*10000)+(as.numeric(PER_P$stidstd))

# Thailand

THA_T <- filter(student.rda, cnt == "THA")
THA_S <- filter(school.rda, cnt == "THA")
THA_P <- merge(THA_T, THA_S, by = "schoolid")
THA_P$cnt <- THA_P$cnt.x
THA_P$cnt.x <- NULL
THA_P$subnatio <- THA_P$subnatio.x
THA_P$subnatio.x <- NULL
THA_P$stratum <- THA_P$stratum.x
THA_P$stratum.x <- NULL
THA_P$oecd <- THA_P$oecd.x
THA_P$oecd.x <- NULL
THA_P$nc <- THA_P$nc.x
THA_P$nc.x <- NULL
THA_P$COUNTRY <-6
THA_P$NEWID <- (THA_P$COUNTRY*10000000)+((as.numeric(THA_P$schoolid))*10000)+(as.numeric(THA_P$stidstd))

# Tunisia

TUN_T <- filter(student.rda, cnt == "TUN")
TUN_S <- filter(school.rda, cnt == "TUN")
TUN_P <- merge(TUN_T, TUN_S, by = "schoolid")
TUN_P$cnt <- TUN_P$cnt.x
TUN_P$cnt.x <- NULL
TUN_P$subnatio <- TUN_P$subnatio.x
TUN_P$subnatio.x <- NULL
TUN_P$stratum <- TUN_P$stratum.x
TUN_P$stratum.x <- NULL
TUN_P$oecd <- TUN_P$oecd.x
TUN_P$oecd.x <- NULL
TUN_P$nc <- TUN_P$nc.x
TUN_P$nc.x <- NULL
TUN_P$COUNTRY <-7
TUN_P$NEWID <- (TUN_P$COUNTRY*10000000)+((as.numeric(TUN_P$schoolid))*10000)+(as.numeric(TUN_P$stidstd))

# Vietnam

VNM_T <- filter(student.rda, cnt == "VNM")
VNM_S <- filter(school.rda, cnt == "VNM")
VNM_P <- merge(VNM_T, VNM_S, by = "schoolid")
VNM_P$cnt <- VNM_P$cnt.x
VNM_P$cnt.x <- NULL
VNM_P$subnatio <- VNM_P$subnatio.x
VNM_P$subnatio.x <- NULL
VNM_P$stratum <- VNM_P$stratum.x
VNM_P$stratum.x <- NULL
VNM_P$oecd <- VNM_P$oecd.x
VNM_P$oecd.x <- NULL
VNM_P$nc <- VNM_P$nc.x
VNM_P$nc.x <- NULL
VNM_P$COUNTRY <-8
VNM_P$NEWID <- (VNM_P$COUNTRY*10000000)+((as.numeric(VNM_P$schoolid))*10000)+(as.numeric(VNM_P$stidstd))

DEVCON8 <- rbind(ALB_P,COL_P,IDN_P,JOR_P,PER_P,THA_P,TUN_P,VNM_P) # combine all country specific files into the "DEVCON8" file, thanks to "dyplr" package

# Finally, we add a Vietnam dummy variable to "DEVCON8", which we will need to produce descriptive statistics 
# and for the modified Fryer-Levitt analysis

DEVCON8$VIETNAM[DEVCON8$COUNTRY==8] <- 1 # dummy takes value = 1, if the country is Vietnam
DEVCON8$VIETNAM[DEVCON8$COUNTRY!=8] <- 0 # dummy takes value = 0, if the country is not Vietnam

# The 'intsvy' package (the most important one for the PISA analysis) requires variable names to be in upper case; 
# you might have noticed how we put all newly created variables already in upper case; no we convert all 

names(DEVCON8) <- toupper(names(DEVCON8)) 

# This will be the main file to work off so you might want to save it

save(DEVCON8, file = "C:/Users/xxx/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8.rda") 

# Get summary of the DEVCON file (variables, mean, sd, etc.) in LATEX via use of the 'stargazer' package
# This will give you a neat ouptut of all variables and you can use it as an overview of all variables in the data set

stargazer(DEVCON8,
          type="latex", out="C:/Users/xxx/Desktop/PISAlatestversions/WPLatex/DescriptiveStats/DEVCON8.tex",
          style="default",
          align=TRUE,
          digit.separator="",
          summary=TRUE)

# Useful tip: read up on the specifics of the stargazer command, especially since you might need to load additional packages in Latex


###################### 2. DESCRIPTIVE STATISTICS WITH VIETNAM + 7 DEVELOPING COUNTRIES ######################

# We generate Kernel plots (comparison of non-parametric univariate density estimates) for Math, Science and Reading scores 
# comparing Vietnam to the group of 7 developing countries as identified above and the OECD Average. 

# The 'sm' package (used for locally smoothed density estimates) requires that country names are converted into factors, 
# (ie categorical variables) so we create a new column (VNM.f), which contains either 'Vietnam' or 'Group of 7' as a factor. 
# Useful resource on the use of factors in R: http://www.stat.berkeley.edu/~s133/factors.html

DEVCON8$VNM.f <- factor(DEVCON8$VIETNAM, levels=c(0,1),labels = c("GROUP OF 7", "Vietnam"))

# For the Kernel plots we are using the 'sm.density.compare' function of the 'sm' package, because it allows
# to superimpose the kernel density plots of two or more groups; in our case 'Vietnam' and 'Group of 7'

# Kernel density plots for MATH (Vietnam and Group of 7)

sm.density.compare(DEVCON8$PV3MATH,lwd=2,DEVCON8$VNM.f,lty=c(2,1),col=c("darkgreen","red"),
                   nbins=0,h=35,xlab="Mathematics Score",weights=DEVCON8$W_FSTUWT)
title(main="Kernel Density PISA 2012 Mathematics Scores")

# Two important points:

# 1. You will notice that from now on, that we will frequently use 'W_FSTUWT'. It stands for the
# final student weight (ie. size of the student body in the respective country), that needs to be applied 
# to the PVs (Plausible Values) to correctly compute the PISA score.
# The methodology can be found here: http://www.oecd.org/pisa/pisaproducts/pisa2012technicalreport.htm (Chapter 8)

# 2. For the numeric vector (PV3MATH) we randomly pick one of the 5 PVs for the Math Score,
# since we are 'smoothing' the values anyway (through Kernel). Plausible values are intermediate values
# provided to obtain consistent estimates of population parameters. Ideally, we would analyse all five PV's, 
# average the result and significance tests adjusting for variation between the five sets of results computed. 
# However, for the purpose of producing descriptive statistics, we deem our approach sufficient. 
# You can double check with different PV's and will notice only slight differences.
# Please refer to the PISA 2012 technical report (cited above) page 147. 

# Note: if 'weights' is used, the number of bins must be set to 0 ('nbins=0') 
# Note: we select bandwith as h=35. There are different approaches to selecting bandwith and you can adjust 'h' to see
# which bin size you prefer to best visualize the density plots. 

# We draw a reference line for the OECD average

abline(v=494, lty=5, col="grey",lwd=2)
axis(1,at=494,labels=494)
legend(-150,0.003, levels(DEVCON8$VNM.f),lty=c(2,1), lwd=2, col=c("darkgreen","red"),bty="n")
text(-50,0.0050, labels="OECD Average",pos=4)
arrows(400, 0.0050, 494, 0.0050)

# Note: The PISA scores have been standardized with an international mean of 500 and a standard deviation of 100.
# The 'OECD Average' scores can be found here: http://www.oecd.org/pisa/keyfindings/pisa-2012-results-overview.pdf (page 5)
# I took the OECD Average not the international (overall PISA) average ... do you agree?

# Kernel density plots for SCIENCE (Vietnam and Group of 7)

sm.density.compare(DEVCON8$PV1SCIE,lwd=2,DEVCON8$VNM.f,lty=c(2,1),col=c("blue","red"),
                   nbins=0,h=35,xlab="Science Score",weights=DEVCON8$W_FSTUWT)
title(main="Kernel Density PISA 2012 Science Scores")

abline(v=501, lty=5, col="grey",lwd=2)
axis(1,at=501,labels=501)
legend(-150,0.003, levels(DEVCON8$VNM.f),lty=c(2,1), lwd=2, col=c("blue","red"),bty="n")
text(-50,0.0051, labels="OECD Average",pos=4)
arrows(400, 0.0051, 501, 0.0051)

# Kernel density plots for READING (Vietnam and Group of 7)

sm.density.compare(DEVCON8$PV5READ,lwd=2,DEVCON8$VNM.f,lty=c(2,1),col=c("purple","red"),
                   nbins=0,h=35,xlab="Reading Score",weights=DEVCON8$W_FSTUWT)
title(main="Kernel Density PISA 2012 Reading Scores")

# Draw reference line for OECD average
abline(v=496, lty=5, col="grey",lwd=2)
axis(1,at=496,labels=496)
legend(-150,0.003, levels(DEVCON8$VNM.f),lty=c(2,1), lwd=2, col=c("purple","red"),bty="n")
text(-50,0.0052, labels="OECD Average",pos=4)
arrows(400, 0.0052, 496, 0.0052)

# Finally note that there are other ways to create Kernel Density Plots; eg for Kernel Density of only
# one variable/group (above we compared two groups: group of 7 & Vietnam) you do not need the sm package and can simply
# code 'plot(density(DEVCON8$PV3MATH))' which will give you the Kernel density for the Math score for the group of
# all 8 countries. 
# For a very basic overview we recommend: http://www.statmethods.net/graphs/density.html 

# We will keep DEVCON8 as a master file, so we create a new version for the subsequent steps

save(DEVCON8, file = "C:/Users/xxx/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8.rda")


############################################## 3. PISA SCORES  ##############################################

# The most important tip from now on: work closely with the PISA 2012 technical manual AND get familiar with
# functions of the 'intsvy' package (which does all the rigorous calculations of PISA PV's etc. for you!)

# To highlight this, look how the intsvy packages helps you find the Mean PISA Scores for Vietnam from the PVs:

meanMATH <- pisa.mean.pv(pvlabel="MATH",by="VIETNAM", data=DEVCON8, weight="W_FSTUWT")
meanMATH
meanSCIE <- pisa.mean.pv(pvlabel="SCIE",by="VIETNAM", data=DEVCON8, weight="W_FSTUWT")
meanSCIE
meanREAD <- pisa.mean.pv(pvlabel="READ",by="VIETNAM", data=DEVCON8, weight="W_FSTUWT")
meanREAD

# Similarly, you can find the mean Math scores for all 8 countries through sorting by "COUNTRY" 
# (remember we assigned countries numerical values from 1 to 8 alphabetically)

meanCNT <- pisa.mean.pv(pvlabel="MATH", by="CNT", data=DEVCON8, weight="W_FSTUWT")
meanCNT


############### 4. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH ################

##################### 4.1 Dummy variable regression - Introduction & baseline regressions ###################

# The Main idea is quite simple: We build a regression on PISA test scores of 1) a dummy representing Vietnam 
# (which we already created) and 2) a vector of other covariates (which we will create in this section). 
# We start with the dummy variable as the only regressor. Covariates are added to the regression subsequently. 
# In each turn, we analyze how and with which specific addition the (Vietnam) dummy variable decreases/becomes 
# insignificant in association with the test scores. 

# See: Roland Fryer and Steven Levitt 'Understanding the Black-White test score gap in the first two years of school', The Review of Economics and Statistics, 2004, Vol 86, 447-464
# Available to download here: http://www.mitpressjournals.org/doi/pdf/10.1162/003465304323031049 (July 2015)

# So lets do the baseline regressions with help of the 'intsvy' package. The command 'pisa.reg.pv' performs linear 
# regression analysis (OLS) with plausible values and replicate weights.

# MATH

MATH0 <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("VIETNAM"),
                     weight="W_FSTUWT",
                     data=DEVCON8,export=FALSE)
MATH0

# SCIENCE

SCIE0 <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("VIETNAM"),
                     weight="W_FSTUWT",
                     data=DEVCON8,export=FALSE)
SCIE0

# READING

READ0 <- pisa.reg.pv(pvlabel="READ", 
                     x=c("VIETNAM"),
                     weight="W_FSTUWT",
                     data=DEVCON8,export=FALSE)
READ0

# The difference between mean score values is the coefficient value on the dummy. For example, the intercept (383.29) +
# VIETNAM (128.05) = 511.34: the Mean Math Score for Vietnam.

DEVCON8a <- DEVCON8
save(DEVCON8a, file = "C:/Users/xxx/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8a.rda") 

############# 4.2 Explanatory variables - Students, teachers, pedagogical practices and schools #############

# Useful tip: Take a moment and think about the specific regressors you want to choose (eg. Teaching variables,
# Student Attitutde, Parent Involvement, etc.) that best fit with your conjecture. Since our target is to 'unravel
# a secret', namely why Vietnam did so well in PISA 2012, it requires quite a rigorous and holistic approach, so
# we analyze many potential sources.

# Please see our conceptual scheme. We have arranged possible explanatory variables into four sets of factors and
# working through the codebooks, questionnaires and Technical Manual, carefully decided which variables (or indices)
# to use to proxy these factors: 

# 1. STUDENTS
# 1.0 Background: First set: ST05, REPEAT (indexed ST07), ST08, ST09, ST115, Second set: MISCED, FISCED, HISEI,
# --------------HISCED, WEALTH, CULTPOS, HEDRES, ST28Q01, ST91Q03 (Problems at home prevent from being good at school, rotated)
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
# 2.2 Quality: STUDREL (Teacher-Student Relations, rotated) and/or response from headmaster SC22Q10 (Poor student-teacher relations, SC), 
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

########################## 4.2.2 Explanatory Variables - rotated & non-rotated questions #######################

# The student questionnaires administered to students contains a common part and three rotated sections (two of
# which were given to each student). For example, Student Ann would answer the common part, rotation question set 1
# and rotation question set 2, Student Bert would answer the common part, rotation question set 3 and question set 1
# and so on, so that all rotation parts are answered by 2/3 of the sampled students. 
# For more details please read: PISA 2012 technical report, p. 58-61 and p. 376-386

# The school questionnaire administered to school administration does not contain rotated parts. 

# We follow our conceptual scheme, ie first student variables, then teacher variables, and so on; we will need to split sub-
# sections to account for the fact that some of the independent variables are in different rotated parts (1-3). No worries
# for now, this will become very clear as we go along.

# Some of the PISA items were designed to be used in analyses as single items (for example, gender). However, most questionnaire
# items were designed to be combined in some way in order to measure latent constructs that cannot be observed directly.
# We will work closely with these indices as regressors; to see on which questionnaire items they are based on, please
# consult the PISA 2012 Technical Manual, Ch. 16

############################### 4.2.2 Explanatory Variables - Student variables #############################

# As we regress on the independent variables from the questionnaires (eg. hours spent learning outside school, etc.), we 
# need to first make sure that we are not faced with too many missing cases per independent variable; otherwise we cannot
# analyze it. We therefore create intermediary files with the specific variables, see how many cases are missing, drop
# the missing cases and add it back to the main file. In the process we will loose quite a few cases and our sensitivity
# analysis later on will work in a reverse order of dropping missing variables to ensure that our findings are coherent.
# For an overview of how to handle missing data in R we recommend: http://www.statmethods.net/input/missingdata.html

# How many cases do we have inititally?

T0 <- DEVCON8a[, c("VIETNAM")] # create a vector only of "VIETNAM"
N0<- NROW(na.omit(T0)) # tell R to delete any rows with missing variables 
N0 # we find that we have 48483 data points, which is a good sample size to start with

# ST01 - GRADE
# We omit as an analytical variable as we already capture that aspect through Preschool and Age variable



############ THIS IS JUST NOTES or tests or for later use ##############

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



