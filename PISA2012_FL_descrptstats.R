# PISA2012_FL_descrptstats
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
# 4. Descriptive Statistics for REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH (in part 2)
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

############# 4.2 Explanatory variables - Students, teachers, pedagogical practices and schools #############

T0 <- DEVCON8a[, c("VIETNAM")] # create a vector only of "VIETNAM" or any other variable you want to look at
N0<- NROW(na.omit(T0)) # tell R to delete any rows with missing variables 
N0 # display, we find that we have 48483 data points, which is a good sample size to start with

############################### 4.2.2 Non-rotated Questions - Student variables #############################

# Background: "FEMALE", "PRESCHOOL","REPEAT","ST08Q01","ST09Q01","ST115Q01", "HISEI", "MISCED",
# ------------"WEALTH", "CULTPOS", "HEDRES", "BOOK_N", 
# Home support: "PARPRESSURE","TIGERMOM", "VOLUMOM","TEACHMOM", "FUNDMOM", "COUNCILMOM"
# Gender Balance: "PCGIRLS"
                              
#ST04Q01 / "FEMALE"
#___________________________________________________________________________________________________________
# How many NA's did we have:
T1z <- DEVCON8a[, c("VIETNAM", "ST04Q01")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 48483
N0-Z1 # 0 NA's
DEVCON8c <- DEVCON8a[complete.cases(T1z),]

# To check for the means for Vietnam and the group of 7, we take a new DEVCON8c file, since it only contains completed cases
# for VIETNAM and our variable of interest. If we took DEVCON8b, where we deleted rows due to missing values in other variables,
# we would unnecessarily use a smaller sample and the results would differ.

DEVCON8c$FEMALE[DEVCON8c$ST04Q01==1] <- 1
DEVCON8c$FEMALE[DEVCON8c$ST04Q01==2] <- 0

mean1A <- t(sapply(DEVCON8c[c("FEMALE")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#        estimate.mean in group 0 estimate.mean in group 1   p.value statistic.t
# FEMALE                0.5271115                0.5339786 0.3584798  -0.9183345

#ST05Q01
#_________________________________________________________________________________________________________
# How many NA's did we have:
T1z <- DEVCON8a[, c("VIETNAM", "ST05Q01")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 47119
N0-Z1 # 1364 NA's
DEVCON8c <- DEVCON8a[complete.cases(T1z),]

DEVCON8c$PRESCHOOL[DEVCON8c$ST05Q01==1] <- 0
DEVCON8c$PRESCHOOL[DEVCON8c$ST05Q01==2] <- 1
DEVCON8c$PRESCHOOL[DEVCON8c$ST05Q01==3] <- 1

mean1A <- t(sapply(DEVCON8c[c("PRESCHOOL")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#         estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# PRESCHOOL                0.7866136                0.9115743 2.124727e-161   -27.73917

#ST08Q01
#_______________________________________________________________________________________________________________
# How many NA's did we have:
T1z <- DEVCON8a[, c("VIETNAM", "ST08Q01")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 47701
N0-Z1 # 782 NA's
DEVCON8c <- DEVCON8a[complete.cases(T1z),]

mean1A <- t(sapply(DEVCON8c[c("ST08Q01")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#           estimate.mean in group 0 estimate.mean in group 1 p.value statistic.t
#ST08Q01                 1.518631                 1.187677       0    43.47177

#ST09Q01
#_______________________________________________________________________________________________________________
# How many NA's did we have:
T1z <- DEVCON8a[, c("VIETNAM", "ST09Q01")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 47688
N0-Z1 # 795 NA's
DEVCON8c <- DEVCON8a[complete.cases(T1z),]

mean1A <- t(sapply(DEVCON8c[c("ST09Q01")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#           estimate.mean in group 0 estimate.mean in group 1      p.value statistic.t
#ST09Q01                 1.217428                 1.102181 7.733371e-89    20.24697

# ST115Q01 
#______________________________________________________________________________________________________________
# How many NA's did we have:
T1z <- DEVCON8a[, c("VIETNAM", "ST115Q01")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 47676
N0-Z1 # 807 NA's
DEVCON8c <- DEVCON8a[complete.cases(T1z),]

mean1A <- t(sapply(DEVCON8c[c("ST115Q01")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#         estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# ST115Q01                 1.257661                 1.076659 4.155306e-243    34.39066

# HISEI
#_____________________________________________________________________________________________________________
# How many NA's did we have:
T1z <- DEVCON8a[, c("VIETNAM", "HISEI")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 39518
N0-Z1 # 8965 NA's
DEVCON8c <- DEVCON8a[complete.cases(T1z),]

mean1A <- t(sapply(DEVCON8c[c("HISEI")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#           estimate.mean in group 0 estimate.mean in group 1 p.value statistic.t
# HISEI                 40.41093                  26.5859       0    44.94913

# MISCED
#____________________________________________________________________________________________________________
T1z <- DEVCON8a[, c("VIETNAM", "MISCED")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 47453
N0-Z1 # 1030 NA's
DEVCON8c <- DEVCON8a[complete.cases(T1z),]

mean1A <- t(sapply(DEVCON8c[c("MISCED")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#           estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# MISCED                 3.111681                 2.167649 3.273498e-287    38.03908

# WEALTH
#____________________________________________________________________________________________________________
T1z <- DEVCON8a[, c("VIETNAM", "WEALTH")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 47876
N0-Z1 # 607 NA's
DEVCON8c <- DEVCON8a[complete.cases(T1z),]

mean1A <- t(sapply(DEVCON8c[c("WEALTH")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#           estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# WEALTH                -1.467498                -2.137781 4.836754e-287    38.17764

# CULTPOS
#____________________________________________________________________________________________________________
T1z <- DEVCON8a[, c("VIETNAM", "CULTPOS")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 46846
N0-Z1 # 1637 NA's
DEVCON8c <- DEVCON8a[complete.cases(T1z),]

mean1A <- t(sapply(DEVCON8c[c("CULTPOS")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#           estimate.mean in group 0 estimate.mean in group 1      p.value statistic.t
# CULTPOS               -0.1461614               -0.2364387 3.840693e-09    5.899749

# HEDRES
#____________________________________________________________________________________________________________
T1z <- DEVCON8a[, c("VIETNAM", "HEDRES")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 47617
N0-Z1 # 866 NA's
DEVCON8c <- DEVCON8a[complete.cases(T1z),]

mean1A <- t(sapply(DEVCON8c[c("HEDRES")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

#ST28Q01 / BOOKS_N
#______________________________________________________________________________________________________________
# How many NA's did we have:
T1z <- DEVCON8a[, c("VIETNAM", "ST28Q01")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 46570
N0-Z1 # 1913 NA's
DEVCON8c <- DEVCON8a[complete.cases(T1z),]

DEVCON8c$BOOK_N[DEVCON8c$ST28Q01==1]  <- 5
DEVCON8c$BOOK_N[DEVCON8c$ST28Q01==2]  <- 15
DEVCON8c$BOOK_N[DEVCON8c$ST28Q01==3]  <- 60
DEVCON8c$BOOK_N[DEVCON8c$ST28Q01==4]  <- 150
DEVCON8c$BOOK_N[DEVCON8c$ST28Q01==5]  <- 350
DEVCON8c$BOOK_N[DEVCON8c$ST28Q01==6]  <- 500

mean1A <- t(sapply(DEVCON8c[c("BOOK_N")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#           estimate.mean in group 0 estimate.mean in group 1    p.value statistic.t
# BOOK_N                 53.00375                 50.91723 0.07624858     1.77315

#SC24Q01 
#________________________________________________________________________________________________________________
# How many NA's did we have:
T1z <- DEVCON8a[, c("VIETNAM", "SC24Q01")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 47285
N0-Z1 # 1198 NA's
DEVCON8c <- DEVCON8a[complete.cases(T1z),]

DEVCON8c$PARPRESSURE[DEVCON8c$SC24Q01==1] <- 1
DEVCON8c$PARPRESSURE[DEVCON8c$SC24Q01==2] <- 0
DEVCON8c$PARPRESSURE[DEVCON8c$SC24Q01==3] <- 0

mean1A <- t(sapply(DEVCON8c[c("PARPRESSURE")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#             estimate.mean in group 0 estimate.mean in group 1      p.value statistic.t
# PARPRESSURE                0.2624817                0.3910581 2.287605e-68   -17.70189

#PCGIRLS
#________________________________________________________________________________________________________________
# How many NA's did we have:
T1z <- DEVCON8a[, c("VIETNAM", "PCGIRLS")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 43241
N0-Z1 # 5242 NA's
DEVCON8c <- DEVCON8a[complete.cases(T1z),]

mean1A <- t(sapply(DEVCON8c[c("PCGIRLS")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#             estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# PCGIRLS                0.4892091                 0.528872 8.658936e-114   -22.80247

#SC25Q01
#_________________________________________________________________________________________________________________
# Theoretically, how many NA's did we have for:

# TIGERMOM
# NA's for TIGERMOM:
T1z <- DEVCON8a[, c("VIETNAM", "SC25Q01", "SC25Q03")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 47268
N0-Z1 # 1215 NA's

# but we assign all NA's 0 in this specific case so that:
DEVCON8c <- DEVCON8a
DEVCON8c$SC25Q01[is.na(DEVCON8c$SC25Q01)]  <- 0
DEVCON8c$SC25Q03[is.na(DEVCON8c$SC25Q03)]  <- 0

DEVCON8c$TIGERMOM  <- DEVCON8c$SC25Q01+DEVCON8c$SC25Q03
DEVCON8c$TIGERMOM[DEVCON8c$TIGERMOM>100] <- 100 

mean1A <- t(sapply(DEVCON8c[c("TIGERMOM")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#             estimate.mean in group 0 estimate.mean in group 1      p.value statistic.t
# TIGERMOM                  52.4234                 62.82434 8.773441e-63   -16.92285

# VOLUMOM
# NA's for VOLUMOM:
T1z <- DEVCON8a[, c("VIETNAM", "SC25Q05", "SC25Q06", "SC25Q07", "SC25Q09", "SC25Q12")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 45785
N0-Z1 # 2698 NA's

# but we assign all NA's 0 in this specific case so that:
DEVCON8c <- DEVCON8a
DEVCON8c$SC25Q05[is.na(DEVCON8c$SC25Q05)]  <- 0
DEVCON8c$SC25Q06[is.na(DEVCON8c$SC25Q06)]  <- 0
DEVCON8c$SC25Q07[is.na(DEVCON8c$SC25Q07)]  <- 0
DEVCON8c$SC25Q09[is.na(DEVCON8c$SC25Q09)]  <- 0
DEVCON8c$SC25Q12[is.na(DEVCON8c$SC25Q12)]  <- 0

DEVCON8c$VOLUMOM <- DEVCON8c$SC25Q05+DEVCON8c$SC25Q06+DEVCON8c$SC25Q07+DEVCON8c$SC25Q09+DEVCON8c$SC25Q12
DEVCON8c$VOLUMOM[DEVCON8c$VOLUMOM>100] <- 100 # censoring at 100 should look familiar now

mean1A <- t(sapply(DEVCON8c[c("VOLUMOM")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#               estimate.mean in group 0 estimate.mean in group 1      p.value statistic.t
# VOLUMOM                 34.41764                  39.0976 8.810321e-15   -7.775012

# TEACHMOM
# NA's for TEACHMOM:
T1z <- DEVCON8a[, c("VIETNAM", "SC25Q08")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 46576
N0-Z1 # 1907 NA's

# but we assign all NA's 0 in this specific case so that:
DEVCON8c <- DEVCON8a
DEVCON8c$SC25Q08[is.na(DEVCON8c$SC25Q08)]  <- 0

DEVCON8c$TEACHMOM <- DEVCON8c$SC25Q08

mean1A <- t(sapply(DEVCON8c[c("TEACHMOM")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#               estimate.mean in group 0 estimate.mean in group 1 p.value statistic.t
# TEACHMOM                 11.91787                 38.95201       0   -44.80017

# FUNDMOM
# NA's for FUNDMOM:
T1z <- DEVCON8a[, c("VIETNAM", "SC25Q11")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 46714
N0-Z1 # 1769 NA's

# but we assign all NA's 0 in this specific case so that:
DEVCON8c <- DEVCON8a
DEVCON8c$SC25Q11[is.na(DEVCON8c$SC25Q11)]  <- 0

DEVCON8c$FUNDMOM <-  DEVCON8c$SC25Q11

mean1A <- t(sapply(DEVCON8c[c("FUNDMOM")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#             estimate.mean in group 0 estimate.mean in group 1 p.value statistic.t
# FUNDMOM                  22.4614                 59.94112       0   -57.98039

# COUNCILMOM
# NA's for COUNCILMOM:
T1z <- DEVCON8a[, c("VIETNAM", "SC25Q10")]
Z1 <- NROW(na.omit(T1z)) 
Z1 # 47007
N0-Z1 # 1476 NA's

# but we assign all NA's 0 in this specific case so that:
DEVCON8c <- DEVCON8a
DEVCON8c$SC25Q10[is.na(DEVCON8c$SC25Q10)]  <- 0

DEVCON8c$COUNCILMOM <- DEVCON8c$SC25Q10

mean1A <- t(sapply(DEVCON8c[c("COUNCILMOM")], function(x) 
  unlist(t.test(x~DEVCON8c$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
#             estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# COUNCILMOM                 35.99807                 23.92299 1.783757e-101    21.79062

# JUST TO SEE THE MARGINAL DIFFERENCES: _____________________________________________________________________________
# If we just take the means from DEVCON8b (where we deleted all missing variables from all variables)
mean1A <- t(sapply(DEVCON8b[c("PRESCHOOL","FEMALE","REPEAT","ST08Q01","ST09Q01","ST115Q01", "HISEI", "MISCED",
                              "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS", "TIGERMOM", "VOLUMOM",
                              "TEACHMOM", "FUNDMOM", "COUNCILMOM")], function(x) 
                                unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

############################### 4.2.3 Non-rotated Questions - Teacher variables #############################

# Quantity: STRATIO, PROPCERT, PROPQUAL, TCSHORT, SMRATIO
# Quality: TCFOCST, SC30Q04, SC30Q02, SC30Q03, SC30Q01,SC31Q01-Q07 (TCH incentive), SC39Q08, SC35Q02



