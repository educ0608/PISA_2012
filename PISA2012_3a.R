# PISA2012_3a.R

# Prepared, Friday, May 8, 2015

# Revised, Thursday, June 4, 2015

# Paper writing version for Fryer-Levitt replication

library(intsvy) # For PISA analysis with PVs and BRRs
library(xtable)# To generate Latex inputs
library(xlsx)# To generate MS-Excel output
library(foreign) # To import or export file
library(dplyr) # for many things
library(gmodels) # For PROC FREQ like tables
library(psych) # for rescaling variables to given mean and sd
library(tables) # For making tables with tabular !


# How many cases ?
T0 <- DEVCON8a[, c("VIETNAM")]
N0<- NROW(na.omit(T0)) # 48483 data points - we have scores on these students for sure

# What is the mean score
mean0 <- pisa.mean.pv(pvlabel="MATH",by="VIETNAM", data=DEVCON8a,weight="W_FSTUWT")
mean0
mean0 <- pisa.mean.pv(pvlabel="SCIE",by="VIETNAM", data=DEVCON8a,weight="W_FSTUWT")
mean0
mean0 <- pisa.mean.pv(pvlabel="READ",by="VIETNAM", data=DEVCON8a,weight="W_FSTUWT")
mean0

# MATH
# VIETNAM  Freq   Mean s.e.    SD  s.e
# 1       0 43524 383.29 2.50 77.58 1.79
# 2       1  4959 511.34 4.84 85.76 2.65

# SCIE
# VIETNAM  Freq   Mean s.e.    SD  s.e
# 1       0 43524 393.86 2.25 75.97 1.29
# 2       1  4959 528.42 4.31 77.36 2.31

# READ
# VIETNAM  Freq   Mean s.e.    SD  s.e
# 1       0 43524 403.06 2.46 81.61 1.51
# 2       1  4959 508.22 4.40 74.09 2.58

# The baseline regressions - the difference between mean score values
# will be the coefficient value on the dummy 

R0 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8a,export=FALSE)
R0

#Estimate Std. Error t value
#(Intercept)   383.29       2.50  153.26
#VIETNAM       128.05       5.65   22.68
#R-squared      27.21       2.25   12.07

R0 <- pisa.reg.pv(pvlabel="SCIE", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8a,export=FALSE)
R0

# Result:-
#Estimate Std. Error t value
#(Intercept)   393.86       2.25  175.00
#VIETNAM       134.56       4.91   27.41
#R-squared      30.75       1.96   15.66

R0 <- pisa.reg.pv(pvlabel="READ", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8a,export=FALSE)
R0

# Result
#Estimate Std. Error t value
#(Intercept)   403.06       2.46  163.78
#VIETNAM       105.16       5.03   20.89
#R-squared      19.61       1.81   10.85

# **********************************************************
# STUDENTS - First set - gap reduced 

# Get the data with non-missing values 

# How many cases ?

# May generate some spurious data, but essentially it is safe to put 0 for na in this specific case of SC25


# How many non-missing values ?
T1b <- DEVCON8a[, c("VIETNAM","ST05Q01","ST07Q01","ST08Q01","ST09Q01","ST115Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 43,626
N0-N1 # 4857 NAs

# I work on dataframe of non-missing
DEVCON8b <- DEVCON8a[complete.cases(T1b),]
# I treat missing as 0 for SC25 variables
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

# I define the student related variables

# I change 3 levels into Yes or No
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==1] <- 0
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==2] <- 1
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==3] <- 1

# I will figure out someday an easier way to do this in 1 step

DEVCON8b$REP1 <- ifelse(DEVCON8b$ST07Q01==1,0,1)
DEVCON8b$REP2 <- ifelse(DEVCON8b$ST07Q02==1,0,1)
DEVCON8b$REP3 <- ifelse(DEVCON8b$ST07Q03==1,0,1)

DEVCON8b$REPEATER <- ifelse(DEVCON8b$REP1==0 & DEVCON8b$REP2==0 & DEVCON8b$REP3==0,0,1)

# I define the parent related variables 

DEVCON8b$TIGERMOM  <- DEVCON8b$SC25Q01+DEVCON8b$SC25Q03
# I want to censor at 100 for cases more than 100
DEVCON8b$TIGERMOM[DEVCON8b$TIGERMOM>100] <- 100

DEVCON8b$VOLUMOM <- (DEVCON8b$SC25Q05+DEVCON8b$SC25Q06+DEVCON8b$SC25Q07+DEVCON8b$SC25Q09+DEVCON8b$SC25Q12)
DEVCON8b$VOLUMOM[DEVCON8b$VOLUMOM>100] <- 100

DEVCON8b$TEACHMOM <- DEVCON8b$SC25Q08

DEVCON8b$FUNDMOM <-  DEVCON8b$SC25Q11

DEVCON8b$COUNCILMOM <- DEVCON8b$SC25Q10

# Now for checking means 

mean1A <- t(sapply(DEVCON8b[c("PRESCHOOL","REPEATER","ST08Q01","ST09Q01","ST115Q01","TIGERMOM","VOLUMOM","TEACHMOM","FUNDMOM","COUNCILMOM")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# Now try the regressions - MATHEMATICS

R0 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R0

#Estimate Std. Error t value
#(Intercept)   386.12       2.59  149.01
#VIETNAM       127.96       5.58   22.93
#R-squared      27.80       2.30   12.09

R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL","REPEATER","ST08Q01","ST09Q01","ST115Q01"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   403.69       5.16   78.22
# VIETNAM       103.40       5.25   19.69
# PRESCHOOL      41.11       4.16    9.88
# REPEATER      -59.25       3.21  -18.47
# ST08Q01        -7.04       1.28   -5.48
# ST09Q01       -14.09       1.88   -7.50
# ST115Q01       -1.71       1.91   -0.90
# R-squared      40.66       1.96   20.71

R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL","REPEATER","ST08Q01","ST09Q01","ST115Q01", "TIGERMOM","VOLUMOM","TEACHMOM","FUNDMOM","COUNCILMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B


# Estimate Std. Error t value
# (Intercept)   401.97       6.95   57.81
# VIETNAM        91.29       4.80   19.02
# PRESCHOOL      36.66       4.06    9.02
# REPEATER      -56.29       3.21  -17.56
# ST08Q01        -7.04       1.22   -5.79
# ST09Q01       -14.24       1.86   -7.65
# ST115Q01       -1.27       1.81   -0.70
# TIGERMOM        0.05       0.06    0.95
# VOLUMOM         0.03       0.07    0.47
# TEACHMOM        0.01       0.08    0.11
# FUNDMOM         0.27       0.05    5.25
# COUNCILMOM     -0.20       0.06   -3.17
#  R-squared      42.12       1.96   21.53

# Very interesting how the xMOM variables are statistically insignficant, but 
# has 12 point effect on ze dummy ! 


# STUDENTS - First and Second set  set - gap reduced then dialed back 

# Get the data with non-missing values 

# How many cases ?

# May generate some spurious data, but essentially it is safe to put 0 for na in this specific case of SC25


# How many non-missing values ?
T1b <- DEVCON8a[, c("VIETNAM","ST05Q01","ST07Q01","ST08Q01","ST09Q01","ST115Q01","HISEI","MISCED","FISCED", 
                    "WEALTH","CULTPOS","HEDRES","ST28Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 33,500
N0-N1 # 14983 NAs

# I work on dataframe of non-missing
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

# Repeat new vars for consistency
# I treat missing as 0 for SC25 variables
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

# I define the student related variables

# I change 3 levels into Yes or No
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==1] <- 0
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==2] <- 1
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==3] <- 1

# I will figure out someday an easier way to do this in 1 step

DEVCON8b$REP1 <- ifelse(DEVCON8b$ST07Q01==1,0,1)
DEVCON8b$REP2 <- ifelse(DEVCON8b$ST07Q02==1,0,1)
DEVCON8b$REP3 <- ifelse(DEVCON8b$ST07Q03==1,0,1)

DEVCON8b$REPEATER <- ifelse(DEVCON8b$REP1==0 & DEVCON8b$REP2==0 & DEVCON8b$REP3==0,0,1)

# I define the parent related variables 

DEVCON8b$TIGERMOM  <- DEVCON8b$SC25Q01+DEVCON8b$SC25Q03
# I want to censor at 100 for cases more than 100
DEVCON8b$TIGERMOM[DEVCON8b$TIGERMOM>100] <- 100

DEVCON8b$VOLUMOM <- (DEVCON8b$SC25Q05+DEVCON8b$SC25Q06+DEVCON8b$SC25Q07+DEVCON8b$SC25Q09+DEVCON8b$SC25Q12)
DEVCON8b$VOLUMOM[DEVCON8b$VOLUMOM>100] <- 100

DEVCON8b$TEACHMOM <- DEVCON8b$SC25Q08

DEVCON8b$FUNDMOM <-  DEVCON8b$SC25Q11

DEVCON8b$COUNCILMOM <- DEVCON8b$SC25Q10

# Generate number of books variable
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==1]  <- 5
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==2]  <- 15
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==3]  <- 60
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==4]  <- 150
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==5]  <- 350
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==6]  <- 500

# Now look at means for all as sample was truncated

mean1A <- t(sapply(DEVCON8b[c("PRESCHOOL","REPEATER","ST08Q01","ST09Q01","ST115Q01","TIGERMOM","VOLUMOM","TEACHMOM",
                              "FUNDMOM","COUNCILMOM","HISEI","MISCED","FISCED", 
                              "WEALTH","CULTPOS","HEDRES","BOOK_N")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# Regs again

R0 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R0

# Estimate Std. Error t value
# (Intercept)   390.36       2.78  140.28
# VIETNAM       126.04       5.63   22.40
# R-squared      28.67       2.42   11.83


R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL","REPEATER","ST08Q01","ST09Q01","ST115Q01", "TIGERMOM","VOLUMOM","TEACHMOM","FUNDMOM","COUNCILMOM",
                       "HISEI","MISCED","FISCED", 
                       "WEALTH","CULTPOS","HEDRES","BOOK_N"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)   410.54       7.27   56.44
# VIETNAM       106.20       4.06   26.13
# PRESCHOOL      18.88       3.43    5.50
# REPEATER      -43.73       3.46  -12.63
# ST08Q01        -7.62       1.15   -6.63
# ST09Q01       -11.15       1.99   -5.62
# ST115Q01       -3.47       1.93   -1.80
# TIGERMOM        0.00       0.06    0.06
# VOLUMOM         0.08       0.07    1.18
# TEACHMOM       -0.03       0.07   -0.53
# FUNDMOM         0.23       0.05    4.71
# COUNCILMOM     -0.15       0.05   -2.90
# HISEI           0.43       0.06    7.64
# MISCED          1.95       0.60    3.26
# FISCED          1.19       0.59    2.03
# WEALTH          7.19       1.35    5.33
# CULTPOS        -4.44       1.07   -4.14
# HEDRES          9.90       1.02    9.69
# BOOK_N          0.04       0.01    4.02
#  R-squared      49.42       1.75   28.30

# Now I go for all the teaching related variables less the -ve sided students variables
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


# How many non-missing values ?

T1b <- DEVCON8a[, c("VIETNAM","ST05Q01","ST07Q01","ST08Q01","ST09Q01","ST115Q01",
                    "STRATIO","TCSHORT","PROPCERT","PROPQUAL","SC35Q02","SC30Q04")]
N1 <- NROW(na.omit(T1b)) 
N1 # 28,884
N0-N1 # 19,599 NAs

# I work on dataframe of non-missing
DEVCON8b1 <- DEVCON8a[complete.cases(T1b),]

DEVCON8b <- merge(DEVCON8b1,DEVCON8Z,by="NEWID")


# Repeat new vars for consistency
# I treat missing as 0 for SC25 variables
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

# I define the student related variables

# I change 3 levels into Yes or No
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==1] <- 0
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==2] <- 1
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==3] <- 1

# I will figure out someday an easier way to do this in 1 step

DEVCON8b$REP1 <- ifelse(DEVCON8b$ST07Q01==1,0,1)
DEVCON8b$REP2 <- ifelse(DEVCON8b$ST07Q02==1,0,1)
DEVCON8b$REP3 <- ifelse(DEVCON8b$ST07Q03==1,0,1)

DEVCON8b$REPEATER <- ifelse(DEVCON8b$REP1==0 & DEVCON8b$REP2==0 & DEVCON8b$REP3==0,0,1)

# I define the parent related variables 

DEVCON8b$TIGERMOM  <- DEVCON8b$SC25Q01+DEVCON8b$SC25Q03
# I want to censor at 100 for cases more than 100
DEVCON8b$TIGERMOM[DEVCON8b$TIGERMOM>100] <- 100

DEVCON8b$VOLUMOM <- (DEVCON8b$SC25Q05+DEVCON8b$SC25Q06+DEVCON8b$SC25Q07+DEVCON8b$SC25Q09+DEVCON8b$SC25Q12)
DEVCON8b$VOLUMOM[DEVCON8b$VOLUMOM>100] <- 100

DEVCON8b$TEACHMOM <- DEVCON8b$SC25Q08

DEVCON8b$FUNDMOM <-  DEVCON8b$SC25Q11

DEVCON8b$COUNCILMOM <- DEVCON8b$SC25Q10

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Inspector observation (INSPE)
DEVCON8b$TCM_INSPE[DEVCON8b$SC30Q04==1] <- 1
DEVCON8b$TCM_INSPE[DEVCON8b$SC30Q04==2] <- 0

mean1A <- t(sapply(DEVCON8b[c("PRESCHOOL","REPEATER","ST08Q01","ST09Q01","ST115Q01","TIGERMOM","VOLUMOM","TEACHMOM",
                              "FUNDMOM","COUNCILMOM",
                              "STRATIO","TCSHORT","PROPCERT","PROPQUAL","SC35Q02","TCH_INCENTV","TCM_INSPE")], function(x) 
                                unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A


# Now try the regressions again

R0 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R0

# Estimate Std. Error t value
# (Intercept)   385.96       2.80  137.72
# VIETNAM       129.40       6.16   21.01
# R-squared      31.03       2.33   13.35




R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL","REPEATER","ST08Q01","ST09Q01","ST115Q01", "TIGERMOM","VOLUMOM","TEACHMOM","FUNDMOM","COUNCILMOM",
                       "STRATIO","TCSHORT","PROPCERT","PROPQUAL","SC35Q02","TCH_INCENTV"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# 90.31 without TCM_INSPE; 93.89 with. 
# Now for the Teaching Learning +ve variables 



#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# How many non-missing values ?

T1b <- DEVCON8a[, c("VIETNAM","ST05Q01","ST07Q01","ST08Q01","ST09Q01","ST115Q01",
                    "STRATIO","TCSHORT","PROPCERT","PROPQUAL","SC35Q02",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC19Q01","SC19Q02","SC20Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 27,495
N0-N1 # 20,988 NAs


# I work on dataframe of non-missing
DEVCON8b1 <- DEVCON8a[complete.cases(T1b),]

DEVCON8b <- merge(DEVCON8b1,DEVCON8Z,by="NEWID")

# Repeat new vars for consistency
# I treat missing as 0 for SC25 variables
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

# I define the student related variables

# I change 3 levels into Yes or No
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==1] <- 0
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==2] <- 1
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==3] <- 1

# I will figure out someday an easier way to do this in 1 step

DEVCON8b$REP1 <- ifelse(DEVCON8b$ST07Q01==1,0,1)
DEVCON8b$REP2 <- ifelse(DEVCON8b$ST07Q02==1,0,1)
DEVCON8b$REP3 <- ifelse(DEVCON8b$ST07Q03==1,0,1)

DEVCON8b$REPEATER <- ifelse(DEVCON8b$REP1==0 & DEVCON8b$REP2==0 & DEVCON8b$REP3==0,0,1)

# I define the parent related variables 

DEVCON8b$TIGERMOM  <- DEVCON8b$SC25Q01+DEVCON8b$SC25Q03
# I want to censor at 100 for cases more than 100
DEVCON8b$TIGERMOM[DEVCON8b$TIGERMOM>100] <- 100

DEVCON8b$VOLUMOM <- (DEVCON8b$SC25Q05+DEVCON8b$SC25Q06+DEVCON8b$SC25Q07+DEVCON8b$SC25Q09+DEVCON8b$SC25Q12)
DEVCON8b$VOLUMOM[DEVCON8b$VOLUMOM>100] <- 100

DEVCON8b$TEACHMOM <- DEVCON8b$SC25Q08

DEVCON8b$FUNDMOM <-  DEVCON8b$SC25Q11

DEVCON8b$COUNCILMOM <- DEVCON8b$SC25Q10

DEVCON8b$EXC2_PLAY[DEVCON8b$SC16Q02==1] <- 1
DEVCON8b$EXC2_PLAY[DEVCON8b$SC16Q02==2] <- 0

DEVCON8b$EXC6_MATHCOMP[DEVCON8b$SC16Q06==1] <- 1
DEVCON8b$EXC6_MATHCOMP[DEVCON8b$SC16Q06==2] <- 0

DEVCON8b$EXC10_SPORT[DEVCON8b$SC16Q10==1] <- 1
DEVCON8b$EXC10_SPORT[DEVCON8b$SC16Q10==2] <- 0

DEVCON8b$EXC11_UNICORN[DEVCON8b$SC16Q11==1] <- 1
DEVCON8b$EXC11_UNICORN[DEVCON8b$SC16Q11==2] <- 0

DEVCON8b$SCORE_PUBLIC[DEVCON8b$SC19Q01==1] <- 1
DEVCON8b$SCORE_PUBLIC[DEVCON8b$SC19Q01==2] <- 0

DEVCON8b$SCORE_AUTHRITS[DEVCON8b$SC19Q02==1] <- 1
DEVCON8b$SCORE_AUTHRITS[DEVCON8b$SC19Q02==2] <- 0

DEVCON8b$SCL_EXTR_CL[DEVCON8b$SC20Q01==1] <- 1
DEVCON8b$SCL_EXTR_CL[DEVCON8b$SC20Q01==2] <- 0

mean1A <- t(sapply(DEVCON8b[c("PRESCHOOL","REPEATER","ST08Q01","ST09Q01","ST115Q01","TIGERMOM","VOLUMOM","TEACHMOM",
                              "FUNDMOM","COUNCILMOM",
                              "STRATIO","TCSHORT","PROPCERT","PROPQUAL","SC35Q02","TCH_INCENTV",
                              "EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                              "SCORE_PUBLIC","SCORE_AUTHRITS","SCL_EXTR_CL")], function(x) 
                                unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A


# Now try the regressions again

R0 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R0

# Estimate Std. Error t value
# (Intercept)   386.07       2.85  135.30
# VIETNAM       128.17       6.31   20.31
# R-squared      30.61       2.39   12.82



R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL","REPEATER","ST08Q01","ST09Q01","ST115Q01", "TIGERMOM","VOLUMOM","TEACHMOM","FUNDMOM","COUNCILMOM",
                       "STRATIO","TCSHORT","PROPCERT","PROPQUAL","SC35Q02","TCH_INCENTV",
                       "EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                       "SCORE_PUBLIC","SCORE_AUTHRITS","SCL_EXTR_CL"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# How many non-missing values ?

T1b <- DEVCON8a[, c("VIETNAM","ST05Q01","ST07Q01","ST08Q01","ST09Q01","ST115Q01",
                    "STRATIO","TCSHORT","PROPCERT","PROPQUAL","SC35Q02",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC19Q01","SC19Q02","SC20Q01",
                    "SC01Q01","SC32Q01","CLSIZE","SCMATBUI","SCMATEDU")]
N1 <- NROW(na.omit(T1b)) 
N1 # 27,326
N0-N1 # 21,157 NAs


# I work on dataframe of non-missing
DEVCON8b1 <- DEVCON8a[complete.cases(T1b),]

DEVCON8b <- merge(DEVCON8b1,DEVCON8Z,by="NEWID")

# Repeat new vars for consistency
# I treat missing as 0 for SC25 variables
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

# I define the student related variables

# I change 3 levels into Yes or No
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==1] <- 0
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==2] <- 1
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==3] <- 1

# I will figure out someday an easier way to do this in 1 step

DEVCON8b$REP1 <- ifelse(DEVCON8b$ST07Q01==1,0,1)
DEVCON8b$REP2 <- ifelse(DEVCON8b$ST07Q02==1,0,1)
DEVCON8b$REP3 <- ifelse(DEVCON8b$ST07Q03==1,0,1)

DEVCON8b$REPEATER <- ifelse(DEVCON8b$REP1==0 & DEVCON8b$REP2==0 & DEVCON8b$REP3==0,0,1)

# I define the parent related variables 

DEVCON8b$TIGERMOM  <- DEVCON8b$SC25Q01+DEVCON8b$SC25Q03
# I want to censor at 100 for cases more than 100
DEVCON8b$TIGERMOM[DEVCON8b$TIGERMOM>100] <- 100

DEVCON8b$VOLUMOM <- (DEVCON8b$SC25Q05+DEVCON8b$SC25Q06+DEVCON8b$SC25Q07+DEVCON8b$SC25Q09+DEVCON8b$SC25Q12)
DEVCON8b$VOLUMOM[DEVCON8b$VOLUMOM>100] <- 100

DEVCON8b$TEACHMOM <- DEVCON8b$SC25Q08

DEVCON8b$FUNDMOM <-  DEVCON8b$SC25Q11

DEVCON8b$COUNCILMOM <- DEVCON8b$SC25Q10

DEVCON8b$EXC2_PLAY[DEVCON8b$SC16Q02==1] <- 1
DEVCON8b$EXC2_PLAY[DEVCON8b$SC16Q02==2] <- 0

DEVCON8b$EXC6_MATHCOMP[DEVCON8b$SC16Q06==1] <- 1
DEVCON8b$EXC6_MATHCOMP[DEVCON8b$SC16Q06==2] <- 0

DEVCON8b$EXC10_SPORT[DEVCON8b$SC16Q10==1] <- 1
DEVCON8b$EXC10_SPORT[DEVCON8b$SC16Q10==2] <- 0

DEVCON8b$EXC11_UNICORN[DEVCON8b$SC16Q11==1] <- 1
DEVCON8b$EXC11_UNICORN[DEVCON8b$SC16Q11==2] <- 0

DEVCON8b$SCORE_PUBLIC[DEVCON8b$SC19Q01==1] <- 1
DEVCON8b$SCORE_PUBLIC[DEVCON8b$SC19Q01==2] <- 0

DEVCON8b$SCORE_AUTHRITS[DEVCON8b$SC19Q02==1] <- 1
DEVCON8b$SCORE_AUTHRITS[DEVCON8b$SC19Q02==2] <- 0

DEVCON8b$SCL_EXTR_CL[DEVCON8b$SC20Q01==1] <- 1
DEVCON8b$SCL_EXTR_CL[DEVCON8b$SC20Q01==2] <- 0

DEVCON8b$PRIVATE_SCL[DEVCON8b$SC01Q01==2] <- 1
DEVCON8b$PRIVATE_SCL[DEVCON8b$SC01Q01==1] <- 0

mean1A <- t(sapply(DEVCON8b[c("PRESCHOOL","REPEATER","ST08Q01","ST09Q01","ST115Q01","TIGERMOM","VOLUMOM","TEACHMOM",
                              "FUNDMOM","COUNCILMOM",
                              "STRATIO","TCSHORT","PROPCERT","PROPQUAL","SC35Q02","TCH_INCENTV",
                              "EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                              "SCORE_PUBLIC","SCORE_AUTHRITS","SCL_EXTR_CL",
                              "PRIVATE_SCL","SC32Q01","CLSIZE","SCMATBUI","SCMATEDU")], function(x) 
                                unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A


# Now try the regressions again

R0 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R0

# Estimate Std. Error t value
# (Intercept)   385.97       2.86  135.14
# VIETNAM       128.27       6.30   20.35
# R-squared      30.69       2.39   12.86

R1B <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL","REPEATER","ST08Q01","ST09Q01","ST115Q01", "TIGERMOM","VOLUMOM","TEACHMOM","FUNDMOM","COUNCILMOM",
                       "STRATIO","TCSHORT","PROPCERT","PROPQUAL","SC35Q02","TCH_INCENTV",
                       "EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                       "SCORE_PUBLIC","SCORE_AUTHRITS","SCL_EXTR_CL",
                       "PRIVATE_SCL","SC32Q01","CLSIZE","SCMATBUI","SCMATEDU"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# (Intercept)      370.45      17.68   20.95
# VIETNAM           74.69       5.92   12.61

R0 <- pisa.reg.pv(pvlabel="SCIE", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R0

# Estimate Std. Error t value
# (Intercept)   397.58       2.85  139.69
# VIETNAM       133.14       5.34   24.94
# R-squared      33.76       2.10   16.09


R1B <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "PRESCHOOL","REPEATER","ST08Q01","ST09Q01","ST115Q01", "TIGERMOM","VOLUMOM","TEACHMOM","FUNDMOM","COUNCILMOM",
                       "STRATIO","TCSHORT","PROPCERT","PROPQUAL","SC35Q02","TCH_INCENTV",
                       "EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                       "SCORE_PUBLIC","SCORE_AUTHRITS","SCL_EXTR_CL",
                       "PRIVATE_SCL","SC32Q01","CLSIZE","SCMATBUI","SCMATEDU"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# VIETNAM           84.06       5.02   16.74
# PRESCHOOL         23.55       3.93    6.00


R0 <- pisa.reg.pv(pvlabel="READ", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R0

# (Intercept)   406.33       3.07  132.29
# VIETNAM       105.66       5.56   19.02
# R-squared      23.12       2.08   11.11


R1B <- pisa.reg.pv(pvlabel="READ", 
                   x=c("VIETNAM",
                       "PRESCHOOL","REPEATER","ST08Q01","ST09Q01","ST115Q01", "TIGERMOM","VOLUMOM","TEACHMOM","FUNDMOM","COUNCILMOM",
                       "STRATIO","TCSHORT","PROPCERT","PROPQUAL","SC35Q02","TCH_INCENTV",
                       "EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                       "SCORE_PUBLIC","SCORE_AUTHRITS","SCL_EXTR_CL",
                       "PRIVATE_SCL","SC32Q01","CLSIZE","SCMATBUI","SCMATEDU"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B

# Estimate Std. Error t value
# (Intercept)      398.49      17.45   22.83
# VIETNAM           52.56       5.56    9.45
#%*&%&%*&%*&%*&%*&%*(&%*(&%&*%*&%*&%*&%*&((*(*(*(*(*(*(*(*(*(*(*(*(*(*)))))))))))))))))******************************
#%*&%&%*&%*&%*&%*&%*(&%*(&%&*%*&%*&%*&%*&((*(*(*(*(*(*(*(*(*(*(*(*(*(*)))))))))))))))))******************************
### The resricted student variables now ###############################################******************************
# 1 Student Effort
# 1A Class time in hours per week   

T1b <- DEVCON8a[, c("VIETNAM","ST05Q01","ST07Q01","ST08Q01","ST09Q01","ST115Q01",
                    "STRATIO","TCSHORT","PROPCERT","PROPQUAL","SC35Q02",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC19Q01","SC19Q02","SC20Q01",
                    "SC01Q01","SC32Q01","CLSIZE","SCMATBUI","SCMATEDU",
                    "SMINS","MMINS", "LMINS")]
N1 <- NROW(na.omit(T1b)) 
N1 # 14,163
N0-N1 # 34,320 NAs

# I work on dataframe of non-missing
DEVCON8b1 <- DEVCON8a[complete.cases(T1b),]

DEVCON8b1 <- DEVCON8a[complete.cases(T1b),]

DEVCON8b <- merge(DEVCON8b1,DEVCON8Z,by="NEWID")

# Repeat new vars for consistency
# I treat missing as 0 for SC25 variables
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

# I define the student related variables

# I change 3 levels into Yes or No
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==1] <- 0
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==2] <- 1
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==3] <- 1

# I will figure out someday an easier way to do this in 1 step

DEVCON8b$REP1 <- ifelse(DEVCON8b$ST07Q01==1,0,1)
DEVCON8b$REP2 <- ifelse(DEVCON8b$ST07Q02==1,0,1)
DEVCON8b$REP3 <- ifelse(DEVCON8b$ST07Q03==1,0,1)

DEVCON8b$REPEATER <- ifelse(DEVCON8b$REP1==0 & DEVCON8b$REP2==0 & DEVCON8b$REP3==0,0,1)

# I define the parent related variables 

DEVCON8b$TIGERMOM  <- DEVCON8b$SC25Q01+DEVCON8b$SC25Q03
# I want to censor at 100 for cases more than 100
DEVCON8b$TIGERMOM[DEVCON8b$TIGERMOM>100] <- 100

DEVCON8b$VOLUMOM <- (DEVCON8b$SC25Q05+DEVCON8b$SC25Q06+DEVCON8b$SC25Q07+DEVCON8b$SC25Q09+DEVCON8b$SC25Q12)
DEVCON8b$VOLUMOM[DEVCON8b$VOLUMOM>100] <- 100

DEVCON8b$TEACHMOM <- DEVCON8b$SC25Q08

DEVCON8b$FUNDMOM <-  DEVCON8b$SC25Q11

DEVCON8b$COUNCILMOM <- DEVCON8b$SC25Q10

DEVCON8b$EXC2_PLAY[DEVCON8b$SC16Q02==1] <- 1
DEVCON8b$EXC2_PLAY[DEVCON8b$SC16Q02==2] <- 0

DEVCON8b$EXC6_MATHCOMP[DEVCON8b$SC16Q06==1] <- 1
DEVCON8b$EXC6_MATHCOMP[DEVCON8b$SC16Q06==2] <- 0

DEVCON8b$EXC10_SPORT[DEVCON8b$SC16Q10==1] <- 1
DEVCON8b$EXC10_SPORT[DEVCON8b$SC16Q10==2] <- 0

DEVCON8b$EXC11_UNICORN[DEVCON8b$SC16Q11==1] <- 1
DEVCON8b$EXC11_UNICORN[DEVCON8b$SC16Q11==2] <- 0

DEVCON8b$SCORE_PUBLIC[DEVCON8b$SC19Q01==1] <- 1
DEVCON8b$SCORE_PUBLIC[DEVCON8b$SC19Q01==2] <- 0

DEVCON8b$SCORE_AUTHRITS[DEVCON8b$SC19Q02==1] <- 1
DEVCON8b$SCORE_AUTHRITS[DEVCON8b$SC19Q02==2] <- 0

DEVCON8b$SCL_EXTR_CL[DEVCON8b$SC20Q01==1] <- 1
DEVCON8b$SCL_EXTR_CL[DEVCON8b$SC20Q01==2] <- 0

DEVCON8b$PRIVATE_SCL[DEVCON8b$SC01Q01==2] <- 1
DEVCON8b$PRIVATE_SCL[DEVCON8b$SC01Q01==1] <- 0


DEVCON8b$SHRS <- (DEVCON8b$SMINS)/60
DEVCON8b$MHRS <- (DEVCON8b$MMINS)/60
DEVCON8b$LHRS <- (DEVCON8b$LMINS)/60
# Total of the hours - I retain separate hours instead of this variable
DEVCON8b$CLASSHRS <- (DEVCON8b$MMINS+DEVCON8b$MMINS+DEVCON8b$LMINS)/60







R1B <- pisa.reg.pv(pvlabel="READ", 
                   x=c("VIETNAM",
                       "PRESCHOOL","REPEATER","ST08Q01","ST09Q01","ST115Q01", "TIGERMOM","VOLUMOM","TEACHMOM","FUNDMOM","COUNCILMOM",
                       "STRATIO","TCSHORT","PROPCERT","PROPQUAL","SC35Q02","TCH_INCENTV",
                       "EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                       "SCORE_PUBLIC","SCORE_AUTHRITS","SCL_EXTR_CL",
                       "PRIVATE_SCL","SC32Q01","CLSIZE","SCMATBUI","SCMATEDU",
                       "SHRS","MHRS","LHRS"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1B
#READ
#Estimate Std. Error t value
#(Intercept)      408.88      20.20   20.24
#VIETNAM           57.24       5.95    9.62






R1A <- pisa.reg.pv(pvlabel="SCIE", 
                   x=c("VIETNAM",
                       "SHRS","MHRS","LHRS"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R1A

mean1A <- t(sapply(DEVCON8b[c("SHRS","MHRS","LHRS")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

class(DEVCON8b$VIETNAM) # shows is numeric
DEVCON8b$F_VIETNAM <- factor(DEVCON8b$VIETNAM)
levels(DEVCON8b$F_VIETNAM) 

CrossTable(REVCON8b$D_PRESCHOOL, REVCON8b$VIETNAM,prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE) 

tabular((F_VIETNAM+1)~Format(digits=2,big.mark=",", big.interval=3)*(N_OBS=1) +
          Format(digits=4)*(TCH_INCENTV)*(Mean+ 
                                            Format(digits=2,big.mark=",", big.interval=3)*ValidN+
                                            Format(digits=2,big.mark=",", big.interval=3)*inValidN+  
                                            Sd+tstat)
        ,data=DEVCON8b)

