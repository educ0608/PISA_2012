# PISA2012_2a.R

# Prepared by SUhas D. Parandekar, Wednesday, March 4, 2015

# Revised by SUhas, Wednesday, March 11, 2015


# 1. I generate DEVCON8a file to use
# 2. I add some new variables - NEWID VIETNAM PRESCHOOL VNM.f
# 3. I generate kernel plots
# 4. I determine location of the new variables 927-930 in DEVCON8
# Also the score and repl wts variables 496-635


library(foreign) # to import and export data from R
library(epicalc) # for use in descriptives
library(stargazer) # For latex summary tables
library(sm) # for locally smoothed regressions
library(lme4) # To run mixed models
library(xtable)# To generate Latex inputs
library(xlsx)# To generate MS-Excel output
library(TDMR)# Need this for tuning data mining in R - eg. detect column of constants in dataframe
library(dplyr) # Hadley Wickham's treasure !

# Import PISA data into R and generate extract of data for countries below Per Capita GDP of 10,000 in PPP for 2010
# as contained in OECD-PISA downloaded data PISA Table IV.3.1 
# DEVCON for Developing Countries DEVCONT Students; DEVCONS Schools; DEVCONP PISA consolidated

student.rda <- read.dta("C:/Country/Vietnam/Data/PISA/STATADATA/stu.dta")
school.rda <- read.dta("C:/Country/Vietnam/Data/PISA/STATADATA/sch.dta")

# Albania 

ALB_T <- filter(student.rda, cnt == "ALB")
ALB_S <- filter(school.rda, cnt == "ALB")
ALB_P <- merge(ALB_T, ALB_S, by = "schoolid")
ALB_P$cnt <- ALB_P$cnt.x
ALB_P$cnt.x <- NULL
ALB_P$cnt <- ALB_P$cnt.x
ALB_P$cnt.x <- NULL
ALB_P$subnatio <- ALB_P$subnatio.x
ALB_P$subnatio.x <- NULL
ALB_P$stratum <- ALB_P$stratum.x
ALB_P$stratum.x <- NULL
ALB_P$oecd <- ALB_P$oecd.x
ALB_P$oecd.x <- NULL
ALB_P$nc <- ALB_P$nc.x
ALB_P$nc.x <- NULL
ALB_P$country <-1
ALB_P$NEWID <- (ALB_P$country*10000000)+((as.numeric(ALB_P$schoolid))*10000)+(as.numeric(ALB_P$stidstd))

# Colombia

COL_T <- filter(student.rda, cnt == "COL")
COL_S <- filter(school.rda, cnt == "COL")
COL_P <- merge(COL_T, COL_S, by = "schoolid")
COL_P$cnt <- COL_P$cnt.x
COL_P$cnt.x <- NULL
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
COL_P$country <-2
COL_P$NEWID <- (COL_P$country*10000000)+((as.numeric(COL_P$schoolid))*10000)+(as.numeric(COL_P$stidstd))

# Indonesia

IDN_T <- filter(student.rda, cnt == "IDN")
IDN_S <- filter(school.rda, cnt == "IDN")
IDN_P <- merge(IDN_T, IDN_S, by = "schoolid")
IDN_P$cnt <- IDN_P$cnt.x
IDN_P$cnt.x <- NULL
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
IDN_P$country <-3
IDN_P$NEWID <- (IDN_P$country*10000000)+((as.numeric(IDN_P$schoolid))*10000)+(as.numeric(IDN_P$stidstd))


# Jordan

JOR_T <- filter(student.rda, cnt == "JOR")
JOR_S <- filter(school.rda, cnt == "JOR")
JOR_P <- merge(JOR_T, JOR_S, by = "schoolid")
JOR_P$cnt <- JOR_P$cnt.x
JOR_P$cnt.x <- NULL
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
JOR_P$country <-4
JOR_P$NEWID <- (JOR_P$country*10000000)+((as.numeric(JOR_P$schoolid))*10000)+(as.numeric(JOR_P$stidstd))

# Peru

PER_T <- filter(student.rda, cnt == "PER")
PER_S <- filter(school.rda, cnt == "PER")
PER_P <- merge(PER_T, PER_S, by = "schoolid")
PER_P$cnt <- PER_P$cnt.x
PER_P$cnt.x <- NULL
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
PER_P$country <-5
PER_P$NEWID <- (PER_P$country*10000000)+((as.numeric(PER_P$schoolid))*10000)+(as.numeric(PER_P$stidstd))


# Thailand

THA_T <- filter(student.rda, cnt == "THA")
THA_S <- filter(school.rda, cnt == "THA")
THA_P <- merge(THA_T, THA_S, by = "schoolid")
THA_P$cnt <- THA_P$cnt.x
THA_P$cnt.x <- NULL
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
THA_P$country <-6
THA_P$NEWID <- (THA_P$country*10000000)+((as.numeric(THA_P$schoolid))*10000)+(as.numeric(THA_P$stidstd))


# Tunisia

TUN_T <- filter(student.rda, cnt == "TUN")
TUN_S <- filter(school.rda, cnt == "TUN")
TUN_P <- merge(TUN_T, TUN_S, by = "schoolid")
TUN_P$cnt <- TUN_P$cnt.x
TUN_P$cnt.x <- NULL
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
TUN_P$country <-7
TUN_P$NEWID <- (TUN_P$country*10000000)+((as.numeric(TUN_P$schoolid))*10000)+(as.numeric(TUN_P$stidstd))

# Vietnam

VNM_T <- filter(student.rda, cnt == "VNM")
VNM_S <- filter(school.rda, cnt == "VNM")
VNM_P <- merge(VNM_T, VNM_S, by = "schoolid")
VNM_P$cnt <- VNM_P$cnt.x
VNM_P$cnt.x <- NULL
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
VNM_P$country <-8
VNM_P$NEWID <- (VNM_P$country*10000000)+((as.numeric(VNM_P$schoolid))*10000)+(as.numeric(VNM_P$stidstd))

DEVCON8 <- rbind(ALB_P,COL_P,IDN_P,JOR_P,PER_P,THA_P,TUN_P,VNM_P)

# I had selected the countries together straight from shool and student but that does not work
# because the id variables for school and student are repeated by country
#DEVCONS12 <- filter(school.rda, cnt == "ALB" | cnt =="COL" | cnt=="IDN" | cnt=="JOR"| cnt=="PER"|cnt=="TUN"|cnt=="VNM")

# In need variable names in upper case for later
# application of INTSVY package

names(DEVCON8) <- toupper(names(DEVCON8))

# Introduce Vietnam dummy to do Fryer-Levitt
DEVCON8$VIETNAM[DEVCON8$COUNTRY==8] <- 1
DEVCON8$VIETNAM[DEVCON8$COUNTRY!=8] <- 0

# Introduce attended pre-school dummy from ST05Q01
DEVCON8$PRESCHOOL[DEVCON8$ST05Q01==1] <- 0
DEVCON8$PRESCHOOL[DEVCON8$ST05Q01==2] <- 1
DEVCON8$PRESCHOOL[DEVCON8$ST05Q01==3] <- 1


# Save the file
save(DEVCON8, file="C:/Country/Vietnam/Data/PISA/RDATA/PISA-2012/DEVCON8.rda")


# Get summary of output 
stargazer(DEVCON8,
          type="latex", out="C:/Country/Vietnam/Data/PISA/LATEX/DEVCON8a.pretex",
          style="default",
          align=TRUE,
          digit.separator="",
          summary=TRUE)

# Get Kernel Plots comparisons - Math Science Reading
# I arbitrarily choose PVs 1, 3, 5 - kernel is smoothing technique anyway and it should not matter
# Need to convert country names to factors to apply sm.density.compare function under sm package

DEVCON8$VNM.f <- factor(DEVCON8$VIETNAM, levels=c(0,1),labels = c("GROUP OF 7", "Vietnam"))

# SCIENCE
# I need nbins to be 0 as I am using a smoothing factor h
sm.density.compare(DEVCON8$PV1SCIE,lwd=2,DEVCON8$VNM.f,lty=c(2,1),col=c("blue","red"),
                    nbins=0,h=35,xlab="Science Score",weights=DEVCON8$W_FSTUWT)
# Draw reference line for OECD average
abline(v=500, lty=5, col="grey",lwd=2)
axis(1,at=500,labels=500)
#colfill<-c("blue","red")
legend(-150,0.003, levels(DEVCON8$VNM.f),lty=c(2,1), lwd=2, col=c("blue","red"),bty="n")
text(-50,0.0051, labels="OECD Average",pos=4)
arrows(400, 0.0051, 500, 0.0051)

# MATHEMATICS
sm.density.compare(DEVCON8$PV3MATH,lwd=2,DEVCON8$VNM.f,lty=c(2,1),col=c("darkgreen","red"),
                   nbins=0,h=35,xlab="Mathematics Score",weights=DEVCON8$W_FSTUWT)
# Draw reference line for OECD average
abline(v=500, lty=5, col="grey",lwd=2)
axis(1,at=500,labels=500)
legend(-150,0.003, levels(DEVCON8$VNM.f),lty=c(2,1), lwd=2, col=c("darkgreen","red"),bty="n")
text(-50,0.0050, labels="OECD Average",pos=4)
arrows(400, 0.0050, 500, 0.0050)


# READING
sm.density.compare(DEVCON8$PV5READ,lwd=2,DEVCON8$VNM.f,lty=c(2,1),col=c("purple","red"),
                   nbins=0,h=35,xlab="Reading Score",weights=DEVCON8$W_FSTUWT)
# Draw reference line for OECD average
abline(v=500, lty=5, col="grey",lwd=2)
axis(1,at=500,labels=500)
legend(-150,0.003, levels(DEVCON8$VNM.f),lty=c(2,1), lwd=2, col=c("purple","red"),bty="n")
text(-50,0.0052, labels="OECD Average",pos=4)
arrows(400, 0.0052, 500, 0.0052)


### I need location of variables to recall for later use
grep("NEWID",colnames(DEVCON8)) # 927 -930 are the 4 new variables 
colnames(DEVCON8[930]) # VNM.f

grep("PV1MATH",colnames(DEVCON8)) # 496 
grep("OECD.Y",colnames(DEVCON8)) # 496


# I change the nomenclature of the master file so I can develop further filtered versions 
# according to availability of data

DEVCON8a <- DEVCON8


# Save the file
save(DEVCON8a, file="C:/Country/Vietnam/Data/PISA/RDATA/PISA-2012/DEVCON8a.rda")





























































### ---> from earlier version

# Get ECDF- separately for VN and CO


# Generate Vietnam and Colombia extracts
vnpisa <- subset(PISA_CO_VN,CNT == "VNM")
copisa <- subset(PISA_CO_VN,CNT == "COL")


# ("ASSESS","COGACT","LMINS","MMINS","SMINS",

# Generate Colombia extract

# Now to plot ECDFs of sets of explanatory variables



plot(ecdf(copisa$ASSESS), verticals=TRUE, pch=46, col="black", lty=2, lwd=2, main=NULL, ylab="", xlab="")
plot(ecdf(vnpisa$ASSESS), verticals=TRUE, pch=46, col="red",lty=1,lwd=2, add=2, main=NULL, ylab="", xlab="")
legend(locator(1),levels(CNT.f),lty=c(2,1), lwd=2, col=c("black","red"),bty="n")
text(locator(1),'ASSESS',font=2,1)

summary(vnsch)


# Examine analysis of variance
results_vn <- lmer(PV1MATH ~1 + (1|stratum) + (1|stratum:SCHOOLID), data=vnstu,weights=W_FSTUWT)
summary(results_vn)


# Examine analysis of variance
results_co <- lmer(PV1MATH ~1 + (1|stratum) + (1|stratum:SCHOOLID), data=costu)
summary(results_co)


# I want to do t-test comparisons of the indices generated in PISA database

# I use grep on colnames to determine the order - fortunately they are all contiguously ordered 
# in the schools database ! 

grep("abgmath",colnames(SCH12_CO_VN))
grep("teacclim",colnames(SCH12_CO_VN))

# This tells me the order is 256-288 - I don't automate it for easier understanding of code

grep("clcuse1",colnames(STU12_CO_VN))
grep("ancsubnorm",colnames(STU12_CO_VN))

# This tells me the order is 404-500 - I don't automate it for easier understanding of code

# I initially thought of making two sperate dataframes, but it is not required 
indices_scl <- SCH12_CO_VN[,names(SCH12_CO_VN)[256:288]]
indices_stu <- STU12_CO_VN[,names(SCH12_CO_VN)[404:500]]

# I use sapply to apply t.test function many times

# First I do for the schools dataset, then I do for the students

comp_scl <- t(sapply(SCH12_CO_VN[256:288], function(x) 
  unlist(t.test(x~SCH12_CO_VN$CNT,paired=FALSE)[c("estimate","p.value","statistic","conf.int")])))
# I wanted to know what is the format of the output - it is a matrix
class(comp_scl)
# I give more reasonable names 
colnames(comp_scl) <- c("MEAN_COL","MEAN_VNM","P_VALUE", "T-STATISTIC", "CONFINT1", "CONFINT2") 
# I select those indices where significant difference at 5% level of significance
comp_scl_sig <- comp_scl[(which(abs(comp_scl[,4]) > 1.96)),]
# I export to MS-Excel
write.xlsx(comp_scl_sig, "C:/Country/Vietnam/PISA WP/comp_scl_sig.xlsx") 
# And generate input for Latex
xtable(comp_scl_sig)

# Now repeat above steps (except for class()) for STU
# Some variables are apparently all constant or all NAs so gives an error message !

# can not run :-( comp_stu <- t(sapply(STU12_CO_VN[404:500], function(x) 
#  unlist(t.test(x~STU12_CO_VN$CNT,paired=FALSE)[c("estimate","p.value","statistic","conf.int")])))

# I find out and eliminate all vars that are all NAs
DELS <- which((sapply(STU12_CO_VN, function(x)all(is.na(x)))))
STU12_CO_VN <- STU12_CO_VN[, -DELS] 
# delete 134 variables, but still has non-numerical variables in the choice range


# First I need to convert the CNT variable to a numeric one - as I am going to have to eliminate
# all non-numeric variables - I generate a new CNT_vn variable that has value 0 for col and 1 for vn

STU12_CO_VN$CNT_n[STU12_CO_VN$CNT=="COL"] <- 1
STU12_CO_VN$CNT_n[STU12_CO_VN$CNT=="VNM"] <- 2

# STU12_CO_VN now has 503 variables
# Then, I need to get rid of any non-numeric and non-logical vectors
temp1 <- STU12_CO_VN[,sapply(STU12_CO_VN,is.numeric)] # 476 out of 503 variables, none logical

# Second, I check for contant columns 
allsame1 <- tdmPreFindConstVar(temp1)
allsame1 # I find two - oecd and easy - I need to delete them from temp1
#Note the - selection !
temp1 <- subset(temp1, select=c(-EASY,-OECD))
# I will also get rid of two identifying variables on qres I do not really know
temp1 <- subset(temp1, select=c(-QUESTID, -BOOKID))
# Check the results - should be NULL now
allsame1 <- tdmPreFindConstVar(temp1)
allsame1
rm(allsame1) # Just needed to identify and then confirm NULL


# I need to  bring back the factor for grouping. 
temp1$CNT.f <- factor(temp1$CNT_n, levels=c(1,2),labels = c("Colombia", "Vietnam"))
table(temp1$CNT.f)


# I try to find out the locations of the variables I need
grep("CLCUSE1",colnames(temp1))
grep("ANCSUBNORM",colnames(temp1))

# I get 272 and 337 - and try to apply this
comp_stu <- t(sapply(temp1[272:337], function(x) 
 unlist(t.test(x~temp1$CNT.f,paired=FALSE)[c("estimate","p.value","statistic","conf.int")])))

colnames(comp_stu) <- c("MEAN_COL","MEAN_VNM","P_VALUE", "T-STATISTIC", "CONFINT1", "CONFINT2") 
# I select those indices where significant difference at 5% level of significance
comp_stu_sig <- comp_stu[(which(abs(comp_stu[,4]) > 1.96)),]
# I export to MS-Excel
write.xlsx(comp_stu_sig, "C:/Country/Vietnam/PISA WP/comp_stu_sig.xlsx") 
# And generate input for Latex
xtable(comp_stu_sig)






comp_stu <- t(sapply(temp1[c(389:400)], function(x) 
    unlist(t.test(x~temp1$CNT.f,paired=FALSE)[c("estimate","p.value","statistic","conf.int")])))
  
tests1 <- lapply(seq(1,ncol(Gf), by=2), 
                 function (x){t.test(Gf[,x],Gf[,x+1],paired=FALSE)})
print(tests1)
table(STU12_CO_VN[,c(450)])

CNT_f <- factor(STU12_CO_VN$CNT_n)
