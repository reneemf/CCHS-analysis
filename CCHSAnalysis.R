##For ease of use, run code in the console
#Import packages and data and set up test frames (2001, 2009 & 2014)
library(readxl)
library(ggplot2)
library(dplyr)

#Import datasets via drop down menu for best results
#2001 is an excel file, 2009 & 2014 are .csv
Y2001_survey <- read_excel("cchs-82M0013-E-2001-c1-1-general-file_F1.xlsx")
Y2001_test <- data.frame(Province=Y2001_survey$GEOAGPRV, Region=Y2001_survey$GEOADPMF, Health=Y2001_survey$GENA_01, Income=Y2001_survey$INCAGHH, Food=Y2001_survey$FINAF1)
Y2001_test_m <- as.matrix(Y2001_test)
head(Y2001_test)
tail(Y2001_test)
str(Y2001_test)

Y2009_survey <- read.csv("CCHS-82M0013-E-2009-2010-Annualcomponent_F1.csv")
Y2009_test <- data.frame(Province=Y2009_survey$GEOGPRV, Region=Y2009_survey$GEODPMF, Health=Y2009_survey$GEN_01, Income=Y2009_survey$INCGHH, Food=Y2009_survey$FSC_010)
Y2009_test_m <- as.matrix(Y2009_test)
head(Y2009_test)
tail(Y2009_test)
str(Y2009_test)

Y2014_survey <- read.csv("CCHS-82M0013-E-2009-2010-Annualcomponent_F1.csv")
Y2014_test <- data.frame(Province=Y2014_survey$GEOGPRV, Region=Y2014_survey$GEODPMF, Health=Y2014_survey$GEN_01, Income=Y2014_survey$INCGHH, Food=Y2014_survey$FSC_010)
Y2014_test_m <- as.matrix(Y2014_test)
head(Y2014_test)
tail(Y2014_test)
str(Y2014_test)

#form plots with test frames
#analysis of test data focused on Self-Reported Health, Province, Health Region, Income, Food (In)security
#NFLD is New Foundland
ggplot(Y2001_test,aes(x=Province,y=Health)) + geom_point()
ggplot(Y2001_test,aes(x=Income,y=Health)) + geom_point()
ggplot(Y2001_test,aes(x=Food,y=Health)) + geom_point() + facet_wrap(~ Province)

Y2001_NFLD <- Y2001_test %>% filter(Province == 10)
ggplot(Y2001_NFLD,aes(x=Income,y=Health)) + geom_point()
gplot(Y2001_NFLD,aes(x=Food,y=Health)) + geom_point()

ggplot(Y2009_test,aes(x=Province,y=Health)) + geom_point()
ggplot(Y2009_test,aes(x=Income,y=Health)) + geom_point()
ggplot(Y2009_test,aes(x=Food,y=Health)) + geom_point() + facet_wrap(~ Province)

Y2009_NFLD <- Y2009_test %>% filter(Province == 10)
ggplot(Y2009_NFLD,aes(x=Income,y=Health)) + geom_point()
ggplot(Y2009_NFLD,aes(x=Food,y=Health)) + geom_point()

ggplot(Y2014_test,aes(x=Province,y=Health)) + geom_point()
ggplot(Y2014_test,aes(x=Income,y=Health)) + geom_point()
ggplot(Y2014_test,aes(x=Food,y=Health)) + geom_point() + facet_wrap(~ Province)

Y2014_NFLD <- Y2009_test %>% filter(Province == 10)
ggplot(Y2014_NFLD,aes(x=Income,y=Health)) + geom_point()
ggplot(Y2014_NFLD,aes(x=Food,y=Health)) + geom_point()

#Log10 plotting test of data frames
ggplot(Y2001_test,aes(x=Province,y=Health)) + geom_point() + scale_y_log10()
ggplot(Y2001_NFLD,aes(x=Income,y=Health)) + geom_point() + scale_y_log10()
ggplot(Y2001_test,aes(x=Food,y=Health)) + geom_point() + scale_x_log10() + facet_wrap(~ Province)
ggplot(Y2001_NFLD,aes(x=Food,y=Health)) + geom_point()+ scale_y_log10()

ggplot(Y2009_test,aes(x=Province,y=Health)) + geom_point() + scale_y_log10()
ggplot(Y2009_test,aes(x=Food,y=Health)) + geom_point() + scale_y_log10() + facet_wrap(~ Province)
ggplot(Y2009_NFLD,aes(x=Income,y=Health)) + geom_point() + scale_y_log10() + facet_wrap(~ Province)
ggplot(Y2009_NFLD,aes(x=Income,y=Health)) + geom_point() + scale_x_log10()

ggplot(Y2014_test,aes(x=Province,y=Health)) + geom_point() + scale_y_log10()
ggplot(Y2014_test,aes(x=Income,y=Health)) + geom_point() + scale_y_log10() + facet_wrap(~ Province)
ggplot(Y2014_NFLD,aes(x=Food,y=Health)) + geom_point() + scale_y_log10()
ggplot(Y2014_NFLD,aes(x=Income,y=Health)) + geom_point() + scale_x_log10()

#Remove or replace unwanted, N/A and infinite variables and values
#9090 is a figure not found prior within the data
#It can later be searched for, isolated, removed, etc.
Y2001_survey$ADM_RNO <- NULL
Y2001_survey[is.na(d)] <- 9090

Y2009_survey$VERDATE <- NULL
Y2009_survey$ADM_RNO <- NULL
Y2009_survey[is.na(d)] <- 9090

Y2014_survey$VERDATE <- NULL
Y2014_survey$ADM_RNO <- NULL
Y2014_survey[is.na(d)] <- 9090

#Below analysis focuses on 2014 data, run code in console
#Check data frame contents
head(Y2014_survey,n=5)
tail(Y2014_survey,n=5)
str(Y2014_survey)

#Create test frames - income (inc) and health
inc_frame <- data.frame(INC1 = Y2014_survey$INCG2, INC2 = Y2014_survey$INCG7, INC3 = Y2014_survey$INCGHH, INC4 = Y2014_survey$INCGPER, INC5 = Y2014_survey$INCDRCA, INC6 = Y2014_survey$INCDRPR, INC7 = Y2014_survey$INCDRRS)
health_frame <- data.frame(GEN1 = Y2014_survey$GEN_01, GEN2 = Y2014_survey$GEN_02, GEN3 = Y2014_survey$GEN_02A2, GEN4 = Y2014_survey$GEN_02B, GEN5 = Y2014_survey$GEN_07, GEN6 = Y2014_survey$GEN_08, GEN7 = Y2014_survey$GEN_09, GEN8 = Y2014_survey$GEN_10, GEN9 = Y2014_survey$GENDHDI, GEN10 = Y2014_survey$GENDMHI, GEN11 = Y2014_survey$GENGSWL)

#Begin parallel (CF) analysis
inc_para <- psych::fa.parallel(inc_frame, fm='minres', fa='fa', n.iter=50, SMC=TRUE, quant=.95)
health_para <- psych::fa.parallel(health_frame, fm='minres', fa='fa', n.iter=50, SMC=TRUE, quant=.95)

#Begin factor analysis - 
inc_factor <- psych::fa(inc_frame,nfactors = 5,rotate = "oblimin",fm="ml")
health_factor <- psych::fa(health_matrix,nfactors = 6,rotate = "oblimin",fm="ml")

