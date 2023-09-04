#loads the Companion to Applied Regression package 
library("car")

#sets where data is stored and where it will be written to
setwd("C:/Users/dgold/Documents/NR")

#reads in the csv data. If a cell is blank, it will be filled with "NA"
lipanData<-read.csv("VHarvestTotalsLipan.csv", header = TRUE, na.strings= c(""))

#This is the first look at the data. 
boxplot(GRAMSPERSQFTCALC~STRAIN, data=lipanData, main="Lipan Grams per Square Foot by Strain", xlab="Strain", ylab="Yield per Square Foot (g)", las=2)  

#Since there are 0 values of GRAMSPERSQFTCALC, we should remove them.
zeroGRAMSPERSQFTCALC <- lipanData[lipanData$GRAMSPERSQFTCALC==0,]
notZeroGRAMSPERSQFTCALC <- lipanData[!lipanData$GRAMSPERSQFTCALC==0,]

as.data.frame(zeroGRAMSPERSQFTCALC)
as.data.frame(notZeroGRAMSPERSQFTCALC)

#This writes a new .csv file with all the records where GRAMSPERSQFTCALC= 0
write.csv(zeroGRAMSPERSQFTCALC, file = "LipanZeroGRAMSPERSQFTCALC.csv")

#New boxplot with the records of GRAMSPERSQFTCALC=0 removed. Notice there are still some possible outliers in the data.   
Boxplot(GRAMSPERSQFTCALC~STRAIN, data=notZeroGRAMSPERSQFTCALC, main="Lipan Grams per Square Foot by Strain Zeros Removed", xlab="Strain", ylab="Yield per Square Foot (g)", las=2)

#This creates a linear model  with STRAIN as the independent variable and GRAMSPERSQFTCALC as the dependent variable 
yieldPerSqFtByStainModel <-lm(GRAMSPERSQFTCALC~STRAIN,data=notZeroGRAMSPERSQFTCALC)

#This performs a statistical outlier test.
modelOutliers <- outlierTest(yieldPerSqFtByStainModel, data=lipanData)

#This extracts the outlier records based on a p value of 0.05 not adjusted for multiple comparisons.
names(modelOutliers$p)

wh<-as.numeric(names(modelOutliers$p))

#lipanOutliers is a data frame with just the outliers. lipanOutliersRemoved is a data frame with the remaining data.  
lipanOutliers <-lipanData[wh,]

lipanOutliersRemoved <-lipanData[-wh,]

#Writes a .csv file of the outliers. 
write.csv(lipanOutliers, file = "lipanOutliers.csv")

#This is a look at the data with the statistical outliers removed. There are some records where the value of GRAMSPERSQFTCALC are outside of 1.5 times the interquartile range. 
boxPlotOutliersRemoved <- Boxplot(GRAMSPERSQFTCALC~STRAIN, data=lipanOutliersRemoved,main="Lipan Grams per Square Foot by Strain Zeros and Outliers Removed", xlab="Strain", ylab="Yield per Square Foot (g)", las=2)

#This extracts the records where the GRAMSPERSQFTCALC  by STRAIN are outside of 1.5 times the interquartile range
boxPlotOutliersExtracted <-Boxplot(GRAMSPERSQFTCALC~STRAIN, data=lipanOutliersRemoved, id.method="y")

boxPlotOutliersExtracted <-as.numeric(boxPlotOutliersExtracted)

dfBoxPlotOutliersExtracted <- lipanOutliersRemoved[boxPlotOutliersExtracted,]

#This creates a .csv file for the records where the GRAMSPERSQFTCALC  by STRAIN are outside of 1.5 times the interquartile range
write.csv(dfBoxPlotOutliersExtracted, file= "LipanBoxPlotOutliersExtracted.csv")

#This removes the boxplot outliers from the data
dfBoxPlotOutliersRemoved <- lipanOutliersRemoved[-boxPlotOutliersExtracted,]

write.csv(dfBoxPlotOutliersRemoved, file="LipanBoxPlotOutliersRemoved.csv")

dfBoxPlotOutliersRemoved <-read.csv("LipanBoxPlotOutliersRemoved.csv", header = TRUE, na.strings= c(""))

#This produced a boxplot with the outliers removed from the previous boxplot. Notice that there are now new outliers as the interquartile range has changed. We could continue to remove outliers but we probably don't want to remove too much data. 
lipanCleanBoxPlot <-Boxplot(GRAMSPERSQFTCALC~STRAIN, data=dfBoxPlotOutliersRemoved, main="Lipan Grams per Square Foot by Strain Zeros and First Boxplot Outliers Removed", xlab="Strain", ylab="Yield per Square Foot (g)", las=2)

#This loads to packages that can do data manipulation including making pivot tables. 
library(dplyr)
library(tidyr)

#These are examples of the data manipulation /pivot tables that can be made. 
lipanData %>%
  dplyr::group_by(lipanData$STRAIN) %>%
summarise( n = n(), "Mean Grams per Square Foot" = mean(GRAMSPERSQFTCALC), "Standard Deviation" = sd(GRAMSPERSQFTCALC))

lipanOutliersRemoved %>%
  dplyr::group_by(lipanOutliersRemoved$STRAIN) %>%
  summarise( n = n(), "Mean Grams per Square Foot" = mean(GRAMSPERSQFTCALC), "Standard Deviation" = sd(GRAMSPERSQFTCALC))

#This loads a package to help with data and time manipulation 
library(lubridate)

#This converts DATE and HARVESTONDATE to a month, day, year format
lipanData$DATE<- mdy(lipanData$DATE)
lipanData$HARVESTONDATE<- mdy(lipanData$HARVESTONDATE)

#This modifies the data frame by inserting a new column (DAYSTOX) which is the difference between DATE and HARVESTONDATE
lipanData$DAYSTOX <-difftime(lipanData$HARVESTONDATE, lipanData$DATE, units = "days")

library(emmeans)
OneWayFit <- aov(GRAMSPERSQFTCALC~STRAIN, data=lipanOutliersRemoved)
summary(OneWayFit)

lsmTukey <- emmeans(OneWayFit, pairwise ~ STRAIN)
lsmTukey

library(multcompView)
CLD(lsmTukey)
multcompView::lsmTukey
