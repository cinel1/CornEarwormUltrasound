#Processing corn earworm (Helicoverpa zea) egg counts for statistical analysis
#Written by Scott D. Cinel, 2018 - 2020

#Setting up file pathways and inserting data
library("BiocManager")
BiocManager::install('readr')
library(readr)
setwd("~/OneDrive - University of Florida/Grad School/UF/Dissertation/Data&Results/Chap2ValidatingResponse/")

#Data Exploration
#Creating subsets of data to explore relationships among trial conditions, i.e. excluding potentially unreliable data entries, etc.
#You first need to create these files from scratch in excel using the cleaned datasheet written out to file and separate those entries into the following two datasets: 1) 'All the Data' and 2) 'Exclusion of trials wherein individuals were stuck in coitus for >48 hrs'.

EggData <- read.csv("~/OneDrive - University of Florida/Grad School/UF/Dissertation/Data&Results/Chap2ValidatingResponse/EggData_NoStuck_1to3only_totalLaid.csv")
summary(EggData)
EggData <- EggData[-c(52,53,54,55,56),]
summary(EggData)
#Just looking at data distributions to get a better feel for the data
par(mfrow = c(1,1))

#Note that we are only looking at total eggs laid at this point, not emerged vs non-emerged
barplot((EggData$TotalToDateScaled), ylab = "Egg Count",xlab="Trial", main = "Total Eggs Laid")
hist(EggData$TotalToDateScaled)
barplot(table(EggData$Ultrasound), ylab = "Samples", main = "Ultrasound?")
barplot(table(EggData$Trial), ylab = "Sample Count")
barplot(table(EggData$ExposureType), ylab = "Sample Count", main = "Sound Exposure Type")
barplot(table(EggData$Room), ylab = "Sample Count", main = "Room Used")
barplot(table(EggData$DaysAlive), ylab = "Sample Count", main = "Days Alive")
barplot(table(EggData$Month), ylab = "Sample Count", main = "Month")

#Subsetting data from 'AllEggData' dataframe into useful chunks for modeling
as.factor(EggData$Trial)
as.factor(EggData$Ultrasound)
as.factor(EggData$ExposureType)
as.factor(EggData$Room)
as.factor(EggData$Stuck)
as.factor(EggData$Ultrasound)

#Using full dataset including matings where female became locked in copulation, as well as reproductive variability due to inclusion of 1to1, 1to2, and 1to4 mating trials and other outliers
summary(EggData)
hist((EggData$TotalToDateScaled))
#Build initial linear models to investigate assumptions of 1) linearity of residuals, 2) independence of residuals, 3) normality of residual distribution, and 4) equal variance throughout residuals

?par
par(mfrow = c(1,1))
testLM<-lm(EggData$TotalToDateScaled ~ EggData$Ultrasound)
anova(testLM)

#Check residual variance distribution for heteroscedasticity, check QQ plot for evidence of non-normally distributed residuals, and the last 2 plots for outliers

#Note that residual distribution is not normal and heteroscedasticity is an issue
plot(testLM)

#Try transforming variables
EggData$LogTotalToDateScaled <- log(1+EggData$TotalToDateScaled)
transformedLM <- lm(LogTotalToDateScaled ~ Ultrasound, data = EggData)
plot(transformedLM)
anova(transformedLM)

#Transforming is unsuccessful, so we move on to using non-parametric tests

#Use Kruskall-Wallis to avoid parametric assumptions (leads to loss in power though)
kruskal1<-kruskal.test(TotalToDateScaled ~ Trial, data = EggData)
kruskal2<-kruskal.test(TotalToDateScaled ~ ExposureType, data = EggData)
kruskal3<-kruskal.test(TotalToDateScaled ~ Ultrasound, data = EggData)
kruskal4<-kruskal.test(TotalToDateScaled ~ Room, data = EggData)
kruskal5<-kruskal.test(TotalToDateScaled ~ Month, data = EggData)

kruskalEmerged <- kruskal.test(EggData$TotalEmergedScaled ~ EggData$Trial)
kruskalEmerged

kruskalNonEmerged <- kruskal.test(EggData$TotalNonEmergedScaled~EggData$Trial)
kruskalNonEmerged

#Use anova if data meets assumptions of normality
anova1<-aov(TotalToDateScaled ~ Trial, data = EggData)
anova2<-aov(TotalToDateScaled ~ ExposureType, data= EggData)
anova3<-aov(TotalToDateScaled ~ Ultrasound, data = EggData)
anova4<-aov(TotalToDateScaled ~ Room, data = EggData)
anova5<-aov(TotalToDateScaled ~ Month, data = EggData)

kruskal1 #non-parametric significance test on finely-defined experimental groups
kruskal2 #non-parametric significance test for no sound vs. bat calls vs. over-stimulation
kruskal3 #non-parametric significance test for effect of any ultrasound vs no ultrasound
kruskal4 #non-parametric significance test for effect of adult rearing location
kruskal5 #non-parametric significance test for effect of month

summary(anova1) #parametric significance test on finely-defined experimental groups
summary(anova2) #parametric significance test for no sound vs. bat calls vs. over-stimulation
summary(anova3) #parametric significance test for effect of any ultrasound vs no ultrasound
summary(anova4) #parametric significance test for effect of adult rearing location
summary(anova5) #parametric significance test for effect of month

#Test significance of any ultrasound (i.e. ultrasound = Y or N) using t test due to only 2 response categories.
?t.test
ttest <- t.test(TotalToDateScaled ~ Ultrasound, data=EggData)
ttest

#Significance tests indicate that month has a strong effect and that any ultrasound exposure at all has a nearly significant effect

#Moving on to general and AICc-based generalized linear modeling to see if we can show a significant effect of sound this way

#Should consider controlling for month as a fixed or random effect (i.e. just make sure it is accounted for in modeling)
install.packages("lme4")

library(nlme)

#So now we know that we can't use this full model as is and we must transform variables in some way or alternatively we can use linear mixed effects models to specify specific variables as fixed vs random effects and essentially this allows us to control for inavoidable effects of experimental setups

library(lme4)
?lme4
install.packages("lmerTest")
library(lmerTest)
?lmer
?lmerTest
?AIC

#This is a good point to try and include fixed and random effects and see if that helps improve our model predictive ability

#Generally, variables we are interested in, even if nuisance variables and we just want to know they did not have an effect, are fixed effects while other sampling artifacts are random effects

#TotalLaid
lmMonth <- lm(TotalToDateScaled ~ Month, data = EggData)
summary(lmMonth)

lmmTotal <- lmer(TotalToDateScaled ~ Ultrasound + (1|Month), data = EggData)
summary(lmmTotal)

lmmTotalByTrial <- lmer(TotalToDateScaled ~ Trial + (1|Month), data = EggData)
summary(lmmTotalByTrial)

lmTotalByTrial2 <- lm(TotalToDateScaled ~ Trial + Month, data = EggData)
summary(lmTotalByTrial2)

#In effect, we see that when controlling for effect of month, ultrasound exposure has a nearly significant effect on total egg output

#Plotting the egg output data for lab meeting
?plot
plot(TotalToDateScaled~Ultrasound, data = EggData)
plot(TotalToDateScaled~Trial, data=EggData)

plot(EggData$TotalEmergedScaled~Ultrasound, data = EggData)
plot(EggData$TotalEmergedScaled~Trial, data = EggData)

plot(TotalNonEmergedScaled~Ultrasound, data = EggData)
plot(TotalNonEmergedScaled~Trial, data = EggData)

#Now we move on to investigating the effect of ultrasound and exposure type on emergence vs non-emergence outcomes

#Consider turning successful and unsuccessful egg rates into ratios or percentages and use that as a dependent variable

#Full model for days alive as adults
lmmDaysAlive <- lmer(DaysAlive ~ Ultrasound + (1|Month), data = EggData)
summary(lmmDaysAlive)

#Full model for total eggs laid through time - Will need to adapt this to be time-sensitive

#Now investigate spermatophore transfer rates
SpermatophoreData <- read.csv("~/OneDrive - University of Florida/Grad School/UF/Dissertation/Data&Results/Chap2ValidatingResponse/Spermatophores_1to3only_cleaned.csv")
hist(SpermatophoreData$SpermatophoreNumber)
as.factor(SpermatophoreData$Trial)
as.factor(SpermatophoreData$Month)
as.factor(SpermatophoreData$Ultrasound)

t.test(SpermatophoreNumber ~ Ultrasound, data=SpermatophoreData)

plot(SpermatophoreNumber ~ Ultrasound, data=SpermatophoreData)
plot(SpermatophoreNumber ~ Trial, data = SpermatophoreData)

lmmSperms <- lmer(SpermatophoreNumber ~ Ultrasound + (1|Month), data = SpermatophoreData)
summary(lmmSperms)

lmmSperms2 <- lmer(SpermatophoreNumber ~ Trial + (1|Month), data = SpermatophoreData)
summary(lmmSperms2)

#Looks like spermatophore transfer rates were unaffected by ultrasound or by trial type

#Now look at emerged and non-enmerged egg numbers and proportions
barplot((EggData$TotalEmergedScaled), ylab = "Egg Count",xlab="Trial", main = "Total Emerged")
barplot((EggData$TotalNonEmergedScaled), ylab = "Egg Count",xlab="Trial", main = "Total Non-Emerged")
hist(EggData$TotalEmergedScaled)
hist(EggData$TotalNonEmergedScaled)

lmmTotalEmerged <- lmer(TotalEmergedScaled ~ Ultrasound + (1|Month), data = EggData)
summary(lmmTotalEmerged)

lmmTotalEmergedTrial <- lmer(TotalEmergedScaled ~ Trial + (1|Month), data = EggData)
summary(lmmTotalEmergedTrial)

##So any ultrasound exposure has an effect on the total number of eggs that emerged
lmmTotalNonEmerged <- lmer(TotalNonEmergedScaled ~ Ultrasound + (1|Month), data = EggData)
summary(lmmTotalNonEmerged)

lmmTotalNonEmergedTrial <- lmer(TotalNonEmergedScaled ~ Trial + (1|Month), data = EggData)
summary(lmmTotalNonEmergedTrial)

##But ultrasound exposure had no effect on the number that did not emerge

lmmProportionEmerged <- lmer(ProportionEmerged ~ Ultrasound + (1|Month), data = EggData)
summary(lmmProportionEmerged)

#And not the proportion of eggs that emerged either
