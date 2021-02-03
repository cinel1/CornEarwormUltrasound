#Corn earworm larval data processing and statistical analyses

#Note that raw data is entered into the 'LarvalData_Compiled&Cleaned datasheet before it is parsed first to the LarvalDataCompiled_1to3only sheet by filtering out any non-1 male to 3 females mate-pairings

#and then further parsed into the specific datasheets for each specific question, i.e. 1to3only_SexCleaned

#loading data
library("BiocManager")
BiocManager::install('readr')
library(readr)
setwd("~/OneDrive - University of Florida/Grad School/UF/Dissertation/Data&Results/Chap2ValidatingResponse/")
library(readr)

#Importing specific datasets for each response without any NAs for each
LarvalData <- read.csv("~/OneDrive - University of Florida/Grad School/UF/Dissertation/Data&Results/Chap2ValidatingResponse/LarvalData.csv")
summary(LarvalData)

SexLarvalData <- read.csv("~/OneDrive - University of Florida/Grad School/UF/Dissertation/Data&Results/Chap2ValidatingResponse/SexLarvalData.csv")
summary(SexLarvalData)
as.factor(SexLarvalData$Sex)
as.factor(SexLarvalData$Trial)
as.factor(SexLarvalData$Ultrasound)

PupalMassLarvalData <- read.csv("~/OneDrive - University of Florida/Grad School/UF/Dissertation/Data&Results/Chap2ValidatingResponse/PupalMassLarvalData.csv")
as.factor(PupalMassLarvalData$Sex)
as.factor(PupalMassLarvalData$Trial)
as.factor(PupalMassLarvalData$Ultrasound)

PupalAgeLarvalData <- read.csv("~/OneDrive - University of Florida/Grad School/UF/Dissertation/Data&Results/Chap2ValidatingResponse/PupalAgeLarvalData.csv")
as.factor(PupalAgeLarvalData$Sex)
as.factor(PupalAgeLarvalData$Trial)
as.factor(PupalAgeLarvalData$Ultrasound)

EmergeAgeLarvalData <- read.csv("~/OneDrive - University of Florida/Grad School/UF/Dissertation/Data&Results/Chap2ValidatingResponse/EmergeAgeLarvalData.csv")
as.factor(EmergeAgeLarvalData$Sex)
as.factor(EmergeAgeLarvalData$Trial)
as.factor(EmergeAgeLarvalData$Ultrasound)

#Need to explore data for normality, etc. 
plot(as.factor(SexLarvalData$Sex))
summary(SexLarvalData)
hist(PupalMassLarvalData$PupalMass)
plot(lm(PupalMassLarvalData$PupalMass ~ PupalMassLarvalData$Trial))
hist(PupalAgeLarvalData$PupationAge)
plot(lm(PupalAgeLarvalData$PupationAge ~ PupalAgeLarvalData$Trial))
hist(EmergeAgeLarvalData$EmergenceAge)
plot(lm(EmergeAgeLarvalData$EmergenceAge ~ EmergeAgeLarvalData$Trial))

plot(PupalMassLarvalData$PupalMass~PupalMassLarvalData$Trial)
plot(SexLarvalData$Sex~SexLarvalData$Trial)
plot(PupalAgeLarvalData$PupationAge ~ PupalAgeLarvalData$Trial)
plot(EmergeAgeLarvalData$EmergenceAge ~ EmergeAgeLarvalData$Trial)

#Chi-Square test for effect of treatment on sex
?kruskal.test
summary(SexLarvalData)
kruskalSexByTrial <- kruskal.test(Sex ~ Trial, data = SexLarvalData)
kruskalSexByTrial

kruskalSexUltrasound <- kruskal.test(Sex ~ Ultrasound, data = SexLarvalData)
kruskalSexUltrasound

kruskalSexDate <- kruskal.test(Sex ~ FoodDate, data = SexLarvalData)
kruskalSexDate

#Test for significant effect on average pupal mass
kruskalMassTrial <- kruskal.test(PupalMass ~ Trial, data=PupalMassLarvalData)
kruskalMassTrial

kruskalMassUltrasound <- kruskal.test(PupalMass ~ Ultrasound, data=PupalMassLarvalData)
kruskalMassUltrasound

kruskalMassDate <- kruskal.test(PupalMass ~ FoodDate, data = PupalMassLarvalData)
kruskalMassDate

#Pupal Age
kruskalPupalAgeTrial <- kruskal.test(PupationAge ~ Trial, data=PupalAgeLarvalData)
kruskalPupalAgeTrial

kruskalPupalAgeUltrasound <- kruskal.test(PupationAge ~ Ultrasound, data=PupalAgeLarvalData)
kruskalPupalAgeUltrasound

kruskalPupalAgeDate <- kruskal.test(PupationAge ~ FoodDate, data = PupalAgeLarvalData)
kruskalPupalAgeDate

#Emergence Age
kruskalEmergenceAgeTrial <- kruskal.test(EmergenceAge ~ Trial, data=EmergeAgeLarvalData)
kruskalEmergenceAgeTrial

kruskalEmergenceAgeUltrasound <- kruskal.test(EmergenceAge ~ Ultrasound, data = EmergeAgeLarvalData)
kruskalEmergenceAgeUltrasound

kruskalEmergenceAgeDate <- kruskal.test(EmergenceAge ~ FoodDate, data = EmergeAgeLarvalData)
kruskalEmergenceAgeDate

#Linear mixed modeling to control for random effect of "Month" (i.e. time of the year and cohort effects in one)
library(lme4)
library(lmerTest)

#Sex
?glmer
glmerSex <- glmer(as.factor(Sex) ~ Ultrasound +  (1|Month), family = binomial(link = "logit"), data = SexLarvalData)
summary(glmerSex)

glmerSex2 <- glmer(as.factor(Sex) ~ Trial + (1|Month), family = binomial, data = SexLarvalData)
summary(glmerSex2)

#PupalMass
lmeMass1 <- lmer(PupalMass ~ Trial (1|Month), data = PupalMassLarvalData)
summary(lmeMass1)

lmeMass2 <- lmer(PupalMass ~ Ultrasound + (1|Month), data = PupalMassLarvalData)
summary(lmeMass2)

lmeMass3 <- lmer(PupalMass ~ Sex + (1|Month), data = PupalMassLarvalData)
summary(lmeMass3)

#Pupal Age

lmePupalAge1 <- lmer(PupationAge ~ Trial + (1|Replicate), data = PupalAgeLarvalData)
summary(lmePupalAge1)

lmePupalAge2 <- lmer(PupationAge ~ Ultrasound + (1|Replicate), data = PupalAgeLarvalData)
summary(lmePupalAge2)

lmePupalAge3 <- lmer(PupationAge ~ Sex + (1|Replicate), data = PupalAgeLarvalData)
summary(lmePupalAge3)

#Emerge Age

lmeEmergeAge1 <- lmer(EmergenceAge ~ Trial + (1|Replicate), data = EmergeAgeLarvalData)
summary(lmeEmergeAge1)

lmeEmergeAge2 <- lmer(EmergenceAge ~ Ultrasound + (1|Replicate), data = EmergeAgeLarvalData)
summary(lmeEmergeAge2)

lmeEmergeAge3 <- lmer(EmergenceAge ~ Sex + (1|Replicate), data = EmergeAgeLarvalData)
summary(lmeEmergeAge3)

#Female and male specific analyses
PupalAgeFemale <- read.csv("~/OneDrive - University of Florida/Grad School/UF/Dissertation/Data&Results/Chap2ValidatingResponse/PupalAgeFemale.csv")
PupalAgeMale <- read.csv("~/OneDrive - University of Florida/Grad School/UF/Dissertation/Data&Results/Chap2ValidatingResponse/PupalAgeMale.csv")

PupalMassFemale <- read.csv("~/OneDrive - University of Florida/Grad School/UF/Dissertation/Data&Results/Chap2ValidatingResponse/PupalMassFemale.csv")
PupalMassMale <- read.csv("~/OneDrive - University of Florida/Grad School/UF/Dissertation/Data&Results/Chap2ValidatingResponse/PupalMassMale.csv")


ttestMassMaleVsFemale <- t.test(PupalMassFemale$PupalMass,PupalMassMale$PupalMass)
ttestMassMaleVsFemale

lmeMassMale1 <- lmer(PupalMass ~ Trial + (1|Replicate), data = PupalMassMale)
summary(lmeMassMale1)

lmeMassMale2 <- lmer(PupalMass ~ Ultrasound + (1|Replicate), data = PupalMassMale)
summary(lmeMassMale2)

lmeMassFemale1 <- lmer(PupalMass ~ Trial + (1|Replicate), data = PupalMassFemale)
summary(lmeMassFemale1)

lmeMassFemale2 <- lmer(PupalMass ~ Ultrasound + (1|Replicate), data = PupalMassFemale)
summary(lmeMassFemale2)


ttestPupationAgeMaleVsFemale <- t.test(PupalAgeFemale$PupationAge,PupalAgeMale$PupationAge)
ttestPupationAgeMaleVsFemale

ttestEmergenceAgeMaleVsFemale <- t.test(EmergeAgeFemale$EmergenceAge,EmergeAgeMale$EmergenceAge)
ttestEmergenceAgeMaleVsFemale

##Pupation and Emergence Rates
PupalRateData <- read.csv("~/OneDrive - University of Florida/Grad School/UF/Dissertation/Data&Results/Chap2ValidatingResponse/LarvalDataCompiled_1to3only.csv")
summary(PupalRateData$Pupated)
summary(PupalRateData$Emerged)

glmerPupalRate <- glmer(Pupated ~ Ultrasound +  (1|Month), family = binomial, data = PupalRateData)
summary(glmerPupalRate)

glmerEmergeRate <- glmer(Emerged ~ Ultrasound +  (1|Month), family = binomial, data = PupalRateData)
summary(glmerEmergeRate)