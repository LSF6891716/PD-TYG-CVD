# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#>>>>> Section 0. Packages and Functions used <<<<< ####
{#* section 0.1 Packages ####
  library(foreign)
  library(dplyr)
  library(tidyverse)
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 11. Combine all data <<<<< ####

{#* section 11.2 NHANES 1999-2004  ####
  load(file="I:/NHANES study/PD&TYG&CVD/Data/CON1_baseline.Rdata")
  load(file="I:/NHANES study/PD&TYG&CVD/Data/Covariates_CON1.Rdata")
  load(file="I:/NHANES study/PD&TYG&CVD/Data/Weight_CON1.Rdata")
  Covariates_CON1$BMI<-NULL
  Covariates_CON1$HEI<-NULL
  Covariates_CON1$HEI_Score<-NULL
  multimerge<-function(dat=list(),...){
    if(length(dat)<2)return(as.data.frame(dat))
    mergedat<-dat[[1]]
    dat[[1]]<-NULL
    for(i in dat){
      mergedat<-merge(mergedat,i,...)
    }
    return(mergedat)
  }
  CON1_alldata<-multimerge(list(CON1_baseline,Covariates_CON1,Weight_CON1),by="SEQN",all.x = T)
}
{#* section 11.3 NHANES 2009-2014  ####
  load(file="I:/NHANES study/PD&TYG&CVD/Data/CON2_baseline.Rdata")
  load(file="I:/NHANES study/PD&TYG&CVD/Data/Covariates_CON2.Rdata")
  load(file="I:/NHANES study/PD&TYG&CVD/Data/Weight_CON2.Rdata")
  Covariates_CON2$BMI<-NULL
  Covariates_CON2$HEI<-NULL
  Covariates_CON2$HEI_Score<-NULL
  multimerge<-function(dat=list(),...){
    if(length(dat)<2)return(as.data.frame(dat))
    mergedat<-dat[[1]]
    dat[[1]]<-NULL
    for(i in dat){
      mergedat<-merge(mergedat,i,...)
    }
    return(mergedat)
  }
  CON2_alldata<-multimerge(list(CON2_baseline,Covariates_CON2,Weight_CON2),by="SEQN",all.x = T)
}
{#* section 11.4 merge all data ####
  All_data<-rbind(CON1_alldata,CON2_alldata)
  All_data$sdmvstra<-as.factor(All_data$sdmvstra)
  All_data$sdmvstra<-as.numeric(All_data$sdmvstra)
  record<-ls()
  rm(list=record[-which(record=='All_data')])
  All_data<-subset(All_data,!is.na(CVD))
  save(All_data,file="I:/NHANES study/PD&TYG&CVD/Data/All_data.Rdata")
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 12. Data clearn <<<<< ####

load(file="I:/NHANES study/PD&TYG&CVD/Data/All_data.Rdata")
table(All_data$PD_diagnosis)
All_data$PD_diagnosis<-factor(All_data$PD_diagnosis,
                              levels = c("No/Mild periodontitis","Moderate/Severe periodontitis"))
table(All_data$PD_diagnosis)
table(All_data$Periodontitis_diagnosis)
All_data$Periodontitis_diagnosis<-factor(All_data$Periodontitis_diagnosis,
                                         levels = c("normal","mild","moderate","severe"))
table(All_data$Periodontitis_diagnosis)


table(All_data$diabetes)
summary(All_data$peryear)
table(All_data$Year)
summary(All_data$Age)
All_data$Age[All_data$Age>=80]<-80

table(All_data$Age_status)
table(All_data$Gender)
All_data$Sex<-factor(All_data$Gender,
                     levels = c("Male","Female"))
table(All_data$Gender)
table(All_data$Race_ethnicity)
All_data$Race_ethnicity[All_data$Race_ethnicity=='Non-Hispanic black']<-
  'Non-Hispanic Black'
All_data$Race_ethnicity[All_data$Race_ethnicity=='Non-Hispanic white']<-
  'Non-Hispanic White'
All_data$Race_ethnicity<-factor(All_data$Race_ethnicity,
                                levels = c("Non-Hispanic White","Non-Hispanic Black","Hispanic","Other_Race"))
table(All_data$Race_ethnicity)
table(All_data$Marital_status)
All_data$Marital_status<-factor(All_data$Marital_status,
                                levels = c("Married","Never_married","Separated"))
table(All_data$Marital_status)
table(All_data$Education_levels)
All_data$Education_levels<-factor(All_data$Education_levels,
                                  levels = c("Less_than_high_school","High_school_or_Equivalent","College_or_above"))
table(All_data$Education_levels)
summary(All_data$PIR_raw)
table(All_data$PIR)
All_data$PIR[All_data$PIR_raw<=1]<-
  '(0, 1]'
All_data$PIR[All_data$PIR_raw>1&All_data$PIR_raw<4]<-
  '(1,4)'
All_data$PIR[All_data$PIR_raw>=4]<-
  '[4,inf)'
All_data$PIR<-factor(All_data$PIR,
                     levels = c("(0, 1]","(1,4)","[4,inf)"))
table(All_data$PIR,useNA = "ifany")
table(All_data$Health_insurance)
All_data$Health_insurance<-factor(All_data$Health_insurance,
                                  levels = c("No_insurance","Public_insurance","Private_insurance"))
table(All_data$Health_insurance)
table(All_data$Smoking_status)
All_data$Smoking_status<-factor(All_data$Smoking_status,
                                levels = c("Never_smoker","Former_smoker","Current_smoker"))
table(All_data$Smoking_status)
table(All_data$Drinking_status)
All_data$Drinking_status<-factor(All_data$Drinking_status,
                                 levels = c("Nondrinker","Light/moderate_drinker","Heavier_drinker"))
table(All_data$Drinking_status)
table(All_data$Physical_status)


summary(All_data$BMI)
table(All_data$HTN_status)
table(All_data$HPL_status)
table(All_data$CVD)
table(All_data$SEI)
All_data$SEI<-factor(All_data$SEI,levels = c("Unemployment","Lower","Upper"))
table(All_data$SEI)
colnames(All_data)
All_data$CVD_status<-All_data$CVD
All_data$T2D_status<-All_data$T2D
All_data$Cancer_status<-All_data$Cancer
All_data$Cohort<-All_data$chort
colnames(All_data)
Clearn_data<-All_data[,c("ID","TYG","TyG_WC","TyG_WHtR","TyG_BMI","CVD_status",
                         "PD_diagnosis",
                         "Cohort",
                         "Age","BMI",
                         "Sex","Race_ethnicity","Education_levels","PIR","Health_insurance","SEI",
                         "Smoking_status","Drinking_status","Physical_status",
                         "Cancer_status","HTN_status","T2D_status",
                         "sdmvpsu","sdmvstra","weight")]



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 13. Data conversion <<<<< ####
#as.factor
colApply <- function(dat, cols = colnames, func = as.factor) {
  dat[cols] <- lapply(dat[cols], func)
  return(dat)
}
colname<-c("Physical_status","HTN_status","CVD_status","T2D_status","Cancer_status","sdmvpsu","sdmvstra")
Characters<-colApply(Clearn_data,colname, as.factor)
colnames(Characters)
table(Characters$PD_diagnosis)
Characters<-subset(Characters,PD_diagnosis=="Moderate/Severe periodontitis")
Characters$PD_diagnosis<-NULL
Characters$Periodontitis_diagnosis<-NULL

save(Characters,file="I:/NHANES study/PD&TYG&CVD/Data/Characters.Rdata")
