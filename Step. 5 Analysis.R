# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 0. Packages and Functions used <<<<< ####
{#* section 0 Packages ####
  library(caret)
  library(car)
  library(cmprsk)
  library(dplyr)
  library(foreign)
  library(ggplot2)
  library(ggsci)
  library(ggrepel)
  library("ggthemes")
  library(lava)
  library(Matching)
  library(mediation)
  library(mice)
  library(pec)
  #install.packages("poLCA", dependencies = TRUE)
  library(poLCA)
  library(plyr)
  library(prodlim)
  library(reshape2)
  library(rms)
  library(riskRegression)
  library(survey)
  library(scales)
  library(survminer)
  library(survival)
  library(splines)
  library(timeROC)
  library(tableone)
  library(rms)
  library(withr)
  library(dplyr)
  library(doParallel)
}
# +++++++++++================================+++++++++++ ####
# +++++++++++============Manuscript==============+++++++++++ ####
# +++++++++++================================+++++++++++ ####  
# +++++++++++============Tables==========+++++++++++ ####  
# >>>>> section 18. Multiple interpolation data (Table 1, Table S3)  ####
load(file="I:/NHANES study/PD&TYG&CVD/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)

rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
var<-c("CVD_status","Age_status","Sex","Race_ethnicity","Education_levels","PIR",
       "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status",
       "HTN_status",
       "BMI_status","T2D_status","Cancer_status","Cohort","TyG_WC_quantile","TyG_WHtR_quantile","TyG_BMI_quantile")
VAR<-c("CVD_status","Age","Age_status","Sex","Race_ethnicity","Education_levels","PIR",
           "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status", "HTN_status",
           "BMI","BMI_status","T2D_status","Cancer_status","Cohort","TYG","TyG_WC","TyG_WHtR","TyG_BMI","TyG_WC_quantile","TyG_WHtR_quantile","TyG_BMI_quantile")

options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)
{ #* section 18.1 Over all ####
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  Over<- ldply(lapply(VAR, model))
}
options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)

{ #* section 18.2 No/Mild periodontitis ####
  rhcSvy_HEI2PD<-subset(rhcSvy,TYG_quantile=="Quantile 1")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_HEI2PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  Quantile1<- ldply(lapply(VAR, model))
}  
{ #* section 18.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,TYG_quantile=="Quantile 2")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  Quantile2<- ldply(lapply(VAR, model))
}
{ #* section 18.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,TYG_quantile=="Quantile 3")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  Quantile3<- ldply(lapply(VAR, model))
}
{ #* section 18.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,TYG_quantile=="Quantile 4")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  Quantile4<- ldply(lapply(VAR, model))
}
Table1<-cbind(Over,Quantile1[,c("counts","Mean","SE")],
              Quantile2[,c("counts","Mean","SE")],
              Quantile3[,c("counts","Mean","SE")],
              Quantile4[,c("counts","Mean","SE")])
table(Interpolation_weighted$TYG_quantile)
save(Table1,file = "I:/NHANES study/PD&TYG&CVD/Data/Table1_Rdata")  
{ #* section 18.4 t-test and chi-test ####
  model<- function(x){
    
    if( x %in% var ) {
      formula<-as.formula(paste0("~",x,"+TYG_quantile"))
      chi_test<-svychisq(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =chi_test[["p.value"]])
      return(model)
    } else {
      formula<-as.formula(paste0(x,"~TYG_quantile"))
      fit<-svyglm(formula, design=rhcSvy)
      result<-regTermTest(fit, "TYG_quantile")
      model <- data.frame('Covariates'=x,
                          'P value' =result$p)
      
      return(model)
    }
  }  
  test_data<- ldply(lapply(VAR, model))
  test_data$P.value<-round(test_data$P.value,3)
  test_data$P.value[test_data$P.value==0]<-"<0.001"
  new.function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  test_data$P.value<-lapply(test_data$P.value,new.function)
  test_data$P.value<-as.character(test_data$P.value)
}
load(file = "I:/NHANES study/PD&TYG&CVD/Data/Table1_Rdata")
Table1<-merge(Table1,test_data,by="Covariates",all.x = T)
Table1$Covariates
Table1$Row<-paste0(Table1$Covariates," ",Table1$grade)
rownames(Table1)<-Table1$Row
colnames(Table1)<-c("Covariates","grade",
                    "counts_all","Mean_all","SE_all",
                    "counts_TYG_quantile1","Mean_TYG_quantile1","SE_TYG_quantile1",
                    "counts_TYG_quantile2","Mean_TYG_quantile2","SE_TYG_quantile2",
                    "counts_TYG_quantile3","Mean_TYG_quantile3","SE_TYG_quantile3",
                    "counts_TYG_quantile4","Mean_TYG_quantile4","SE_TYG_quantile4",
                    "P.value","Row")
rownames(Table1)
{ #* section 18.5 Combine  Table 1#####
#Events
#all.cause
PD.counts<-table(Interpolation_weighted$TYG_quantile,useNA = "ifany")

PD_quantile1<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 1"),]
PD_quantile2<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 2"),]
PD_quantile3<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 3"),]
PD_quantile4<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 4"),]

PD_quantile1.counts_ad<-format(round(sum(PD_quantile1$weight)), big.mark = ",", scientific = FALSE)
PD_quantile2.counts_ad<-format(round(sum(PD_quantile2$weight)), big.mark = ",", scientific = FALSE)
PD_quantile3.counts_ad<-format(round(sum(PD_quantile3$weight)), big.mark = ",", scientific = FALSE)
PD_quantile4.counts_ad<-format(round(sum(PD_quantile4$weight)), big.mark = ",", scientific = FALSE)


Counts_ad<-format(round(sum(PD_quantile1$weight))+
                  round(sum(PD_quantile2$weight))+
                  round(sum(PD_quantile3$weight))+
                  round(sum(PD_quantile4$weight)), big.mark = ",", scientific = FALSE)


Table<-c("","","","TYG quantile","","","","","","","","")
Table<-rbind(Table,c("","Over all","",
                     "TYG Quantile 1","",
                     "TYG Quantile 2","",
                     "TYG Quantile 3","",
                     "TYG Quantile 4","",
                     ""))
Table<-rbind(Table,c("Characteristics",
                     "Mean/ %*","SE*",
                     "Mean/ %","SE",
                     "Mean/ %","SE*",
                     "Mean/ %","SE",
                     "Mean/ %","SE","P†"))
Table<-rbind(Table,c("No.(Unweighted)",PD.counts[1]+PD.counts[2]+PD.counts[3]+PD.counts[4],"",
                     PD.counts[1],"",
                     PD.counts[2],"",
                     PD.counts[3],"",
                     PD.counts[4],"",
                     ""))
Table<-rbind(Table,c("No.(Weighted)",Counts_ad,"",
                     PD_quantile1.counts_ad,"",
                     PD_quantile2.counts_ad,"",
                     PD_quantile3.counts_ad,"",
                     PD_quantile4.counts_ad,"",""))
#Diabetes mellitus
Table<-rbind(Table,c("Cardiovascular disease",
                     Table1["CVD_status YES","Mean_all"],Table1["CVD_status YES","SE_all"],
                     Table1["CVD_status YES","Mean_TYG_quantile1"],Table1["CVD_status YES","SE_TYG_quantile1"],
                     Table1["CVD_status YES","Mean_TYG_quantile2"],Table1["CVD_status YES","SE_TYG_quantile2"],
                     Table1["CVD_status YES","Mean_TYG_quantile3"],Table1["CVD_status YES","SE_TYG_quantile3"],
                     Table1["CVD_status YES","Mean_TYG_quantile4"],Table1["CVD_status YES","SE_TYG_quantile4"],
                     Table1["CVD_status YES","P.value"]))
#Age
Table<-rbind(Table,c("Age (years), mean",
                     Table1["Age Mean ± SE","Mean_all"],Table1["Age Mean ± SE","SE_all"],
                     Table1["Age Mean ± SE","Mean_TYG_quantile1"],Table1["Age Mean ± SE","SE_TYG_quantile1"],
                     Table1["Age Mean ± SE","Mean_TYG_quantile2"],Table1["Age Mean ± SE","SE_TYG_quantile2"],
                     Table1["Age Mean ± SE","Mean_TYG_quantile3"],Table1["Age Mean ± SE","SE_TYG_quantile3"],
                     Table1["Age Mean ± SE","Mean_TYG_quantile4"],Table1["Age Mean ± SE","SE_TYG_quantile4"],
                     Table1["Age Mean ± SE","P.value"] ))
Table<-rbind(Table,c("Age status, %","","","","","","","","","","",Table1["Age_status <45","P.value"]))
Table<-rbind(Table,c("<45",
                     Table1["Age_status <45","Mean_all"],Table1["Age_status <45","SE_all"],
                     Table1["Age_status <45","Mean_TYG_quantile1"],Table1["Age_status <45","SE_TYG_quantile1"],
                     Table1["Age_status <45","Mean_TYG_quantile2"],Table1["Age_status <45","SE_TYG_quantile2"],
                     Table1["Age_status <45","Mean_TYG_quantile3"],Table1["Age_status <45","SE_TYG_quantile3"],
                     Table1["Age_status <45","Mean_TYG_quantile4"],Table1["Age_status <45","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("[45, 65)",
                     Table1["Age_status [45,65)","Mean_all"],Table1["Age_status <45","SE_all"],
                     Table1["Age_status [45,65)","Mean_TYG_quantile1"],Table1["Age_status [45,65)","SE_TYG_quantile1"],
                     Table1["Age_status [45,65)","Mean_TYG_quantile2"],Table1["Age_status [45,65)","SE_TYG_quantile2"],
                     Table1["Age_status [45,65)","Mean_TYG_quantile3"],Table1["Age_status [45,65)","SE_TYG_quantile3"],
                     Table1["Age_status [45,65)","Mean_TYG_quantile4"],Table1["Age_status [45,65)","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("≥65",
                     Table1["Age_status >=65","Mean_all"],Table1["Age_status >=65","SE_all"],
                     Table1["Age_status >=65","Mean_TYG_quantile1"],Table1["Age_status >=65","SE_TYG_quantile1"],
                     Table1["Age_status >=65","Mean_TYG_quantile2"],Table1["Age_status >=65","SE_TYG_quantile2"],
                     Table1["Age_status >=65","Mean_TYG_quantile3"],Table1["Age_status >=65","SE_TYG_quantile3"],
                     Table1["Age_status >=65","Mean_TYG_quantile4"],Table1["Age_status >=65","SE_TYG_quantile4"],
                     "" ))

#Sex
Table<-rbind(Table,c("Sex, Female",
                     Table1["Sex Female","Mean_all"],Table1["Sex Female","SE_all"],
                     Table1["Sex Female","Mean_TYG_quantile1"],Table1["Sex Female","SE_TYG_quantile1"],
                     Table1["Sex Female","Mean_TYG_quantile2"],Table1["Sex Female","SE_TYG_quantile2"],
                     Table1["Sex Female","Mean_TYG_quantile3"],Table1["Sex Female","SE_TYG_quantile3"],
                     Table1["Sex Female","Mean_TYG_quantile4"],Table1["Sex Female","SE_TYG_quantile4"],
                     Table1["Sex Female","P.value"]))

#Race/ ethnicity
Table<-rbind(Table,c("Race/ ethnicity, %","","","","","","","","","","",Table1["Race_ethnicity Non-Hispanic White","P.value"]))
Table<-rbind(Table,c("Non-Hispanic white",
                     Table1["Race_ethnicity Non-Hispanic White","Mean_all"],Table1["Race_ethnicity Non-Hispanic White","SE_all"],
                     Table1["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile1"],Table1["Race_ethnicity Non-Hispanic White","SE_TYG_quantile1"],
                     Table1["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile2"],Table1["Race_ethnicity Non-Hispanic White","SE_TYG_quantile2"],
                     Table1["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile3"],Table1["Race_ethnicity Non-Hispanic White","SE_TYG_quantile3"],
                     Table1["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile4"],Table1["Race_ethnicity Non-Hispanic White","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Non-Hispanic black",
                     Table1["Race_ethnicity Non-Hispanic Black","Mean_all"],Table1["Race_ethnicity Non-Hispanic Black","SE_all"],
                     Table1["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile1"],Table1["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile1"],
                     Table1["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile2"],Table1["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile2"],
                     Table1["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile3"],Table1["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile3"],
                     Table1["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile4"],Table1["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Hispanic",
                     Table1["Race_ethnicity Hispanic","Mean_all"],Table1["Race_ethnicity Hispanic","SE_all"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile1"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile1"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile2"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile2"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile3"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile3"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile4"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Other race/ ethnicity",
                     Table1["Race_ethnicity Other_Race","Mean_all"],Table1["Race_ethnicity Other_Race","SE_all"],
                     Table1["Race_ethnicity Other_Race","Mean_TYG_quantile1"],Table1["Race_ethnicity Other_Race","SE_TYG_quantile1"],
                     Table1["Race_ethnicity Other_Race","Mean_TYG_quantile2"],Table1["Race_ethnicity Other_Race","SE_TYG_quantile2"],
                     Table1["Race_ethnicity Other_Race","Mean_TYG_quantile3"],Table1["Race_ethnicity Other_Race","SE_TYG_quantile3"],
                     Table1["Race_ethnicity Other_Race","Mean_TYG_quantile4"],Table1["Race_ethnicity Other_Race","SE_TYG_quantile4"],
                     "" ))

#Socioeconomic Status
Table<-rbind(Table,c("Socioeconomic Status, %","","","","","","","","","","",Table1["SES low","P.value"]))
Table<-rbind(Table,c("Low",
                     Table1["SES low","Mean_all"],Table1["SES low","SE_all"],
                     Table1["SES low","Mean_TYG_quantile1"],Table1["SES low","SE_TYG_quantile1"],
                     Table1["SES low","Mean_TYG_quantile2"],Table1["SES low","SE_TYG_quantile2"],
                     Table1["SES low","Mean_TYG_quantile3"],Table1["SES low","SE_TYG_quantile3"],
                     Table1["SES low","Mean_TYG_quantile4"],Table1["SES low","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Medium",
                     Table1["SES medium","Mean_all"],Table1["SES medium","SE_all"],
                     Table1["SES medium","Mean_TYG_quantile1"],Table1["SES medium","SE_TYG_quantile1"],
                     Table1["SES medium","Mean_TYG_quantile2"],Table1["SES medium","SE_TYG_quantile2"],
                     Table1["SES medium","Mean_TYG_quantile3"],Table1["SES medium","SE_TYG_quantile3"],
                     Table1["SES medium","Mean_TYG_quantile4"],Table1["SES medium","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("High",
                     Table1["SES high","Mean_all"],Table1["SES high","SE_all"],
                     Table1["SES high","Mean_TYG_quantile1"],Table1["SES high","SE_TYG_quantile1"],
                     Table1["SES high","Mean_TYG_quantile2"],Table1["SES high","SE_TYG_quantile2"],
                     Table1["SES high","Mean_TYG_quantile3"],Table1["SES high","SE_TYG_quantile3"],
                     Table1["SES high","Mean_TYG_quantile4"],Table1["SES high","SE_TYG_quantile4"],
                     "" ))

#Smoking status
Table<-rbind(Table,c("Smoking status, %","","","","","","","","","","",Table1["Smoking_status Never_smoker","P.value"]))
Table<-rbind(Table,c("Never smoker",
                     Table1["Smoking_status Never_smoker","Mean_all"],Table1["Smoking_status Never_smoker","SE_all"],
                     Table1["Smoking_status Never_smoker","Mean_TYG_quantile1"],Table1["Smoking_status Never_smoker","SE_TYG_quantile1"],
                     Table1["Smoking_status Never_smoker","Mean_TYG_quantile2"],Table1["Smoking_status Never_smoker","SE_TYG_quantile2"],
                     Table1["Smoking_status Never_smoker","Mean_TYG_quantile3"],Table1["Smoking_status Never_smoker","SE_TYG_quantile3"],
                     Table1["Smoking_status Never_smoker","Mean_TYG_quantile4"],Table1["Smoking_status Never_smoker","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Former smoker",
                     Table1["Smoking_status Former_smoker","Mean_all"],Table1["Smoking_status Former_smoker","SE_all"],
                     Table1["Smoking_status Former_smoker","Mean_TYG_quantile1"],Table1["Smoking_status Former_smoker","SE_TYG_quantile1"],
                     Table1["Smoking_status Former_smoker","Mean_TYG_quantile2"],Table1["Smoking_status Former_smoker","SE_TYG_quantile2"],
                     Table1["Smoking_status Former_smoker","Mean_TYG_quantile3"],Table1["Smoking_status Former_smoker","SE_TYG_quantile3"],
                     Table1["Smoking_status Former_smoker","Mean_TYG_quantile4"],Table1["Smoking_status Former_smoker","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Current smoker",
                     Table1["Smoking_status Current_smoker","Mean_all"],Table1["Smoking_status Current_smoker","SE_all"],
                     Table1["Smoking_status Current_smoker","Mean_TYG_quantile1"],Table1["Smoking_status Current_smoker","SE_TYG_quantile1"],
                     Table1["Smoking_status Current_smoker","Mean_TYG_quantile2"],Table1["Smoking_status Current_smoker","SE_TYG_quantile2"],
                     Table1["Smoking_status Current_smoker","Mean_TYG_quantile3"],Table1["Smoking_status Current_smoker","SE_TYG_quantile3"],
                     Table1["Smoking_status Current_smoker","Mean_TYG_quantile4"],Table1["Smoking_status Current_smoker","SE_TYG_quantile4"],
                     "" ))

#Drinking status
Table<-rbind(Table,c("Drinking status, %","","","","","","","","","","",Table1["Drinking_status Nondrinker","P.value"]))
Table<-rbind(Table,c("Nondrinker",
                     Table1["Drinking_status Nondrinker","Mean_all"],Table1["Drinking_status Nondrinker","SE_all"],
                     Table1["Drinking_status Nondrinker","Mean_TYG_quantile1"],Table1["Drinking_status Nondrinker","SE_TYG_quantile1"],
                     Table1["Drinking_status Nondrinker","Mean_TYG_quantile2"],Table1["Drinking_status Nondrinker","SE_TYG_quantile2"],
                     Table1["Drinking_status Nondrinker","Mean_TYG_quantile3"],Table1["Drinking_status Nondrinker","SE_TYG_quantile3"],
                     Table1["Drinking_status Nondrinker","Mean_TYG_quantile4"],Table1["Drinking_status Nondrinker","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Light/ moderate drinker",
                     Table1["Drinking_status Light/moderate_drinker","Mean_all"],Table1["Drinking_status Light/moderate_drinker","SE_all"],
                     Table1["Drinking_status Light/moderate_drinker","Mean_TYG_quantile1"],Table1["Drinking_status Light/moderate_drinker","SE_TYG_quantile1"],
                     Table1["Drinking_status Light/moderate_drinker","Mean_TYG_quantile2"],Table1["Drinking_status Light/moderate_drinker","SE_TYG_quantile2"],
                     Table1["Drinking_status Light/moderate_drinker","Mean_TYG_quantile3"],Table1["Drinking_status Light/moderate_drinker","SE_TYG_quantile3"],
                     Table1["Drinking_status Light/moderate_drinker","Mean_TYG_quantile4"],Table1["Drinking_status Light/moderate_drinker","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Heavier drinker",
                     Table1["Drinking_status Heavier_drinker","Mean_all"],Table1["Drinking_status Heavier_drinker","SE_all"],
                     Table1["Drinking_status Heavier_drinker","Mean_TYG_quantile1"],Table1["Drinking_status Heavier_drinker","SE_TYG_quantile1"],
                     Table1["Drinking_status Heavier_drinker","Mean_TYG_quantile2"],Table1["Drinking_status Heavier_drinker","SE_TYG_quantile2"],
                     Table1["Drinking_status Heavier_drinker","Mean_TYG_quantile3"],Table1["Drinking_status Heavier_drinker","SE_TYG_quantile3"],
                     Table1["Drinking_status Heavier_drinker","Mean_TYG_quantile4"],Table1["Drinking_status Heavier_drinker","SE_TYG_quantile4"],
                     "" ))
#Physical status
Table<-rbind(Table,c("Physical status, %","","","","","","","","","","",Table1["Physical_status Inactive","P.value"]))
Table<-rbind(Table,c("Inactive",
                     Table1["Physical_status Inactive","Mean_all"],Table1["Physical_status Inactive","SE_all"],
                     Table1["Physical_status Inactive","Mean_TYG_quantile1"],Table1["Physical_status Inactive","SE_TYG_quantile1"],
                     Table1["Physical_status Inactive","Mean_TYG_quantile2"],Table1["Physical_status Inactive","SE_TYG_quantile2"],
                     Table1["Physical_status Inactive","Mean_TYG_quantile3"],Table1["Physical_status Inactive","SE_TYG_quantile3"],
                     Table1["Physical_status Inactive","Mean_TYG_quantile4"],Table1["Physical_status Inactive","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Insufficient",
                     Table1["Physical_status Insufficient","Mean_all"],Table1["Physical_status Insufficient","SE_all"],
                     Table1["Physical_status Insufficient","Mean_TYG_quantile1"],Table1["Physical_status Insufficient","SE_TYG_quantile1"],
                     Table1["Physical_status Insufficient","Mean_TYG_quantile2"],Table1["Physical_status Insufficient","SE_TYG_quantile2"],
                     Table1["Physical_status Insufficient","Mean_TYG_quantile3"],Table1["Physical_status Insufficient","SE_TYG_quantile3"],
                     Table1["Physical_status Insufficient","Mean_TYG_quantile4"],Table1["Physical_status Insufficient","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Recommended",
                     Table1["Physical_status Recommended","Mean_all"],Table1["Physical_status Recommended","SE_all"],
                     Table1["Physical_status Recommended","Mean_TYG_quantile1"],Table1["Physical_status Recommended","SE_TYG_quantile1"],
                     Table1["Physical_status Recommended","Mean_TYG_quantile2"],Table1["Physical_status Recommended","SE_TYG_quantile2"],
                     Table1["Physical_status Recommended","Mean_TYG_quantile3"],Table1["Physical_status Recommended","SE_TYG_quantile3"],
                     Table1["Physical_status Recommended","Mean_TYG_quantile4"],Table1["Physical_status Recommended","SE_TYG_quantile4"],
                     "" ))


#BMI
Table<-rbind(Table,c("BMI, Mean",
                     Table1["BMI Mean ± SE","Mean_all"],Table1["BMI Mean ± SE","SE_all"],
                     Table1["BMI Mean ± SE","Mean_TYG_quantile1"],Table1["BMI Mean ± SE","SE_TYG_quantile1"],
                     Table1["BMI Mean ± SE","Mean_TYG_quantile2"],Table1["BMI Mean ± SE","SE_TYG_quantile2"],
                     Table1["BMI Mean ± SE","Mean_TYG_quantile3"],Table1["BMI Mean ± SE","SE_TYG_quantile3"],
                     Table1["BMI Mean ± SE","Mean_TYG_quantile4"],Table1["BMI Mean ± SE","SE_TYG_quantile4"],
                     Table1["BMI Mean ± SE","P.value"] ))

Table<-rbind(Table,c("BMI status (kg/m2), %","","","","","","","","","","",Table1["BMI_status (0,25)","P.value"]))
Table<-rbind(Table,c("<25",
                     Table1["BMI_status (0,25)","Mean_all"],Table1["BMI_status (0,25)","SE_all"],
                     Table1["BMI_status (0,25)","Mean_TYG_quantile1"],Table1["BMI_status (0,25)","SE_TYG_quantile1"],
                     Table1["BMI_status (0,25)","Mean_TYG_quantile2"],Table1["BMI_status (0,25)","SE_TYG_quantile2"],
                     Table1["BMI_status (0,25)","Mean_TYG_quantile3"],Table1["BMI_status (0,25)","SE_TYG_quantile3"],
                     Table1["BMI_status (0,25)","Mean_TYG_quantile4"],Table1["BMI_status (0,25)","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("[25.0 -30)",
                     Table1["BMI_status [25.0-30)","Mean_all"],Table1["BMI_status [25.0-30)","SE_all"],
                     Table1["BMI_status [25.0-30)","Mean_TYG_quantile1"],Table1["BMI_status [25.0-30)","SE_TYG_quantile1"],
                     Table1["BMI_status [25.0-30)","Mean_TYG_quantile2"],Table1["BMI_status [25.0-30)","SE_TYG_quantile2"],
                     Table1["BMI_status [25.0-30)","Mean_TYG_quantile3"],Table1["BMI_status [25.0-30)","SE_TYG_quantile3"],
                     Table1["BMI_status [25.0-30)","Mean_TYG_quantile4"],Table1["BMI_status [25.0-30)","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("≥30",
                     Table1["BMI_status [30,inf)","Mean_all"],Table1["BMI_status [30,inf)","SE_all"],
                     Table1["BMI_status [30,inf)","Mean_TYG_quantile1"],Table1["BMI_status [30,inf)","SE_TYG_quantile1"],
                     Table1["BMI_status [30,inf)","Mean_TYG_quantile2"],Table1["BMI_status [30,inf)","SE_TYG_quantile2"],
                     Table1["BMI_status [30,inf)","Mean_TYG_quantile3"],Table1["BMI_status [30,inf)","SE_TYG_quantile3"],
                     Table1["BMI_status [30,inf)","Mean_TYG_quantile4"],Table1["BMI_status [30,inf)","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Comorbidities, %","","","","","","","","","","",""))
#Hypertension
Table<-rbind(Table,c("Hypertension",
                     Table1["HTN_status YES","Mean_all"],Table1["HTN_status YES","SE_all"],
                     Table1["HTN_status YES","Mean_TYG_quantile1"],Table1["HTN_status YES","SE_TYG_quantile1"],
                     Table1["HTN_status YES","Mean_TYG_quantile2"],Table1["HTN_status YES","SE_TYG_quantile2"],
                     Table1["HTN_status YES","Mean_TYG_quantile3"],Table1["HTN_status YES","SE_TYG_quantile3"],
                     Table1["HTN_status YES","Mean_TYG_quantile4"],Table1["HTN_status YES","SE_TYG_quantile4"],
                     Table1["HTN_status YES","P.value"]))
#Diabetes mellitus
Table<-rbind(Table,c("Diabetes mellitus",
                     Table1["T2D_status YES","Mean_all"],Table1["T2D_status YES","SE_all"],
                     Table1["T2D_status YES","Mean_TYG_quantile1"],Table1["T2D_status YES","SE_TYG_quantile1"],
                     Table1["T2D_status YES","Mean_TYG_quantile2"],Table1["T2D_status YES","SE_TYG_quantile2"],
                     Table1["T2D_status YES","Mean_TYG_quantile3"],Table1["T2D_status YES","SE_TYG_quantile3"],
                     Table1["T2D_status YES","Mean_TYG_quantile4"],Table1["T2D_status YES","SE_TYG_quantile4"],
                     Table1["T2D_status YES","P.value"]))
#Cohort
Table<-rbind(Table,c("Cohort period, %","","","","","","","","","","",Table1["Cohort NHANES_CON1","P.value"]))

Table<-rbind(Table,c("NHANES 1999-2004",
                     Table1["Cohort NHANES_CON1","Mean_all"],Table1["Cohort NHANES_CON1","SE_all"],
                     Table1["Cohort NHANES_CON1","Mean_TYG_quantile1"],Table1["Cohort NHANES_CON1","SE_TYG_quantile1"],
                     Table1["Cohort NHANES_CON1","Mean_TYG_quantile2"],Table1["Cohort NHANES_CON1","SE_TYG_quantile2"],
                     Table1["Cohort NHANES_CON1","Mean_TYG_quantile3"],Table1["Cohort NHANES_CON1","SE_TYG_quantile3"],
                     Table1["Cohort NHANES_CON1","Mean_TYG_quantile4"],Table1["Cohort NHANES_CON1","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("NHANES 2009-2014",
                     Table1["Cohort NHANES_CON2","Mean_all"],Table1["Cohort NHANES_CON2","SE_all"],
                     Table1["Cohort NHANES_CON2","Mean_TYG_quantile1"],Table1["Cohort NHANES_CON2","SE_TYG_quantile1"],
                     Table1["Cohort NHANES_CON2","Mean_TYG_quantile2"],Table1["Cohort NHANES_CON2","SE_TYG_quantile2"],
                     Table1["Cohort NHANES_CON2","Mean_TYG_quantile3"],Table1["Cohort NHANES_CON2","SE_TYG_quantile3"],
                     Table1["Cohort NHANES_CON2","Mean_TYG_quantile4"],Table1["Cohort NHANES_CON2","SE_TYG_quantile4"],
                     "" ))

Table_1<-Table
write.table(Table_1,sep = ",",file ="I:/NHANES study/PD&TYG&CVD/Result/Table 1.csv" ,row.names =F,col.names =F )
}




{ #* section 18.6 Combine  Table S3#####
  Table<-c("","","","TYG quantile","","","","","","","","")
  Table<-rbind(Table,c("","Over all","",
                       "TYG Quantile 1","",
                       "TYG Quantile 2","",
                       "TYG Quantile 3","",
                       "TYG Quantile 4","",
                       ""))
  Table<-rbind(Table,c("Characteristics",
                       "Mean/ %*","SE*",
                       "Mean/ %","SE",
                       "Mean/ %","SE*",
                       "Mean/ %","SE",
                       "Mean/ %","SE","P†"))
  Table<-rbind(Table,c("No.(Unweighted)",PD.counts[1]+PD.counts[2]+PD.counts[3]+PD.counts[4],"",
                       PD.counts[1],"",
                       PD.counts[2],"",
                       PD.counts[3],"",
                       PD.counts[4],"",
                       ""))
  Table<-rbind(Table,c("No.(Weighted)",Counts_ad,"",
                       PD_quantile1.counts_ad,"",
                       PD_quantile2.counts_ad,"",
                       PD_quantile3.counts_ad,"",
                       PD_quantile4.counts_ad,"",""))
#Socioeconomic index
Table<-rbind(Table,c("Socioeconomic index, %","","","","","","","","","","",Table1["SEI Unemployment","P.value"]))
Table<-rbind(Table,c("Unemployment",
                     Table1["SEI Unemployment","Mean_all"],Table1["SEI Unemployment","SE_all"],
                     Table1["SEI Unemployment","Mean_TYG_quantile1"],Table1["SEI Unemployment","SE_TYG_quantile1"],
                     Table1["SEI Unemployment","Mean_TYG_quantile2"],Table1["SEI Unemployment","SE_TYG_quantile2"],
                     Table1["SEI Unemployment","Mean_TYG_quantile3"],Table1["SEI Unemployment","SE_TYG_quantile3"],
                     Table1["SEI Unemployment","Mean_TYG_quantile4"],Table1["SEI Unemployment","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Lower",
                     Table1["SEI Lower","Mean_all"],Table1["SEI Lower","SE_all"],
                     Table1["SEI Lower","Mean_TYG_quantile1"],Table1["SEI Lower","SE_TYG_quantile1"],
                     Table1["SEI Lower","Mean_TYG_quantile2"],Table1["SEI Lower","SE_TYG_quantile2"],
                     Table1["SEI Lower","Mean_TYG_quantile3"],Table1["SEI Lower","SE_TYG_quantile3"],
                     Table1["SEI Lower","Mean_TYG_quantile4"],Table1["SEI Lower","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Upper",
                     Table1["SEI Upper","Mean_all"],Table1["SEI Upper","SE_all"],
                     Table1["SEI Upper","Mean_TYG_quantile1"],Table1["SEI Upper","SE_TYG_quantile1"],
                     Table1["SEI Upper","Mean_TYG_quantile2"],Table1["SEI Upper","SE_TYG_quantile2"],
                     Table1["SEI Upper","Mean_TYG_quantile3"],Table1["SEI Upper","SE_TYG_quantile3"],
                     Table1["SEI Upper","Mean_TYG_quantile4"],Table1["SEI Upper","SE_TYG_quantile4"],
                     "" ))

#Poverty income ratio
Table<-rbind(Table,c("Poverty income ratio, %","","","","","","","","","","",Table1["PIR (0, 1]","P.value"]))
Table<-rbind(Table,c("<1.00",
                     Table1["PIR (0, 1]","Mean_all"],Table1["PIR (0, 1]","SE_all"],
                     Table1["PIR (0, 1]","Mean_TYG_quantile1"],Table1["PIR (0, 1]","SE_TYG_quantile1"],
                     Table1["PIR (0, 1]","Mean_TYG_quantile2"],Table1["PIR (0, 1]","SE_TYG_quantile2"],
                     Table1["PIR (0, 1]","Mean_TYG_quantile3"],Table1["PIR (0, 1]","SE_TYG_quantile3"],
                     Table1["PIR (0, 1]","Mean_TYG_quantile4"],Table1["PIR (0, 1]","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("1.00-3.99",
                     Table1["PIR (1,4)","Mean_all"],Table1["PIR (1,4)","SE_all"],
                     Table1["PIR (1,4)","Mean_TYG_quantile1"],Table1["PIR (1,4)","SE_TYG_quantile1"],
                     Table1["PIR (1,4)","Mean_TYG_quantile2"],Table1["PIR (1,4)","SE_TYG_quantile2"],
                     Table1["PIR (1,4)","Mean_TYG_quantile3"],Table1["PIR (1,4)","SE_TYG_quantile3"],
                     Table1["PIR (1,4)","Mean_TYG_quantile4"],Table1["PIR (1,4)","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("≥4.00",
                     Table1["PIR [4,inf)","Mean_all"],Table1["PIR [4,inf)","SE_all"],
                     Table1["PIR [4,inf)","Mean_TYG_quantile1"],Table1["PIR [4,inf)","SE_TYG_quantile1"],
                     Table1["PIR [4,inf)","Mean_TYG_quantile2"],Table1["PIR [4,inf)","SE_TYG_quantile2"],
                     Table1["PIR [4,inf)","Mean_TYG_quantile3"],Table1["PIR [4,inf)","SE_TYG_quantile3"],
                     Table1["PIR [4,inf)","Mean_TYG_quantile4"],Table1["PIR [4,inf)","SE_TYG_quantile4"],
                     "" ))
#Health insurance
Table<-rbind(Table,c("Health insurance, %","","","","","","","","","","",Table1["Health_insurance No_insurance","P.value"]))
Table<-rbind(Table,c("No insurance",
                     Table1["Health_insurance No_insurance","Mean_all"],Table1["Health_insurance No_insurance","SE_all"],
                     Table1["Health_insurance No_insurance","Mean_TYG_quantile1"],Table1["Health_insurance No_insurance","SE_TYG_quantile1"],
                     Table1["Health_insurance No_insurance","Mean_TYG_quantile2"],Table1["Health_insurance No_insurance","SE_TYG_quantile2"],
                     Table1["Health_insurance No_insurance","Mean_TYG_quantile3"],Table1["Health_insurance No_insurance","SE_TYG_quantile3"],
                     Table1["Health_insurance No_insurance","Mean_TYG_quantile4"],Table1["Health_insurance No_insurance","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Public insurance only",
                     Table1["Health_insurance Public_insurance","Mean_all"],Table1["Health_insurance Public_insurance","SE_all"],
                     Table1["Health_insurance Public_insurance","Mean_TYG_quantile1"],Table1["Health_insurance Public_insurance","SE_TYG_quantile1"],
                     Table1["Health_insurance Public_insurance","Mean_TYG_quantile2"],Table1["Health_insurance Public_insurance","SE_TYG_quantile2"],
                     Table1["Health_insurance Public_insurance","Mean_TYG_quantile3"],Table1["Health_insurance Public_insurance","SE_TYG_quantile3"],
                     Table1["Health_insurance Public_insurance","Mean_TYG_quantile4"],Table1["Health_insurance Public_insurance","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Private insurance",
                     Table1["Health_insurance Private_insurance","Mean_all"],Table1["Health_insurance Private_insurance","SE_all"],
                     Table1["Health_insurance Private_insurance","Mean_TYG_quantile1"],Table1["Health_insurance Private_insurance","SE_TYG_quantile1"],
                     Table1["Health_insurance Private_insurance","Mean_TYG_quantile2"],Table1["Health_insurance Private_insurance","SE_TYG_quantile2"],
                     Table1["Health_insurance Private_insurance","Mean_TYG_quantile3"],Table1["Health_insurance Private_insurance","SE_TYG_quantile3"],
                     Table1["Health_insurance Private_insurance","Mean_TYG_quantile4"],Table1["Health_insurance Private_insurance","SE_TYG_quantile4"],
                     "" ))
#Education levels
Table<-rbind(Table,c("Education levels, %","","","","","","","","","","",Table1["Education_levels Less_than_high_school","P.value"]))
Table<-rbind(Table,c("Less than high school",
                     Table1["Education_levels Less_than_high_school","Mean_all"],Table1["Education_levels Less_than_high_school","SE_all"],
                     Table1["Education_levels Less_than_high_school","Mean_TYG_quantile1"],Table1["Education_levels Less_than_high_school","SE_TYG_quantile1"],
                     Table1["Education_levels Less_than_high_school","Mean_TYG_quantile2"],Table1["Education_levels Less_than_high_school","SE_TYG_quantile2"],
                     Table1["Education_levels Less_than_high_school","Mean_TYG_quantile3"],Table1["Education_levels Less_than_high_school","SE_TYG_quantile3"],
                     Table1["Education_levels Less_than_high_school","Mean_TYG_quantile4"],Table1["Education_levels Less_than_high_school","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("High school or equivalent",
                     Table1["Education_levels High_school_or_Equivalent","Mean_all"],Table1["Education_levels High_school_or_Equivalent","SE_all"],
                     Table1["Education_levels High_school_or_Equivalent","Mean_TYG_quantile1"],Table1["Education_levels High_school_or_Equivalent","SE_TYG_quantile1"],
                     Table1["Education_levels High_school_or_Equivalent","Mean_TYG_quantile2"],Table1["Education_levels High_school_or_Equivalent","SE_TYG_quantile2"],
                     Table1["Education_levels High_school_or_Equivalent","Mean_TYG_quantile3"],Table1["Education_levels High_school_or_Equivalent","SE_TYG_quantile3"],
                     Table1["Education_levels High_school_or_Equivalent","Mean_TYG_quantile4"],Table1["Education_levels High_school_or_Equivalent","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Hispanic",
                     Table1["Race_ethnicity Hispanic","Mean_all"],Table1["Race_ethnicity Hispanic","SE_all"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile1"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile1"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile2"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile2"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile3"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile3"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile4"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile4"],
                     "" ))
#Socioeconomic Status
Table<-rbind(Table,c("Socioeconomic Status, %","","","","","","","","","","",Table1["SES low","P.value"]))
Table<-rbind(Table,c("Low",
                     Table1["SES low","Mean_all"],Table1["SES low","SE_all"],
                     Table1["SES low","Mean_TYG_quantile1"],Table1["SES low","SE_TYG_quantile1"],
                     Table1["SES low","Mean_TYG_quantile2"],Table1["SES low","SE_TYG_quantile2"],
                     Table1["SES low","Mean_TYG_quantile3"],Table1["SES low","SE_TYG_quantile3"],
                     Table1["SES low","Mean_TYG_quantile4"],Table1["SES low","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Medium",
                     Table1["SES medium","Mean_all"],Table1["SES medium","SE_all"],
                     Table1["SES medium","Mean_TYG_quantile1"],Table1["SES medium","SE_TYG_quantile1"],
                     Table1["SES medium","Mean_TYG_quantile2"],Table1["SES medium","SE_TYG_quantile2"],
                     Table1["SES medium","Mean_TYG_quantile3"],Table1["SES medium","SE_TYG_quantile3"],
                     Table1["SES medium","Mean_TYG_quantile4"],Table1["SES medium","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("High",
                     Table1["SES high","Mean_all"],Table1["SES high","SE_all"],
                     Table1["SES high","Mean_TYG_quantile1"],Table1["SES high","SE_TYG_quantile1"],
                     Table1["SES high","Mean_TYG_quantile2"],Table1["SES high","SE_TYG_quantile2"],
                     Table1["SES high","Mean_TYG_quantile3"],Table1["SES high","SE_TYG_quantile3"],
                     Table1["SES high","Mean_TYG_quantile4"],Table1["SES high","SE_TYG_quantile4"],
                     "" ))
Table_S3<-Table
write.table(Table_S3,sep = ",",file ="I:/NHANES study/PD&TYG&CVD/Result/Supplementary Table 3.csv" ,row.names =F,col.names =F )
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####

# >>>>> section 19 Arrangement ####  
load(file="I:/NHANES study/PD&TYG&CVD/Data/Interpolation_weighted.Rdata")
Interpolation_weighted$CVD_status<-as.character(Interpolation_weighted$CVD_status)
Interpolation_weighted$CVD_status[Interpolation_weighted$CVD_status=="YES"]<-1
Interpolation_weighted$CVD_status[Interpolation_weighted$CVD_status=="NO"]<-0
Interpolation_weighted$CVD_status<-as.numeric(Interpolation_weighted$CVD_status)

save(Interpolation_weighted,file="I:/NHANES study/PD&TYG&CVD/Data/Interpolation_weighted.Rdata")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 22 Relative CVD rates (Table 2) ####  
load(file="I:/NHANES study/PD&TYG&CVD/Data/Interpolation_weighted.Rdata")
#Q1
{#* TYg ####
  {#** Q1 ####
    table(Interpolation_weighted$CVD_status)
    Q1<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 1"),]
    Q1_death_CVD<-Q1[which(Q1$CVD_status==1),]
    Q1_Perseon<-sum(Q1$weight)
    Q1_Perseon_ad_CVD<-as.numeric(round(sum(Q1_death_CVD$weight)))

    Q1_CVD<-Q1_Perseon_ad_CVD
    Q1_CVD_UCL<-(Q1_Perseon_ad_CVD+(1.96*sqrt(Q1_Perseon_ad_CVD)))
    Q1_CVD_LCL<-(Q1_Perseon_ad_CVD-(1.96*sqrt(Q1_Perseon_ad_CVD)))
    Q1_CVD_Incidence<-paste0(round(Q1_CVD,0)," (",round(Q1_CVD_LCL,0),"-",round(Q1_CVD_UCL,0),")")
    Q1_CVD_Incidence

  }
  {#** Q2 ####
    Q2<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 2"),]
    Q2_death_CVD<-Q2[which(Q2$CVD_status==1),]
    Q2_Perseon<-sum(Q2$weight)
    Q2_Perseon_ad_CVD<-as.numeric(round(sum(Q2_death_CVD$weight)))
    #Q2_CVD
    Q2_CVD<-Q2_Perseon_ad_CVD
    Q2_CVD_UCL<-(Q2_Perseon_ad_CVD+(1.96*sqrt(Q2_Perseon_ad_CVD)))
    Q2_CVD_LCL<-(Q2_Perseon_ad_CVD-(1.96*sqrt(Q2_Perseon_ad_CVD)))
    Q2_CVD_Incidence<-paste0(round(Q2_CVD,0)," (",round(Q2_CVD_LCL,0),"-",round(Q2_CVD_UCL,0),")")
    Q2_CVD_Incidence
  }
  {#** Q3 ####
    Q3<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 2"),]
    Q3_death_CVD<-Q3[which(Q3$CVD_status==1),]
    Q3_Perseon<-sum(Q3$weight)
    Q3_Perseon_ad_CVD<-as.numeric(round(sum(Q3_death_CVD$weight)))
    #Q3_CVD
    Q3_CVD<-Q3_Perseon_ad_CVD
    Q3_CVD_UCL<-(Q3_Perseon_ad_CVD+(1.96*sqrt(Q3_Perseon_ad_CVD)))
    Q3_CVD_LCL<-(Q3_Perseon_ad_CVD-(1.96*sqrt(Q3_Perseon_ad_CVD)))
    Q3_CVD_Incidence<-paste0(round(Q3_CVD,0)," (",round(Q3_CVD_LCL,0),"-",round(Q3_CVD_UCL,0),")")
    Q3_CVD_Incidence
  }
  {#** Q4 ####
    Q4<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 2"),]
    Q4_death_CVD<-Q4[which(Q4$CVD_status==1),]
    Q4_Perseon<-sum(Q4$weight)
    Q4_Perseon_ad_CVD<-as.numeric(round(sum(Q4_death_CVD$weight)))
    #Q4_CVD
    Q4_CVD<-Q4_Perseon_ad_CVD
    Q4_CVD_UCL<-(Q4_Perseon_ad_CVD+(1.96*sqrt(Q4_Perseon_ad_CVD)))
    Q4_CVD_LCL<-(Q4_Perseon_ad_CVD-(1.96*sqrt(Q4_Perseon_ad_CVD)))
    Q4_CVD_Incidence<-paste0(round(Q4_CVD,0)," (",round(Q4_CVD_LCL,0),"-",round(Q4_CVD_UCL,0),")")
    Q4_CVD_Incidence 
  }
  TYG_Incidence<-cbind(Q1_CVD_Incidence,Q2_CVD_Incidence,Q3_CVD_Incidence,Q4_CVD_Incidence)
  {#** EVENTS ####
#CVD.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_CVD$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_CVD$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_CVD$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_CVD$weight))
    
    CVD.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                 paste0(Q2.cause.counts,"/",Q2.counts),
                 paste0(Q3.cause.counts,"/",Q3.counts),
                 paste0(Q4.cause.counts,"/",Q4.counts))
    CVD.cause

  }
  TYG_counts<-as.data.frame(rbind(CVD.cause))
}
{#* TyG_WC ####
  {#** Q1 ####
    table(Interpolation_weighted$CVD_status)
    Q1<-Interpolation_weighted[which(Interpolation_weighted$TyG_WC_quantile=="Quantile 1"),]
    Q1_death_CVD<-Q1[which(Q1$CVD_status==1),]
    Q1_Perseon<-sum(Q1$weight)
    Q1_Perseon_ad_CVD<-as.numeric(round(sum(Q1_death_CVD$weight)))
    
    Q1_CVD<-Q1_Perseon_ad_CVD
    Q1_CVD_UCL<-(Q1_Perseon_ad_CVD+(1.96*sqrt(Q1_Perseon_ad_CVD)))
    Q1_CVD_LCL<-(Q1_Perseon_ad_CVD-(1.96*sqrt(Q1_Perseon_ad_CVD)))
    Q1_CVD_Incidence<-paste0(round(Q1_CVD,0)," (",round(Q1_CVD_LCL,0),"-",round(Q1_CVD_UCL,0),")")
    Q1_CVD_Incidence
    
  }
  {#** Q2 ####
    Q2<-Interpolation_weighted[which(Interpolation_weighted$TyG_WC_quantile=="Quantile 2"),]
    Q2_death_CVD<-Q2[which(Q2$CVD_status==1),]
    Q2_Perseon<-sum(Q2$weight)
    Q2_Perseon_ad_CVD<-as.numeric(round(sum(Q2_death_CVD$weight)))
    #Q2_CVD
    Q2_CVD<-Q2_Perseon_ad_CVD
    Q2_CVD_UCL<-(Q2_Perseon_ad_CVD+(1.96*sqrt(Q2_Perseon_ad_CVD)))
    Q2_CVD_LCL<-(Q2_Perseon_ad_CVD-(1.96*sqrt(Q2_Perseon_ad_CVD)))
    Q2_CVD_Incidence<-paste0(round(Q2_CVD,0)," (",round(Q2_CVD_LCL,0),"-",round(Q2_CVD_UCL,0),")")
    Q2_CVD_Incidence
  }
  {#** Q3 ####
    Q3<-Interpolation_weighted[which(Interpolation_weighted$TyG_WC_quantile=="Quantile 2"),]
    Q3_death_CVD<-Q3[which(Q3$CVD_status==1),]
    Q3_Perseon<-sum(Q3$weight)
    Q3_Perseon_ad_CVD<-as.numeric(round(sum(Q3_death_CVD$weight)))
    #Q3_CVD
    Q3_CVD<-Q3_Perseon_ad_CVD
    Q3_CVD_UCL<-(Q3_Perseon_ad_CVD+(1.96*sqrt(Q3_Perseon_ad_CVD)))
    Q3_CVD_LCL<-(Q3_Perseon_ad_CVD-(1.96*sqrt(Q3_Perseon_ad_CVD)))
    Q3_CVD_Incidence<-paste0(round(Q3_CVD,0)," (",round(Q3_CVD_LCL,0),"-",round(Q3_CVD_UCL,0),")")
    Q3_CVD_Incidence
  }
  {#** Q4 ####
    Q4<-Interpolation_weighted[which(Interpolation_weighted$TyG_WC_quantile=="Quantile 2"),]
    Q4_death_CVD<-Q4[which(Q4$CVD_status==1),]
    Q4_Perseon<-sum(Q4$weight)
    Q4_Perseon_ad_CVD<-as.numeric(round(sum(Q4_death_CVD$weight)))
    #Q4_CVD
    Q4_CVD<-Q4_Perseon_ad_CVD
    Q4_CVD_UCL<-(Q4_Perseon_ad_CVD+(1.96*sqrt(Q4_Perseon_ad_CVD)))
    Q4_CVD_LCL<-(Q4_Perseon_ad_CVD-(1.96*sqrt(Q4_Perseon_ad_CVD)))
    Q4_CVD_Incidence<-paste0(round(Q4_CVD,0)," (",round(Q4_CVD_LCL,0),"-",round(Q4_CVD_UCL,0),")")
    Q4_CVD_Incidence 
  }
  TyG_WC_Incidence<-cbind(Q1_CVD_Incidence,Q2_CVD_Incidence,Q3_CVD_Incidence,Q4_CVD_Incidence)
  {#** EVENTS ####
    #CVD.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_CVD$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_CVD$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_CVD$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_CVD$weight))
    
    CVD.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                 paste0(Q2.cause.counts,"/",Q2.counts),
                 paste0(Q3.cause.counts,"/",Q3.counts),
                 paste0(Q4.cause.counts,"/",Q4.counts))
    CVD.cause
    
  }
  TyG_WC_counts<-as.data.frame(rbind(CVD.cause))
}
{#* TyG_WHtR ####
  {#** Q1 ####
    table(Interpolation_weighted$CVD_status)
    Q1<-Interpolation_weighted[which(Interpolation_weighted$TyG_WHtR_quantile=="Quantile 1"),]
    Q1_death_CVD<-Q1[which(Q1$CVD_status==1),]
    Q1_Perseon<-sum(Q1$weight)
    Q1_Perseon_ad_CVD<-as.numeric(round(sum(Q1_death_CVD$weight)))
    
    Q1_CVD<-Q1_Perseon_ad_CVD
    Q1_CVD_UCL<-(Q1_Perseon_ad_CVD+(1.96*sqrt(Q1_Perseon_ad_CVD)))
    Q1_CVD_LCL<-(Q1_Perseon_ad_CVD-(1.96*sqrt(Q1_Perseon_ad_CVD)))
    Q1_CVD_Incidence<-paste0(round(Q1_CVD,0)," (",round(Q1_CVD_LCL,0),"-",round(Q1_CVD_UCL,0),")")
    Q1_CVD_Incidence
    
  }
  {#** Q2 ####
    Q2<-Interpolation_weighted[which(Interpolation_weighted$TyG_WHtR_quantile=="Quantile 2"),]
    Q2_death_CVD<-Q2[which(Q2$CVD_status==1),]
    Q2_Perseon<-sum(Q2$weight)
    Q2_Perseon_ad_CVD<-as.numeric(round(sum(Q2_death_CVD$weight)))
    #Q2_CVD
    Q2_CVD<-Q2_Perseon_ad_CVD
    Q2_CVD_UCL<-(Q2_Perseon_ad_CVD+(1.96*sqrt(Q2_Perseon_ad_CVD)))
    Q2_CVD_LCL<-(Q2_Perseon_ad_CVD-(1.96*sqrt(Q2_Perseon_ad_CVD)))
    Q2_CVD_Incidence<-paste0(round(Q2_CVD,0)," (",round(Q2_CVD_LCL,0),"-",round(Q2_CVD_UCL,0),")")
    Q2_CVD_Incidence
  }
  {#** Q3 ####
    Q3<-Interpolation_weighted[which(Interpolation_weighted$TyG_WHtR_quantile=="Quantile 2"),]
    Q3_death_CVD<-Q3[which(Q3$CVD_status==1),]
    Q3_Perseon<-sum(Q3$weight)
    Q3_Perseon_ad_CVD<-as.numeric(round(sum(Q3_death_CVD$weight)))
    #Q3_CVD
    Q3_CVD<-Q3_Perseon_ad_CVD
    Q3_CVD_UCL<-(Q3_Perseon_ad_CVD+(1.96*sqrt(Q3_Perseon_ad_CVD)))
    Q3_CVD_LCL<-(Q3_Perseon_ad_CVD-(1.96*sqrt(Q3_Perseon_ad_CVD)))
    Q3_CVD_Incidence<-paste0(round(Q3_CVD,0)," (",round(Q3_CVD_LCL,0),"-",round(Q3_CVD_UCL,0),")")
    Q3_CVD_Incidence
  }
  {#** Q4 ####
    Q4<-Interpolation_weighted[which(Interpolation_weighted$TyG_WHtR_quantile=="Quantile 2"),]
    Q4_death_CVD<-Q4[which(Q4$CVD_status==1),]
    Q4_Perseon<-sum(Q4$weight)
    Q4_Perseon_ad_CVD<-as.numeric(round(sum(Q4_death_CVD$weight)))
    #Q4_CVD
    Q4_CVD<-Q4_Perseon_ad_CVD
    Q4_CVD_UCL<-(Q4_Perseon_ad_CVD+(1.96*sqrt(Q4_Perseon_ad_CVD)))
    Q4_CVD_LCL<-(Q4_Perseon_ad_CVD-(1.96*sqrt(Q4_Perseon_ad_CVD)))
    Q4_CVD_Incidence<-paste0(round(Q4_CVD,0)," (",round(Q4_CVD_LCL,0),"-",round(Q4_CVD_UCL,0),")")
    Q4_CVD_Incidence 
  }
  TyG_WHtR_Incidence<-cbind(Q1_CVD_Incidence,Q2_CVD_Incidence,Q3_CVD_Incidence,Q4_CVD_Incidence)
  {#** EVENTS ####
    #CVD.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_CVD$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_CVD$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_CVD$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_CVD$weight))
    
    CVD.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                 paste0(Q2.cause.counts,"/",Q2.counts),
                 paste0(Q3.cause.counts,"/",Q3.counts),
                 paste0(Q4.cause.counts,"/",Q4.counts))
    CVD.cause
    
  }
  TyG_WHtR_counts<-as.data.frame(rbind(CVD.cause))
}
{#* TyG_BMI ####
  {#** Q1 ####
    table(Interpolation_weighted$CVD_status)
    Q1<-Interpolation_weighted[which(Interpolation_weighted$TyG_BMI_quantile=="Quantile 1"),]
    Q1_death_CVD<-Q1[which(Q1$CVD_status==1),]
    Q1_Perseon<-sum(Q1$weight)
    Q1_Perseon_ad_CVD<-as.numeric(round(sum(Q1_death_CVD$weight)))
    
    Q1_CVD<-Q1_Perseon_ad_CVD
    Q1_CVD_UCL<-(Q1_Perseon_ad_CVD+(1.96*sqrt(Q1_Perseon_ad_CVD)))
    Q1_CVD_LCL<-(Q1_Perseon_ad_CVD-(1.96*sqrt(Q1_Perseon_ad_CVD)))
    Q1_CVD_Incidence<-paste0(round(Q1_CVD,0)," (",round(Q1_CVD_LCL,0),"-",round(Q1_CVD_UCL,0),")")
    Q1_CVD_Incidence
    
  }
  {#** Q2 ####
    Q2<-Interpolation_weighted[which(Interpolation_weighted$TyG_BMI_quantile=="Quantile 2"),]
    Q2_death_CVD<-Q2[which(Q2$CVD_status==1),]
    Q2_Perseon<-sum(Q2$weight)
    Q2_Perseon_ad_CVD<-as.numeric(round(sum(Q2_death_CVD$weight)))
    #Q2_CVD
    Q2_CVD<-Q2_Perseon_ad_CVD
    Q2_CVD_UCL<-(Q2_Perseon_ad_CVD+(1.96*sqrt(Q2_Perseon_ad_CVD)))
    Q2_CVD_LCL<-(Q2_Perseon_ad_CVD-(1.96*sqrt(Q2_Perseon_ad_CVD)))
    Q2_CVD_Incidence<-paste0(round(Q2_CVD,0)," (",round(Q2_CVD_LCL,0),"-",round(Q2_CVD_UCL,0),")")
    Q2_CVD_Incidence
  }
  {#** Q3 ####
    Q3<-Interpolation_weighted[which(Interpolation_weighted$TyG_BMI_quantile=="Quantile 2"),]
    Q3_death_CVD<-Q3[which(Q3$CVD_status==1),]
    Q3_Perseon<-sum(Q3$weight)
    Q3_Perseon_ad_CVD<-as.numeric(round(sum(Q3_death_CVD$weight)))
    #Q3_CVD
    Q3_CVD<-Q3_Perseon_ad_CVD
    Q3_CVD_UCL<-(Q3_Perseon_ad_CVD+(1.96*sqrt(Q3_Perseon_ad_CVD)))
    Q3_CVD_LCL<-(Q3_Perseon_ad_CVD-(1.96*sqrt(Q3_Perseon_ad_CVD)))
    Q3_CVD_Incidence<-paste0(round(Q3_CVD,0)," (",round(Q3_CVD_LCL,0),"-",round(Q3_CVD_UCL,0),")")
    Q3_CVD_Incidence
  }
  {#** Q4 ####
    Q4<-Interpolation_weighted[which(Interpolation_weighted$TyG_BMI_quantile=="Quantile 2"),]
    Q4_death_CVD<-Q4[which(Q4$CVD_status==1),]
    Q4_Perseon<-sum(Q4$weight)
    Q4_Perseon_ad_CVD<-as.numeric(round(sum(Q4_death_CVD$weight)))
    #Q4_CVD
    Q4_CVD<-Q4_Perseon_ad_CVD
    Q4_CVD_UCL<-(Q4_Perseon_ad_CVD+(1.96*sqrt(Q4_Perseon_ad_CVD)))
    Q4_CVD_LCL<-(Q4_Perseon_ad_CVD-(1.96*sqrt(Q4_Perseon_ad_CVD)))
    Q4_CVD_Incidence<-paste0(round(Q4_CVD,0)," (",round(Q4_CVD_LCL,0),"-",round(Q4_CVD_UCL,0),")")
    Q4_CVD_Incidence 
  }
  TyG_BMI_Incidence<-cbind(Q1_CVD_Incidence,Q2_CVD_Incidence,Q3_CVD_Incidence,Q4_CVD_Incidence)
  {#** EVENTS ####
    #CVD.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_CVD$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_CVD$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_CVD$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_CVD$weight))
    
    CVD.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                 paste0(Q2.cause.counts,"/",Q2.counts),
                 paste0(Q3.cause.counts,"/",Q3.counts),
                 paste0(Q4.cause.counts,"/",Q4.counts))
    CVD.cause
    
  }
  TyG_BMI_counts<-as.data.frame(rbind(CVD.cause))
}
{#* Table 2 ####
Table<-c("Outcome","Quantile 1","Quantile 2",
         "Quantile 3","Quantile 4")
#CVD cause ####
Table<-rbind(Table,c("TyG index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TYG_counts[1],TYG_counts[2],
                     TYG_counts[3],TYG_counts[4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TYG_Incidence[1],TYG_Incidence[2],
                     TYG_Incidence[3],TYG_Incidence[4])) 

Table<-rbind(Table,c("TyG-WC index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TyG_WC_counts[1],TyG_WC_counts[2],
                     TyG_WC_counts[3],TyG_WC_counts[4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TyG_WC_Incidence[1],TyG_WC_Incidence[2],
                     TyG_WC_Incidence[3],TyG_WC_Incidence[4])) 

Table<-rbind(Table,c("TyG-WHtR index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TyG_WHtR_counts[1],TyG_WHtR_counts[2],
                     TyG_WHtR_counts[3],TyG_WHtR_counts[4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TyG_WHtR_Incidence[1],TyG_WHtR_Incidence[2],
                     TyG_WHtR_Incidence[3],TyG_WHtR_Incidence[4])) 

Table<-rbind(Table,c("TyG-BMI index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TyG_BMI_counts[1],TyG_BMI_counts[2],
                     TyG_BMI_counts[3],TyG_BMI_counts[4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TyG_BMI_Incidence[1],TyG_BMI_Incidence[2],
                     TyG_BMI_Incidence[3],TyG_BMI_Incidence[4])) 
Table<-as.data.frame(Table)
Event= data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)

}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section TYG ####  
load(file="I:/NHANES study/PD&TYG&CVD/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
model<-svyglm(CVD_status ~ TYG, design = rhcSvy, family = quasibinomial())
model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TYG", "Estimate"]
se <- model_result[["coefficients"]]["TYG", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                     'P value' =P,'model'="model1")
result
model<-svyglm(CVD_status ~ TYG+
                Age_status+Sex+Race_ethnicity+SES, design = rhcSvy, family = quasibinomial())
model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TYG", "Estimate"]
se <- model_result[["coefficients"]]["TYG", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result2 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                      'P value' =P,'model'="model2")
result<-rbind(result,result2) 
model<-svyglm(CVD_status ~ TYG+
                Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status, design = rhcSvy, family = quasibinomial())
model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TYG", "Estimate"]
se <- model_result[["coefficients"]]["TYG", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result3 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                      'P value' =P,'model'="model3")
result<-rbind(result,result3) 
model<-svyglm(CVD_status ~ TYG+
                Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status+
                HTN_status+T2D_status+Cohort, design = rhcSvy, family = quasibinomial())
model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TYG", "Estimate"]
se <- model_result[["coefficients"]]["TYG", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                      'P value' =P,'model'="model4")
result<-rbind(result,result4) 
result



{ #* Combine #####
  #Results
  result.cause<-result
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="1000"]<-"1.00"
  result.cause[result.cause=="2000"]<-"2.00"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  result_TYG<-as.data.frame(result.cause)
  result_TYG= data.frame(lapply(result_TYG, as.character), stringsAsFactors=FALSE)
}
result_TYG
# >>>>> section TyG_WC ####  
load(file="I:/NHANES study/PD&TyG&CVD/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
Interpolation_weighted$TyG_WC<-Interpolation_weighted$TyG_WC/100
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

model<-svyglm(CVD_status ~ TyG_WC, design = rhcSvy, family = quasibinomial())
model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TyG_WC", "Estimate"]
se <- model_result[["coefficients"]]["TyG_WC", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                     'P value' =P,'model'="model1")
result
model<-svyglm(CVD_status ~ TyG_WC+
                Age_status+Sex+Race_ethnicity+SES, design = rhcSvy, family = quasibinomial())
model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TyG_WC", "Estimate"]
se <- model_result[["coefficients"]]["TyG_WC", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result2 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                      'P value' =P,'model'="model2")
result<-rbind(result,result2) 
model<-svyglm(CVD_status ~ TyG_WC+
                Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status, design = rhcSvy, family = quasibinomial())

model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TyG_WC", "Estimate"]
se <- model_result[["coefficients"]]["TyG_WC", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result3 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                      'P value' =P,'model'="model3")
result<-rbind(result,result3) 
model<-svyglm(CVD_status ~ TyG_WC+
                Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status+
                HTN_status+T2D_status+Cohort, design = rhcSvy, family = quasibinomial())
model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TyG_WC", "Estimate"]
se <- model_result[["coefficients"]]["TyG_WC", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                      'P value' =P,'model'="model4")
result<-rbind(result,result4) 
result



{ #* Combine #####
  #Results
  result.cause<-result
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="1000"]<-"1.00"
  result.cause[result.cause=="2000"]<-"2.00"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  result_TyG_WC<-as.data.frame(result.cause)
  result_TyG_WC= data.frame(lapply(result_TyG_WC, as.character), stringsAsFactors=FALSE)
}
result_TyG_WC


# >>>>> section TyG_WHtR ####  
load(file="I:/NHANES study/PD&TyG&CVD/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

model<-svyglm(CVD_status ~ TyG_WHtR, design = rhcSvy, family = quasibinomial())
model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TyG_WHtR", "Estimate"]
se <- model_result[["coefficients"]]["TyG_WHtR", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                     'P value' =P,'model'="model1")
result
model<-svyglm(CVD_status ~ TyG_WHtR+
                Age_status+Sex+Race_ethnicity+SES, design = rhcSvy, family = quasibinomial())
model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TyG_WHtR", "Estimate"]
se <- model_result[["coefficients"]]["TyG_WHtR", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result2 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                      'P value' =P,'model'="model2")
result<-rbind(result,result2) 
model<-svyglm(CVD_status ~ TyG_WHtR+
                Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status, design = rhcSvy, family = quasibinomial())

model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TyG_WHtR", "Estimate"]
se <- model_result[["coefficients"]]["TyG_WHtR", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result3 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                      'P value' =P,'model'="model3")
result<-rbind(result,result3) 
model<-svyglm(CVD_status ~ TyG_WHtR+
                Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status+
                HTN_status+T2D_status+Cohort, design = rhcSvy, family = quasibinomial())
model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TyG_WHtR", "Estimate"]
se <- model_result[["coefficients"]]["TyG_WHtR", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                      'P value' =P,'model'="model4")
result<-rbind(result,result4) 
result



{ #* Combine #####
  #Results
  result.cause<-result
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="1000"]<-"1.00"
  result.cause[result.cause=="2000"]<-"2.00"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  result_TyG_WHtR<-as.data.frame(result.cause)
  result_TyG_WHtR= data.frame(lapply(result_TyG_WHtR, as.character), stringsAsFactors=FALSE)
}
result_TyG_WHtR
# >>>>> section TYG BMI ####  
load(file="I:/NHANES study/PD&TyG&CVD/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
Interpolation_weighted$TyG_BMI<-Interpolation_weighted$TyG_BMI/100
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

model<-svyglm(CVD_status ~ TyG_BMI, design = rhcSvy, family = quasibinomial())
model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TyG_BMI", "Estimate"]
se <- model_result[["coefficients"]]["TyG_BMI", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                     'P value' =P,'model'="model1")
result
model<-svyglm(CVD_status ~ TyG_BMI+
                Age_status+Sex+Race_ethnicity+SES, design = rhcSvy, family = quasibinomial())
model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TyG_BMI", "Estimate"]
se <- model_result[["coefficients"]]["TyG_BMI", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result2 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                      'P value' =P,'model'="model2")
result<-rbind(result,result2) 
model<-svyglm(CVD_status ~ TyG_BMI+
                Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status, design = rhcSvy, family = quasibinomial())

model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TyG_BMI", "Estimate"]
se <- model_result[["coefficients"]]["TyG_BMI", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result3 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                      'P value' =P,'model'="model3")
result<-rbind(result,result3) 
model<-svyglm(CVD_status ~ TyG_BMI+
                Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status+
                HTN_status+T2D_status+Cohort, design = rhcSvy, family = quasibinomial())
model_result<-summary(model)
model_result
P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
estimate <- model_result[["coefficients"]]["TyG_BMI", "Estimate"]
se <- model_result[["coefficients"]]["TyG_BMI", "Std. Error"]
RR <- exp(estimate)
CI_lower <- exp(estimate - 1.96 * se)
CI_upper <- exp(estimate + 1.96 * se)
result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                      'P value' =P,'model'="model4")
result<-rbind(result,result4) 
result



{ #* Combine #####
  #Results
  result.cause<-result
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="1000"]<-"1.00"
  result.cause[result.cause=="2000"]<-"2.00"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  result_TyG_BMI<-as.data.frame(result.cause)
  result_TyG_BMI= data.frame(lapply(result_TyG_BMI, as.character), stringsAsFactors=FALSE)
}
result_TyG_BMI

# >>>>> section Combined ####  
  Table<-c("","TyG index","P")
  Table<-rbind(Table,c("TyG index (Per 1-index)*","",""))
  Table<-rbind(Table,c("Model 1†",paste0(result_TYG[1,1]," (",result_TYG[1,2],", ",result_TYG[1,3],")"),result_TYG[1,4]))
  Table<-rbind(Table,c("Model 2‡",paste0(result_TYG[2,1]," (",result_TYG[2,2],", ",result_TYG[2,3],")"),result_TYG[2,4]))
  Table<-rbind(Table,c("Model 3§",paste0(result_TYG[3,1]," (",result_TYG[3,2],", ",result_TYG[3,3],")"),result_TYG[3,4]))
  Table<-rbind(Table,c("Model 4¶",paste0(result_TYG[4,1]," (",result_TYG[4,2],", ",result_TYG[4,3],")"),result_TYG[4,4]))
  Table<-rbind(Table,c("TyG-WC index (Per 100-index)*","",""))
  Table<-rbind(Table,c("Model 1",paste0(result_TyG_WC[1,1]," (",result_TyG_WC[1,2],", ",result_TyG_WC[1,3],")"),result_TyG_WC[1,4]))
  Table<-rbind(Table,c("Model 2",paste0(result_TyG_WC[2,1]," (",result_TyG_WC[2,2],", ",result_TyG_WC[2,3],")"),result_TyG_WC[2,4]))
  Table<-rbind(Table,c("Model 3",paste0(result_TyG_WC[3,1]," (",result_TyG_WC[3,2],", ",result_TyG_WC[3,3],")"),result_TyG_WC[3,4]))
  Table<-rbind(Table,c("Model 4",paste0(result_TyG_WC[4,1]," (",result_TyG_WC[4,2],", ",result_TyG_WC[4,3],")"),result_TyG_WC[4,4]))
  Table<-rbind(Table,c("TyG-WHtR index (Per 1-index)*","",""))
  Table<-rbind(Table,c("Model 1",paste0(result_TyG_WHtR[1,1]," (",result_TyG_WHtR[1,2],", ",result_TyG_WHtR[1,3],")"),result_TyG_WHtR[1,4]))
  Table<-rbind(Table,c("Model 2",paste0(result_TyG_WHtR[2,1]," (",result_TyG_WHtR[2,2],", ",result_TyG_WHtR[2,3],")"),result_TyG_WHtR[2,4]))
  Table<-rbind(Table,c("Model 3",paste0(result_TyG_WHtR[3,1]," (",result_TyG_WHtR[3,2],", ",result_TyG_WHtR[3,3],")"),result_TyG_WHtR[3,4]))
  Table<-rbind(Table,c("Model 4",paste0(result_TyG_WHtR[4,1]," (",result_TyG_WHtR[4,2],", ",result_TyG_WHtR[4,3],")"),result_TyG_WHtR[4,4]))
  Table<-rbind(Table,c("TyG-BMI index (Per 100-index)*","",""))
  Table<-rbind(Table,c("Model 1",paste0(result_TyG_BMI[1,1]," (",result_TyG_BMI[1,2],", ",result_TyG_BMI[1,3],")"),result_TyG_BMI[1,4]))
  Table<-rbind(Table,c("Model 2",paste0(result_TyG_BMI[2,1]," (",result_TyG_BMI[2,2],", ",result_TyG_BMI[2,3],")"),result_TyG_BMI[2,4]))
  Table<-rbind(Table,c("Model 3",paste0(result_TyG_BMI[3,1]," (",result_TyG_BMI[3,2],", ",result_TyG_BMI[3,3],")"),result_TyG_BMI[3,4]))
  Table<-rbind(Table,c("Model 4",paste0(result_TyG_BMI[4,1]," (",result_TyG_BMI[4,2],", ",result_TyG_BMI[4,3],")"),result_TyG_BMI[4,4]))
  Table<-as.data.frame(Table)
  Table_continue = data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table_continue
  write.table(Table_continue,sep = ",",file ="I:/NHANES study/PD&TYG&CVD/Result/Supplementary Table 4.csv",row.names =F,col.names =F )

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section Load data ####  
  load(file="I:/NHANES study/PD&TYG&CVD/Data/Interpolation_weighted.Rdata")
  Interpolation_weighted$TyG_WC<-Interpolation_weighted$TyG_WC/100
  Interpolation_weighted$TyG_BMI<-Interpolation_weighted$TyG_BMI/100
  Interpolation_weighted$TYG_quantile<-as.character(Interpolation_weighted$TYG_quantile)
  Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 1"]<-0
  Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 2"]<-1
  Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 3"]<-2
  Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 4"]<-3
  Interpolation_weighted$TYG_qua<-as.numeric(Interpolation_weighted$TYG_qua)
  table(Interpolation_weighted$TyG_WC_quantile)
  Interpolation_weighted$TyG_WC_quantile<-as.character(Interpolation_weighted$TyG_WC_quantile)
  Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 1"]<-0
  Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 2"]<-1
  Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 3"]<-2
  Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 4"]<-3
  Interpolation_weighted$TyG_WC_qua<-as.numeric(Interpolation_weighted$TyG_WC_qua)
  table(Interpolation_weighted$TyG_WC_qua)
  Interpolation_weighted$TyG_WHtR_quantile<-as.character(Interpolation_weighted$TyG_WHtR_quantile)
  Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 1"]<-0
  Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 2"]<-1
  Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 3"]<-2
  Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 4"]<-3
  Interpolation_weighted$TyG_WHtR_qua<-as.numeric(Interpolation_weighted$TyG_WHtR_qua)
  table(Interpolation_weighted$TyG_WHtR_qua)
  Interpolation_weighted$TyG_BMI_quantile<-as.character(Interpolation_weighted$TyG_BMI_quantile)
  Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 1"]<-0
  Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 2"]<-1
  Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 3"]<-2
  Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 4"]<-3
  Interpolation_weighted$TyG_BMI_qua<-as.numeric(Interpolation_weighted$TyG_BMI_qua)
  table(Interpolation_weighted$TyG_BMI_qua)
  rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
# >>>>> section TYG quantile  #### 
  { #*  section TYG_quantile ####  
    colnames(Interpolation_weighted)
    rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
    model<-svyglm(CVD_status ~ TYG_quantile, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                         'P value' =P,'model'="model1")
    result
    model<-svyglm(CVD_status ~ TYG_quantile+
                    Age_status+Sex+Race_ethnicity+SES, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result2 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model2")
    result<-rbind(result,result2) 
    model<-svyglm(CVD_status ~ TYG_quantile+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result3 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model3")
    result<-rbind(result,result3) 
    model<-svyglm(CVD_status ~ TYG_quantile+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status+
                    HTN_status+T2D_status+Cohort, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model4")
    result<-rbind(result,result4) 
    result_TYG_quantile<-result
  }
  { #*   section TYG_qua ####  
    model<-svyglm(CVD_status ~ TYG_qua, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                         'P value' =P,'model'="model1")
    result
    model<-svyglm(CVD_status ~ TYG_qua+
                    Age_status+Sex+Race_ethnicity+SES, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result2 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model2")
    result<-rbind(result,result2) 
    model<-svyglm(CVD_status ~ TYG_qua+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result3 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model3")
    result<-rbind(result,result3) 
    model<-svyglm(CVD_status ~ TYG_qua+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status+
                    HTN_status+T2D_status+Cohort, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model4")
    result<-rbind(result,result4) 
    result_TYG_qua<-result
  }
  
  { #*   section TYG_quantile combind ####   
    result.cause<-rbind(result_TYG_quantile,result_TYG_qua)
    result.cause$HR<-round(result.cause$HR,2)
    result.cause$lower..95<-round(result.cause$lower..95,2)
    result.cause$upper..95<-round(result.cause$upper..95,2)
    result.cause$P.value<-round(result.cause$P.value,3)
    round_2_function <- function(x){
      while(nchar(x)<4){
        temp <- paste(x,0)
        x <- temp
        x <- gsub(" ","",x)
      }
      return(x)
    }
    round_3_function <- function(x){
      while(nchar(x)<5){
        temp <- paste(x,0)
        x <- temp
        x <- gsub(" ","",x)
      }
      return(x)
    }
    result.cause$HR<-lapply(result.cause$HR,round_2_function)
    result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
    result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
    result.cause$HR_CI<-paste0(result.cause$HR," (",
                               result.cause$lower..95,", ",
                               result.cause$upper..95,")")
    result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
    result.cause[result.cause=="00000"]<-"<0.001"
    TyG_qua<-result.cause
    TyG_qua
  }
# >>>>> section TyG_WC quantile  #### 
  { #* section TyG_WC_quantile ####   
    colnames(Interpolation_weighted)
    rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
    model<-svyglm(CVD_status ~ TyG_WC_quantile, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                         'P value' =P,'model'="model1")
    result
    model<-svyglm(CVD_status ~ TyG_WC_quantile+
                    Age_status+Sex+Race_ethnicity+SES, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result2 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model2")
    result<-rbind(result,result2) 
    model<-svyglm(CVD_status ~ TyG_WC_quantile+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result3 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model3")
    result<-rbind(result,result3) 
    model<-svyglm(CVD_status ~ TyG_WC_quantile+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status+
                    HTN_status+T2D_status+Cohort, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model4")
    result<-rbind(result,result4) 
    result_TyG_WC_quantile<-result
  } 
  { #* section TyG_WC_qua ####    
    model<-svyglm(CVD_status ~ TyG_WC_qua, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                         'P value' =P,'model'="model1")
    result
    model<-svyglm(CVD_status ~ TyG_WC_qua+
                    Age_status+Sex+Race_ethnicity+SES, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result2 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model2")
    result<-rbind(result,result2) 
    model<-svyglm(CVD_status ~ TyG_WC_qua+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result3 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model3")
    result<-rbind(result,result3) 
    model<-svyglm(CVD_status ~ TyG_WC_qua+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status+
                    HTN_status+T2D_status+Cohort, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model4")
    result<-rbind(result,result4) 
    result_TyG_WC_qua<-result
  }  
  { #* section TyG_WC_quantile combind ####     
    result.cause<-rbind(result_TyG_WC_quantile,result_TyG_WC_qua)
    result.cause$HR<-round(result.cause$HR,2)
    result.cause$lower..95<-round(result.cause$lower..95,2)
    result.cause$upper..95<-round(result.cause$upper..95,2)
    result.cause$P.value<-round(result.cause$P.value,3)
    round_2_function <- function(x){
      while(nchar(x)<4){
        temp <- paste(x,0)
        x <- temp
        x <- gsub(" ","",x)
      }
      return(x)
    }
    round_3_function <- function(x){
      while(nchar(x)<5){
        temp <- paste(x,0)
        x <- temp
        x <- gsub(" ","",x)
      }
      return(x)
    }
    result.cause$HR<-lapply(result.cause$HR,round_2_function)
    result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
    result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
    result.cause$HR_CI<-paste0(result.cause$HR," (",
                               result.cause$lower..95,", ",
                               result.cause$upper..95,")")
    result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
    result.cause[result.cause=="00000"]<-"<0.001"
    TyG_WC_qua<-result.cause
    TyG_WC_qua  
  }
# >>>>> section TyG_WHtR quantile  #### 
  { #* section TyG_WHtR_quantile ####   
    colnames(Interpolation_weighted)
    rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
    model<-svyglm(CVD_status ~ TyG_WHtR_quantile, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                         'P value' =P,'model'="model1")
    result
    model<-svyglm(CVD_status ~ TyG_WHtR_quantile+
                    Age_status+Sex+Race_ethnicity+SES, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result2 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model2")
    result<-rbind(result,result2) 
    model<-svyglm(CVD_status ~ TyG_WHtR_quantile+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result3 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model3")
    result<-rbind(result,result3) 
    model<-svyglm(CVD_status ~ TyG_WHtR_quantile+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status+
                    HTN_status+T2D_status+Cohort, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model4")
    result<-rbind(result,result4) 
    result_TyG_WHtR_quantile<-result
  }  
  { #* section TyG_WHtR_qua ####    
    model<-svyglm(CVD_status ~ TyG_WHtR_qua, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                         'P value' =P,'model'="model1")
    result
    model<-svyglm(CVD_status ~ TyG_WHtR_qua+
                    Age_status+Sex+Race_ethnicity+SES, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result2 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model2")
    result<-rbind(result,result2) 
    model<-svyglm(CVD_status ~ TyG_WHtR_qua+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result3 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model3")
    result<-rbind(result,result3) 
    model<-svyglm(CVD_status ~ TyG_WHtR_qua+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status+
                    HTN_status+T2D_status+Cohort, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model4")
    result<-rbind(result,result4) 
    result_TyG_WHtR_qua<-result
  }  
  { #* section TyG_WHtR_quantile combind ####     
    result.cause<-rbind(result_TyG_WHtR_quantile,result_TyG_WHtR_qua)
    result.cause$HR<-round(result.cause$HR,2)
    result.cause$lower..95<-round(result.cause$lower..95,2)
    result.cause$upper..95<-round(result.cause$upper..95,2)
    result.cause$P.value<-round(result.cause$P.value,3)
    round_2_function <- function(x){
      while(nchar(x)<4){
        temp <- paste(x,0)
        x <- temp
        x <- gsub(" ","",x)
      }
      return(x)
    }
    round_3_function <- function(x){
      while(nchar(x)<5){
        temp <- paste(x,0)
        x <- temp
        x <- gsub(" ","",x)
      }
      return(x)
    }
    result.cause$HR<-lapply(result.cause$HR,round_2_function)
    result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
    result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
    result.cause$HR_CI<-paste0(result.cause$HR," (",
                               result.cause$lower..95,", ",
                               result.cause$upper..95,")")
    result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
    result.cause[result.cause=="00000"]<-"<0.001"
    TyG_WHtR_qua<-result.cause
    TyG_WHtR_qua      
  } 
# >>>>> section TyG_BMI quantile  #### 
  { #* section TyG_BMI_quantile ####   
    colnames(Interpolation_weighted)
    rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
    model<-svyglm(CVD_status ~ TyG_BMI_quantile, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                         'P value' =P,'model'="model1")
    result
    model<-svyglm(CVD_status ~ TyG_BMI_quantile+
                    Age_status+Sex+Race_ethnicity+SES, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result2 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model2")
    result<-rbind(result,result2) 
    model<-svyglm(CVD_status ~ TyG_BMI_quantile+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result3 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model3")
    result<-rbind(result,result3) 
    model<-svyglm(CVD_status ~ TyG_BMI_quantile+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status+
                    HTN_status+T2D_status+Cohort, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2:4,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2:4, "Estimate"]
    se <- model_result[["coefficients"]][2:4, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model4")
    result<-rbind(result,result4) 
    result_TyG_BMI_quantile<-result
  }  
  { #* section TyG_BMI_qua ####    
    model<-svyglm(CVD_status ~ TyG_BMI_qua, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                         'P value' =P,'model'="model1")
    result
    model<-svyglm(CVD_status ~ TyG_BMI_qua+
                    Age_status+Sex+Race_ethnicity+SES, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result2 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model2")
    result<-rbind(result,result2) 
    model<-svyglm(CVD_status ~ TyG_BMI_qua+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result3 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model3")
    result<-rbind(result,result3) 
    model<-svyglm(CVD_status ~ TyG_BMI_qua+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+Physical_status+Smoking_status+Drinking_status+
                    HTN_status+T2D_status+Cohort, design = rhcSvy, family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]][2, "Estimate"]
    se <- model_result[["coefficients"]][2, "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="model4")
    result<-rbind(result,result4) 
    result_TyG_BMI_qua<-result
  }  
  { #* section TyG_BMI_quantile combind ####     
    result.cause<-rbind(result_TyG_BMI_quantile,result_TyG_BMI_qua)
    result.cause$HR<-round(result.cause$HR,2)
    result.cause$lower..95<-round(result.cause$lower..95,2)
    result.cause$upper..95<-round(result.cause$upper..95,2)
    result.cause$P.value<-round(result.cause$P.value,3)
    round_2_function <- function(x){
      while(nchar(x)<4){
        temp <- paste(x,0)
        x <- temp
        x <- gsub(" ","",x)
      }
      return(x)
    }
    round_3_function <- function(x){
      while(nchar(x)<5){
        temp <- paste(x,0)
        x <- temp
        x <- gsub(" ","",x)
      }
      return(x)
    }
    result.cause$HR<-lapply(result.cause$HR,round_2_function)
    result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
    result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
    result.cause$HR_CI<-paste0(result.cause$HR," (",
                               result.cause$lower..95,", ",
                               result.cause$upper..95,")")
    result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
    result.cause[result.cause=="00000"]<-"<0.001"
    TyG_BMI_qua<-result.cause
    TyG_BMI_qua      
  } 
# >>>>> section Combined  #### 
  Table<-c("","Quantile 1*", "Quantile 2","Quantile 3","Quantile 4","P for trend")
  
  Table<-rbind(Table,c("TyG Quantile","", "","","",""))
  Table<-rbind(Table,c("Model 1†","1.00 [Reference]",TyG_qua[1,"HR_CI"],TyG_qua[2,"HR_CI"],TyG_qua[3,"HR_CI"],TyG_qua[13,"P.value"]))
  Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",TyG_qua[4,"HR_CI"],TyG_qua[5,"HR_CI"],TyG_qua[6,"HR_CI"],TyG_qua[14,"P.value"]))
  Table<-rbind(Table,c("Model 3§","1.00 [Reference]",TyG_qua[7,"HR_CI"],TyG_qua[8,"HR_CI"],TyG_qua[9,"HR_CI"],TyG_qua[15,"P.value"]))
  Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",TyG_qua[10,"HR_CI"],TyG_qua[11,"HR_CI"],TyG_qua[12,"HR_CI"],TyG_qua[16,"P.value"]))
  
  Table<-rbind(Table,c("TyG-WC Quantile","", "","","",""))
  Table<-rbind(Table,c("Model 1†","1.00 [Reference]",TyG_WC_qua[1,"HR_CI"],TyG_WC_qua[2,"HR_CI"],TyG_WC_qua[3,"HR_CI"],TyG_WC_qua[13,"P.value"]))
  Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",TyG_WC_qua[4,"HR_CI"],TyG_WC_qua[5,"HR_CI"],TyG_WC_qua[6,"HR_CI"],TyG_WC_qua[14,"P.value"]))
  Table<-rbind(Table,c("Model 3§","1.00 [Reference]",TyG_WC_qua[7,"HR_CI"],TyG_WC_qua[8,"HR_CI"],TyG_WC_qua[9,"HR_CI"],TyG_WC_qua[15,"P.value"]))
  Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",TyG_WC_qua[10,"HR_CI"],TyG_WC_qua[11,"HR_CI"],TyG_WC_qua[12,"HR_CI"],TyG_WC_qua[16,"P.value"]))
  
  Table<-rbind(Table,c("TyG-WHtR Quantile","", "","","",""))
  Table<-rbind(Table,c("Model 1†","1.00 [Reference]",TyG_WHtR_qua[1,"HR_CI"],TyG_WHtR_qua[2,"HR_CI"],TyG_WHtR_qua[3,"HR_CI"],TyG_WHtR_qua[13,"P.value"]))
  Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",TyG_WHtR_qua[4,"HR_CI"],TyG_WHtR_qua[5,"HR_CI"],TyG_WHtR_qua[6,"HR_CI"],TyG_WHtR_qua[14,"P.value"]))
  Table<-rbind(Table,c("Model 3§","1.00 [Reference]",TyG_WHtR_qua[7,"HR_CI"],TyG_WHtR_qua[8,"HR_CI"],TyG_WHtR_qua[9,"HR_CI"],TyG_WHtR_qua[15,"P.value"]))
  Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",TyG_WHtR_qua[10,"HR_CI"],TyG_WHtR_qua[11,"HR_CI"],TyG_WHtR_qua[12,"HR_CI"],TyG_WHtR_qua[16,"P.value"]))
  
  Table<-rbind(Table,c("TyG-BMI Quantile","", "","","",""))
  Table<-rbind(Table,c("Model 1†","1.00 [Reference]",TyG_BMI_qua[1,"HR_CI"],TyG_BMI_qua[2,"HR_CI"],TyG_BMI_qua[3,"HR_CI"],TyG_BMI_qua[13,"P.value"]))
  Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",TyG_BMI_qua[4,"HR_CI"],TyG_BMI_qua[5,"HR_CI"],TyG_BMI_qua[6,"HR_CI"],TyG_BMI_qua[14,"P.value"]))
  Table<-rbind(Table,c("Model 3§","1.00 [Reference]",TyG_BMI_qua[7,"HR_CI"],TyG_BMI_qua[8,"HR_CI"],TyG_BMI_qua[9,"HR_CI"],TyG_BMI_qua[15,"P.value"]))
  Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",TyG_BMI_qua[10,"HR_CI"],TyG_BMI_qua[11,"HR_CI"],TyG_BMI_qua[12,"HR_CI"],TyG_BMI_qua[16,"P.value"]))
  Table
  Table<-as.data.frame(Table)
  Table_quantile= data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  write.table(Table_quantile,sep = ",",file ="I:/NHANES study/PD&TYG&CVD/Result/Supplementary Table 5.csv",row.names =F,col.names =F )
  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 Total (Table 2) #### 
Table<-c("Exposure","Univariate model","","Multivariate model","")
Table<-rbind(Table,c("","RR (95% CI)*","P†", "RR (95% CI)","P"))
Table<-rbind(Table,c("TyG index","", "",  "",""))
Table<-rbind(Table,c("Per 1-index‡",
                     Table_continue[3,2],Table_continue[3,3], 
                     Table_continue[6,1],Table_continue[6,3]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","","1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_quantile[3,3],"", 
                     Table_quantile[6,3],"" ))
Table<-rbind(Table,c("Quantile 2", 
                     Table_quantile[3,4],"", 
                     Table_quantile[6,4],"" ))
Table<-rbind(Table,c("Quantile 4", 
                     Table_quantile[3,5],Table_quantile[3,6],
                     Table_quantile[6,5],Table_quantile[6,6]))

Table<-rbind(Table,c("TyG-WC index","", "",  "",""))
Table<-rbind(Table,c("Per 100-index",
                     Table_continue[8,2],Table_continue[8,3], 
                     Table_continue[11,1],Table_continue[11,3]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","","1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_quantile[8,3],"", 
                     Table_quantile[11,3],"" ))
Table<-rbind(Table,c("Quantile 2", 
                     Table_quantile[8,4],"", 
                     Table_quantile[11,4],"" ))
Table<-rbind(Table,c("Quantile 4", 
                     Table_quantile[8,5],Table_quantile[8,6],
                     Table_quantile[11,5],Table_quantile[11,6]))

Table<-rbind(Table,c("TyG-WHtR index","", "",  "",""))
Table<-rbind(Table,c("Per 1-index",
                     Table_continue[13,2],Table_continue[13,3], 
                     Table_continue[16,1],Table_continue[16,3]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","","1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_quantile[13,3],"", 
                     Table_quantile[16,3],"" ))
Table<-rbind(Table,c("Quantile 2", 
                     Table_quantile[13,4],"", 
                     Table_quantile[16,4],"" ))
Table<-rbind(Table,c("Quantile 4", 
                     Table_quantile[13,5],Table_quantile[13,6],
                     Table_quantile[16,5],Table_quantile[16,6]))

Table<-rbind(Table,c("TyG-BMI index","", "",  "",""))
Table<-rbind(Table,c("Per 100-index",
                     Table_continue[18,2],Table_continue[18,3], 
                     Table_continue[21,1],Table_continue[21,3]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","","1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_quantile[18,3],"", 
                     Table_quantile[21,3],"" ))
Table<-rbind(Table,c("Quantile 2", 
                     Table_quantile[18,4],"", 
                     Table_quantile[21,4],"" ))
Table<-rbind(Table,c("Quantile 4", 
                     Table_quantile[18,5],Table_quantile[18,6],
                     Table_quantile[21,5],Table_quantile[21,6]))

Table<-as.data.frame(Table)
Table= data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&CVD/Result/Table 2.csv",row.names =F,col.names =F )



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# +++++++++++============Figures==========+++++++++++ ####  
# >>>>> section 23 RCS for TyGs (Figure 2, Figure S6, S7) ####
setwd("I:/NHANES study/PD&TYG&CVD/Result")
{#* TYG ####
  load(file="I:/NHANES study/PD&TYG&CVD/Data/Interpolation_weighted.Rdata")
  RCS <- Interpolation_weighted
  
  # 生成 PDF 输出
  pdf("Figure 1a.pdf", width = 8, height = 7)
  
  # 数据分布设定
  options(datadist = "ddist")
  ddist <- datadist(RCS)
  
  # 去除 TYG 的异常值
  limUp <- 3 * IQR(RCS[,"TYG"], na.rm = TRUE) + quantile(RCS[,"TYG"], 3/4, na.rm = TRUE, names = FALSE)
  limDown <- quantile(RCS[,"TYG"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TYG"], na.rm = TRUE)
  RCS <- RCS[!RCS[,"TYG"] >= limUp & !RCS[,"TYG"] <= limDown,]
  
  # 确保因变量是二元的
  RCS$CVD_status <- as.numeric(RCS$CVD_status)
  
  # 逻辑回归模型
  fit <- lrm(CVD_status ~ rcs(TYG, 3) + Age_status + Sex + Race_ethnicity + SES + 
               Smoking_status + Drinking_status + BMI_status + Physical_status +
               HTN_status + T2D_status +  Cohort, data = RCS, x = TRUE, y = TRUE)
  
  anova(fit)
  
  # 预测 OR (Odds Ratio)
  p <- round(anova(fit)[, 3], 3)
  options(datadist = "ddist")
  pred_OR <- Predict(fit, TYG, ref.zero = TRUE, fun = exp)
  
  # 计算最接近 OR = 1 的 TYG 值
  pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
  pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
  pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
  pred_OR$distance <- pred_OR$distance_to_1 + pred_OR$distance_to_2 + pred_OR$distance_to_3
  closest_point <- pred_OR[which.min(pred_OR$distance), ]
  closest_TYG_value <- closest_point$TYG
  refvalue <- round(closest_TYG_value, 2)
  
  # 颜色定义
  violet <- "#96C37D"
  par(mar = c(5, 4, 4, 4) + 0.3)
  par(xpd = NA)
  ylim.bot <- min(pred_OR[,"lower"])
  ylim.top <- max(pred_OR[,"upper"])
  
  # 绘制密度图
  dens <- density(RCS$TYG)
  plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd = 1.7)
  polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5))
  axis(side = 4, cex.axis = 1.7)
  par(new = TRUE)
  
  # 绘制 OR vs. TYG 图
  plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
       col = "#CC5B45", lwd = 3, cex.axis =1.7, cex.lab=1.7)
  polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
          c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
          col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  
  
  lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5, col ="#6A8EC9")
  lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5, col ="#6A8EC9")
  
  # 添加参考线
  lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) 
  
  # 标记 OR = 1 位置
  points(refvalue, 1, pch = 16, cex = 1.5)
  box(lwd = 3)
  
  dev.off()
}
p
#0.061
refvalue 
#8.71

{#* TyG_WC  ####
  load(file="I:/NHANES study/PD&TYG&CVD/Data/Interpolation_weighted.Rdata")
  RCS <- Interpolation_weighted
  
  # 生成 PDF 输出
  pdf("Figure 1b.pdf", width = 8, height = 7)
  
  # 数据分布设定
  options(datadist = "ddist")
  ddist <- datadist(RCS)
  
  # 去除 TyG_WC 的异常值
  limUp <- 3 * IQR(RCS[,"TyG_WC"], na.rm = TRUE) + quantile(RCS[,"TyG_WC"], 3/4, na.rm = TRUE, names = FALSE)
  limDown <- quantile(RCS[,"TyG_WC"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TyG_WC"], na.rm = TRUE)
  RCS <- RCS[!RCS[,"TyG_WC"] >= limUp & !RCS[,"TyG_WC"] <= limDown,]
  
  # 确保因变量是二元的
  RCS$CVD_status <- as.numeric(RCS$CVD_status)
  
  # 逻辑回归模型
  fit <- lrm(CVD_status ~ rcs(TyG_WC, 3) + Age_status + Sex + Race_ethnicity + SES + 
               Smoking_status + Drinking_status + BMI_status + Physical_status +
               HTN_status + T2D_status +  Cohort, data = RCS, x = TRUE, y = TRUE)
  
  anova(fit)
  
  # 预测 OR (Odds Ratio)
  p <- round(anova(fit)[, 3], 3)
  options(datadist = "ddist")
  pred_OR <- Predict(fit, TyG_WC, ref.zero = TRUE, fun = exp)
  
  # 计算最接近 OR = 1 的 TyG_WC 值
  pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
  pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
  pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
  pred_OR$distance <- pred_OR$distance_to_1 + pred_OR$distance_to_2 + pred_OR$distance_to_3
  closest_point <- pred_OR[which.min(pred_OR$distance), ]
  closest_TyG_WC_value <- closest_point$TyG_WC
  refvalue <- round(closest_TyG_WC_value, 2)
  
  # 颜色定义
  violet <- "#5CB0C3"
  par(mar = c(5, 4, 4, 4) + 0.3)
  par(xpd = NA)
  ylim.bot <- min(pred_OR[,"lower"])
  ylim.top <- max(pred_OR[,"upper"])
  
  # 绘制密度图
  dens <- density(RCS$TyG_WC)
  plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd = 1.7)
  polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5))
  axis(side = 4, cex.axis = 1.7)
  par(new = TRUE)
  
  # 绘制 OR vs. TyG_WC 图
  plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
       col = "#CC5B45", lwd = 3, cex.axis =1.7, cex.lab=1.7)
  polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
          c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
          col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  
  
  lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5, col ="#6A8EC9")
  lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5, col ="#6A8EC9")
  
  # 添加参考线
  lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) 
  
  # 标记 OR = 1 位置
  points(refvalue, 1, pch = 16, cex = 1.5)
  box(lwd = 3)
  
  dev.off()
}
p
#0.018
refvalue 
#869.3  

{#* TyG_WHtR  ####
  load(file="I:/NHANES study/PD&TYG&CVD/Data/Interpolation_weighted.Rdata")
  RCS <- Interpolation_weighted
  
  # 生成 PDF 输出
  pdf("Figure 1c.pdf", width = 8, height = 7)
  
  # 数据分布设定
  options(datadist = "ddist")
  ddist <- datadist(RCS)
  
  # 去除 TyG_WHtR 的异常值
  limUp <- 3 * IQR(RCS[,"TyG_WHtR"], na.rm = TRUE) + quantile(RCS[,"TyG_WHtR"], 3/4, na.rm = TRUE, names = FALSE)
  limDown <- quantile(RCS[,"TyG_WHtR"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TyG_WHtR"], na.rm = TRUE)
  RCS <- RCS[!RCS[,"TyG_WHtR"] >= limUp & !RCS[,"TyG_WHtR"] <= limDown,]
  
  # 确保因变量是二元的
  RCS$CVD_status <- as.numeric(RCS$CVD_status)
  
  # 逻辑回归模型
  fit <- lrm(CVD_status ~ rcs(TyG_WHtR, 3) + Age_status + Sex + Race_ethnicity + SES + 
               Smoking_status + Drinking_status + BMI_status + Physical_status +
               HTN_status + T2D_status +  Cohort, data = RCS, x = TRUE, y = TRUE)
  
  anova(fit)
  
  # 预测 OR (Odds Ratio)
  p <- round(anova(fit)[, 3], 3)
  options(datadist = "ddist")
  pred_OR <- Predict(fit, TyG_WHtR, ref.zero = TRUE, fun = exp)
  
  # 计算最接近 OR = 1 的 TyG_WHtR 值
  pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
  pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
  pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
  pred_OR$distance <- pred_OR$distance_to_1 + pred_OR$distance_to_2 + pred_OR$distance_to_3
  closest_point <- pred_OR[which.min(pred_OR$distance), ]
  closest_TyG_WHtR_value <- closest_point$TyG_WHtR
  refvalue <- round(closest_TyG_WHtR_value, 2)
  
  # 颜色定义
  violet <- "#F5A216"
  par(mar = c(5, 4, 4, 4) + 0.3)
  par(xpd = NA)
  ylim.bot <- min(pred_OR[,"lower"])
  ylim.top <- max(pred_OR[,"upper"])
  
  # 绘制密度图
  dens <- density(RCS$TyG_WHtR)
  plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd = 1.7)
  polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5))
  axis(side = 4, cex.axis = 1.7)
  par(new = TRUE)
  
  # 绘制 OR vs. TyG_WHtR 图
  plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
       col = "#CC5B45", lwd = 3, cex.axis =1.7, cex.lab=1.7)
  polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
          c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
          col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  
  
  lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5, col ="#6A8EC9")
  lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5, col ="#6A8EC9")
  
  # 添加参考线
  lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) 
  
  # 标记 OR = 1 位置
  points(refvalue, 1, pch = 16, cex = 1.5)
  box(lwd = 3)
  
  dev.off()
}
p
#0.005
refvalue 
#5.18

{#* TyG_BMI  ####
  load(file="I:/NHANES study/PD&TYG&CVD/Data/Interpolation_weighted.Rdata")
  RCS <- Interpolation_weighted
  # 生成 PDF 输出
  pdf("Figure 1d.pdf", width = 8, height = 7)
  
  # 数据分布设定
  options(datadist = "ddist")
  ddist <- datadist(RCS)
  
  # 去除 TyG_BMI 的异常值
  limUp <- 3 * IQR(RCS[,"TyG_BMI"], na.rm = TRUE) + quantile(RCS[,"TyG_BMI"], 3/4, na.rm = TRUE, names = FALSE)
  limDown <- quantile(RCS[,"TyG_BMI"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TyG_BMI"], na.rm = TRUE)
  RCS <- RCS[!RCS[,"TyG_BMI"] >= limUp & !RCS[,"TyG_BMI"] <= limDown,]
  
  # 确保因变量是二元的
  RCS$CVD_status <- as.numeric(RCS$CVD_status)
  
  # 逻辑回归模型
  fit <- lrm(CVD_status ~ rcs(TyG_BMI, 3) + Age_status + Sex + Race_ethnicity + SES + 
               Smoking_status + Drinking_status + BMI_status + Physical_status +
               HTN_status + T2D_status +  Cohort, data = RCS, x = TRUE, y = TRUE)
  
  anova(fit)
  
  # 预测 OR (Odds Ratio)
  p <- round(anova(fit)[, 3], 3)
  options(datadist = "ddist")
  pred_OR <- Predict(fit, TyG_BMI, ref.zero = TRUE, fun = exp)
  
  # 计算最接近 OR = 1 的 TyG_BMI 值
  pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
  pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
  pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
  pred_OR$distance <- pred_OR$distance_to_1 + pred_OR$distance_to_2 + pred_OR$distance_to_3
  closest_point <- pred_OR[which.min(pred_OR$distance), ]
  closest_TyG_BMI_value <- closest_point$TyG_BMI
  refvalue <- round(closest_TyG_BMI_value, 2)
  
  # 颜色定义
  violet <- "#B46DA9"
  par(mar = c(5, 4, 4, 4) + 0.3)
  par(xpd = NA)
  ylim.bot <- min(pred_OR[,"lower"])
  ylim.top <- max(pred_OR[,"upper"])
  
  # 绘制密度图
  dens <- density(RCS$TyG_BMI)
  plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd = 1.7)
  polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5))
  axis(side = 4, cex.axis = 1.7)
  par(new = TRUE)
  
  # 绘制 OR vs. TyG_BMI 图
  plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
       col = "#CC5B45", lwd = 3, cex.axis =1.7, cex.lab=1.7)
  polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
          c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
          col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  
  
  lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5, col ="#6A8EC9")
  lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5, col ="#6A8EC9")
  
  # 添加参考线
  lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) 
  
  # 标记 OR = 1 位置
  points(refvalue, 1, pch = 16, cex = 1.5)
  box(lwd = 3)
  
  dev.off()
}
p
#0.066
refvalue 
#243.89
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 Threshold effect analyses (Table 3) #### 
load(file="I:/NHANES study/PD&TYG&CVD/Data/Interpolation_weighted.Rdata")
options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)
Interpolation_weighted$TyG_threshold <- ifelse(Interpolation_weighted$TYG<= 8.71, 0, 1)
Interpolation_weighted$TyG_WC_threshold <- ifelse(Interpolation_weighted$TyG_WC<= 869.3, 0, 1)
Interpolation_weighted$TyG_WHtR_threshold <- ifelse(Interpolation_weighted$TyG_WHtR<= 5.18, 0, 1)
Interpolation_weighted$TyG_BMI_threshold <- ifelse(Interpolation_weighted$TyG_BMI<= 243.89, 0, 1)
Interpolation_weighted$TyG_WC<-Interpolation_weighted$TyG_WC/100
Interpolation_weighted$TyG_BMI<-Interpolation_weighted$TyG_BMI/100
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{#* Combine TYG #####
  {#** TYG<= 8.71 ####
    model <- svyglm(CVD_status ~ TYG+
                      Age_status+Sex+Race_ethnicity+SES+BMI_status+
                      Physical_status+Smoking_status+Drinking_status+
                      HTN_status+T2D_status+Cohort,design = rhcSvy,subset=(TyG_threshold==1),
                    family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]]["TYG", "Estimate"]
    se <- model_result[["coefficients"]]["TYG", "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="<= 8.71")
    result<-result4
  }
  result
  {#** TYG> 8.71 ####
    model<-svyglm(CVD_status ~ TYG+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+
                    Physical_status+Smoking_status+Drinking_status+
                    HTN_status+T2D_status+Cohort,design = rhcSvy,subset=(TyG_threshold==0),
                  family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]]["TYG", "Estimate"]
    se <- model_result[["coefficients"]]["TYG", "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="> 8.71")
    result<-rbind(result,result4)
  }
  {#** LRT ####
    null_model <- svyglm(CVD_status ~ TYG + TyG_threshold +
                           Age_status + Sex + Race_ethnicity + SES + BMI_status +
                           Physical_status + Smoking_status + Drinking_status +
                           HTN_status + T2D_status + Cohort, 
                         design = rhcSvy,family = quasibinomial())
    
    full_model <- svyglm(CVD_status ~ TYG * TyG_threshold + 
                           Age_status + Sex + Race_ethnicity + SES + BMI_status +
                           Physical_status + Smoking_status + Drinking_status +
                           HTN_status + T2D_status + Cohort, 
                         design = rhcSvy, family = quasibinomial())
    
    P<-anova(null_model, full_model, test = "Chisq")
    P$p
    result$P_LRT<-P$p
    result_TYG<-result
  }
}
{#* Combine TyG_WC #####
  {#** TyG_WC<= 869.3 ####
    model <- svyglm(CVD_status ~ TyG_WC+
                      Age_status+Sex+Race_ethnicity+SES+BMI_status+
                      Physical_status+Smoking_status+Drinking_status+
                      HTN_status+T2D_status+Cohort,design = rhcSvy,subset=(TyG_WC_threshold==1),
                    family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]]["TyG_WC", "Estimate"]
    se <- model_result[["coefficients"]]["TyG_WC", "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="<= 869.3")
    result<-result4
  }
  {#** TyG_WC> 869.3 ####
    model<-svyglm(CVD_status ~ TyG_WC+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+
                    Physical_status+Smoking_status+Drinking_status+
                    HTN_status+T2D_status+Cohort,design = rhcSvy,subset=(TyG_WC_threshold==0),
                  family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]]["TyG_WC", "Estimate"]
    se <- model_result[["coefficients"]]["TyG_WC", "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="> 869.3")
    result<-rbind(result,result4)
  }
  {#** LRT ####
    null_model <- svyglm(CVD_status ~ TyG_WC + TyG_WC_threshold +
                           Age_status + Sex + Race_ethnicity + SES + BMI_status +
                           Physical_status + Smoking_status + Drinking_status +
                           HTN_status + T2D_status + Cohort, 
                         design = rhcSvy,family = quasibinomial())
    
    full_model <- svyglm(CVD_status ~ TyG_WC * TyG_WC_threshold + 
                           Age_status + Sex + Race_ethnicity + SES + BMI_status +
                           Physical_status + Smoking_status + Drinking_status +
                           HTN_status + T2D_status + Cohort, 
                         design = rhcSvy, family = quasibinomial())
    
    P<-anova(null_model, full_model, test = "Chisq")
    P$p
    result$P_LRT<-P$p
  }
  result_TyG_WC<-result
}
{#* Combine TyG_WHtR #####
  {#** TyG_WHtR<= 5.18 ####
    model <- svyglm(CVD_status ~ TyG_WHtR+
                      Age_status+Sex+Race_ethnicity+SES+BMI_status+
                      Physical_status+Smoking_status+Drinking_status+
                      HTN_status+T2D_status+Cohort,design = rhcSvy,subset=(TyG_WHtR_threshold==1),
                    family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]]["TyG_WHtR", "Estimate"]
    se <- model_result[["coefficients"]]["TyG_WHtR", "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="<= 5.18")
    result<-result4
  }
  {#** TyG_WHtR> 5.18 ####
    model<-svyglm(CVD_status ~ TyG_WHtR+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+
                    Physical_status+Smoking_status+Drinking_status+
                    HTN_status+T2D_status+Cohort,design = rhcSvy,subset=(TyG_WHtR_threshold==0),
                  family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]]["TyG_WHtR", "Estimate"]
    se <- model_result[["coefficients"]]["TyG_WHtR", "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="> 5.18")
    result<-rbind(result,result4)
  }
  {#** LRT ####
    null_model <- svyglm(CVD_status ~ TyG_WHtR + TyG_WHtR_threshold +
                           Age_status + Sex + Race_ethnicity + SES + BMI_status +
                           Physical_status + Smoking_status + Drinking_status +
                           HTN_status + T2D_status + Cohort, 
                         design = rhcSvy,family = quasibinomial())
    
    full_model <- svyglm(CVD_status ~ TyG_WHtR * TyG_WHtR_threshold + 
                           Age_status + Sex + Race_ethnicity + SES + BMI_status +
                           Physical_status + Smoking_status + Drinking_status +
                           HTN_status + T2D_status + Cohort, 
                         design = rhcSvy, family = quasibinomial())
    
    P<-anova(null_model, full_model, test = "Chisq")
    P$p
    result$P_LRT<-P$p
  }
  result_TyG_WHtR<-result
}
{#* Combine TyG_BMI #####
  {#** TyG_BMI<= 243.89 ####
    model <- svyglm(CVD_status ~ TyG_BMI+
                      Age_status+Sex+Race_ethnicity+SES+BMI_status+
                      Physical_status+Smoking_status+Drinking_status+
                      HTN_status+T2D_status+Cohort,design = rhcSvy,subset=(TyG_BMI_threshold==1),
                    family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]]["TyG_BMI", "Estimate"]
    se <- model_result[["coefficients"]]["TyG_BMI", "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="<= 243.89")
    result<-result4
  }
  {#** TyG_BMI> 243.89 ####
    model<-svyglm(CVD_status ~ TyG_BMI+
                    Age_status+Sex+Race_ethnicity+SES+BMI_status+
                    Physical_status+Smoking_status+Drinking_status+
                    HTN_status+T2D_status+Cohort,design = rhcSvy,subset=(TyG_BMI_threshold==0),
                  family = quasibinomial())
    model_result<-summary(model)
    model_result
    P<-model_result[["coefficients"]][2,"Pr(>|t|)"]
    estimate <- model_result[["coefficients"]]["TyG_BMI", "Estimate"]
    se <- model_result[["coefficients"]]["TyG_BMI", "Std. Error"]
    RR <- exp(estimate)
    CI_lower <- exp(estimate - 1.96 * se)
    CI_upper <- exp(estimate + 1.96 * se)
    result4 <- data.frame('HR'=RR,'lower .95'=CI_lower ,'upper .95'=CI_upper,
                          'P value' =P,'model'="> 243.89")
    result<-rbind(result,result4)
  }
  {#** LRT ####
    null_model <- svyglm(CVD_status ~ TyG_BMI + TyG_BMI_threshold +
                           Age_status + Sex + Race_ethnicity + SES + BMI_status +
                           Physical_status + Smoking_status + Drinking_status +
                           HTN_status + T2D_status + Cohort, 
                         design = rhcSvy,family = quasibinomial())
    
    full_model <- svyglm(CVD_status ~ TyG_BMI * TyG_BMI_threshold + 
                           Age_status + Sex + Race_ethnicity + SES + BMI_status +
                           Physical_status + Smoking_status + Drinking_status +
                           HTN_status + T2D_status + Cohort, 
                         design = rhcSvy, family = quasibinomial())
    
    P<-anova(null_model, full_model, test = "Chisq")
    P$p
    result$P_LRT<-P$p
  }
  result_TyG_BMI<-result
}
{ #* Combine #####
  result.cause<-rbind(result_TYG,result_TyG_WC,result_TyG_WHtR,result_TyG_BMI)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  result.cause$P_LRT<-round(result.cause$P_LRT,3)
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause[result.cause=="1000"]<-"1.00"
  result.cause[result.cause=="2000"]<-"2.00"
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  result.cause$P_LRT<-lapply(result.cause$P_LRT,round_3_function)
  result.cause[result.cause=="00000"]<-"<0.001"
}
result.cause
{#* Cbind #####
  Table<-c("","TyG index","","")
  Table<-rbind(Table,c("", "< Inflection point",">= Inflection point","P for Log-likelihood ratio"))
  Table<-rbind(Table,c("TyG *","<8.71",">=8.71",""))
  Table<-rbind(Table,c("",result.cause[1,7],result.cause[2,7],result.cause[2,6]))
  Table<-rbind(Table,c("TyG *","<869.3",">=869.3",""))
  Table<-rbind(Table,c("",result.cause[3,7],result.cause[4,7],result.cause[4,6]))
  Table<-rbind(Table,c("TyG *","<5.18",">=5.18",""))
  Table<-rbind(Table,c("",result.cause[5,7],result.cause[6,7],result.cause[6,6]))
  Table<-rbind(Table,c("TyG *","<243.89",">=243.89",""))
  Table<-rbind(Table,c("",result.cause[7,7],result.cause[8,7],result.cause[8,6]))
  Table<-as.data.frame(Table)
  Table= data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table
  write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&CVD/Result/Table 3.csv",row.names =F,col.names =F )
}

