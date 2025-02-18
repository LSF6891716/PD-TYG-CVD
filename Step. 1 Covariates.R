# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 0. Packages and Functions used <<<<< ####
{#* section 0.1 Packages ####
  library(foreign)
  library(dplyr)
  library(nhanesR)
  library(tidyverse)
  library(tidyr)
  library(PSCBS)
}
{#* section 0.2 Functions ####
  {#** section 0.2.1 multi_merge ####
    multimerge<-function(dat=list(),...){
      if(length(dat)<2)return(as.data.frame(dat))
      mergedat<-dat[[1]]
      dat[[1]]<-NULL
      for(i in dat){
        mergedat<-merge(mergedat,i,...)
      }
      return(mergedat)
    }
  }
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 8. Covariates of NHANES 1999-2004 <<<<< ####
{#** Section 8.1 Age,Gender,Race/ethnicity,Marital status, Education_levels,PIR  ####
    nhs_tsv('demo')[1:3]
    DEMO_DATA<-nhs_read(nhs_tsv('demo')[1:3],"RIDRETH2:Race_ethnicity","RIAGENDR:Gender",
                        "RIDAGEYR:Age","DMDMARTL:Marital_status","DMDEDUC2:Education_levels","INDFMPIR:INDFMPIR")
    DEMO_CON<-DEMO_DATA[,c("seqn","Year","Age","Gender","Race_ethnicity","Marital_status","Education_levels","INDFMPIR")]
    colnames(DEMO_CON)[1]<-"SEQN"
    table(DEMO_CON$Marital_status,useNA = "ifany")
    DEMO_CON$Marital_status[DEMO_CON$Marital_status=="Living with partner"|
                            DEMO_CON$Marital_status=="Married"]<-"Married"
    DEMO_CON$Marital_status[DEMO_CON$Marital_status=="Divorced"|
                            DEMO_CON$Marital_status=="Separated"|
                            DEMO_CON$Marital_status=="Widowed"]<-"Separated"
    DEMO_CON$Marital_status[DEMO_CON$Marital_status=="Never married"]<-"Never_married"
    table(DEMO_CON$Marital_status,useNA = "ifany")
    table(DEMO_CON$Education_levels)
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="Less Than 9th Grade"]<-"Less_than_high_school"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="9-11th Grade (Includes 12th grade with no diploma)"]<-"Less_than_high_school"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="High School Grad/GED or Equivalent"]<-"High_school_or_Equivalent"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="Some College or AA degree"]<-"College_or_above"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="College Graduate or above"]<-"College_or_above"
    table(DEMO_CON$`Race_ethnicity`)
    DEMO_CON$Race_ethnicity[DEMO_CON$Race_ethnicity=="Other Race - Including Multi-Racial"]<-"Other_Race"
    DEMO_CON$Race_ethnicity[DEMO_CON$Race_ethnicity=="Mexican American"]<-"Hispanic"
    DEMO_CON$Race_ethnicity[DEMO_CON$Race_ethnicity=="Other Hispanic"]<-"Hispanic"
    DEMO_CON$PIR_raw=DEMO_CON$INDFMPIR
    DEMO_CON$PIR[DEMO_CON$INDFMPIR<1.30]<-"<1.30"
    DEMO_CON$PIR[DEMO_CON$INDFMPIR>=1.30&DEMO_CON$INDFMPIR<3]<-"1.30–2.99"
    DEMO_CON$PIR[DEMO_CON$INDFMPIR>=3&DEMO_CON$INDFMPIR<5]<-"3.00–4.99"
    DEMO_CON$PIR[DEMO_CON$INDFMPIR>=5]<-">=5.00"
    DEMO_CON$INDFMPIR<-NULL
    Covariates_CON1_1<-DEMO_CON
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1')])
}
{#** Section 8.2 health insurance  ####
    nhs_tsv('HIQ')[1:3]
    Insurance_DATA<-nhs_read(nhs_tsv('HIQ')[1:3],"HID010:insurance","HID030A:Private","HID030B:Medicare",
                             "HID030C:Medicaid","HID030D:Others","HID030E:Other",codebook = F)
    Insurance_DATA$Health_insurance[Insurance_DATA$insurance==2]<-"No_insurance"
    Insurance_DATA$Health_insurance[Insurance_DATA$insurance==1&(Insurance_DATA$Medicare==1|
                                                                   Insurance_DATA$Medicaid==1|Insurance_DATA$Others==1|Insurance_DATA$Other==1)]<-"Public_insurance"
    Insurance_DATA$Health_insurance[Insurance_DATA$insurance==1&Insurance_DATA$Private==1]<-"Private_insurance"
    
    Insurance_CON<-Insurance_DATA[,c("seqn","Health_insurance")]
    table(Insurance_DATA$Health_insurance)
    colnames(Insurance_CON)[1]<-"SEQN"
    Covariates_CON1_2<-merge(Covariates_CON1_1,Insurance_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2')])
}
{#** Section 8.3 Smoking status  ####
    nhs_tsv('smq')[c(1,4,7)]
    #SMQ020 - Smoked at least 100 cigarettes in life
    #SMQ040 - Do you now smoke cigarettes
    SMQ_DATA<-nhs_read(nhs_tsv('smq')[c(1,4,7)],"SMQ020","SMQ040",Year = F,codebook = F)
    table(SMQ_DATA$SMQ040)
    SMQ_DATA$Smoking_status[SMQ_DATA$SMQ020==1&(SMQ_DATA$SMQ040==1|SMQ_DATA$SMQ040==2)]<-
      "Current_smoker"
    SMQ_DATA$Smoking_status[SMQ_DATA$SMQ020==1&(SMQ_DATA$SMQ040==3)]<-
      "Former_smoker"
    SMQ_DATA$Smoking_status[SMQ_DATA$SMQ020==2]<-
      "Never_smoker"
    SMQ_CON<-SMQ_DATA[,c("seqn","Smoking_status")]
    colnames(SMQ_CON)[1]<-"SEQN"
    Covariates_CON1_3<-merge(Covariates_CON1_2,SMQ_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                          record=='Covariates_CON1_3')])
}
{#** Section 8.4 Drinking status  ####
    Drink_DATA<-nhs_read(nhs_tsv('alq')[1:3],"ALQ110","ALQ100","ALD100:ALQ100","ALQ101:ALQ100","ALQ120Q","ALQ120U","ALQ130","ALQ140Q","ALQ140U","ALQ150",Year = F,codebook = F)
    #ALQ100 - Had at least 12 alcohol drinks/1 yr?
    #ALQ110 - Had at least 12 alcohol drinks/lifetime?
    #ALQ120Q - How often drink alcohol over past 12 mos
    #ALQ130 - Avg # alcoholic drinks/day -past 12 mos
    #days have 5 or more drinks/past 12 mos
    #Ever have 5 or more drinks every day?
    Drink_DATA$ALQ120Q[Drink_DATA$ALQ120Q==777]<-NA
    Drink_DATA$ALQ120Q[Drink_DATA$ALQ120Q==999]<-NA
    Drink_DATA$ALQ130[Drink_DATA$ALQ130==77]<-NA
    Drink_DATA$ALQ130[Drink_DATA$ALQ130==99]<-NA
    Drink_DATA$ALQ140Q[Drink_DATA$ALQ140Q==777]<-NA
    Drink_DATA$ALQ140Q[Drink_DATA$ALQ140Q==999]<-NA
    Drink_DATA$ALQ150[Drink_DATA$ALQ150==7]<-NA
    Drink_DATA$ALQ150[Drink_DATA$ALQ150==9]<-NA
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==7]<-NA
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==9]<-NA
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==1]<-(365/7)
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==2]<-12
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==3]<-1
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==7]<-NA
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==9]<-NA
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==1]<-(365/7)
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==2]<-12
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==3]<-1
    gender_DATA<-nhs_read(nhs_tsv('demo')[1:3],"RIAGENDR:Gender",Year = F,codebook = T)
    Drink_DATA<-merge(Drink_DATA,gender_DATA,by = "seqn",all.x = T)
    Drink_DATA$Drinking_status[Drink_DATA$ALQ110==2|Drink_DATA$ALQ100==2|Drink_DATA$ALQ150==2]<-"Nondrinker"
    Drink_DATA$Drinking_status[Drink_DATA$ALQ110==1|Drink_DATA$ALQ100==1|Drink_DATA$ALQ140Q>=3]<-"Light/moderate_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130>1&Drink_DATA$Gender=="Female")|
                                   (Drink_DATA$ALQ130>2&Drink_DATA$Gender=="Male")))&Drink_DATA$ALQ120Q>=1]<-
      "Heavier_drinker"
    Drink_DATA$Drinking_status[Drink_DATA$ALQ150==1]<-
      "Heavier_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130<=1)&Drink_DATA$Gender=="Female")|
                                  ((Drink_DATA$ALQ130<=2)&Drink_DATA$Gender=="Male"))&Drink_DATA$ALQ120Q>=1]<-
      "Light/moderate_drinker"
    
    Drink_DATA$Drink_status[Drink_DATA$ALQ110==2|Drink_DATA$ALQ100==2]<-"Nondrinker"
    Drink_DATA$Drink_status[Drink_DATA$ALQ110==1|Drink_DATA$ALQ100==1|(Drink_DATA$ALQ120Q*Drink_DATA$ALQ130>=12)|Drink_DATA$ALQ150==1]<-"Drinker"
    Drink_DATA$Drinking_status[Drink_DATA$Drink_status=="Nondrinker"]<-"Nondrinker"
    Drink_DATA$Drinking_status[Drink_DATA$Drink_status=="Drinker"]<-"Light/moderate_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130>1&Drink_DATA$Gender=="Female")|
                                   (Drink_DATA$ALQ130>2&Drink_DATA$Gender=="Male"))
                                &Drink_DATA$Drink_status=="Drinker")|Drink_DATA$ALQ150==1]<-"Heavier_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130<=1&Drink_DATA$Gender=="Female")|
                                   (Drink_DATA$ALQ130<=2&Drink_DATA$Gender=="Male"))
                                &Drink_DATA$Drink_status=="Drinker")|Drink_DATA$ALQ150==1]<-"Light/moderate_drinker"
    
    table(Drink_DATA$Drinking_status,useNA = "ifany")
    Drink_CON<-Drink_DATA[,c("seqn","Drinking_status")]
    colnames(Drink_CON)[1]<-"SEQN"
    Covariates_CON1_4<-merge(Covariates_CON1_3,Drink_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                          record=='Covariates_CON1_3'|record=='Covariates_CON1_4')])
}
{#** Section 8.5 Physical_activity  ####
    Physical_DATA<-nhs_read(nhs_tsv('paq')[c(1,3,5)],"PAD020","PAQ050Q","PAQ050U","PAQ100","PAD120","PAD200",
                            "PAD440","PAD460","PAQ560","PAD320","PAD440",
                            Year = F,codebook = F)
    # PAD020 - Walked or bicycled over past 30 days
    # PAQ050Q - # times walked or bicycled
    # PAQ050U - Unit of measure (day/week/month)
    # PAD080 - How long per day (minutes)
    # PAQ100 - Tasks around home/yard past 30 days
    # PAD120 - # of times past 30 days
    # PAD160 - How long each time (minutes)
    # PAQ180 - Avg level of physical activity each day
    # PAD200 - Vigorous activity over past 30 days
    # PAD320 - Moderate activity over past 30 days
    # PAD440 - Muscle strengthening activities
    # PAD460 - Number of times past 30 days
    # PAQ480 - Daily hours of TV, video or computer use
    # PAQ500 - Activity comparison last mo - last yr
    # PAQ520 - Compare activity w/others same age
    # PAQ540 - Compare activity with 10 years ago
    # PAQ560 - # time/week you play or exercise hard
    # PAD570 - # of hours watch TV or videos yesterday
    # PAQ580 - # hours use computer/games yesterday
    Physical_DATA$PAQ050Q[Physical_DATA$PAQ050Q==77777]<-NA
    Physical_DATA$PAQ050Q[Physical_DATA$PAQ050Q==99999]<-NA
    Physical_DATA$PAQ050U[Physical_DATA$PAQ050U==7]<-NA
    Physical_DATA$PAQ050U[Physical_DATA$PAQ050U==9]<-NA
    Physical_DATA$PAQ050U[Physical_DATA$PAQ050U==1]<-30
    Physical_DATA$PAQ050U[Physical_DATA$PAQ050U==2]<-(30/7)
    Physical_DATA$PAQ050U[Physical_DATA$PAQ050U==3]<-1
    Physical_DATA$PAD120[Physical_DATA$PAD120==77777]<-NA
    Physical_DATA$PAD120[Physical_DATA$PAD120==99999]<-NA
    Physical_DATA$PAD460[Physical_DATA$PAD460==77777]<-NA
    Physical_DATA$PAD460[Physical_DATA$PAD460==99999]<-NA
    Physical_DATA$PAQ560[Physical_DATA$PAQ560==77777]<-NA
    Physical_DATA$PAQ560[Physical_DATA$PAQ560==99999]<-NA
    Physical_DATA$Physical_status[Physical_DATA$PAD020==1|Physical_DATA$PAQ100==1|Physical_DATA$PAD200==1|
                                    Physical_DATA$PAD320==1|Physical_DATA$PAD440==1]<-"Insufficient"
    Physical_DATA$Physical_status[(Physical_DATA$PAD320==2|Physical_DATA$PAD320==3|is.na(Physical_DATA$PAD020))&
                                    (Physical_DATA$PAQ100==2|Physical_DATA$PAQ100==3|is.na(Physical_DATA$PAQ100))&
                                    (Physical_DATA$PAD320==2|Physical_DATA$PAD320==3|is.na(Physical_DATA$PAD320))&
                                    (Physical_DATA$PAD440==2|Physical_DATA$PAD440==3|is.na(Physical_DATA$PAD020))&
                                    (Physical_DATA$PAD200==2|Physical_DATA$PAD200==3|is.na(Physical_DATA$PAD020))&
                                    (Physical_DATA$PAD020==2|Physical_DATA$PAD020==3|is.na(Physical_DATA$PAD020))]<-"Inactive"
    Physical_DATA$Physical_status[is.na(Physical_DATA$PAD020)&is.na(Physical_DATA$PAD100)&is.na(Physical_DATA$PAD200)&is.na(Physical_DATA$PAD320)&is.na(Physical_DATA$PAD440)]<-NA
    Physical_DATA$PAQ050Q[is.na(Physical_DATA$PAQ050Q)]<-0
    Physical_DATA$PAQ050U[is.na(Physical_DATA$PAQ050U)]<-0    
    Physical_DATA$PAD120[is.na(Physical_DATA$PAQ050Q)]<-0    
    Physical_DATA$PAD460[is.na(Physical_DATA$PAD460)]<-0
    Physical_DATA$PAQ560[is.na(Physical_DATA$PAQ560)]<-0
    Physical_DATA$Physical_status[(((Physical_DATA$PAQ050Q*Physical_DATA$PAQ050U)/30*7)+(Physical_DATA$PAD120/30*7)+
                                     (Physical_DATA$PAD460/30*7)>=5)|(Physical_DATA$PAQ560>=3)]<-"Recommended"
    table(Physical_DATA$Physical_status,useNA = 'ifany')
    
    Physical_CON<-Physical_DATA[,c("seqn","Physical_status")]
    colnames(Physical_CON)[1]<-"SEQN"
    Covariates_CON1_5<-merge(Covariates_CON1_4,Physical_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                          record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                          record=='Covariates_CON1_5')])
}
{#** Section 8.6 HEI  ####
    nhs_tsv("drxtot|dr1tot")[1:3]
    data_HEI<-dex_HEI(years = 1999:2004,version = "2015",method = "ssum",dietary = "tot",day = 1)
    HEI_data<-nhs_read(nhs_tsv("drxtot|dr1tot")[1:3],'WTDRD1:weight2','WTDR4YR:weight4',Year = T,codebook = F)
    CON_HEI<-merge(data_HEI[,c("seqn","hei2015_total_score")],HEI_data[,c("seqn","weight2","weight4",'Year')],by="seqn",all.x = T)
    CON_HEI <- CON_HEI %>% mutate(weight = ifelse(Year=="1999-2000"|Year=="2001-2002",weight4*2/3,weight2/3))
    colnames(CON_HEI)[1]<-"SEQN"
    load("I:/NHANES study/PD&TYG&CVD/Data/CON1_baseline.Rdata")
    CON_HEI<-merge(CON_HEI,CON1_baseline[,c("SEQN","ID")],by="SEQN",all.y = T)
    Quantile<-weightedQuantile(CON_HEI$hei2015_total_score,weights = CON_HEI$weight, probs=c(0.2,0.4,0.6,0.8),  na.rm=TRUE)
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[1]]<-"Quintile 1"
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[2]&CON_HEI$hei2015_total_score>Quantile[1]]<-"Quintile 2"
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[3]&CON_HEI$hei2015_total_score>Quantile[2]]<-"Quintile 3"
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[4]&CON_HEI$hei2015_total_score>Quantile[3]]<-"Quintile 4"
    CON_HEI$HEI[CON_HEI$hei2015_total_score>Quantile[4]]<-"Quintile 5"
    table(CON_HEI$HEI)
    colnames(CON_HEI)[2]<-"HEI_Score"
    HEI_CON<-CON_HEI[,c("SEQN","HEI_Score","HEI")]
    Covariates_CON1_6<-merge(Covariates_CON1_5,HEI_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                          record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                          record=='Covariates_CON1_5'|record=='Covariates_CON1_6')])
}
{#** Section 8.7 BMI  ####
    BMI_CON<-nhs_read(nhs_tsv("bmx")[1:3],'BMXBMI:BMI',Year = F,codebook = F)
    BMI_CON$BMI_Grade[BMI_CON$BMI<18.5]<-"<18.5"
    BMI_CON$BMI_Grade[BMI_CON$BMI>=18.5&BMI_CON$BMI<25]<-"[18.5,25.0)"
    BMI_CON$BMI_Grade[BMI_CON$BMI>=25&BMI_CON$BMI<30]<-"[25.0-30)"
    BMI_CON$BMI_Grade[BMI_CON$BMI>=30]<-">=30"
    colnames(BMI_CON)[1]<-"SEQN"
    Covariates_CON1_7<-merge(Covariates_CON1_6,BMI_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                            record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                            record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                            record=='Covariates_CON1_7')])
}
{#** Section 8.8 Hypertension  ####
    HTN_exam_CON<-diag_Hypertension(told = F,drug = F,bpx = TRUE,method = c("mean"),years = 1999:2004,join = "left")
    colnames(HTN_exam_CON)<-c("SEQN","HTN_exam_status")
    nhs_tsv("bpq")[1:3]
    HTN_self_CON<-nhs_read(nhs_tsv("bpq")[1:3],'BPQ020','BPQ030','BPQ040A','BPQ040B','BPQ040C',
                           'BPQ040D','BPQ040E','BPQ040F','BPQ043A','BPQ043B','BPQ043C','BPQ043D',Year = F,codebook = F)
    HTN_self_CON$HTN_self_status[HTN_self_CON$BPQ020==2]<-"NO"
    HTN_self_CON$HTN_self_status[HTN_self_CON$BPQ020==1|HTN_self_CON$BPQ030==1|HTN_self_CON$BPQ040A==1|HTN_self_CON$BPQ040B==1|
                                   HTN_self_CON$BPQ040C==1|HTN_self_CON$BPQ040D==1|HTN_self_CON$BPQ040E==1|
                                   HTN_self_CON$BPQ040F==1|HTN_self_CON$BPQ043A==1|HTN_self_CON$BPQ043B==1|
                                   HTN_self_CON$BPQ043C==1|HTN_self_CON$BPQ043D==1]<-"YES"
    colnames(HTN_self_CON)[1]<-"SEQN"
    HTN_exam_CON$HTN_exam_status[HTN_exam_CON$HTN_exam_status=="no"]<-"NO"
    HTN_exam_CON$HTN_exam_status[HTN_exam_CON$HTN_exam_status=="yes"]<-"YES"
    HTN_CON<-merge(HTN_exam_CON,HTN_self_CON,by = "SEQN",all = T)
    HTN_CON$HTN_status[HTN_CON$HTN_exam_status=="NO"|HTN_CON$HTN_self_status=="NO"]<-"NO"
    HTN_CON$HTN_status[HTN_CON$HTN_exam_status=="YES"|HTN_CON$HTN_self_status=="YES"]<-"YES"
    HTN_CON<-HTN_CON[,c("SEQN","HTN_status")]
    table(HTN_CON$HTN_status)
    Covariates_CON1_8<-merge(Covariates_CON1_7,HTN_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                            record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                            record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                            record=='Covariates_CON1_7'|record=='Covariates_CON1_8')])
}
{#** Section 8.9 hyperlipoidemia  ####
    HPL_exam_CON<-HTN_self_CON<-nhs_read(nhs_tsv("lab13|l13")[c(1,4,6)],'LBXTC',Year = F,codebook = F)
    HPL_exam_CON$HPL_exam_status[HPL_exam_CON$LBXTC>=200]<-"YES"
    HPL_exam_CON$HPL_exam_status[HPL_exam_CON$LBXTC<200]<-"NO"
    colnames(HPL_exam_CON)[1]<-"SEQN"
    HPL_self_CON<-nhs_read(nhs_tsv("bpq")[1:3],'BPQ080','BPQ090A','BPQ090B','BPQ090C','BPQ090D',Year = F,codebook = F)
    HPL_self_CON$HPL_self_status<-NULL
    HPL_self_CON$HPL_self_status[HPL_self_CON$BPQ080==2]<-"NO"
    HPL_self_CON$HPL_self_status[HPL_self_CON$BPQ080==1|HPL_self_CON$BPQ090A==1|
                                   HPL_self_CON$BPQ090B==1|HPL_self_CON$BPQ090C==1|
                                   HPL_self_CON$BPQ090D==1]<-"YES"
    colnames(HPL_self_CON)[1]<-"SEQN"
    HPL_CON<-merge(HPL_exam_CON,HPL_self_CON,by = "SEQN",all = T)
    HPL_CON$HPL_status[HPL_CON$HPL_exam_status=="NO"|HPL_CON$HPL_self_status=="NO"]<-"NO"
    HPL_CON$HPL_status[HPL_CON$HPL_exam_status=="YES"|HPL_CON$HPL_self_status=="YES"]<-"YES"
    HPL_CON<-HPL_CON[,c("SEQN","HPL_status")]
    Covariates_CON1_9<-merge(Covariates_CON1_8,HPL_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                            record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                            record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                            record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                            record=='Covariates_CON1_9')])
}
{#** Section 8.10 CVD ####
    #CVD
    CVD_CON<-diag_CVD(years = 1999:2004,join = "left")
    table(CVD_CON$CVD)
    CVD_CON$CVD[CVD_CON$CVD=="no"]<-"NO"
    CVD_CON$CVD[CVD_CON$CVD=="yes"]<-"YES"
    colnames(CVD_CON)[1]<-'SEQN'
    Covariates_CON1_10<-merge(Covariates_CON1_9,CVD_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                            record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                            record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                            record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                            record=='Covariates_CON1_9'|record=='Covariates_CON1_10')])
}
{#** Section 8.11 Diabetes ####
  DM_CON1<-diag_DM(told = T,HbA1c = T,fast_glu = T,OGTT2 = T,
                   rand_glu = T,drug = T,DM1 = F,cat = T,
                   years = 1999:2004,join = "left")
  DM_CON1$DM[DM_CON1$DM=="IFG"]<-'NO'
  DM_CON1$DM[DM_CON1$DM=="no"]<-'NO'
  DM_CON1$DM[DM_CON1$DM=="DM"]<-'YES'
  colnames(DM_CON1)<-c("SEQN","T2D")
  Covariates_CON1_11<-merge(Covariates_CON1_10,DM_CON1,by="SEQN",all.x = T)
  record<-ls()
  rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                          record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                          record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                          record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                          record=='Covariates_CON1_9'|record=='Covariates_CON1_10'|
                          record=='Covariates_CON1_11')])
}
{#** Section 8.12 Caner ####
    #Cancer
    tsv<-nhs_tsv('mcq',years = 1999:2004)
    Cancer_CON<-nhs_read(tsv,'mcq220:Cancer',Year = F)
    Cancer_CON$Cancer[Cancer_CON$Cancer=="Yes"]<-"YES"
    Cancer_CON$Cancer[Cancer_CON$Cancer=="No"]<-"NO"
    colnames(Cancer_CON)[1]<-'SEQN'
    Covariates_CON1_12<-merge(Covariates_CON1_11,Cancer_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                            record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                            record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                            record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                            record=='Covariates_CON1_9'|record=='Covariates_CON1_10'|
                            record=='Covariates_CON1_11'|record=='Covariates_CON1_12')])
}
{#** Section 8.13 occupation  ####
  occupation_CON<-HTN_self_CON<-nhs_read(nhs_tsv("ocq")[1:3],'OCD240:job','OCQ380:status',Year = F,codebook = F)
  table(occupation_CON$job)
  occupation_CON$status[occupation_CON$status==1]<-"Keeping house"
  occupation_CON$status[occupation_CON$status==2]<-"Going to school"
  occupation_CON$status[occupation_CON$status==3]<-"Retired"
  occupation_CON$status[occupation_CON$status==4]<-"Disabled"
  occupation_CON$status[occupation_CON$status==5]<-"Unemployment"
  occupation_CON$status[occupation_CON$status==6]<-"Unemployment"
  occupation_CON$status[occupation_CON$status==7]<-"Unemployment"
  occupation_CON$status[occupation_CON$status==77]<-NA
  occupation_CON$status[occupation_CON$status==99]<-NA
  occupation_CON$job_status[occupation_CON$status=="Retired"]<-"Retired"
  occupation_CON$job_status[occupation_CON$status=="Keeping house"]<-"Unemployment"
  occupation_CON$job_status[occupation_CON$status=="Going to school"]<-"Going to school"
  occupation_CON$job_status[occupation_CON$status=="Something else"]<-"Unemployment"
  occupation_CON$job_status[occupation_CON$status=="Disabled"]<-"Unemployment"
  occupation_CON$job_status[occupation_CON$status=="Unemployment"]<-"Unemployment"
  
  
  occupation_CON$job_status[occupation_CON$job==1]<-"Executive/administrators/managers"
  occupation_CON$job_status[occupation_CON$job==2]<-"Management related"
  occupation_CON$job_status[occupation_CON$job==3]<-"Engineers and scientists"
  occupation_CON$job_status[occupation_CON$job==4]<-"Health diagnosing/assessment/treating occupations"
  occupation_CON$job_status[occupation_CON$job==5]<-"Teachers"
  occupation_CON$job_status[occupation_CON$job==6]<-"Writers/artists/entertainers/athletes"
  occupation_CON$job_status[occupation_CON$job==7]<-"Other professional specialty occupations"
  occupation_CON$job_status[occupation_CON$job==8]<-"Technicians and related support occupations"
  occupation_CON$job_status[occupation_CON$job==9]<-"Supervisors/proprietors/sales occupations"
  occupation_CON$job_status[occupation_CON$job==10]<-"Sales representatives/finance/business/commodities except retail"
  occupation_CON$job_status[occupation_CON$job==11]<-"Sales workers/retail/personal services"
  occupation_CON$job_status[occupation_CON$job==12]<-"Secretaries/stenographers/typists"
  occupation_CON$job_status[occupation_CON$job==13]<-"Information clerks"
  occupation_CON$job_status[occupation_CON$job==14]<-"Records processing occupations"
  occupation_CON$job_status[occupation_CON$job==15]<-"Material recording/scheduling/distributing clerks"
  occupation_CON$job_status[occupation_CON$job==16]<-"Miscellaneous administrative support occupations"
  occupation_CON$job_status[occupation_CON$job==17]<-"Private household occupations"
  occupation_CON$job_status[occupation_CON$job==18]<-"Protective service occupations"
  occupation_CON$job_status[occupation_CON$job==19]<-"Waiters and waitresses"
  occupation_CON$job_status[occupation_CON$job==20]<-"Cooks"
  occupation_CON$job_status[occupation_CON$job==21]<-"Miscellaneous food preparation/service occupations"
  occupation_CON$job_status[occupation_CON$job==22]<-"Health service occupations"
  occupation_CON$job_status[occupation_CON$job==23]<-"Cleaning/building serviceoccupations"
  occupation_CON$job_status[occupation_CON$job==24]<-"Personal service occupations"
  occupation_CON$job_status[occupation_CON$job==25]<-"Farm operators/managers/supervisors"
  occupation_CON$job_status[occupation_CON$job==26]<-"Farm and nursery workers"
  occupation_CON$job_status[occupation_CON$job==27]<-"Related agricultural/forestry/fishing occupations"
  occupation_CON$job_status[occupation_CON$job==28]<-"Vehicle and mobile equipment mechanics and repairers"
  occupation_CON$job_status[occupation_CON$job==29]<-"Other mechanics and repairers"
  occupation_CON$job_status[occupation_CON$job==30]<-"Construction trades"
  occupation_CON$job_status[occupation_CON$job==31]<-"Extractive and precision production occupations"
  occupation_CON$job_status[occupation_CON$job==32]<-"Textile/apparel/furnishings machine operators"
  occupation_CON$job_status[occupation_CON$job==33]<-"Machine operators/assorted materials"
  occupation_CON$job_status[occupation_CON$job==34]<-"Fabricators/assemblers/inspectors/samplers"
  occupation_CON$job_status[occupation_CON$job==35]<-"Motor vehicle operators"
  occupation_CON$job_status[occupation_CON$job==36]<-"Other transportation and material moving occupations"
  occupation_CON$job_status[occupation_CON$job==37]<-"Construction laborers"
  occupation_CON$job_status[occupation_CON$job==38]<-"Laborers/except construction"
  occupation_CON$job_status[occupation_CON$job==39]<-"Freight/stock/material movers/hand"
  occupation_CON$job_status[occupation_CON$job==40]<-"Other handlers/equipment cleaners/handlers"
  occupation_CON$job_status[occupation_CON$job==41]<-"Military occupations"
  
  occupation_CON$SEI[occupation_CON$job_status=="Executive/administrators/managers"|
                       occupation_CON$job_status=="Management related"|  
                       occupation_CON$job_status=="Engineers and scientists"|
                       occupation_CON$job_status=="Health diagnosing/assessment/treating occupations"|
                       occupation_CON$job_status=="Teachers"|
                       occupation_CON$job_status=="Writers/artists/entertainers/athletes"|
                       occupation_CON$job_status=="Other professional specialty occupations"|
                       occupation_CON$job_status=="Technicians and related support occupations"|
                       occupation_CON$job_status=="Supervisors/proprietors/sales occupations"|
                       occupation_CON$job_status=="Sales representatives/finance/business/commodities except retail"|
                       occupation_CON$job_status=="Military occupations"
  ]<-"Upper"
  occupation_CON$SEI[occupation_CON$job_status=="Sales workers/retail/personal services"|
                       occupation_CON$job_status=="Secretaries/stenographers/typists"|
                       occupation_CON$job_status=="Information clerks"|
                       occupation_CON$job_status=="Records processing occupations"|
                       occupation_CON$job_status=="Material recording/scheduling/distributing clerks"|
                       occupation_CON$job_status=="Private household occupations"|
                       occupation_CON$job_status=="Protective service occupations"|
                       occupation_CON$job_status=="Waiters and waitresses"|
                       occupation_CON$job_status=="Miscellaneous administrative support occupations"|
                       occupation_CON$job_status=="Cooks"|
                       occupation_CON$job_status=="Miscellaneous food preparation/service occupations"|
                       occupation_CON$job_status=="Health service occupations"|
                       occupation_CON$job_status=="Cleaning/building serviceoccupations"|
                       occupation_CON$job_status=="Personal service occupations"|
                       occupation_CON$job_status=="Farm operators/managers/supervisors"|
                       occupation_CON$job_status=="Farm and nursery workers"|
                       occupation_CON$job_status=="Related agricultural/forestry/fishing occupations"|
                       occupation_CON$job_status=="Vehicle and mobile equipment mechanics and repairers"|
                       occupation_CON$job_status=="mechanics and repairers"|
                       occupation_CON$job_status=="Other mechanics and repairers"|
                       occupation_CON$job_status=="Construction trades"|
                       occupation_CON$job_status=="Extractive and precision production occupations"|
                       occupation_CON$job_status=="Textile/apparel/furnishings machine operators"|
                       occupation_CON$job_status=="Machine operators/assorted materials"|
                       occupation_CON$job_status=="Fabricators/assemblers/inspectors/samplers"|
                       occupation_CON$job_status=="Motor vehicle operators"|
                       occupation_CON$job_status=="Other transportation and material moving occupations"|
                       occupation_CON$job_status=="Construction laborers"|
                       occupation_CON$job_status=="Laborers/except construction"|
                       occupation_CON$job_status=="Freight/stock/material movers/hand"|
                       occupation_CON$job_status=="Other handlers/equipment cleaners/handlers"]<-"Lower"
  occupation_CON$SEI[occupation_CON$job_status=="Retired"]<-"Lower"
  occupation_CON$SEI[occupation_CON$job_status=="Unemployment"]<-"Unemployment"
  occupation_CON$SEI[occupation_CON$job_status=="Going to school"]<-"Lower"
  occupation_CON$SEI[occupation_CON$job_status=="Unemployment"]<-"Unemployment"
  occupation_CON$SEI[occupation_CON$job_status=="Unemployment"]<-"Unemployment"
  occupation_CON<-occupation_CON[,c("seqn","SEI")]
  colnames(occupation_CON)[1]<-"SEQN"
  Covariates_CON1_13<-merge(Covariates_CON1_12,occupation_CON,by="SEQN",all.x = T)
  record<-ls()
  rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                          record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                          record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                          record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                          record=='Covariates_CON1_9'|record=='Covariates_CON1_10'|
                          record=='Covariates_CON1_11'|record=='Covariates_CON1_12'|
                          record=='Covariates_CON1_13')])
}
Covariates_CON1<-Covariates_CON1_13
record<-ls()
rm(list=record[-which(record=='Covariates_CON1')])
save(Covariates_CON1,file="I:/NHANES study/PD&TYG&CVD/Data/Covariates_CON1.Rdata")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 9. Covariates of NHANES 2009-2014 <<<<< ####
{#** Section 9.1 Age,Gender,Race/ethnicity,Marital status, Education_levels,PIR  ####
    nhs_tsv('demo')[6:8]
    DEMO_DATA<-nhs_read(nhs_tsv('demo')[6:8],"RIDRETH1:Race_ethnicity","RIAGENDR:Gender",
                        "RIDAGEYR:Age","DMDMARTL:Marital_status","DMDEDUC2:Education_levels","INDFMPIR:INDFMPIR")
    DEMO_CON<-DEMO_DATA[,c("seqn","Year","Age","Gender","Race_ethnicity","Marital_status","Education_levels","INDFMPIR")]
    colnames(DEMO_CON)[1]<-"SEQN"
    table(DEMO_CON$Marital_status,useNA = "ifany")
    DEMO_CON$Marital_status[DEMO_CON$Marital_status=="Living with partner"|
                              DEMO_CON$Marital_status=="Married"]<-"Married"
    DEMO_CON$Marital_status[DEMO_CON$Marital_status=="Divorced"|
                              DEMO_CON$Marital_status=="Separated"|
                              DEMO_CON$Marital_status=="Widowed"]<-"Separated"
    DEMO_CON$Marital_status[DEMO_CON$Marital_status=="Never married"]<-"Never_married"
    table(DEMO_CON$Marital_status,useNA = "ifany")
    table(DEMO_CON$Education_levels)
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="Less Than 9th Grade"]<-"Less_than_high_school"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="Less than 9th grade"]<-"Less_than_high_school"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="9-11th Grade (Includes 12th grade with no diploma)"]<-"Less_than_high_school"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="9-11th grade (Includes 12th grade with no diploma)"]<-"Less_than_high_school"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="High School Grad/GED or Equivalent"]<-"High_school_or_Equivalent"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="High school graduate/GED or equivalent"]<-"High_school_or_Equivalent"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="Some College or AA degree"]<-"College_or_above"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="Some college or AA degree"]<-"College_or_above"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="College Graduate or above"]<-"College_or_above"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="College graduate or above"]<-"College_or_above"
    table(DEMO_CON$`Race_ethnicity`)
    DEMO_CON$Race_ethnicity[DEMO_CON$Race_ethnicity=="Other Race - Including Multi-Racial"]<-"Other_Race"
    DEMO_CON$Race_ethnicity[DEMO_CON$Race_ethnicity=="Mexican American"]<-"Hispanic"
    DEMO_CON$Race_ethnicity[DEMO_CON$Race_ethnicity=="Other Hispanic"]<-"Hispanic"
    DEMO_CON$PIR_raw=DEMO_CON$INDFMPIR
    DEMO_CON$PIR[DEMO_CON$INDFMPIR<1.30]<-"<1.30"
    DEMO_CON$PIR[DEMO_CON$INDFMPIR>=1.30&DEMO_CON$INDFMPIR<3]<-"1.30–2.99"
    DEMO_CON$PIR[DEMO_CON$INDFMPIR>=3&DEMO_CON$INDFMPIR<5]<-"3.00–4.99"
    DEMO_CON$PIR[DEMO_CON$INDFMPIR>=5]<-">=5.00"
    DEMO_CON$INDFMPIR<-NULL
    Covariates_CON2_1<-DEMO_CON
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1')])
}
{#** Section 9.2 health insurance  ####
    nhs_tsv('HIQ')[6:8]
    Insurance_DATA<-nhs_read(nhs_tsv('HIQ')[6:8],"HIQ011:insurance","HIQ031A:Private","HIQ031B:Medicare",
                             "HIQ031C:MediGap","HIQ031D:Medicaid","HIQ031E:SCHIP","HIQ031F:military","HIQ031H:state","HIQ031I:government",codebook = F)
    table(Insurance_DATA$insurance)
    Insurance_DATA$Health_insurance[Insurance_DATA$insurance==2]<-"No_insurance"
    Insurance_DATA$Health_insurance[Insurance_DATA$insurance==1&(Insurance_DATA$Medicare==15|Insurance_DATA$MediGap==16|Insurance_DATA$Medicaid==17|
                                                                   Insurance_DATA$SCHIP==18|Insurance_DATA$military==19|
                                                                   Insurance_DATA$state==21|Insurance_DATA$government==22)]<-"Public_insurance"
    Insurance_DATA$Health_insurance[Insurance_DATA$insurance==1&Insurance_DATA$Private==14]<-"Private_insurance"
    
    Insurance_CON<-Insurance_DATA[,c("seqn","Health_insurance")]
    table(Insurance_DATA$Health_insurance)
    colnames(Insurance_CON)[1]<-"SEQN"
    Covariates_CON2_2<-merge(Covariates_CON2_1,Insurance_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2')])
}
{#** Section 9.3 Smoking status  ####
    nhs_tsv('smq')[c(16,19,22)]
    #SMQ020 - Smoked at least 100 cigarettes in life
    #SMQ040 - Do you now smoke cigarettes
    SMQ_DATA<-nhs_read(nhs_tsv('smq')[c(16,19,22)],"SMQ020","SMQ040",Year = F,codebook = F)
    table(SMQ_DATA$SMQ040)
    SMQ_DATA$Smoking_status[SMQ_DATA$SMQ020==1&(SMQ_DATA$SMQ040==1|SMQ_DATA$SMQ040==2)]<-
      "Current_smoker"
    SMQ_DATA$Smoking_status[SMQ_DATA$SMQ020==1&(SMQ_DATA$SMQ040==3)]<-
      "Former_smoker"
    SMQ_DATA$Smoking_status[SMQ_DATA$SMQ020==2]<-
      "Never_smoker"
    SMQ_CON<-SMQ_DATA[,c("seqn","Smoking_status")]
    colnames(SMQ_CON)[1]<-"SEQN"
    Covariates_CON2_3<-merge(Covariates_CON2_2,SMQ_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                          record=='Covariates_CON2_3')])
}
{#** Section 9.4 Drinking status  ####
    nhs_tsv('alq')[c(6,8,9)]
    Drink_DATA<-nhs_read(nhs_tsv('alq')[c(6,8,9)],"ALQ110","ALQ100","ALD100:ALQ100","ALQ101:ALQ100","ALQ120Q","ALQ120U","ALQ130","ALQ140Q","ALQ141Q:ALQ140Q","ALQ140U","ALQ141U:ALQ140U","ALQ150","ALQ151:ALQ150",Year = F,codebook = F)
    #ALQ100 - Had at least 12 alcohol drinks/1 yr?
    #ALQ110 - Had at least 12 alcohol drinks/lifetime?
    #ALQ120Q - How often drink alcohol over past 12 mos
    #ALQ130 - Avg # alcoholic drinks/day -past 12 mos
    #days have 5 or more drinks/past 12 mos
    #Ever have 5 or more drinks every day?
    Drink_DATA$ALQ120Q[Drink_DATA$ALQ120Q==777]<-NA
    Drink_DATA$ALQ120Q[Drink_DATA$ALQ120Q==999]<-NA
    Drink_DATA$ALQ130[Drink_DATA$ALQ130==77]<-NA
    Drink_DATA$ALQ130[Drink_DATA$ALQ130==99]<-NA
    Drink_DATA$ALQ140Q[Drink_DATA$ALQ140Q==777]<-NA
    Drink_DATA$ALQ140Q[Drink_DATA$ALQ140Q==999]<-NA
    Drink_DATA$ALQ150[Drink_DATA$ALQ150==7]<-NA
    Drink_DATA$ALQ150[Drink_DATA$ALQ150==9]<-NA
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==7]<-NA
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==9]<-NA
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==1]<-(365/7)
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==2]<-12
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==3]<-1
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==7]<-NA
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==9]<-NA
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==1]<-(365/7)
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==2]<-12
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==3]<-1
    gender_DATA<-nhs_read(nhs_tsv('demo')[6:8],"RIAGENDR:Gender",Year = F,codebook = T)
    Drink_DATA<-merge(Drink_DATA,gender_DATA,by = "seqn",all.x = T)
    Drink_DATA$Drinking_status[Drink_DATA$ALQ110==2|Drink_DATA$ALQ100==2|Drink_DATA$ALQ150==2]<-"Nondrinker"
    Drink_DATA$Drinking_status[Drink_DATA$ALQ110==1|Drink_DATA$ALQ100==1|Drink_DATA$ALQ140Q>=3]<-"Light/moderate_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130>1&Drink_DATA$Gender=="Female")|
                                   (Drink_DATA$ALQ130>2&Drink_DATA$Gender=="Male")))&Drink_DATA$ALQ120Q>=1]<-
      "Heavier_drinker"
    Drink_DATA$Drinking_status[Drink_DATA$ALQ150==1]<-
      "Heavier_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130<=1)&Drink_DATA$Gender=="Female")|
                                  ((Drink_DATA$ALQ130<=2)&Drink_DATA$Gender=="Male"))&Drink_DATA$ALQ120Q>=1]<-
      "Light/moderate_drinker"
    
    Drink_DATA$Drink_status[Drink_DATA$ALQ110==2|Drink_DATA$ALQ100==2]<-"Nondrinker"
    Drink_DATA$Drink_status[Drink_DATA$ALQ110==1|Drink_DATA$ALQ100==1|(Drink_DATA$ALQ120Q*Drink_DATA$ALQ130>=12)|Drink_DATA$ALQ150==1]<-"Drinker"
    Drink_DATA$Drinking_status[Drink_DATA$Drink_status=="Nondrinker"]<-"Nondrinker"
    Drink_DATA$Drinking_status[Drink_DATA$Drink_status=="Drinker"]<-"Light/moderate_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130>1&Drink_DATA$Gender=="Female")|
                                   (Drink_DATA$ALQ130>2&Drink_DATA$Gender=="Male"))
                                &Drink_DATA$Drink_status=="Drinker")|Drink_DATA$ALQ150==1]<-"Heavier_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130<=1&Drink_DATA$Gender=="Female")|
                                   (Drink_DATA$ALQ130<=2&Drink_DATA$Gender=="Male"))
                                &Drink_DATA$Drink_status=="Drinker")|Drink_DATA$ALQ150==1]<-"Light/moderate_drinker"
    
    
    
    
    Drink_CON<-Drink_DATA[,c("seqn","Drinking_status")]
    colnames(Drink_CON)[1]<-"SEQN"
    Covariates_CON2_4<-merge(Covariates_CON2_3,Drink_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4')])
}
{#** Section 9.5 Physical_activity  ####
    nhs_tsv('paq')[10:12]
    Physical_DATA<-nhs_read(nhs_tsv('paq')[10:12],"PAQ706","PAQ605","PAQ610",
                            "PAQ620","PAQ625","PAQ635","PAQ640","PAQ650",
                            "PAQ655","PAQ665","PAQ670",
                            Year = F,codebook = F)
    # PAD020 - Walked or bicycled over past 30 days
    # PAQ050Q - # times walked or bicycled
    # PAQ050U - Unit of measure (day/week/month)
    # PAD080 - How long per day (minutes)
    # PAQ100 - Tasks around home/yard past 30 days
    # PAD120 - # of times past 30 days
    # PAD160 - How long each time (minutes)
    # PAQ180 - Avg level of physical activity each day
    # PAD200 - Vigorous activity over past 30 days
    # PAD320 - Moderate activity over past 30 days
    # PAD440 - Muscle strengthening activities
    # PAD460 - Number of times past 30 days
    # PAQ480 - Daily hours of TV, video or computer use
    # PAQ500 - Activity comparison last mo - last yr
    # PAQ520 - Compare activity w/others same age
    # PAQ540 - Compare activity with 10 years ago
    # PAQ560 - # time/week you play or exercise hard
    # PAD570 - # of hours watch TV or videos yesterday
    # PAQ580 - # hours use computer/games yesterday
    Physical_DATA[Physical_DATA==77]<-NA
    Physical_DATA[Physical_DATA==99]<-NA
    Physical_DATA$PAQ605[Physical_DATA$PAQ605==7]<-NA
    Physical_DATA$PAQ620[Physical_DATA$PAQ620==7]<-NA
    Physical_DATA$PAQ635[Physical_DATA$PAQ635==7]<-NA
    Physical_DATA$PAQ650[Physical_DATA$PAQ650==7]<-NA
    Physical_DATA$PAQ665[Physical_DATA$PAQ665==7]<-NA
    Physical_DATA$PAQ605[Physical_DATA$PAQ605==9]<-NA
    Physical_DATA$PAQ620[Physical_DATA$PAQ620==9]<-NA
    Physical_DATA$PAQ635[Physical_DATA$PAQ635==9]<-NA
    Physical_DATA$PAQ650[Physical_DATA$PAQ650==9]<-NA
    Physical_DATA$PAQ665[Physical_DATA$PAQ665==9]<-NA
    
    
    Physical_DATA$Physical_status[Physical_DATA$PAQ605==1|Physical_DATA$PAQ620==1|
                                    Physical_DATA$PAQ635==1|Physical_DATA$PAQ650==1|Physical_DATA$PAQ665==1]<-"Insufficient"
    Physical_DATA$Physical_status[
      (Physical_DATA$PAQ605==2|is.na(Physical_DATA$PAQ605))&
        (Physical_DATA$PAQ620==2|is.na(Physical_DATA$PAQ620))&
        (Physical_DATA$PAQ635==2|is.na(Physical_DATA$PAQ635))&
        (Physical_DATA$PAQ650==2|is.na(Physical_DATA$PAQ650))&
        (Physical_DATA$PAQ665==2|is.na(Physical_DATA$PAQ665))]<-"Inactive"
    Physical_DATA$Physical_status[is.na(Physical_DATA$PAQ605)&
                                    is.na(Physical_DATA$PAQ620)&is.na(Physical_DATA$PAQ635)&
                                    is.na(Physical_DATA$PAQ650)&is.na(Physical_DATA$PAQ665)]<-NA
    Physical_DATA$PAQ610[is.na(Physical_DATA$PAQ610)]<-0
    Physical_DATA$PAQ625[is.na(Physical_DATA$PAQ625)]<-0 
    Physical_DATA$PAQ640[is.na(Physical_DATA$PAQ640)]<-0
    Physical_DATA$PAQ655[is.na(Physical_DATA$PAQ655)]<-0
    Physical_DATA$PAQ670[is.na(Physical_DATA$PAQ670)]<-0
    Physical_DATA$Physical_status[(Physical_DATA$PAQ610)+(Physical_DATA$PAQ655)>=3|
                                    (Physical_DATA$PAQ625)+(Physical_DATA$PAQ640)+(Physical_DATA$PAQ670)>=5]<-"Recommended"
    table(Physical_DATA$Physical_status,useNA = 'ifany')
    
    Physical_CON<-Physical_DATA[,c("seqn","Physical_status")]
    colnames(Physical_CON)[1]<-"SEQN"
    Covariates_CON2_5<-merge(Covariates_CON2_4,Physical_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5')])
}
{#** Section 9.6 HEI  ####
    nhs_tsv("drxtot|dr1tot")[6:8]
    data_HEI<-dex_HEI(years = 2009:2014,version = "2015",method = "ssum",dietary = "tot",day = 1)
    HEI_data<-nhs_read(nhs_tsv("drxtot|dr1tot")[6:8],'WTDRD1:weight2',Year = T,codebook = F)
    CON_HEI<-merge(data_HEI[,c("seqn","hei2015_total_score")],HEI_data[,c("seqn","weight2",'Year')],by="seqn",all.x = T)
    CON_HEI$ weight =CON_HEI$ weight/2
    colnames(CON_HEI)[1]<-"SEQN"
    load("I:/NHANES study/PD&TYG&CVD/Data/CON2_baseline.Rdata")
    CON_HEI<-merge(CON_HEI,CON2_baseline[,c("SEQN","ID")],by="SEQN",all.y = T)
    Quantile<-weightedQuantile(CON_HEI$hei2015_total_score,weights = CON_HEI$weight, probs=c(0.2,0.4,0.6,0.8),  na.rm=TRUE)
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[1]]<-"Quintile 1"
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[2]&CON_HEI$hei2015_total_score>Quantile[1]]<-"Quintile 2"
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[3]&CON_HEI$hei2015_total_score>Quantile[2]]<-"Quintile 3"
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[4]&CON_HEI$hei2015_total_score>Quantile[3]]<-"Quintile 4"
    CON_HEI$HEI[CON_HEI$hei2015_total_score>Quantile[4]]<-"Quintile 5"
    colnames(CON_HEI)[2]<-"HEI_Score"
    HEI_CON<-CON_HEI[,c("SEQN","HEI_Score","HEI")]
    Covariates_CON2_6<-merge(Covariates_CON2_5,HEI_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5'|record=='Covariates_CON2_6')])
}
{#** Section 9.7 BMI  ####
    nhs_tsv("bmx")[6:8]
    BMI_CON<-nhs_read(nhs_tsv("bmx")[6:8],'BMXBMI:BMI',Year = F,codebook = F)
    BMI_CON$BMI_Grade[BMI_CON$BMI<18.5]<-"<18.5"
    BMI_CON$BMI_Grade[BMI_CON$BMI>=18.5&BMI_CON$BMI<25]<-"[18.5,25.0)"
    BMI_CON$BMI_Grade[BMI_CON$BMI>=25&BMI_CON$BMI<30]<-"[25.0-30)"
    BMI_CON$BMI_Grade[BMI_CON$BMI>=30]<-">=30"
    colnames(BMI_CON)[1]<-"SEQN"
    Covariates_CON2_7<-merge(Covariates_CON2_6,BMI_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                            record=='Covariates_CON2_7')])
}
{#** Section 9.8 Hypertension  ####
    HTN_exam_CON<-diag_Hypertension(told = F,drug = F,bpx = TRUE,method = c("mean"),years = 2009:2014,join = "left")
    colnames(HTN_exam_CON)<-c("SEQN","HTN_exam_status")
    nhs_tsv("bpq")[6:8]
    HTN_self_CON<-nhs_read(nhs_tsv("bpq")[6:8],'BPQ020','BPQ030','BPQ040A',Year = F,codebook = F)
    HTN_self_CON$HTN_self_status[HTN_self_CON$BPQ020==2]<-"NO"
    HTN_self_CON$HTN_self_status[HTN_self_CON$BPQ020==1|HTN_self_CON$BPQ030==1|HTN_self_CON$BPQ040A==1]<-"YES"
    colnames(HTN_self_CON)[1]<-"SEQN"
    HTN_exam_CON$HTN_exam_status[HTN_exam_CON$HTN_exam_status=="no"]<-"NO"
    HTN_exam_CON$HTN_exam_status[HTN_exam_CON$HTN_exam_status=="yes"]<-"YES"
    HTN_CON<-merge(HTN_exam_CON,HTN_self_CON,by = "SEQN",all = T)
    HTN_CON$HTN_status[HTN_CON$HTN_exam_status=="NO"|HTN_CON$HTN_self_status=="NO"]<-"NO"
    HTN_CON$HTN_status[HTN_CON$HTN_exam_status=="YES"|HTN_CON$HTN_self_status=="YES"]<-"YES"
    HTN_CON<-HTN_CON[,c("SEQN","HTN_status")]
    table(HTN_CON$HTN_status)
    Covariates_CON2_8<-merge(Covariates_CON2_7,HTN_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                            record=='Covariates_CON2_7'|record=='Covariates_CON2_8')])
}
{#** Section 9.9 hyperlipoidemia  ####
    nhs_tsv("TCHOL")[3:5]
    HPL_exam_CON<-HTN_self_CON<-nhs_read(nhs_tsv("TCHOL")[3:5],'LBXTC',Year = F,codebook = F)
    HPL_exam_CON$HPL_exam_status[HPL_exam_CON$LBXTC>=200]<-"YES"
    HPL_exam_CON$HPL_exam_status[HPL_exam_CON$LBXTC<200]<-"NO"
    table(HPL_exam_CON$HPL_exam_status)
    colnames(HPL_exam_CON)[1]<-"SEQN"
    nhs_tsv("bpq")[6:8]
    HPL_self_CON<-nhs_read(nhs_tsv("bpq")[6:8],'BPQ080','BPQ090D',Year = F,codebook = F)
    HPL_self_CON$HPL_self_status<-NULL
    HPL_self_CON$HPL_self_status[HPL_self_CON$BPQ080==2]<-"NO"
    HPL_self_CON$HPL_self_status[HPL_self_CON$BPQ080==1|HPL_self_CON$BPQ090D==1]<-"YES"
    colnames(HPL_self_CON)[1]<-"SEQN"
    table(HPL_self_CON$HPL_self_status)
    HPL_CON<-merge(HPL_exam_CON,HPL_self_CON,by = "SEQN",all = T)
    HPL_CON$HPL_status[HPL_CON$HPL_exam_status=="NO"|HPL_CON$HPL_self_status=="NO"]<-"NO"
    HPL_CON$HPL_status[HPL_CON$HPL_exam_status=="YES"|HPL_CON$HPL_self_status=="YES"]<-"YES"
    table(HPL_CON$HPL_status)
    HPL_CON<-HPL_CON[,c("SEQN","HPL_status")]
    Covariates_CON2_9<-merge(Covariates_CON2_8,HPL_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                            record=='Covariates_CON2_7'|record=='Covariates_CON2_8'|
                            record=='Covariates_CON2_9')])
    
}
{#** Section 9.10 CVD ####
    #CVD
    CVD_CON<-diag_CVD(years = 2009:2014,join = "left")
    table(CVD_CON$CVD)
    CVD_CON$CVD[CVD_CON$CVD=="no"]<-"NO"
    CVD_CON$CVD[CVD_CON$CVD=="yes"]<-"YES"
    colnames(CVD_CON)[1]<-'SEQN'
    Covariates_CON2_10<-merge(Covariates_CON2_9,CVD_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                            record=='Covariates_CON2_7'|record=='Covariates_CON2_8'|
                            record=='Covariates_CON2_9'|record=='Covariates_CON2_10')])
}
{#** Section 9.11 Diabetes ####
  #CVD
  DM_CON2<-diag_DM(told = T,HbA1c = T,fast_glu = T,OGTT2 = T,
                   rand_glu = T,drug = T,DM1 = F,cat = T,
                   years = 2009:2014,join = "left")
  DM_CON2$DM[DM_CON2$DM=="IGT"]<-'NO'
  DM_CON2$DM[DM_CON2$DM=="IFG"]<-'NO'
  DM_CON2$DM[DM_CON2$DM=="no"]<-'NO'
  DM_CON2$DM[DM_CON2$DM=="DM"]<-'YES'
  colnames(DM_CON2)<-c("SEQN","T2D")
  Covariates_CON2_11<-merge(Covariates_CON2_10,DM_CON2,by="SEQN",all.x = T)
  record<-ls()
  rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                          record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                          record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                          record=='Covariates_CON2_7'|record=='Covariates_CON2_8'|
                          record=='Covariates_CON2_9'|record=='Covariates_CON2_10'|
                          record=='Covariates_CON2_11')])
}
{#** Section 9.12 Caner ####
    tsv<-nhs_tsv('mcq',years = 2009:2014)
    Cancer_CON<-nhs_read(tsv,'mcq220:Cancer',Year = F)
    Cancer_CON$Cancer[Cancer_CON$Cancer=="Yes"]<-"YES"
    Cancer_CON$Cancer[Cancer_CON$Cancer=="No"]<-"NO"
    colnames(Cancer_CON)[1]<-'SEQN'
    Covariates_CON2_12<-merge(Covariates_CON2_11,Cancer_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                            record=='Covariates_CON2_7'|record=='Covariates_CON2_8'|
                            record=='Covariates_CON2_9'|record=='Covariates_CON2_10'|
                            record=='Covariates_CON2_11'|record=='Covariates_CON2_12')])
}
{#** Section 9.13 occupation  ####
  occupation_CON<-HTN_self_CON<-nhs_read(nhs_tsv("ocq")[6:8],'OCD241:job','OCQ380:status',Year = F,codebook = F)
  table(occupation_CON$job)
  occupation_CON$status[occupation_CON$status==1]<-"Keeping house"
  occupation_CON$status[occupation_CON$status==2]<-"Going to school"
  occupation_CON$status[occupation_CON$status==3]<-"Retired"
  occupation_CON$status[occupation_CON$status==4]<-"Disabled"
  occupation_CON$status[occupation_CON$status==5]<-"Unemployment"
  occupation_CON$status[occupation_CON$status==6]<-"Unemployment"
  occupation_CON$status[occupation_CON$status==7]<-"Unemployment"
  occupation_CON$status[occupation_CON$status==77]<-NA
  occupation_CON$status[occupation_CON$status==99]<-NA
  occupation_CON$job_status[occupation_CON$status=="Retired"]<-"Retired"
  occupation_CON$job_status[occupation_CON$status=="Keeping house"]<-"Unemployment"
  occupation_CON$job_status[occupation_CON$status=="Going to school"]<-"Going to school"
  occupation_CON$job_status[occupation_CON$status=="Something else"]<-"Unemployment"
  occupation_CON$job_status[occupation_CON$status=="Disabled"]<-"Unemployment"
  occupation_CON$job_status[occupation_CON$status=="Unemployment"]<-"Unemployment"
  
  
  occupation_CON$job_status[occupation_CON$job==1]<-"Management Occupations"
  occupation_CON$job_status[occupation_CON$job==2]<-"Business, Financial Operations Occupations"
  occupation_CON$job_status[occupation_CON$job==3]<-"Computer, Mathematical Occupations"
  occupation_CON$job_status[occupation_CON$job==4]<-"Architecture, Engineering Occupations"
  occupation_CON$job_status[occupation_CON$job==5]<-"Life, Physical, Social Science Occupations"
  occupation_CON$job_status[occupation_CON$job==6]<-"Community, Social Services Occupations"
  occupation_CON$job_status[occupation_CON$job==7]<-"Legal Occupations"
  occupation_CON$job_status[occupation_CON$job==8]<-"Education, Training, Library Occupations"
  occupation_CON$job_status[occupation_CON$job==9]<-"Arts, Design, Entertainment, Sports, Media Occupations"
  occupation_CON$job_status[occupation_CON$job==10]<-"Healthcare Practitioner, Technical Occupations"
  occupation_CON$job_status[occupation_CON$job==11]<-"Healthcare Support Occupations"
  occupation_CON$job_status[occupation_CON$job==12]<-"Protective Service Occupations"
  occupation_CON$job_status[occupation_CON$job==13]<-"Food Preparation, Serving Occupations"
  occupation_CON$job_status[occupation_CON$job==14]<-"Building & Grounds Cleaning, Maintenance Occupations"
  occupation_CON$job_status[occupation_CON$job==15]<-"Personal Care, Service Occupations"
  occupation_CON$job_status[occupation_CON$job==16]<-"Sales & Related Occupations"
  occupation_CON$job_status[occupation_CON$job==17]<-"Office, Administrative Support Occupations"
  occupation_CON$job_status[occupation_CON$job==18]<-"Farming, Fishing, Forestry Occupations"
  occupation_CON$job_status[occupation_CON$job==19]<-"Construction, Extraction Occupations"
  occupation_CON$job_status[occupation_CON$job==20]<-"Installation, Maintenance, Repair Occupations"
  occupation_CON$job_status[occupation_CON$job==21]<-"Production Occupations"
  occupation_CON$job_status[occupation_CON$job==22]<-"Transportation, Material Moving Occupations"
  occupation_CON$job_status[occupation_CON$job==23]<-"Armed Forces"
  
  
  occupation_CON$SEI[occupation_CON$job_status=="Management Occupations"|
                       occupation_CON$job_status=="Business, Financial Operations Occupations"|  
                       occupation_CON$job_status=="Computer, Mathematical Occupations"|
                       occupation_CON$job_status=="Architecture, Engineering Occupations"|
                       occupation_CON$job_status=="Life, Physical, Social Science Occupations"|
                       occupation_CON$job_status=="Community, Social Services Occupations"|
                       occupation_CON$job_status=="Legal Occupations"|
                       occupation_CON$job_status=="Education, Training, Library Occupations"|
                       occupation_CON$job_status=="Arts, Design, Entertainment, Sports, Media Occupations"|
                       occupation_CON$job_status=="Healthcare Practitioner, Technical Occupations"|
                       occupation_CON$job_status=="Office, Administrative Support Occupations"|
                       occupation_CON$job_status=="Armed Forces"
  ]<-"Upper"
  occupation_CON$SEI[occupation_CON$job_status=="Healthcare Support Occupations"|
                       occupation_CON$job_status=="Protective Service Occupations"|
                       occupation_CON$job_status=="Food Preparation, Serving Occupations"|
                       occupation_CON$job_status=="Building & Grounds Cleaning, Maintenance Occupations"|
                       occupation_CON$job_status=="Personal Care, Service Occupations"|
                       occupation_CON$job_status=="Farming, Fishing, Forestry Occupations"|
                       occupation_CON$job_status=="Construction, Extraction Occupations"|
                       occupation_CON$job_status=="Installation, Maintenance, Repair Occupations"|
                       occupation_CON$job_status=="Production Occupations"|
                       occupation_CON$job_status=="Transportation, Material Moving Occupations"|
                       occupation_CON$job_status=="Sales & Related Occupations"
  ]<-"Lower"
  occupation_CON$SEI[occupation_CON$job_status=="Retired"]<-"Lower"
  occupation_CON$SEI[occupation_CON$job_status=="Unemployment"]<-"Unemployment"
  occupation_CON$SEI[occupation_CON$job_status=="Going to school"]<-"Lower"
  occupation_CON$SEI[occupation_CON$job_status=="Unemployment"]<-"Unemployment"
  occupation_CON$SEI[occupation_CON$job_status=="Unemployment"]<-"Unemployment"
  occupation_CON<-occupation_CON[,c("seqn","SEI")]
  colnames(occupation_CON)[1]<-"SEQN"
  Covariates_CON2_13<-merge(Covariates_CON2_12,occupation_CON,by="SEQN",all.x = T)
  record<-ls()
  rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                          record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                          record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                          record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                          record=='Covariates_CON1_9'|record=='Covariates_CON1_10'|
                          record=='Covariates_CON1_11'|record=='Covariates_CON1_12'|
                          record=='Covariates_CON1_13')])
}



Covariates_CON2<-Covariates_CON2_13
record<-ls()
rm(list=record[-which(record=='Covariates_CON2')])
save(Covariates_CON2,file="I:/NHANES study/PD&TYG&CVD/Data/Covariates_CON2.Rdata")


# +++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 10. Weight <<<<< ####
{#*** section 10.2 NHANES 1999-2004  ####
  Weight_data<-nhs_read(nhs_tsv("demo")[1:3],'WTMEC2YR:weight2','WTMEC4YR:weight4',Year = T,codebook = F)
  Weight_data <- Weight_data %>% mutate(weight = ifelse(Year=="1999-2000"|Year=="2001-2002",weight4*2/3,weight2/3))
  colnames(Weight_data)[2]<-"SEQN"
  Weight_data $weight2<-NULL
  Weight_data $weight4<-NULL
  Weight_CON1<-Weight_data[,c("SEQN","sdmvpsu","sdmvstra","weight")]
  save(Weight_CON1,file="I:/NHANES study/PD&TYG&CVD/Data/Weight_CON1.Rdata")
}
{#*** section 10.3 NHANES 2009-2014  ####
  Weight_data<-nhs_read(nhs_tsv("demo")[6:8],'WTMEC2YR:weight2',Year = T,codebook = F)
  library(dplyr)
  Weight_data $weight <- Weight_data $weight2/3
  Weight_data $weight2<-NULL
  colnames(Weight_data)[2]<-"SEQN"
  Weight_CON2<-Weight_data[,c("SEQN","sdmvpsu","sdmvstra","weight")]
  save(Weight_CON2,file="I:/NHANES study/PD&TYG&CVD/Data/Weight_CON2.Rdata")
}


