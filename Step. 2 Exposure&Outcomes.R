# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#>>>>> Section 0. Packages and Functions used <<<<< ####
{#* section 0.1 Packages ####
  library(foreign)
  library(dplyr)
  library(tidyverse)
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
  {#** section 0.2.1 Batch conversion to numeric ####
    colApply <- function(dat, cols = colnames(dat), func = as.numeric) {
      dat[cols] <- lapply(dat[cols], func)
      return(dat)
    }
  }
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 1. Periodontitis diagnosis <<<<< ####
{#* section 1.2 NHANES 1999-2004 Periodontitis diagnosis ####
  {#** section 1.2.1 Data Preparation ####
    #periodontal_data
    OHXPER_9900<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/PD/OHXPERIO.XPT")
    OHXPERL_0102<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/PD/OHXPRL_B.XPT")
    OHXPERU_0102<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/PD/OHXPRU_B.XPT")
    OHXPERL_0304<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/PD/OHXPRL_C.XPT")
    OHXPERU_0304<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/PD/OHXPRU_C.XPT")
    OHXPER_0102<-merge(OHXPERU_0102,OHXPERL_0102[,
                                                 -which(names(OHXPERU_0102)%in%c("OHAEXSTS","OHASCST4"))],by = "SEQN",all=T)
    OHXPER_0304<-merge(OHXPERU_0304,OHXPERL_0304[,
                                                 -which(names(OHXPERU_0304)%in%c("OHAEXSTS","OHASCST4"))],by = "SEQN",all=T)
    #Periodontal examination integrity and patient number
    myvars<-c("SEQN","OHASCST4")
    PDdata_9900<-OHXPER_9900[myvars]
    OHXPER_9900
    PDdata_0102<-OHXPER_0102[myvars]
    PDdata_0304<-OHXPER_0304[myvars]
    #All patients with complete periodontal examination were combined
    PDdata_9904<-rbind(PDdata_9900,PDdata_0102,PDdata_0304)
    #Only records  with complete periodontal data were selected
    PDdata.Complete<-filter(PDdata_9904,OHASCST4 == 1)
    #Complete periodontal records of the patient's periodontal examination results
    #colnames(OHXPER_9900)<-sub('PCM','PCM',colnames(OHXPER_9900),fixed = F)
    OHXPER_9900
   # colnames(OHXPER_9900)<-sub('LAM','LAM',colnames(OHXPER_9900),fixed = F)
    drop<-c("OHXURGIN","OHXULGIN","OHXLLGIN","OHXLRGIN")
    colnames<-setdiff(colnames(OHXPER_9900),c("OHXURGIN","OHXULGIN","OHXLLGIN","OHXLRGIN"))
    colnames
    colnames<-na.omit(sub('OHD..CJ.',NA,colnames,fixed = F))
    colnames
    OHXPER_9904<-rbind(OHXPER_9900[,colnames],OHXPER_0102[,colnames],OHXPER_0304[,colnames])
    OHXPER.Complete<-filter(OHXPER_9904,OHASCST4 == 1)
    OHXPER<- OHXPER.Complete[,-which(names(OHXPERU_0304)%in%c("OHAEXSTS","OHASCST4"))]
    rownames(OHXPER)<-OHXPER$SEQN
    colnames<-colnames(OHXPER)
    colnames
    #Filter pocket depth data
    pocket_depth <- grep('OHD..PC.', colnames, value = T)
    PPD<-OHXPER[,pocket_depth]
    #Filter CAL data
    CAL<-grep('OHD..LA.', colnames, value = T)
    CAL<-OHXPER[,CAL]
    record<-ls()
    rm(list=record[which(record!='PPD'& record!='CAL') ])
  }

  {#** section 1.2.2 CAL diagnosis ####
    #cal=6
    #Set the point with CAL>=6-> 1 and the point CAL<6->0
    CAL[CAL==99]<-NA
    CAL_6<-CAL
    CAL_6[CAL_6<6]<-0
    CAL_6[CAL_6>=6]<-1
    colnames<-colnames(CAL_6)
    tooth<-colnames(CAL)
    toothnumber<-substr(tooth,4,5)
    #Sum the sites of each tooth position CAL>=6. 
    #If it is greater than 1, there are point CAL>=6
    CAL6<-t(CAL_6)
    CAL_TOOTH6<-rowsum(CAL6, toothnumber,na.rm = F)
    #According to the tooth position, the teeth with CAL>=6 are recorded as 1 
    #and those without CAL>=6 are recorded as 0
    CAL_TOOTH6[CAL_TOOTH6<1]<-0
    CAL_TOOTH6[CAL_TOOTH6>=1]<-1
    CAL_TOOTH61<-as.data.frame(t(CAL_TOOTH6))
    #For teeth with CAL>=6, if it is greater than 2, 
    #there are more than 2 teeth with CAL>=6
    CAL_6number<-rowSums(CAL_TOOTH61,na.rm = T)
    CAL_6number<-data.frame(CAL_6number)
    CAL_6number$SEQN<-rownames(CAL_6number)
    #Cal=4
    #Set the point with CAL>=4-> 1 and the point CAL<4->0
    CAL_4<-CAL
    CAL_4[CAL_4<4]<-0
    CAL_4[CAL_4>=4]<-1
    colnames<-colnames(CAL_4)
    #Sum the sites of each tooth position CAL>=4. 
    #If it is greater than 1, there are point CAL>=4
    CAL4<-t(CAL_4)
    CAL_TOOTH4<-rowsum(CAL4, toothnumber,na.rm = F)
    #According to the tooth position, the teeth with CAL>=4are recorded as 1 
    #and those without CAL>=4 are recorded as 0
    CAL_TOOTH4[CAL_TOOTH4<1]<-0
    CAL_TOOTH4[CAL_TOOTH4>=1]<-1
    CAL_TOOTH41<-t(CAL_TOOTH4)
    #For teeth with CAL>=4, if it is greater than 2, 
    #there are more than 2 teeth with CAL>=4
    CAL_4number<-rowSums(CAL_TOOTH41,na.rm = T)
    CAL_4number<-data.frame(CAL_4number)
    CAL_4number$SEQN<-rownames(CAL_4number)
    #Merge the CAL>=4 data into the summary table
    OHXPER.CAL_46<-merge(CAL_6number,CAL_4number,by="SEQN",all = T)
    #Cal=3
    #Set the point with CAL>=3-> 1 and the point CAL<3->0
    CAL_3<-CAL
    CAL_3[CAL_3<3]<-0
    CAL_3[CAL_3>=3]<-1
    colnames<-colnames(CAL_3)
    #Sum the sites of each tooth position CAL>=3. 
    #If it is greater than 1, there are point CAL>=3
    CAL3<-t(CAL_3)
    CAL_TOOTH3<-rowsum(CAL3, toothnumber,na.rm = F)
    #According to the tooth position, the teeth with CAL>=3are recorded as 1 
    #and those without CAL>=3 are recorded as 0
    CAL_TOOTH3[CAL_TOOTH3<1]<-0
    CAL_TOOTH3[CAL_TOOTH3>=1]<-1
    CAL_TOOTH31<-t(CAL_TOOTH3)
    #For teeth with CAL>=3, if it is greater than 2, 
    #there are more than 2 teeth with CAL>=3
    CAL_3number<-rowSums(CAL_TOOTH31,na.rm = T)
    CAL_3number<-data.frame(CAL_3number)
    CAL_3number$SEQN<-rownames(CAL_3number)
    OHXPER.CAL_346<-merge(OHXPER.CAL_46,CAL_3number,by="SEQN",all = T)
  }
  {#** section 1.2.3 PPD diagnosis ####
    #PPD=5
    PPD[PPD==99]<-NA
    PPD_5<-PPD
    #Set the point with ppd>=5-> 1 and the point ppd<5->0
    PPD_5[PPD_5<5]<-0
    PPD_5[PPD_5>=5]<-1
    colnames<-colnames(PPD_5)
    #Sum the sites of each tooth position ppd>=5. 
    #If it is greater than 1, there are point ppd>=5
    tooth_PPD<-colnames(PPD)
    toothnumber_PPD<-substr(tooth_PPD,4,5)
    PPD51<-t(PPD_5)
    PPD_TOOTH5<-rowsum(PPD51, toothnumber_PPD,na.rm = F)
    #According to the tooth position, the teeth with PPD>=5are recorded as 1 
    #and those without PPD>=5 are recorded as 0
    PPD_TOOTH5[PPD_TOOTH5<1]<-0
    PPD_TOOTH5[PPD_TOOTH5>=1]<-1
    PPD_TOOTH51<-t(PPD_TOOTH5)
    #For teeth with PPD>=5, if it is greater than 2, 
    #there are more than 2 teeth with PPD>=5
    PPD_5number<-rowSums(PPD_TOOTH51,na.rm = T)
    PPD_5number<-data.frame(PPD_5number)
    #Merge the PPD>=5 data into the summary table
    PPD_5number$SEQN<-rownames(PPD_5number)
    OHXPER.CAL_346.PPD_5<-merge(OHXPER.CAL_346,PPD_5number,by="SEQN",all = T)
    #PPD=4
    #Set the point with ppd>=4-> 1 and the point ppd<4->0
    PPD_4<-PPD
    PPD_4[PPD_4<4]<-0
    PPD_4[PPD_4>=4]<-1
    #Sum the sites of each tooth position ppd>=4. 
    #If it is greater than 1, there are point ppd>=4
    PPD41<-t(PPD_4)
    PPD_TOOTH4<-rowsum(PPD41, toothnumber_PPD,na.rm = F)
    #According to the tooth position, the teeth with PPD>=4are recorded as 1 
    #and those without PPD>=4 are recorded as 0
    PPD_TOOTH4[PPD_TOOTH4<1]<-0
    PPD_TOOTH4[PPD_TOOTH4>=1]<-1
    PPD_TOOTH41<-t(PPD_TOOTH4)
    #For teeth with PPD>=4, if it is greater than 2, 
    #there are more than 2 teeth with PPD>=4
    PPD_4number<-rowSums(PPD_TOOTH41,na.rm = T)
    PPD_4number<-data.frame(PPD_4number)
    #Merge the PPD>=4 data into the summary table
    PPD_4number$SEQN<-rownames(PPD_4number)
    CAL_346.PPD_45<-merge(OHXPER.CAL_346.PPD_5,PPD_4number,by="SEQN",all = T)
    OXPER.ALL<-CAL_346.PPD_45
    
  }
  {#** section 1.2.4 CAL mean ####
    CAL_mean=as.data.frame(rowMeans(CAL,na.rm = T))
    colnames(CAL_mean)<-"CAL_mean"
    colnames(CAL_mean)
    CAL_mean[CAL_mean=="NaN"]<-NA
    CAL_mean$SEQN<-rownames(CAL_mean)
  }
  
  {#** section 1.2.5 PPD mean ####
    PPD_mean=as.data.frame(rowMeans(PPD,na.rm = T))
    colnames(PPD_mean)<-"PPD_mean"
    colnames(PPD_mean)
    PPD_mean[PPD_mean=="NaN"]<-NA
    PPD_mean$SEQN<-rownames(PPD_mean)
  }
  record<-ls()
  rm(list=record[which(record!='OXPER.ALL'&record!='CAL_mean'&record!='PPD_mean'&
                         record!='CAL'&record!='PPD')])
  {#** section 1.2.6 data Collation ####
    CAL_mean$SEQN<-as.numeric(CAL_mean$SEQN)
    PPD_mean$SEQN<-as.numeric(PPD_mean$SEQN)
    OXPER.ALL$SEQN<-as.numeric(OXPER.ALL$SEQN)
    OXPER.CAL<-merge(OXPER.ALL,CAL_mean,by="SEQN",all.x = T)
    OXPER.PPD<-merge(OXPER.CAL,PPD_mean,by="SEQN",all.x = T)
    OXPER.ALL<-na.omit(OXPER.PPD)
    #Graded periodontal disease and normal
    #severe periodontitis 
    OXPER.ALL$severe[OXPER.ALL$CAL_6number>=1&OXPER.ALL$PPD_5number>=1]<-"severe"
    table(OXPER.ALL$severe)
    OXPER.ALL$severe[is.na(OXPER.ALL$severe)] <-"no severe"
    #moderate periodontitis
    OXPER.ALL$moderate[OXPER.ALL$CAL_4number>=1|OXPER.ALL$PPD_5number>=1]<-"moderate"
    OXPER.ALL$moderate[is.na(OXPER.ALL$moderate)] <-"no moderate"
    #mild periodontitis
    OXPER.ALL$mild[(OXPER.ALL$CAL_3number>=1&OXPER.ALL$PPD_4number>=1)]<-"mild"
    OXPER.ALL$mild[is.na(OXPER.ALL$mild)] <-"no Mild"
    #only severe
    OXPER.ALL$Periodontitis_diagnosis[OXPER.ALL$severe=="severe"]<-"severe"
    #moderat not severe
    OXPER.ALL$Periodontitis_diagnosis[OXPER.ALL$moderate=="moderate"& OXPER.ALL$severe=="no severe"]<-"moderate"
    #Mild not moderate
    OXPER.ALL$Periodontitis_diagnosis[OXPER.ALL$moderate=="no moderate"& OXPER.ALL$mild=="mild"]<-"mild"
    #normal
    OXPER.ALL$Periodontitis_diagnosis[is.na(OXPER.ALL$Periodontitis_diagnosis)] <-"normal"
    #?
    OXPER.ALL$PD_diagnosis[OXPER.ALL$CAL_4number>=1|OXPER.ALL$PPD_5number>=1]<-"Moderate/Severe periodontitis"
    OXPER.ALL$PD_diagnosis[OXPER.ALL$CAL_4number<1&OXPER.ALL$PPD_5number<1]<-"No/Mild periodontitis"
    table(OXPER.ALL$PD_diagnosis,OXPER.ALL$Periodontitis_diagnosis)
    OXPER.diagnosis<-OXPER.ALL[c("SEQN","CAL_mean","PPD_mean","PD_diagnosis","Periodontitis_diagnosis")]
    record<-ls()
    rm(list=record[which(record!='OXPER.diagnosis')])
  }
  {#** section 1.2.7 data selction age>=30 ####
    
    age<-db_demo(years = 1999:2004,ageyr=T)
    age$ageyr[age$ageyr<30]<-NA
    age<-na.omit(age)
    colnames(age)[1]<-"SEQN"
    PD_dia_30<-merge(OXPER.diagnosis,age,by="SEQN",all.x = T)
    PD_CON1<-na.omit(PD_dia_30)
    PD_CON1$chort<-"NHANES_CON1"
    PD_CON1$ID<-paste(PD_CON1$chort,PD_CON1$SEQN,sep="_")
    PD_CON1<-PD_CON1[,c("SEQN","ID","chort","CAL_mean","PPD_mean","PD_diagnosis","Periodontitis_diagnosis")]
    record<-ls()
    rm(list=record[which(record!='PD_CON1')])
    #table(PD_CON1$Periodontitis_diagnosis)
    save(PD_CON1,file="I:/NHANES study/PD&TYG&CVD/Data/PD_CON1.Rdata")
  }
}
{#* section 1.3 NHANES 2009-2014 Periodontitis diagnosis ####
  {#** section 1.3.1 Data Preparation ####
    #periodontal_data
    
    setwd("I:/NHANES study/PD&TYG&CVD/Data/PD")
    OHXPER_0910<-read.xport("OHXPER_F.XPT")
    OHXPER_1112<-read.xport("OHXPER_G.XPT")
    OHXPER_1314<-read.xport("OHXPER_H.XPT")
    
    #Periodontal examination integrity and patient number
    myvars<-c("SEQN","OHDPDSTS")
    PDdata_0910<-OHXPER_0910[myvars]
    PDdata_1112<-OHXPER_1112[myvars]
    PDdata_1314<-OHXPER_1314[myvars]
    #All patients with complete periodontal examination were combined
    PDdata_0914<-rbind(PDdata_0910,PDdata_1112,PDdata_1314)
    #Only records  with complete periodontal data were selected
    PDdata.Complete<-filter(PDdata_0914,OHDPDSTS == 1)
    #Complete periodontal records of the patient's periodontal examination results
    OHXPER_0914<-rbind(OHXPER_0910,OHXPER_1112,OHXPER_1314)
    OHXPER.Complete<-filter(OHXPER_0914,OHDPDSTS == 1)
    
    colnames<-colnames(OHXPER.Complete)
    colnames<-sub('OHX..CJ.',NA,colnames,fixed = F)
    #ɸѡPPD????
    colnames<-sub('OHX..PCL',NA,colnames,fixed = F)#舌中
    colnames<-sub('OHX..PCP',NA,colnames,fixed = F)#远舌 
    colnames<-sub('OHX..PCA',NA,colnames,fixed = F)#近舌 
    colnames<-sub('OHX..PCD',NA,colnames,fixed = F)#颊中 
    #ɸѡcal????
    colnames<-sub('OHX..LAL',NA,colnames,fixed = F)
    colnames<-sub('OHX..LAP',NA,colnames,fixed = F)
    colnames<-sub('OHX..LAA',NA,colnames,fixed = F)
    colnames<-sub('OHX..LAD',NA,colnames,fixed = F)
    colnames
    colnames<-na.omit(colnames)
    OHXPER_9904<-rbind(OHXPER_0910[,colnames],OHXPER_1112[,colnames],OHXPER_1314[,colnames])
    OHXPER.Complete<-filter(OHXPER_9904,OHDPDSTS == 1)
    OHXPER<- OHXPER.Complete[,-c(2,3,4)]
    rownames(OHXPER)<-OHXPER$SEQN
    colnames<-colnames(OHXPER)
    colnames
    #Filter pocket depth data
    pocket_depth <- grep('OHX..PC.', colnames, value = T)
    PPD<-OHXPER[,pocket_depth]
    #Filter CAL data
    CAL<-grep('OHX..LA.', colnames, value = T)
    CAL<-OHXPER[,CAL]
    record<-ls()
    rm(list=record[which(record!='PPD'& record!='CAL') ])
  }
  {#** section 1.3.2 Random screening####
    #Converting 99 to NA values
    CAL[CAL==99]<-NA
    #Set random number
    set.seed(1234)
    Random<-sample(1:4, length(rownames(CAL)),replace = T)
    
    CAL_full=cbind(Random,CAL)   
    CAL_random <- matrix(NA,ncol=length(colnames(CAL)),nrow=length(rownames(CAL)))
    CAL_random <-as.data.frame(CAL_random)
    colnames(CAL_full)
    colnames(CAL_random)=colnames(CAL_full)[2:length(colnames(CAL_full))]
    rownames(CAL_random)=rownames(CAL_full)
    length(colnames(CAL))
    colnames(CAL_full)
    colnames(CAL_full)[2:6]
    for(i in 1:length(rownames(CAL)))
    {
      if(CAL_full[i,"Random"]=="1"){
        CAL_random[i,c("OHX02LAM","OHX02LAS","OHX03LAM","OHX03LAS",
                       "OHX04LAM","OHX04LAS","OHX05LAM","OHX05LAS",
                       "OHX06LAM","OHX06LAS","OHX07LAM","OHX07LAS",
                       "OHX08LAM","OHX08LAS",
                       "OHX18LAM","OHX18LAS","OHX19LAM","OHX19LAS",
                       "OHX20LAM","OHX20LAS","OHX21LAM","OHX21LAS",
                       "OHX22LAM","OHX22LAS","OHX23LAM","OHX23LAS",
                       "OHX24LAM","OHX24LAS")]= 
          CAL_full[i,c("OHX02LAM","OHX02LAS","OHX03LAM","OHX03LAS",
                       "OHX04LAM","OHX04LAS","OHX05LAM","OHX05LAS",
                       "OHX06LAM","OHX06LAS","OHX07LAM","OHX07LAS",
                       "OHX08LAM","OHX08LAS",
                       "OHX18LAM","OHX18LAS","OHX19LAM","OHX19LAS",
                       "OHX20LAM","OHX20LAS","OHX21LAM","OHX21LAS",
                       "OHX22LAM","OHX22LAS","OHX23LAM","OHX23LAS",
                       "OHX24LAM","OHX24LAS")]
      }else if (CAL_full[i,"Random"]=="2"){
        CAL_random[i,c("OHX02LAM","OHX02LAS","OHX03LAM","OHX03LAS",
                       "OHX04LAM","OHX04LAS","OHX05LAM","OHX05LAS",
                       "OHX06LAM","OHX06LAS","OHX07LAM","OHX07LAS",
                       "OHX08LAM","OHX08LAS",
                       "OHX25LAM","OHX25LAS","OHX26LAM","OHX26LAS",
                       "OHX27LAM","OHX27LAS","OHX28LAM","OHX28LAS",
                       "OHX29LAM","OHX29LAS","OHX30LAM","OHX30LAS",
                       "OHX31LAM","OHX31LAS")]= 
          CAL_full[i,c("OHX02LAM","OHX02LAS","OHX03LAM","OHX03LAS",
                       "OHX04LAM","OHX04LAS","OHX05LAM","OHX05LAS",
                       "OHX06LAM","OHX06LAS","OHX07LAM","OHX07LAS",
                       "OHX08LAM","OHX08LAS",
                       "OHX25LAM","OHX25LAS","OHX26LAM","OHX26LAS",
                       "OHX27LAM","OHX27LAS","OHX28LAM","OHX28LAS",
                       "OHX29LAM","OHX29LAS","OHX30LAM","OHX30LAS",
                       "OHX31LAM","OHX31LAS")]
      } else if (CAL_full[i,"Random"]=="3"){
        CAL_random[i,c("OHX09LAM","OHX09LAS","OHX10LAM","OHX10LAS",
                       "OHX11LAM","OHX11LAS","OHX12LAM","OHX12LAS",
                       "OHX13LAM","OHX13LAS","OHX14LAM","OHX14LAS",
                       "OHX15LAM","OHX15LAS",
                       "OHX18LAM","OHX18LAS","OHX19LAM","OHX19LAS",
                       "OHX20LAM","OHX20LAS","OHX21LAM","OHX21LAS",
                       "OHX22LAM","OHX22LAS","OHX23LAM","OHX23LAS",
                       "OHX24LAM","OHX24LAS")]= 
          CAL_full[i,c("OHX09LAM","OHX09LAS","OHX10LAM","OHX10LAS",
                       "OHX11LAM","OHX11LAS","OHX12LAM","OHX12LAS",
                       "OHX13LAM","OHX13LAS","OHX14LAM","OHX14LAS",
                       "OHX15LAM","OHX15LAS",
                       "OHX18LAM","OHX18LAS","OHX19LAM","OHX19LAS",
                       "OHX20LAM","OHX20LAS","OHX21LAM","OHX21LAS",
                       "OHX22LAM","OHX22LAS","OHX23LAM","OHX23LAS",
                       "OHX24LAM","OHX24LAS")]
      } else if (CAL_full[i,"Random"]=="4"){
        CAL_random[i,c("OHX09LAM","OHX09LAS","OHX10LAM","OHX10LAS",
                       "OHX11LAM","OHX11LAS","OHX12LAM","OHX12LAS",
                       "OHX13LAM","OHX13LAS","OHX14LAM","OHX14LAS",
                       "OHX15LAM","OHX15LAS",
                       "OHX25LAM","OHX25LAS","OHX26LAM","OHX26LAS",
                       "OHX27LAM","OHX27LAS","OHX28LAM","OHX28LAS",
                       "OHX29LAM","OHX29LAS","OHX30LAM","OHX30LAS",
                       "OHX31LAM","OHX31LAS")]= 
          CAL_full[i,c("OHX09LAM","OHX09LAS","OHX10LAM","OHX10LAS",
                       "OHX11LAM","OHX11LAS","OHX12LAM","OHX12LAS",
                       "OHX13LAM","OHX13LAS","OHX14LAM","OHX14LAS",
                       "OHX15LAM","OHX15LAS",
                       "OHX25LAM","OHX25LAS","OHX26LAM","OHX26LAS",
                       "OHX27LAM","OHX27LAS","OHX28LAM","OHX28LAS",
                       "OHX29LAM","OHX29LAS","OHX30LAM","OHX30LAS",
                       "OHX31LAM","OHX31LAS")]
      }
    }
    CAL=CAL_random
    PPD[PPD==99]<-NA
    PPD_full=cbind(Random,PPD)   
    PPD_random <- matrix(NA,ncol=length(colnames(PPD)),nrow=length(rownames(PPD)))
    PPD_random <-as.data.frame(PPD_random)
    colnames(PPD_full)
    colnames(PPD_random)=colnames(PPD_full)[2:length(colnames(PPD_full))]
    rownames(PPD_random)=rownames(PPD_full)
    length(colnames(PPD))
    colnames(PPD_full)
    colnames(PPD_full)[2:6]
    for(i in 1:length(rownames(PPD)))
    {
      if(PPD_full[i,"Random"]=="1"){
        PPD_random[i,c("OHX02PCM","OHX02PCS","OHX03PCM","OHX03PCS",
                       "OHX04PCM","OHX04PCS","OHX05PCM","OHX05PCS",
                       "OHX06PCM","OHX06PCS","OHX07PCM","OHX07PCS",
                       "OHX08PCM","OHX08PCS",
                       "OHX18PCM","OHX18PCS","OHX19PCM","OHX19PCS",
                       "OHX20PCM","OHX20PCS","OHX21PCM","OHX21PCS",
                       "OHX22PCM","OHX22PCS","OHX23PCM","OHX23PCS",
                       "OHX24PCM","OHX24PCS")]= 
          PPD_full[i,c("OHX02PCM","OHX02PCS","OHX03PCM","OHX03PCS",
                       "OHX04PCM","OHX04PCS","OHX05PCM","OHX05PCS",
                       "OHX06PCM","OHX06PCS","OHX07PCM","OHX07PCS",
                       "OHX08PCM","OHX08PCS",
                       "OHX18PCM","OHX18PCS","OHX19PCM","OHX19PCS",
                       "OHX20PCM","OHX20PCS","OHX21PCM","OHX21PCS",
                       "OHX22PCM","OHX22PCS","OHX23PCM","OHX23PCS",
                       "OHX24PCM","OHX24PCS")]
      }else if (PPD_full[i,"Random"]=="2"){
        PPD_random[i,c("OHX02PCM","OHX02PCS","OHX03PCM","OHX03PCS",
                       "OHX04PCM","OHX04PCS","OHX05PCM","OHX05PCS",
                       "OHX06PCM","OHX06PCS","OHX07PCM","OHX07PCS",
                       "OHX08PCM","OHX08PCS",
                       "OHX25PCM","OHX25PCS","OHX26PCM","OHX26PCS",
                       "OHX27PCM","OHX27PCS","OHX28PCM","OHX28PCS",
                       "OHX29PCM","OHX29PCS","OHX30PCM","OHX30PCS",
                       "OHX31PCM","OHX31PCS")]= 
          PPD_full[i,c("OHX02PCM","OHX02PCS","OHX03PCM","OHX03PCS",
                       "OHX04PCM","OHX04PCS","OHX05PCM","OHX05PCS",
                       "OHX06PCM","OHX06PCS","OHX07PCM","OHX07PCS",
                       "OHX08PCM","OHX08PCS",
                       "OHX25PCM","OHX25PCS","OHX26PCM","OHX26PCS",
                       "OHX27PCM","OHX27PCS","OHX28PCM","OHX28PCS",
                       "OHX29PCM","OHX29PCS","OHX30PCM","OHX30PCS",
                       "OHX31PCM","OHX31PCS")]
      } else if (PPD_full[i,"Random"]=="3"){
        PPD_random[i,c("OHX09PCM","OHX09PCS","OHX10PCM","OHX10PCS",
                       "OHX11PCM","OHX11PCS","OHX12PCM","OHX12PCS",
                       "OHX13PCM","OHX13PCS","OHX14PCM","OHX14PCS",
                       "OHX15PCM","OHX15PCS",
                       "OHX18PCM","OHX18PCS","OHX19PCM","OHX19PCS",
                       "OHX20PCM","OHX20PCS","OHX21PCM","OHX21PCS",
                       "OHX22PCM","OHX22PCS","OHX23PCM","OHX23PCS",
                       "OHX24PCM","OHX24PCS")]= 
          PPD_full[i,c("OHX09PCM","OHX09PCS","OHX10PCM","OHX10PCS",
                       "OHX11PCM","OHX11PCS","OHX12PCM","OHX12PCS",
                       "OHX13PCM","OHX13PCS","OHX14PCM","OHX14PCS",
                       "OHX15PCM","OHX15PCS",
                       "OHX18PCM","OHX18PCS","OHX19PCM","OHX19PCS",
                       "OHX20PCM","OHX20PCS","OHX21PCM","OHX21PCS",
                       "OHX22PCM","OHX22PCS","OHX23PCM","OHX23PCS",
                       "OHX24PCM","OHX24PCS")]
      } else if (PPD_full[i,"Random"]=="4"){
        PPD_random[i,c("OHX09PCM","OHX09PCS","OHX10PCM","OHX10PCS",
                       "OHX11PCM","OHX11PCS","OHX12PCM","OHX12PCS",
                       "OHX13PCM","OHX13PCS","OHX14PCM","OHX14PCS",
                       "OHX15PCM","OHX15PCS",
                       "OHX25PCM","OHX25PCS","OHX26PCM","OHX26PCS",
                       "OHX27PCM","OHX27PCS","OHX28PCM","OHX28PCS",
                       "OHX29PCM","OHX29PCS","OHX30PCM","OHX30PCS",
                       "OHX31PCM","OHX31PCS")]= 
          PPD_full[i,c("OHX09PCM","OHX09PCS","OHX10PCM","OHX10PCS",
                       "OHX11PCM","OHX11PCS","OHX12PCM","OHX12PCS",
                       "OHX13PCM","OHX13PCS","OHX14PCM","OHX14PCS",
                       "OHX15PCM","OHX15PCS",
                       "OHX25PCM","OHX25PCS","OHX26PCM","OHX26PCS",
                       "OHX27PCM","OHX27PCS","OHX28PCM","OHX28PCS",
                       "OHX29PCM","OHX29PCS","OHX30PCM","OHX30PCS",
                       "OHX31PCM","OHX31PCS")]
      }
    }
    PPD<-PPD_random
  }
  {#** section 1.3.3 CAL diagnosis ####
    #cal=6
    #Set the point with CAL>=6-> 1 and the point CAL<6->0
    CAL[CAL==99]<-NA
    CAL_6<-CAL
    CAL_6[CAL_6<6]<-0
    CAL_6[CAL_6>=6]<-1
    colnames<-colnames(CAL_6)
    tooth<-colnames(CAL)
    toothnumber<-substr(tooth,4,5)
    #Sum the sites of each tooth position CAL>=6. 
    #If it is greater than 1, there are point CAL>=6
    CAL6<-t(CAL_6)
    CAL_TOOTH6<-rowsum(CAL6, toothnumber,na.rm = F)
    #According to the tooth position, the teeth with CAL>=6 are recorded as 1 
    #and those without CAL>=6 are recorded as 0
    CAL_TOOTH6[CAL_TOOTH6<1]<-0
    CAL_TOOTH6[CAL_TOOTH6>=1]<-1
    CAL_TOOTH61<-as.data.frame(t(CAL_TOOTH6))
    #For teeth with CAL>=6, if it is greater than 2, 
    #there are more than 2 teeth with CAL>=6
    CAL_6number<-rowSums(CAL_TOOTH61,na.rm = T)
    CAL_6number<-data.frame(CAL_6number)
    CAL_6number$SEQN<-rownames(CAL_6number)
    #Cal=4
    #Set the point with CAL>=4-> 1 and the point CAL<4->0
    CAL_4<-CAL
    CAL_4[CAL_4<4]<-0
    CAL_4[CAL_4>=4]<-1
    colnames<-colnames(CAL_4)
    #Sum the sites of each tooth position CAL>=4. 
    #If it is greater than 1, there are point CAL>=4
    CAL4<-t(CAL_4)
    CAL_TOOTH4<-rowsum(CAL4, toothnumber,na.rm = F)
    #According to the tooth position, the teeth with CAL>=4are recorded as 1 
    #and those without CAL>=4 are recorded as 0
    CAL_TOOTH4[CAL_TOOTH4<1]<-0
    CAL_TOOTH4[CAL_TOOTH4>=1]<-1
    CAL_TOOTH41<-t(CAL_TOOTH4)
    #For teeth with CAL>=4, if it is greater than 2, 
    #there are more than 2 teeth with CAL>=4
    CAL_4number<-rowSums(CAL_TOOTH41,na.rm = T)
    CAL_4number<-data.frame(CAL_4number)
    CAL_4number$SEQN<-rownames(CAL_4number)
    #Merge the CAL>=4 data into the summary table
    OHXPER.CAL_46<-merge(CAL_6number,CAL_4number,by="SEQN",all = T)
    #Cal=3
    #Set the point with CAL>=3-> 1 and the point CAL<3->0
    CAL_3<-CAL
    CAL_3[CAL_3<3]<-0
    CAL_3[CAL_3>=3]<-1
    colnames<-colnames(CAL_3)
    #Sum the sites of each tooth position CAL>=3. 
    #If it is greater than 1, there are point CAL>=3
    CAL3<-t(CAL_3)
    CAL_TOOTH3<-rowsum(CAL3, toothnumber,na.rm = F)
    #According to the tooth position, the teeth with CAL>=3are recorded as 1 
    #and those without CAL>=3 are recorded as 0
    CAL_TOOTH3[CAL_TOOTH3<1]<-0
    CAL_TOOTH3[CAL_TOOTH3>=1]<-1
    CAL_TOOTH31<-t(CAL_TOOTH3)
    #For teeth with CAL>=3, if it is greater than 2, 
    #there are more than 2 teeth with CAL>=3
    CAL_3number<-rowSums(CAL_TOOTH31,na.rm = T)
    CAL_3number<-data.frame(CAL_3number)
    CAL_3number$SEQN<-rownames(CAL_3number)
    OHXPER.CAL_346<-merge(OHXPER.CAL_46,CAL_3number,by="SEQN",all = T)
  }
  {#** section 1.3.4 PPD diagnosis ####
    #PPD=5
    PPD[PPD==99]<-NA
    PPD_5<-PPD
    #Set the point with ppd>=5-> 1 and the point ppd<5->0
    PPD_5[PPD_5<5]<-0
    PPD_5[PPD_5>=5]<-1
    colnames<-colnames(PPD_5)
    #Sum the sites of each tooth position ppd>=5. 
    #If it is greater than 1, there are point ppd>=5
    tooth_PPD<-colnames(PPD)
    toothnumber_PPD<-substr(tooth_PPD,4,5)
    PPD51<-t(PPD_5)
    PPD_TOOTH5<-rowsum(PPD51, toothnumber_PPD,na.rm = F)
    #According to the tooth position, the teeth with PPD>=5are recorded as 1 
    #and those without PPD>=5 are recorded as 0
    PPD_TOOTH5[PPD_TOOTH5<1]<-0
    PPD_TOOTH5[PPD_TOOTH5>=1]<-1
    PPD_TOOTH51<-t(PPD_TOOTH5)
    #For teeth with PPD>=5, if it is greater than 2, 
    #there are more than 2 teeth with PPD>=5
    PPD_5number<-rowSums(PPD_TOOTH51,na.rm = T)
    PPD_5number<-data.frame(PPD_5number)
    #Merge the PPD>=5 data into the summary table
    PPD_5number$SEQN<-rownames(PPD_5number)
    OHXPER.CAL_346.PPD_5<-merge(OHXPER.CAL_346,PPD_5number,by="SEQN",all = T)
    #PPD=4
    #Set the point with ppd>=4-> 1 and the point ppd<4->0
    PPD_4<-PPD
    PPD_4[PPD_4<4]<-0
    PPD_4[PPD_4>=4]<-1
    #Sum the sites of each tooth position ppd>=4. 
    #If it is greater than 1, there are point ppd>=4
    PPD41<-t(PPD_4)
    PPD_TOOTH4<-rowsum(PPD41, toothnumber_PPD,na.rm = F)
    #According to the tooth position, the teeth with PPD>=4are recorded as 1 
    #and those without PPD>=4 are recorded as 0
    PPD_TOOTH4[PPD_TOOTH4<1]<-0
    PPD_TOOTH4[PPD_TOOTH4>=1]<-1
    PPD_TOOTH41<-t(PPD_TOOTH4)
    #For teeth with PPD>=4, if it is greater than 2, 
    #there are more than 2 teeth with PPD>=4
    PPD_4number<-rowSums(PPD_TOOTH41,na.rm = T)
    PPD_4number<-data.frame(PPD_4number)
    #Merge the PPD>=4 data into the summary table
    PPD_4number$SEQN<-rownames(PPD_4number)
    CAL_346.PPD_45<-merge(OHXPER.CAL_346.PPD_5,PPD_4number,by="SEQN",all = T)
    OXPER.ALL<-CAL_346.PPD_45
    
  }
  {#** section 1.3.5 CAL mean ####
    CAL_mean=as.data.frame(rowMeans(CAL,na.rm = T))
    colnames(CAL_mean)<-"CAL_mean"
    colnames(CAL_mean)
    CAL_mean[CAL_mean=="NaN"]<-NA
    CAL_mean$SEQN<-rownames(CAL_mean)
  }
  
  {#** section 1.3.6 PPD mean ####
    PPD_mean=as.data.frame(rowMeans(PPD,na.rm = T))
    colnames(PPD_mean)<-"PPD_mean"
    colnames(PPD_mean)
    PPD_mean[PPD_mean=="NaN"]<-NA
    PPD_mean$SEQN<-rownames(PPD_mean)
    record<-ls()
    rm(list=record[which(record!='OXPER.ALL'&record!='CAL_mean'&record!='PPD_mean'&
                           record!='CAL'&record!='PPD')])
  }
  
  {#** section 1.3.7 data Collation ####
  CAL_mean$SEQN<-as.numeric(CAL_mean$SEQN)
  PPD_mean$SEQN<-as.numeric(PPD_mean$SEQN)
  OXPER.ALL$SEQN<-as.numeric(OXPER.ALL$SEQN)
  OXPER.CAL<-merge(OXPER.ALL,CAL_mean,by="SEQN",all.x = T)
  OXPER.PPD<-merge(OXPER.CAL,PPD_mean,by="SEQN",all.x = T)
  OXPER.ALL<-na.omit(OXPER.PPD)
  #Graded periodontal disease and normal
  #severe periodontitis 
  OXPER.ALL$severe[OXPER.ALL$CAL_6number>=1&OXPER.ALL$PPD_5number>=1]<-"severe"
  table(OXPER.ALL$severe)
  OXPER.ALL$severe[is.na(OXPER.ALL$severe)] <-"no severe"
  #moderate periodontitis
  OXPER.ALL$moderate[OXPER.ALL$CAL_4number>=1|OXPER.ALL$PPD_5number>=1]<-"moderate"
  OXPER.ALL$moderate[is.na(OXPER.ALL$moderate)] <-"no moderate"
  #mild periodontitis
  OXPER.ALL$mild[(OXPER.ALL$CAL_3number>=1&OXPER.ALL$PPD_4number>=1)]<-"mild"
  OXPER.ALL$mild[is.na(OXPER.ALL$mild)] <-"no Mild"
  #only severe
  OXPER.ALL$Periodontitis_diagnosis[OXPER.ALL$severe=="severe"]<-"severe"
  #moderat not severe
  OXPER.ALL$Periodontitis_diagnosis[OXPER.ALL$moderate=="moderate"& OXPER.ALL$severe=="no severe"]<-"moderate"
  #Mild not moderate
  OXPER.ALL$Periodontitis_diagnosis[OXPER.ALL$moderate=="no moderate"& OXPER.ALL$mild=="mild"]<-"mild"
  #normal
  OXPER.ALL$Periodontitis_diagnosis[is.na(OXPER.ALL$Periodontitis_diagnosis)] <-"normal"
  #?
  OXPER.ALL$PD_diagnosis[OXPER.ALL$CAL_4number>=1|OXPER.ALL$PPD_5number>=1]<-"Moderate/Severe periodontitis"
  OXPER.ALL$PD_diagnosis[OXPER.ALL$CAL_4number<1&OXPER.ALL$PPD_5number<1]<-"No/Mild periodontitis"
  table(OXPER.ALL$PD_diagnosis,OXPER.ALL$Periodontitis_diagnosis)
  OXPER.diagnosis<-OXPER.ALL[c("SEQN","CAL_mean","PPD_mean","PD_diagnosis","Periodontitis_diagnosis")]
  record<-ls()
  rm(list=record[which(record!='OXPER.diagnosis')])
  }
  {#** section 1.3.8 data selction age>=20 ####
    age<-db_demo(years = 2009:2014,ageyr=T)
    age$ageyr[age$ageyr<20]<-NA
    age<-na.omit(age[,c(1,4)])
    colnames(age)[1]<-"SEQN"
    PD_dia_30<-merge(OXPER.diagnosis,age,by="SEQN",all.x = T)
    PD_CON2<-na.omit(PD_dia_30)
    PD_CON2$chort<-"NHANES_CON2"
    PD_CON2$ID<-paste(PD_CON2$chort,PD_CON2$SEQN,sep="_")
    PD_CON2<-PD_CON2[,c("SEQN","ID","chort","CAL_mean","PPD_mean","PD_diagnosis","Periodontitis_diagnosis")]
    record<-ls()
    rm(list=record[which(record!='PD_CON2')])
    #table(PD_CON1$Periodontitis_diagnosis)
    
    save(PD_CON2,file="I:/NHANES study/PD&TYG&CVD/Data/PD_CON2.Rdata")
  }
  
  
}
table(PD_CON2$Periodontitis_diagnosis)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 2. TYG Data <<<<< ####
adult=read.table("I:/NHANES study/PD&TYG&CVD/Data/NHANESIII/adult.dat",sep=",",fill=T)
adult$SEQN<-as.numeric(substring(adult$V1,1,5))
lab=read.table("I:/NHANES study/PD&TYG&CVD/Data/NHANESIII/lab.dat",sep=",",fill=T)
lab$SEQN<-as.numeric(substring(lab$V1,1,5))

{#* section 2.1 NHANES 1999-2004 Exclusion Data ####

    TGP_9900<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/LAB13AM.xpt")
    TGP_0102<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/L13AM_B.xpt")
    TGP_0304<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/L13AM_C.xpt")
    TGP_CON1<-rbind(TGP_9900[,c("SEQN","LBXTR")],
                    TGP_0102[,c("SEQN","LBXTR")],
                    TGP_0304[,c("SEQN","LBXTR")])
    G1P_9900<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/LAB10AM.xpt")
    G1P_0102<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/L10AM_B.xpt")
    G1P_0304<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/L10AM_C.xpt")
    G1P_CON1<-rbind(G1P_9900[,c("SEQN","LBXGLU")],
                    G1P_0102[,c("SEQN","LBXGLU")],
                    G1P_0304[,c("SEQN","LBXGLU")])
    Lab_CON1<-merge(TGP_CON1,G1P_CON1,by ="SEQN",all = T)
    
    Exam_9900<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/BMX.xpt")
    Exam_0102<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/BMX_B.xpt")
    Exam_0304<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/BMX_C.xpt")
    Exam_CON1<-rbind(Exam_9900[,c("SEQN","BMXBMI","BMXWAIST","BMXHT")],
                    Exam_0102[,c("SEQN","BMXBMI","BMXWAIST","BMXHT")],
                    Exam_0304[,c("SEQN","BMXBMI","BMXWAIST","BMXHT")])
    
    TYG_CON1<-merge(Lab_CON1,Exam_CON1,by ="SEQN",all = T)
    colnames(TYG_CON1)<-c("SEQN", "TGP","G1P","BMI","Waist","Height")
    colnames(TYG_CON1)
    TYG_CON1$TYG <- log((TYG_CON1$TGP * TYG_CON1$G1P) / 2)
    TYG_CON1$WHtR<-TYG_CON1$Waist/TYG_CON1$Height
    TYG_CON1$TyG_WC<-TYG_CON1$TYG*TYG_CON1$Waist
    TYG_CON1$TyG_WHtR<-TYG_CON1$TYG*TYG_CON1$WHtR
    TYG_CON1$TyG_BMI<-TYG_CON1$TYG*TYG_CON1$BMI
    colnames(TYG_CON1)
    save(TYG_CON1,file="I:/NHANES study/PD&TYG&CVD/Data/TYG_CON1.Rdata")

}


{#* section 2.1 NHANES 2009-2014 Exclusion Data ####
  TGP_0910<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/TRIGLY_F.xpt")
  TGP_1112<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/TRIGLY_G.xpt")
  TGP_1314<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/TRIGLY_H.xpt")
  TGP_CON2<-rbind(TGP_0910[,c("SEQN","LBXTR")],
                  TGP_1112[,c("SEQN","LBXTR")],
                  TGP_1314[,c("SEQN","LBXTR")])
  G1P_0910<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/GLU_F.xpt")
  G1P_1112<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/GLU_G.xpt")
  G1P_1314<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/GLU_H.xpt")
  G1P_CON2<-rbind(G1P_0910[,c("SEQN","LBXGLU")],
                  G1P_1112[,c("SEQN","LBXGLU")],
                  G1P_1314[,c("SEQN","LBXGLU")])
  Lab_CON2<-merge(TGP_CON2,G1P_CON2,by ="SEQN",all = T)
  
  Exam_0910<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/BMX_F.xpt")
  Exam_1112<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/BMX_G.xpt")
  Exam_1314<-read.xport("I:/NHANES study/PD&TYG&CVD/Data/TYG/BMX_H.xpt")
  Exam_CON2<-rbind(Exam_0910[,c("SEQN","BMXBMI","BMXWAIST","BMXHT")],
                   Exam_1112[,c("SEQN","BMXBMI","BMXWAIST","BMXHT")],
                   Exam_1314[,c("SEQN","BMXBMI","BMXWAIST","BMXHT")])
  
  TYG_CON2<-merge(Lab_CON2,Exam_CON2,by ="SEQN",all = T)
  colnames(TYG_CON2)<-c("SEQN", "TGP","G1P","BMI","Waist","Height")
  colnames(TYG_CON2)
  TYG_CON2$TYG <- log((TYG_CON2$TGP * TYG_CON2$G1P) / 2)
  TYG_CON2$WHtR<-TYG_CON2$Waist/TYG_CON2$Height
  TYG_CON2$TyG_WC<-TYG_CON2$TYG*TYG_CON2$Waist
  TYG_CON2$TyG_WHtR<-TYG_CON2$TYG*TYG_CON2$WHtR
  TYG_CON2$TyG_BMI<-TYG_CON2$TYG*TYG_CON2$BMI
  colnames(TYG_CON2)
  save(TYG_CON2,file="I:/NHANES study/PD&TYG&CVD/Data/TYG_CON2.Rdata")
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 3. Exclusion Data <<<<< ####
{#* section 3.2 NHANES1999-2004 Exclusion Data ####
  {#** section 3.2.1 Pregnancy ####
    tsv<-nhs_tsv('demo',years = 1999:2004)
    Pregnancy_CON<-nhs_read(tsv,'ridexprg:Pregnancy')
    table(Pregnancy_CON$Pregnancy)
    Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy==
                              "Cannot ascertain if SP is pregnant at exam"]<-NA
    Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy==
                              "Yes, positive lab pregnancy test or self-reported pregnant at exam"]<-"YES"
    Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy==
                              "SP not pregnant at exam"]<-"NO"
    Pregnancy_CON$Pregnancy[is.na(Pregnancy_CON$Pregnancy)]<-"NO"
    EX_CON1<-Pregnancy_CON[,c("seqn","Pregnancy")]
    colnames(EX_CON1)[1]<-"SEQN"
    record<-ls()
    rm(list=record[-which(record==c('EX_CON1'))])
  }
}
save(EX_CON1,file="I:/NHANES study/PD&TYG&CVD/Data/EX_CON1.Rdata")
{#* section 3.3 NHANES 2009-2014 Exclusion Data ####
  {#** section 3.3.1 Pregnancy ####
    tsv<-nhs_tsv('demo',years = 2009:2014)
    Pregnancy_CON<-nhs_read(tsv,'ridexprg:Pregnancy')
    table(Pregnancy_CON$Pregnancy,useNA = "ifany")
    Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy==
                              "Cannot ascertain if SP is pregnant at exam"|
                              Pregnancy_CON$Pregnancy=="Cannot ascertain if the participant is pregnant at exam"]<-NA
    Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy==
                              "Yes, positive lab pregnancy test or self-reported pregnant at exam"]<-"YES"
    Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy==
                              "SP not pregnant at exam"]<-"NO"
    Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy==
                              "The participant was not pregnant at exam"]<-"NO"
    Pregnancy_CON$Pregnancy[is.na(Pregnancy_CON$Pregnancy)]<-"NO"
    EX_CON2<-Pregnancy_CON[,c("seqn","Pregnancy")]
    colnames(EX_CON2)[1]<-"SEQN"
    record<-ls()
    rm(list=record[-which(record==c('EX_CON2'))])
  }
}
save(EX_CON2,file="I:/NHANES study/PD&TYG&CVD/Data/EX_CON2.Rdata")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 4.  Merge data <<<<< ####
load(file="I:/NHANES study/PD&TYG&CVD/Data/TYG_CON1.Rdata")
load(file="I:/NHANES study/PD&TYG&CVD/Data/TYG_CON2.Rdata")
TYG_CON1<-na.omit(TYG_CON1)
TYG_CON2<-na.omit(TYG_CON2)
load(file="I:/NHANES study/PD&TYG&CVD/Data/PD_CON1.Rdata")
load(file="I:/NHANES study/PD&TYG&CVD/Data/PD_CON2.Rdata")

load(file="I:/NHANES study/PD&TYG&CVD/Data/EX_CON1.Rdata")
load(file="I:/NHANES study/PD&TYG&CVD/Data/EX_CON2.Rdata")
multimerge<-function(dat=list(),...){
  if(length(dat)<2)return(as.data.frame(dat))
  mergedat<-dat[[1]]
  dat[[1]]<-NULL
  for(i in dat){
    mergedat<-merge(mergedat,i,...)
  }
  return(mergedat)
}
CON1<-multimerge(list(TYG_CON1,PD_CON1,EX_CON1),by="SEQN",all=T)
TYG_CON1<-subset(CON1,!is.na(TYG))
TYG_PD_CON1<-subset(TYG_CON1,!is.na(PD_diagnosis))

CON2<-multimerge(list(TYG_CON2,PD_CON2,EX_CON2),by="SEQN",all=T)
TYG_CON2<-subset(CON2,!is.na(TYG))
TYG_PD_CON2<-subset(TYG_CON2,!is.na(PD_diagnosis))

TYG_PD_CON1$Pregnancy[is.na(TYG_PD_CON1$Pregnancy)]<-"NO"
TYG_PD_CON2$Pregnancy[is.na(TYG_PD_CON2$Pregnancy)]<-"NO"
TYG_PD_CON1<-na.omit(TYG_PD_CON1)
TYG_PD_CON1<-subset(TYG_PD_CON1,Pregnancy=="NO")
TYG_PD_CON1$Pregnancy<-NULL
TYG_PD_CON2<-na.omit(TYG_PD_CON2)
TYG_PD_CON2<-subset(TYG_PD_CON2,Pregnancy=="NO")
TYG_PD_CON2$Pregnancy<-NULL
III_baseline<-TYG_PD_III
CON1_baseline<-TYG_PD_CON1
CON2_baseline<-TYG_PD_CON2
save(III_baseline,file="I:/NHANES study/PD&TYG&CVD/Data/III_baseline.Rdata")
save(CON1_baseline,file="I:/NHANES study/PD&TYG&CVD/Data/CON1_baseline.Rdata")
save(CON2_baseline,file="I:/NHANES study/PD&TYG&CVD/Data/CON2_baseline.Rdata")

