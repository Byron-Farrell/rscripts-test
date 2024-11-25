# HDS to ADS connversion for Obj 3
# Authors: Bola Coker/ Lilia Dela Cruz
#
# ver 1.0 (16/11/24): use of HDS v1.06 to convert to ADS Obj3
#

###############
# Define path #
###############
# PATH="D:\\King's College London\\GSTT BRC Data Management Service - CTU Data\\AStar\\D2T\\Obj3_151124\\ADS"
# setwd(PATH)
library(readr)
library(tidyverse)
library(DSI)
library(DSLite)
library(dsBase)
library(dsBaseClient)
library(dsSwissKnife)
library(dsSwissKnifeClient)

####################
# Define constants #
####################
HDS_VER="1.06"
ADS_VER="1.0"
NEVER_VALUE=0
PTR_NULL_VALUE=99

##################
# open hds files #
##################
hds.visits=read.csv("visits.csv")
hds.demographics=read.csv("demographics.csv")
hds.systemictherapy=read.csv("systemictherapy.csv")
hds.dlqi=read.csv("dlqi.csv")
hds.comorbidities=read.csv("comorbidities.csv")
hds.poem=read.csv("poem.csv")
hds.nrs=read.csv("nrs.csv")
hds.phototherapy=read.csv("phototherapy.csv")
hds.systemictherapyhx=read.csv("systemictherapyhx.csv")
hds.iga=read.csv("iga.csv")
hds.easi=read.csv("easi.csv")
hds.topicaltherapy=read.csv("topicaltherapy.csv")

# remove NA values
hds.visits[is.na(hds.visits)]=""
hds.demographics[is.na(hds.demographics)]=""
hds.systemictherapy[is.na(hds.systemictherapy)]=""
hds.dlqi[is.na(hds.dlqi)]=""
hds.comorbidities[is.na(hds.comorbidities)]=""
hds.poem[is.na(hds.poem)]=""
hds.nrs[is.na(hds.nrs)]=""
hds.phototherapy[is.na(hds.phototherapy)]=""
hds.systemictherapyhx[is.na(hds.systemictherapyhx)]=""
hds.iga[is.na(hds.iga)]=""
hds.easi[is.na(hds.easi)]=""
hds.topicaltherapy[is.na(hds.topicaltherapy)]=""

########################################
# Missing data check for key variables #
########################################

# hds.systemictherapy startdate
print(paste("Start date percentage missing for hds.systemictherapy ",
            round(nrow(hds.systemictherapy[hds.systemictherapy$startdate=="",])*100/
              nrow(hds.systemictherapy),0),"%",sep=""))

# hds.systemictherapyhx startdate
print(paste("Start date percentage missing for hds.systemictherapyhx ",
            round(nrow(hds.systemictherapyhx[hds.systemictherapyhx$startdate=="",])*100/
                    nrow(hds.systemictherapyhx),0),"%",sep=""))

#### NOTE ###
# For Objective 1 missing systemictherapyhx dates were inputted with 1900-01-01
# for ONLY Germany and Netherlands.
#
# example of imputation code:
# hds.systemictherapyhx_impute=hds.systemictherapyhx
# hds.systemictherapyhx_impute[hds.systemictherapyhx_impute$startdate=="","startdate"]="1900-01-01"

# hds.easi easidate
print(paste("Start date percentage missing for hds.easi ",
            round(nrow(hds.easi[hds.easi$startdate=="",])*100/
                    nrow(hds.easi),0),"%",sep=""))

# hds.iga igadate
print(paste("Start date percentage missing for hds.iga ",
            round(nrow(hds.iga[hds.iga$startdate=="",])*100/
                    nrow(hds.iga),0),"%",sep=""))

# hds.poem poemdate
print(paste("Start date percentage missing for hds.poem ",
            round(nrow(hds.poem[hds.poem$startdate=="",])*100/
                    nrow(hds.poem),0),"%",sep=""))

# hds.dlqi dlqidate
print(paste("Start date percentage missing for hds.dlqi ",
            round(nrow(hds.dlqi[hds.dlqi$startdate=="",])*100/
                    nrow(hds.dlqi),0),"%",sep=""))

# hds.nrs nrsdate
print(paste("Start date percentage missing for hds.nrs ",
            round(nrow(hds.nrs[hds.nrs$startdate=="",])*100/
                    nrow(hds.nrs),0),"%",sep=""))


###########################
# Miscellaneous Functions #
###########################

# function to get minimum visit date given anonymisedID
getMinVisitDate=function(anonymisedID){
  return (min(as.Date(hds.visits[hds.visits$anonymisedID==anonymisedID,"visitdate"])))
}

# function to get minimum start date of a treatment given anonymisedID and treatment
getMinStartDate=function(anonymisedID,treatment){
  return (min(as.Date(hds.systemictherapy[hds.systemictherapy$anonymisedID==anonymisedID &
                                            hds.systemictherapy$treatment==treatment &
                                            hds.systemictherapy$mainad==1,"startdate"])))
}

############################
# Past Treatment Functions #
############################

getPtrData=function(adm_c_pid,btr_d_start,ptr_var){
  ptr.df=ads.ptr[ads.ptr$adm_c_pid==adm_c_pid & ads.ptr[,ptr_var]>0,
                 c("adm_c_pid","startdate","enddate",ptr_var)]
  ptr.df.summ=data.frame(adm_c_pid=character(),ptr_var_start=character(),
                         ptr_var_remove=character(),btr_d_start=character(),
                         ptr_var=character())
  if (nrow(ptr.df)==0){
    row=data.frame(adm_c_pid,ptr_var_start="",ptr_var_remove="",
                   btr_d_start,ptr_var=0)
    ptr.df.summ=rbind(ptr.df.summ,row)
  } else if (nrow(ptr.df)>0){
    #get maximum end date with ptr
    startdate.max=""
    enddate.max=""
    ptr.max=0
    for (i in 1:nrow(ptr.df)){
      if (ptr.df$enddate[i]!=""){
        if (ptr.df$startdate[i]!=""){
          if (as.Date(ptr.df$startdate[i])<as.Date(btr_d_start)){
            if (enddate.max==""){
              startdate.max=ptr.df$startdate[i]
              enddate.max=ptr.df$enddate[i]
              ptr.max=ptr.df[,ptr_var][i]
            } else if (as.Date(ptr.df$enddate[i])>as.Date(enddate.max)){
              startdate.max=ptr.df$startdate[i]
              enddate.max=ptr.df$enddate[i]
              ptr.max=ptr.df[,ptr_var][i]
            }
          }  
        }  
      } else if(ptr.df$enddate[i]==""){
        if (ptr.df$startdate[i]!=""){
          if (as.Date(ptr.df$startdate[i])<as.Date(btr_d_start)){
            row=data.frame(adm_c_pid,ptr_var_start=ptr.df$startdate[i],
                           ptr_var_remove="",btr_d_start,ptr_var=0)
            ptr.df.summ=rbind(ptr.df.summ,row)
          }
        }
      }
    }
    row=data.frame(adm_c_pid,ptr_var_start=startdate.max,
                   ptr_var_remove=enddate.max,btr_d_start,ptr_var=ptr.max)
    ptr.df.summ=rbind(ptr.df.summ,row)
  }
  ptr.df.summ=ptr.df.summ[order(-ptr.df.summ$ptr_var),][1,]
  names(ptr.df.summ)=c("adm_c_pid",sub("f","d_start",ptr_var),
                       sub("f","d_remove",ptr_var),"btr_d_start",ptr_var)
  return(ptr.df.summ)
}

getPtrDf=function(ptr_var){
  ptr_var_start=sub("f","d_start",ptr_var)
  ptr_var_remove=sub("f","d_remove",ptr_var)
  ptr.df=data.frame(adm_c_pid=character(),ptr_var_start=character(),
                    ptr_var_remove=character(),btr_d_start=character(),
                    ptr_var=character())
  for (i in 1:nrow(ads.btr)){
    row=getPtr(ads.btr$adm_c_pid[i],ads.btr$btr_d_start[i],ptr_var)
    ptr.df=rbind(ptr.df,row)
  }
  ptr.df=unique(ptr.df[ptr.df[,ptr_var_start]!="",])
  return(ptr.df)
}

##################
# EASI Functions #
##################

# function to get previous easi score and date given anonymisedID,treatment,startdate,duration in weeks
getPrevEasiData=function(adm_c_pid,btr_d_start,wk_duration){
  dateref=as.character(as.Date(btr_d_start)+wk_duration*7)
  easi.prev=c(adm_c_pid,"","",dateref,btr_d_start,wk_duration,"")
  easi.df=hds.easi[hds.easi$anonymisedID==adm_c_pid,]
  if (nrow(easi.df)>0){
    easi.df$btr_d_start=btr_d_start
    easi.df$wk_duration=wk_duration
    easi.df$daysfromtreatment=""
    easi.df$dateref=dateref
    for (i in 1:nrow(easi.df)){
      if (easi.df$easidate[i]!=""){
        easi.df$daysfromtreatment[i]=as.Date(easi.df$easidate[i])-(as.Date(btr_d_start)+wk_duration*7)
      }
    }
    if (nrow(easi.df)>0){
      if (nrow(as.data.frame(easi.df[easi.df$daysfromtreatment<0,"daysfromtreatment"]))>0){
        closestprevdays=max(easi.df[easi.df$daysfromtreatment<0,"daysfromtreatment"])
        easi.prev=easi.df[easi.df$daysfromtreatment==closestprevdays,c("anonymisedID","easidate","easi",
                                                                       "dateref","btr_d_start",
                                                                       "wk_duration","daysfromtreatment")][1,]
      }  
    }
  }  
  if (wk_duration==0){
    names(easi.prev)=c("adm_c_pid","prev_d_easi","prev_n_easi","date_w0_ref",
                       "btr_d_start","wk_duration","daysfromtreatment")
  } else if (wk_duration>0){
    names(easi.prev)=c("adm_c_pid",paste("pre",wk_duration,"_d_easi",sep=""),paste("pre",wk_duration,"_n_easi",sep=""),
                       paste("date_w",wk_duration,"_ref",sep=""),
                       "btr_d_start","wk_duration","daysfromtreatment")
  }
  return(easi.prev)
}

# function to create previous easi period dataframe
getPrevEasiDf=function(wk_duration){
  preveasi.df=getPrevEasiData(ads.btr$adm_c_pid[1],ads.btr$btr_d_start[1],wk_duration)
  ads.btr.unique=unique(ads.btr[,c("adm_c_pid","btr_d_start")])
  for (i in 2:nrow(ads.btr.unique)){
    preveasi.df=rbind(preveasi.df,getPrevEasiData(ads.btr.unique$adm_c_pid[i],
                                                  ads.btr.unique$btr_d_start[i],wk_duration))
  }
  return(preveasi.df)
}

# function to get post easi score and date given anonymisedID,treatment,startdate,duration in weeks
getPostEasiData=function(adm_c_pid,btr_d_start,wk_duration){
  dateref=as.character(as.Date(btr_d_start)+wk_duration*7)
  easi.post=c(adm_c_pid,"","",dateref,btr_d_start,wk_duration,"")
  easi.df=hds.easi[hds.easi$anonymisedID==adm_c_pid,]
  if (nrow(easi.df)>0){
    easi.df$btr_d_start=btr_d_start
    easi.df$wk_duration=wk_duration
    easi.df$daysfromtreatment=""
    easi.df$dateref=dateref
    for (i in 1:nrow(easi.df)){
      if (easi.df$easidate[i]!=""){
        easi.df$daysfromtreatment[i]=as.Date(easi.df$easidate[i])-(as.Date(btr_d_start)+wk_duration*7)
      }
    }
    if (nrow(easi.df)>0){
      if (nrow(as.data.frame(easi.df[easi.df$daysfromtreatment>=0,"daysfromtreatment"]))>0){
        closestpostdays=min(easi.df[easi.df$daysfromtreatment>=0,"daysfromtreatment"])
        easi.post=easi.df[easi.df$daysfromtreatment==closestpostdays,c("anonymisedID","easidate","easi",
                                                                       "dateref","btr_d_start",
                                                                       "wk_duration","daysfromtreatment")][1,]
      }  
    }
  }  
  if (wk_duration==0){
    names(easi.post)=c("adm_c_pid","post_d_easi","post_n_easi","date_w0_ref",
                       "btr_d_start","wk_duration","daysfromtreatment")
  } else if (wk_duration>0){
    names(easi.post)=c("adm_c_pid",paste("post",wk_duration,"_d_easi",sep=""),paste("post",wk_duration,"_n_easi",sep=""),
                       paste("date_w",wk_duration,"_ref",sep=""),
                       "btr_d_start","wk_duration","daysfromtreatment")
  }
  return(easi.post)
}


# function to create post easi period dataframe
getPostEasiDf=function(wk_duration){
  posteasi.df=getPostEasiData(ads.btr$adm_c_pid[1],ads.btr$btr_d_start[1],wk_duration)
  ads.btr.unique=unique(ads.btr[,c("adm_c_pid","btr_d_start")])
  for (i in 2:nrow(ads.btr.unique)){
    posteasi.df=rbind(posteasi.df,getPostEasiData(ads.btr.unique$adm_c_pid[i],
                                                  ads.btr.unique$btr_d_start[i],wk_duration))
  }
  return(posteasi.df)
}

#################
# IGA Functions #
#################

# function to get previous iga score and date given anonymisedID,treatment,startdate,duration in weeks
getPrevIgaData=function(adm_c_pid,btr_d_start,wk_duration){
  dateref=as.character(as.Date(btr_d_start)+wk_duration*7)
  iga.prev=c(adm_c_pid,"","",dateref,btr_d_start,wk_duration,"")
  iga.df=hds.iga[hds.iga$anonymisedID==adm_c_pid,]
  if (nrow(iga.df)>0){
    iga.df$btr_d_start=btr_d_start
    iga.df$wk_duration=wk_duration
    iga.df$daysfromtreatment=""
    iga.df$dateref=dateref
    for (i in 1:nrow(iga.df)){
      if (iga.df$igadate[i]!=""){
        iga.df$daysfromtreatment[i]=as.Date(iga.df$igadate[i])-(as.Date(btr_d_start)+wk_duration*7)
      }
    }
    if (nrow(iga.df)>0){
      if (nrow(as.data.frame(iga.df[iga.df$daysfromtreatment<0,"daysfromtreatment"]))>0){
        closestprevdays=max(iga.df[iga.df$daysfromtreatment<0,"daysfromtreatment"])
        iga.prev=iga.df[iga.df$daysfromtreatment==closestprevdays,c("anonymisedID","igadate","iga",
                                                                       "dateref","btr_d_start",
                                                                       "wk_duration","daysfromtreatment")][1,]
      }  
    }
  }  
  if (wk_duration==0){
    names(iga.prev)=c("adm_c_pid","prev_d_iga","prev_n_iga","date_w0_ref",
                       "btr_d_start","wk_duration","daysfromtreatment")
  } else if (wk_duration>0){
    names(iga.prev)=c("adm_c_pid",paste("pre",wk_duration,"_d_iga",sep=""),paste("pre",wk_duration,"_n_iga",sep=""),
                       paste("date_w",wk_duration,"_ref",sep=""),
                       "btr_d_start","wk_duration","daysfromtreatment")
  }
  return(iga.prev)
}

# function to create previous iga period dataframe
getPrevIgaDf=function(wk_duration){
  previga.df=getPrevIgaData(ads.btr$adm_c_pid[1],ads.btr$btr_d_start[1],wk_duration)
  ads.btr.unique=unique(ads.btr[,c("adm_c_pid","btr_d_start")])
  for (i in 2:nrow(ads.btr.unique)){
    previga.df=rbind(previga.df,getPrevIgaData(ads.btr.unique$adm_c_pid[i],
                                                  ads.btr.unique$btr_d_start[i],wk_duration))
  }
  return(previga.df)
}

# function to get post iga score and date given anonymisedID,treatment,startdate,duration in weeks
getPostIgaData=function(adm_c_pid,btr_d_start,wk_duration){
  dateref=as.character(as.Date(btr_d_start)+wk_duration*7)
  iga.post=c(adm_c_pid,"","",dateref,btr_d_start,wk_duration,"")
  iga.df=hds.iga[hds.iga$anonymisedID==adm_c_pid,]
  if (nrow(iga.df)>0){
    iga.df$btr_d_start=btr_d_start
    iga.df$wk_duration=wk_duration
    iga.df$daysfromtreatment=""
    iga.df$dateref=dateref
    for (i in 1:nrow(iga.df)){
      if (iga.df$igadate[i]!=""){
        iga.df$daysfromtreatment[i]=as.Date(iga.df$igadate[i])-(as.Date(btr_d_start)+wk_duration*7)
      }
    }
    if (nrow(iga.df)>0){
      if (nrow(as.data.frame(iga.df[iga.df$daysfromtreatment>=0,"daysfromtreatment"]))>0){
        closestpostdays=min(iga.df[iga.df$daysfromtreatment>=0,"daysfromtreatment"])
        iga.post=iga.df[iga.df$daysfromtreatment==closestpostdays,c("anonymisedID","igadate","iga",
                                                                       "dateref","btr_d_start",
                                                                       "wk_duration","daysfromtreatment")][1,]
      }  
    }
  }  
  if (wk_duration==0){
    names(iga.post)=c("adm_c_pid","post_d_iga","post_n_iga","date_w0_ref",
                       "btr_d_start","wk_duration","daysfromtreatment")
  } else if (wk_duration>0){
    names(iga.post)=c("adm_c_pid",paste("post",wk_duration,"_d_iga",sep=""),paste("post",wk_duration,"_n_iga",sep=""),
                       paste("date_w",wk_duration,"_ref",sep=""),
                       "btr_d_start","wk_duration","daysfromtreatment")
  }
  return(iga.post)
}


# function to create post iga period dataframe
getPostIgaDf=function(wk_duration){
  postiga.df=getPostIgaData(ads.btr$adm_c_pid[1],ads.btr$btr_d_start[1],wk_duration)
  ads.btr.unique=unique(ads.btr[,c("adm_c_pid","btr_d_start")])
  for (i in 2:nrow(ads.btr.unique)){
    postiga.df=rbind(postiga.df,getPostIgaData(ads.btr.unique$adm_c_pid[i],
                                                  ads.btr.unique$btr_d_start[i],wk_duration))
  }
  return(postiga.df)
}

##################
# POEM Functions #
##################

# function to get previous poem score and date given anonymisedID,treatment,startdate,duration in weeks
getPrevPoemData=function(adm_c_pid,btr_d_start,wk_duration){
  dateref=as.character(as.Date(btr_d_start)+wk_duration*7)
  poem.prev=c(adm_c_pid,"","",dateref,btr_d_start,wk_duration,"")
  poem.df=hds.poem[hds.poem$anonymisedID==adm_c_pid,]
  if (nrow(poem.df)>0){
    poem.df$btr_d_start=btr_d_start
    poem.df$wk_duration=wk_duration
    poem.df$daysfromtreatment=""
    poem.df$dateref=dateref
    for (i in 1:nrow(poem.df)){
      if (poem.df$poemdate[i]!=""){
        poem.df$daysfromtreatment[i]=as.Date(poem.df$poemdate[i])-(as.Date(btr_d_start)+wk_duration*7)
      }
    }
    if (nrow(poem.df)>0){
      if (nrow(as.data.frame(poem.df[poem.df$daysfromtreatment<0,"daysfromtreatment"]))>0){
        closestprevdays=max(poem.df[poem.df$daysfromtreatment<0,"daysfromtreatment"])
        poem.prev=poem.df[poem.df$daysfromtreatment==closestprevdays,c("anonymisedID","poemdate","poem",
                                                                    "dateref","btr_d_start",
                                                                    "wk_duration","daysfromtreatment")][1,]
      }  
    }
  }  
  if (wk_duration==0){
    names(poem.prev)=c("adm_c_pid","prev_d_poem","prev_n_poem","date_w0_ref",
                      "btr_d_start","wk_duration","daysfromtreatment")
  } else if (wk_duration>0){
    names(poem.prev)=c("adm_c_pid",paste("pre",wk_duration,"_d_poem",sep=""),paste("pre",wk_duration,"_n_poem",sep=""),
                      paste("date_w",wk_duration,"_ref",sep=""),
                      "btr_d_start","wk_duration","daysfromtreatment")
  }
  return(poem.prev)
}

# function to create previous poem period dataframe
getPrevPoemDf=function(wk_duration){
  prevpoem.df=getPrevPoemData(ads.btr$adm_c_pid[1],ads.btr$btr_d_start[1],wk_duration)
  ads.btr.unique=unique(ads.btr[,c("adm_c_pid","btr_d_start")])
  for (i in 2:nrow(ads.btr.unique)){
    prevpoem.df=rbind(prevpoem.df,getPrevPoemData(ads.btr.unique$adm_c_pid[i],
                                               ads.btr.unique$btr_d_start[i],wk_duration))
  }
  return(prevpoem.df)
}

# function to get post poem score and date given anonymisedID,treatment,startdate,duration in weeks
getPostPoemData=function(adm_c_pid,btr_d_start,wk_duration){
  dateref=as.character(as.Date(btr_d_start)+wk_duration*7)
  poem.post=c(adm_c_pid,"","",dateref,btr_d_start,wk_duration,"")
  poem.df=hds.poem[hds.poem$anonymisedID==adm_c_pid,]
  if (nrow(poem.df)>0){
    poem.df$btr_d_start=btr_d_start
    poem.df$wk_duration=wk_duration
    poem.df$daysfromtreatment=""
    poem.df$dateref=dateref
    for (i in 1:nrow(poem.df)){
      if (poem.df$poemdate[i]!=""){
        poem.df$daysfromtreatment[i]=as.Date(poem.df$poemdate[i])-(as.Date(btr_d_start)+wk_duration*7)
      }
    }
    if (nrow(poem.df)>0){
      if (nrow(as.data.frame(poem.df[poem.df$daysfromtreatment>=0,"daysfromtreatment"]))>0){
        closestpostdays=min(poem.df[poem.df$daysfromtreatment>=0,"daysfromtreatment"])
        poem.post=poem.df[poem.df$daysfromtreatment==closestpostdays,c("anonymisedID","poemdate","poem",
                                                                    "dateref","btr_d_start",
                                                                    "wk_duration","daysfromtreatment")][1,]
      }  
    }
  }  
  if (wk_duration==0){
    names(poem.post)=c("adm_c_pid","post_d_poem","post_n_poem","date_w0_ref",
                      "btr_d_start","wk_duration","daysfromtreatment")
  } else if (wk_duration>0){
    names(poem.post)=c("adm_c_pid",paste("post",wk_duration,"_d_poem",sep=""),paste("post",wk_duration,"_n_poem",sep=""),
                      paste("date_w",wk_duration,"_ref",sep=""),
                      "btr_d_start","wk_duration","daysfromtreatment")
  }
  return(poem.post)
}


# function to create post poem period dataframe
getPostPoemDf=function(wk_duration){
  postpoem.df=getPostPoemData(ads.btr$adm_c_pid[1],ads.btr$btr_d_start[1],wk_duration)
  ads.btr.unique=unique(ads.btr[,c("adm_c_pid","btr_d_start")])
  for (i in 2:nrow(ads.btr.unique)){
    postpoem.df=rbind(postpoem.df,getPostPoemData(ads.btr.unique$adm_c_pid[i],
                                               ads.btr.unique$btr_d_start[i],wk_duration))
  }
  return(postpoem.df)
}

##################
# DLQI Functions #
##################

# function to get previous dlqi score and date given anonymisedID,treatment,startdate,duration in weeks
getPrevDlqiData=function(adm_c_pid,btr_d_start,wk_duration){
  dateref=as.character(as.Date(btr_d_start)+wk_duration*7)
  dlqi.prev=c(adm_c_pid,"","",dateref,btr_d_start,wk_duration,"")
  dlqi.df=hds.dlqi[hds.dlqi$anonymisedID==adm_c_pid,]
  if (nrow(dlqi.df)>0){
    dlqi.df$btr_d_start=btr_d_start
    dlqi.df$wk_duration=wk_duration
    dlqi.df$daysfromtreatment=""
    dlqi.df$dateref=dateref
    for (i in 1:nrow(dlqi.df)){
      if (dlqi.df$dlqidate[i]!=""){
        dlqi.df$daysfromtreatment[i]=as.Date(dlqi.df$dlqidate[i])-(as.Date(btr_d_start)+wk_duration*7)
      }
    }
    if (nrow(dlqi.df)>0){
      if (nrow(as.data.frame(dlqi.df[dlqi.df$daysfromtreatment<0,"daysfromtreatment"]))>0){
        closestprevdays=max(dlqi.df[dlqi.df$daysfromtreatment<0,"daysfromtreatment"])
        dlqi.prev=dlqi.df[dlqi.df$daysfromtreatment==closestprevdays,c("anonymisedID","dlqidate","dlqi",
                                                                       "dateref","btr_d_start",
                                                                       "wk_duration","daysfromtreatment")][1,]
      }  
    }
  }  
  if (wk_duration==0){
    names(dlqi.prev)=c("adm_c_pid","prev_d_dlqi","prev_n_dlqi","date_w0_ref",
                       "btr_d_start","wk_duration","daysfromtreatment")
  } else if (wk_duration>0){
    names(dlqi.prev)=c("adm_c_pid",paste("pre",wk_duration,"_d_dlqi",sep=""),paste("pre",wk_duration,"_n_dlqi",sep=""),
                       paste("date_w",wk_duration,"_ref",sep=""),
                       "btr_d_start","wk_duration","daysfromtreatment")
  }
  return(dlqi.prev)
}

# function to create previous dlqi period dataframe
getPrevDlqiDf=function(wk_duration){
  prevdlqi.df=getPrevDlqiData(ads.btr$adm_c_pid[1],ads.btr$btr_d_start[1],wk_duration)
  ads.btr.unique=unique(ads.btr[,c("adm_c_pid","btr_d_start")])
  for (i in 2:nrow(ads.btr.unique)){
    prevdlqi.df=rbind(prevdlqi.df,getPrevDlqiData(ads.btr.unique$adm_c_pid[i],
                                                  ads.btr.unique$btr_d_start[i],wk_duration))
  }
  return(prevdlqi.df)
}

# function to get post dlqi score and date given anonymisedID,treatment,startdate,duration in weeks
getPostDlqiData=function(adm_c_pid,btr_d_start,wk_duration){
  dateref=as.character(as.Date(btr_d_start)+wk_duration*7)
  dlqi.post=c(adm_c_pid,"","",dateref,btr_d_start,wk_duration,"")
  dlqi.df=hds.dlqi[hds.dlqi$anonymisedID==adm_c_pid,]
  if (nrow(dlqi.df)>0){
    dlqi.df$btr_d_start=btr_d_start
    dlqi.df$wk_duration=wk_duration
    dlqi.df$daysfromtreatment=""
    dlqi.df$dateref=dateref
    for (i in 1:nrow(dlqi.df)){
      if (dlqi.df$dlqidate[i]!=""){
        dlqi.df$daysfromtreatment[i]=as.Date(dlqi.df$dlqidate[i])-(as.Date(btr_d_start)+wk_duration*7)
      }
    }
    if (nrow(dlqi.df)>0){
      if (nrow(as.data.frame(dlqi.df[dlqi.df$daysfromtreatment>=0,"daysfromtreatment"]))>0){
        closestpostdays=min(dlqi.df[dlqi.df$daysfromtreatment>=0,"daysfromtreatment"])
        dlqi.post=dlqi.df[dlqi.df$daysfromtreatment==closestpostdays,c("anonymisedID","dlqidate","dlqi",
                                                                       "dateref","btr_d_start",
                                                                       "wk_duration","daysfromtreatment")][1,]
      }  
    }
  }  
  if (wk_duration==0){
    names(dlqi.post)=c("adm_c_pid","post_d_dlqi","post_n_dlqi","date_w0_ref",
                       "btr_d_start","wk_duration","daysfromtreatment")
  } else if (wk_duration>0){
    names(dlqi.post)=c("adm_c_pid",paste("post",wk_duration,"_d_dlqi",sep=""),paste("post",wk_duration,"_n_dlqi",sep=""),
                       paste("date_w",wk_duration,"_ref",sep=""),
                       "btr_d_start","wk_duration","daysfromtreatment")
  }
  return(dlqi.post)
}


# function to create post dlqi period dataframe
getPostDlqiDf=function(wk_duration){
  postdlqi.df=getPostDlqiData(ads.btr$adm_c_pid[1],ads.btr$btr_d_start[1],wk_duration)
  ads.btr.unique=unique(ads.btr[,c("adm_c_pid","btr_d_start")])
  for (i in 2:nrow(ads.btr.unique)){
    postdlqi.df=rbind(postdlqi.df,getPostDlqiData(ads.btr.unique$adm_c_pid[i],
                                                  ads.btr.unique$btr_d_start[i],wk_duration))
  }
  return(postdlqi.df)
}

#################
# NRS Functions #
#################

# function to get previous nrs score and date given anonymisedID,treatment,startdate,duration in weeks
getPrevNrsData=function(adm_c_pid,btr_d_start,wk_duration){
  dateref=as.character(as.Date(btr_d_start)+wk_duration*7)
  nrs.prev=c(adm_c_pid,"","",dateref,btr_d_start,wk_duration,"")
  nrs.df=hds.nrs[hds.nrs$anonymisedID==adm_c_pid,]
  if (nrow(nrs.df)>0){
    nrs.df$btr_d_start=btr_d_start
    nrs.df$wk_duration=wk_duration
    nrs.df$daysfromtreatment=""
    nrs.df$dateref=dateref
    for (i in 1:nrow(nrs.df)){
      if (nrs.df$nrsdate[i]!=""){
        nrs.df$daysfromtreatment[i]=as.Date(nrs.df$nrsdate[i])-(as.Date(btr_d_start)+wk_duration*7)
      }
    }
    if (nrow(nrs.df)>0){
      if (nrow(as.data.frame(nrs.df[nrs.df$daysfromtreatment<0,"daysfromtreatment"]))>0){
        closestprevdays=max(nrs.df[nrs.df$daysfromtreatment<0,"daysfromtreatment"])
        nrs.prev=nrs.df[nrs.df$daysfromtreatment==closestprevdays,c("anonymisedID","nrsdate","nrs",
                                                                       "dateref","btr_d_start",
                                                                       "wk_duration","daysfromtreatment")][1,]
      }  
    }
  }  
  if (wk_duration==0){
    names(nrs.prev)=c("adm_c_pid","prev_d_itch","prev_n_itch","date_w0_ref",
                       "btr_d_start","wk_duration","daysfromtreatment")
  } else if (wk_duration>0){
    names(nrs.prev)=c("adm_c_pid",paste("pre",wk_duration,"_d_itch",sep=""),paste("pre",wk_duration,"_n_itch",sep=""),
                       paste("date_w",wk_duration,"_ref",sep=""),
                       "btr_d_start","wk_duration","daysfromtreatment")
  }
  return(nrs.prev)
}

# function to create previous nrs period dataframe
getPrevNrsDf=function(wk_duration){
  prevnrs.df=getPrevNrsData(ads.btr$adm_c_pid[1],ads.btr$btr_d_start[1],wk_duration)
  ads.btr.unique=unique(ads.btr[,c("adm_c_pid","btr_d_start")])
  for (i in 2:nrow(ads.btr.unique)){
    prevnrs.df=rbind(prevnrs.df,getPrevNrsData(ads.btr.unique$adm_c_pid[i],
                                                  ads.btr.unique$btr_d_start[i],wk_duration))
  }
  return(prevnrs.df)
}

# function to get post nrs score and date given anonymisedID,treatment,startdate,duration in weeks
getPostNrsData=function(adm_c_pid,btr_d_start,wk_duration){
  dateref=as.character(as.Date(btr_d_start)+wk_duration*7)
  nrs.post=c(adm_c_pid,"","",dateref,btr_d_start,wk_duration,"")
  nrs.df=hds.nrs[hds.nrs$anonymisedID==adm_c_pid,]
  if (nrow(nrs.df)>0){
    nrs.df$btr_d_start=btr_d_start
    nrs.df$wk_duration=wk_duration
    nrs.df$daysfromtreatment=""
    nrs.df$dateref=dateref
    for (i in 1:nrow(nrs.df)){
      if (nrs.df$nrsdate[i]!=""){
        nrs.df$daysfromtreatment[i]=as.Date(nrs.df$nrsdate[i])-(as.Date(btr_d_start)+wk_duration*7)
      }
    }
    if (nrow(nrs.df)>0){
      if (nrow(as.data.frame(nrs.df[nrs.df$daysfromtreatment>=0,"daysfromtreatment"]))>0){
        closestpostdays=min(nrs.df[nrs.df$daysfromtreatment>=0,"daysfromtreatment"])
        nrs.post=nrs.df[nrs.df$daysfromtreatment==closestpostdays,c("anonymisedID","nrsdate","nrs",
                                                                       "dateref","btr_d_start",
                                                                       "wk_duration","daysfromtreatment")][1,]
      }  
    }
  }  
  if (wk_duration==0){
    names(nrs.post)=c("adm_c_pid","post_d_itch","post_n_itch","date_w0_ref",
                       "btr_d_start","wk_duration","daysfromtreatment")
  } else if (wk_duration>0){
    names(nrs.post)=c("adm_c_pid",paste("post",wk_duration,"_d_itch",sep=""),paste("post",wk_duration,"_n_itch",sep=""),
                       paste("date_w",wk_duration,"_ref",sep=""),
                       "btr_d_start","wk_duration","daysfromtreatment")
  }
  return(nrs.post)
}


# function to create post nrs period dataframe
getPostNrsDf=function(wk_duration){
  postnrs.df=getPostNrsData(ads.btr$adm_c_pid[1],ads.btr$btr_d_start[1],wk_duration)
  ads.btr.unique=unique(ads.btr[,c("adm_c_pid","btr_d_start")])
  for (i in 2:nrow(ads.btr.unique)){
    postnrs.df=rbind(postnrs.df,getPostNrsData(ads.btr.unique$adm_c_pid[i],
                                                  ads.btr.unique$btr_d_start[i],wk_duration))
  }
  return(postnrs.df)
}

#################################
# Get Week Variables Function #
#################################

getWeekVariables=function(wk_duration){
  date_ref=paste("date_w",wk_duration,"_ref",sep="")
  prev.easi=getPrevEasiDf(wk_duration)
  prev.easi=prev.easi[!colnames(prev.easi) %in% c("wk_duration","daysfromtreatment")]
  prev.iga=getPrevIgaDf(wk_duration)
  prev.iga=prev.iga[!colnames(prev.iga) %in% c("wk_duration","daysfromtreatment",date_ref)]
  prev.poem=getPrevPoemDf(wk_duration)
  prev.poem=prev.poem[!colnames(prev.poem) %in% c("wk_duration","daysfromtreatment",date_ref)]
  prev.dlqi=getPrevDlqiDf(wk_duration)
  prev.dlqi=prev.dlqi[!colnames(prev.dlqi) %in% c("wk_duration","daysfromtreatment",date_ref)]
  prev.nrs=getPrevNrsDf(wk_duration)
  prev.nrs=prev.nrs[!colnames(prev.nrs) %in% c("wk_duration","daysfromtreatment",date_ref)]
  post.easi=getPostEasiDf(wk_duration)
  post.easi=post.easi[!colnames(post.easi) %in% c("wk_duration","daysfromtreatment",date_ref)]
  post.iga=getPostIgaDf(wk_duration)
  post.iga=post.iga[!colnames(post.iga) %in% c("wk_duration","daysfromtreatment",date_ref)]
  post.poem=getPostPoemDf(wk_duration)
  post.poem=post.poem[!colnames(post.poem) %in% c("wk_duration","daysfromtreatment",date_ref)]
  post.dlqi=getPostDlqiDf(wk_duration)
  post.dlqi=post.dlqi[!colnames(post.dlqi) %in% c("wk_duration","daysfromtreatment",date_ref)]
  post.nrs=getPostNrsDf(wk_duration)
  post.nrs=post.nrs[!colnames(post.nrs) %in% c("wk_duration","daysfromtreatment",date_ref)]
  merge.list=list(unique(ads.btr[,c("adm_c_pid","btr_d_start")]),prev.easi,prev.iga,prev.poem,prev.dlqi,prev.nrs,
                  post.easi,post.iga,post.poem,post.dlqi,post.nrs)
  merge.df=Reduce(function(x,y) merge(x,y, by=c("adm_c_pid","btr_d_start"),all.x=TRUE),merge.list)
  return(merge.df)  
}

############################
# create ADS Obj3 from HDS #
############################

## Admin Obj1

# adm_c_pid
ads.adm_c_pid=as.data.frame(hds.demographics[,"anonymisedID"])
names(ads.adm_c_pid)="adm_c_pid"

# adm_d_enrol
ads.adm_d_enrol=data.frame(adm_c_pid=character(),adm_d_enrol=character())
for (i in 1:nrow(hds.demographics)){
  row=data.frame(adm_c_pid=hds.demographics$anonymisedID[i],
                 adm_d_enrol=as.character(getMinVisitDate(hds.demographics$anonymisedID[i])))
  ads.adm_d_enrol=rbind(ads.adm_d_enrol,row)
}

## Demographics Obj1

# dem_d_birth, dem_f_sex, dem_d_ageonset
ads.dem=hds.demographics[,c("anonymisedID","dateofbirth","sex","dateofadonset")]
names(ads.dem)=c("adm_c_pid","dem_d_birth","dem_f_sex","dem_d_ageonset")

## Baseline AD treatment Obj1

# btr_c_name,btr_n_potency,btr_n_freqweek,btr_d_start,btr_f_ongoing, btr_d_remove
ads.btr=data.frame(adm_c_pid=character(),btr_c_name=character(),btr_n_potency=character(),btr_n_freqweek=character(),
                   btr_d_start=character(),btr_f_ongoing=character(),btr_d_remove=character())
for (i in 1:nrow(hds.systemictherapy)){
  treatment.startdate=getMinStartDate(hds.systemictherapy$anonymisedID[i],hds.systemictherapy$treatment[i])
  row=hds.systemictherapy[hds.systemictherapy$anonymisedID==hds.systemictherapy$anonymisedID[i] & 
                            hds.systemictherapy$treatment==hds.systemictherapy$treatment[i] & 
                            hds.systemictherapy$mainad==1 & 
                            hds.systemictherapy$startdate==as.character(treatment.startdate),
                          c("anonymisedID","treatment","dosage","frequency","startdate","ongoing","enddate")]
  names(row)=c("adm_c_pid","btr_c_name","btr_n_potency","btr_n_freqweek","btr_d_start","btr_f_ongoing","btr_d_remove")
  ads.btr=rbind(ads.btr,row)
}
ads.btr=unique(ads.btr)

## Past AD treatments Obj 1

# past AD therapies: stopreasons
# append hds.systemictherapy and hds.systemictherapyhx
ads.systemictherapy.all=rbind(hds.systemictherapy[,c("anonymisedID","treatment","startdate","enddate","stopreason")],
                             hds.systemictherapyhx[,c("anonymisedID","treatment","startdate","enddate","stopreason")])
ads.ptr=data.frame(adm_c_pid=character(),startdate=character(),enddate=character(),ptr_f_cs_mtx=integer(),
                   ptr_f_cs_ciclo=integer(),ptr_f_cs_myco=integer(),ptr_f_cs_azath=integer(),ptr_f_cs_other=integer(),
                   ptr_f_bi_dupi=integer(),ptr_f_bi_nemo=integer(),ptr_f_bi_lebri=integer(),ptr_f_bi_tralo=integer(),
                   ptr_f_bi_roca=integer(),ptr_f_bi_omali=integer(),ptr_f_bi_other=integer(),ptr_f_jak_abro=integer(),
                   ptr_f_jak_bari=integer(),ptr_f_jak_upad=integer(),ptr_f_jak_other=integer())
for (i in 1:nrow(ads.systemictherapy.all)){
  if (ads.systemictherapy.all$treatment[i]==1){
    # ptr_f_cs_ciclo
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=ads.systemictherapy.all$stopreason[i],
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=NEVER_VALUE,ptr_f_bi_dupi=NEVER_VALUE,
                   ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=NEVER_VALUE,ptr_f_bi_tralo=NEVER_VALUE,ptr_f_bi_roca=NEVER_VALUE,
                   ptr_f_bi_omali=NEVER_VALUE,ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=NEVER_VALUE,
                   ptr_f_jak_bari=NEVER_VALUE,ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==2){
    # ptr_f_cs_mx
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=ads.systemictherapy.all$stopreason[i],ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=NEVER_VALUE,ptr_f_bi_dupi=NEVER_VALUE,
                   ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=NEVER_VALUE,ptr_f_bi_tralo=NEVER_VALUE,ptr_f_bi_roca=NEVER_VALUE,
                   ptr_f_bi_omali=NEVER_VALUE,ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=NEVER_VALUE,
                   ptr_f_jak_bari=NEVER_VALUE,ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==3){
    # ptr_f_cs_azath
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=ads.systemictherapy.all$stopreason[i],ptr_f_cs_other=NEVER_VALUE,ptr_f_bi_dupi=NEVER_VALUE,
                   ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=NEVER_VALUE,ptr_f_bi_tralo=NEVER_VALUE,ptr_f_bi_roca=NEVER_VALUE,
                   ptr_f_bi_omali=NEVER_VALUE,ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=NEVER_VALUE,
                   ptr_f_jak_bari=NEVER_VALUE,ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==4){
    # ptr_f_cs_myco
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=ads.systemictherapy.all$stopreason[i],ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=NEVER_VALUE,ptr_f_bi_dupi=NEVER_VALUE,
                   ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=NEVER_VALUE,ptr_f_bi_tralo=NEVER_VALUE,ptr_f_bi_roca=NEVER_VALUE,
                   ptr_f_bi_omali=NEVER_VALUE,ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=NEVER_VALUE,
                   ptr_f_jak_bari=NEVER_VALUE,ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==5){
    # ptr_f_cs_other
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=ads.systemictherapy.all$stopreason[i],
                   ptr_f_bi_dupi=NEVER_VALUE,ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=NEVER_VALUE,ptr_f_bi_tralo=NEVER_VALUE,
                   ptr_f_bi_roca=NEVER_VALUE,ptr_f_bi_omali=NEVER_VALUE,ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=NEVER_VALUE,
                   ptr_f_jak_bari=NEVER_VALUE,ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==6){
    # ptr_f_bi_dupi
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=NEVER_VALUE,
                   ptr_f_bi_dupi=ads.systemictherapy.all$stopreason[i],ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=NEVER_VALUE,
                   ptr_f_bi_tralo=NEVER_VALUE,ptr_f_bi_roca=NEVER_VALUE,ptr_f_bi_omali=NEVER_VALUE,
                   ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=NEVER_VALUE,ptr_f_jak_bari=NEVER_VALUE,
                   ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==7){
    # ptr_f_bi_nemo
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=NEVER_VALUE,ptr_f_bi_dupi=NEVER_VALUE,
                   ptr_f_bi_nemo=ads.systemictherapy.all$stopreason[i],ptr_f_bi_lebri=NEVER_VALUE,ptr_f_bi_tralo=NEVER_VALUE,ptr_f_bi_roca=NEVER_VALUE,
                   ptr_f_bi_omali=NEVER_VALUE,ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=NEVER_VALUE,
                   ptr_f_jak_bari=NEVER_VALUE,ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==8){
    # ptr_f_bi_lebri
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=NEVER_VALUE,ptr_f_bi_dupi=NEVER_VALUE,
                   ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=ads.systemictherapy.all$stopreason[i],ptr_f_bi_tralo=NEVER_VALUE,ptr_f_bi_roca=NEVER_VALUE,
                   ptr_f_bi_omali=NEVER_VALUE,ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=NEVER_VALUE,
                   ptr_f_jak_bari=NEVER_VALUE,ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==9){
    # ptr_f_bi_tralo
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=NEVER_VALUE,ptr_f_bi_dupi=NEVER_VALUE,
                   ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=NEVER_VALUE,ptr_f_bi_tralo=ads.systemictherapy.all$stopreason[i],ptr_f_bi_roca=NEVER_VALUE,
                   ptr_f_bi_omali=NEVER_VALUE,ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=NEVER_VALUE,
                   ptr_f_jak_bari=NEVER_VALUE,ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==10){
    # ptr_f_bi_roca
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=NEVER_VALUE,ptr_f_bi_dupi=NEVER_VALUE,
                   ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=NEVER_VALUE,ptr_f_bi_tralo=NEVER_VALUE,ptr_f_bi_roca=ads.systemictherapy.all$stopreason[i],
                   ptr_f_bi_omali=NEVER_VALUE,ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=NEVER_VALUE,
                   ptr_f_jak_bari=NEVER_VALUE,ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==11){
    # ptr_f_bi_omali
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=NEVER_VALUE,ptr_f_bi_dupi=NEVER_VALUE,
                   ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=NEVER_VALUE,ptr_f_bi_tralo=NEVER_VALUE,ptr_f_bi_roca=NEVER_VALUE,
                   ptr_f_bi_omali=ads.systemictherapy.all$stopreason[i],ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=NEVER_VALUE,
                   ptr_f_jak_bari=NEVER_VALUE,ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==12){
    # ptr_f_bi_other
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=NEVER_VALUE,ptr_f_bi_dupi=NEVER_VALUE,
                   ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=NEVER_VALUE,ptr_f_bi_tralo=NEVER_VALUE,ptr_f_bi_roca=NEVER_VALUE,
                   ptr_f_bi_omali=NEVER_VALUE,ptr_f_bi_other=ads.systemictherapy.all$stopreason[i],ptr_f_jak_abro=NEVER_VALUE,
                   ptr_f_jak_bari=NEVER_VALUE,ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==13){
    # ptr_f_jak_abro
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=NEVER_VALUE,ptr_f_bi_dupi=NEVER_VALUE,
                   ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=NEVER_VALUE,ptr_f_bi_tralo=NEVER_VALUE,ptr_f_bi_roca=NEVER_VALUE,
                   ptr_f_bi_omali=NEVER_VALUE,ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=ads.systemictherapy.all$stopreason[i],
                   ptr_f_jak_bari=NEVER_VALUE,ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==14){
    # ptr_f_jak_bari
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=NEVER_VALUE,ptr_f_bi_dupi=NEVER_VALUE,
                   ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=NEVER_VALUE,ptr_f_bi_tralo=NEVER_VALUE,ptr_f_bi_roca=NEVER_VALUE,
                   ptr_f_bi_omali=NEVER_VALUE,ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=NEVER_VALUE,
                   ptr_f_jak_bari=ads.systemictherapy.all$stopreason[i],ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==15){
    # ptr_f_jak_upad
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=NEVER_VALUE,ptr_f_bi_dupi=NEVER_VALUE,
                   ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=NEVER_VALUE,ptr_f_bi_tralo=NEVER_VALUE,ptr_f_bi_roca=NEVER_VALUE,
                   ptr_f_bi_omali=NEVER_VALUE,ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=NEVER_VALUE,
                   ptr_f_jak_bari=NEVER_VALUE,ptr_f_jak_upad=ads.systemictherapy.all$stopreason[i],ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==16){
    # ptr_f_jak_other
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=NEVER_VALUE,ptr_f_bi_dupi=NEVER_VALUE,
                   ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=NEVER_VALUE,ptr_f_bi_tralo=NEVER_VALUE,ptr_f_bi_roca=NEVER_VALUE,
                   ptr_f_bi_omali=NEVER_VALUE,ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=NEVER_VALUE,
                   ptr_f_jak_bari=NEVER_VALUE,ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=ads.systemictherapy.all$stopreason[i])
    ads.ptr=rbind(ads.ptr,row)
  } else if (ads.systemictherapy.all$treatment[i]==99){
    # unknown if ever previous treatment
    row=data.frame(adm_c_pid=ads.systemictherapy.all$anonymisedID[i],startdate=ads.systemictherapy.all$startdate[i],
                   enddate=ads.systemictherapy.all$enddate[i],ptr_f_cs_mtx=NEVER_VALUE,ptr_f_cs_ciclo=NEVER_VALUE,
                   ptr_f_cs_myco=NEVER_VALUE,ptr_f_cs_azath=NEVER_VALUE,ptr_f_cs_other=NEVER_VALUE,ptr_f_bi_dupi=NEVER_VALUE,
                   ptr_f_bi_nemo=NEVER_VALUE,ptr_f_bi_lebri=NEVER_VALUE,ptr_f_bi_tralo=NEVER_VALUE,ptr_f_bi_roca=NEVER_VALUE,
                   ptr_f_bi_omali=NEVER_VALUE,ptr_f_bi_other=NEVER_VALUE,ptr_f_jak_abro=NEVER_VALUE,
                   ptr_f_jak_bari=NEVER_VALUE,ptr_f_jak_upad=NEVER_VALUE,ptr_f_jak_other=NEVER_VALUE)
    ads.ptr=rbind(ads.ptr,row)
  }
}

# past AD therapies: stopreasons aggregate
ads.ptr.mtx=getPtrDf("ptr_f_cs_mtx")[,c("adm_c_pid","btr_d_start","ptr_f_cs_mtx")]
ads.ptr.ciclo=getPtrDf("ptr_f_cs_ciclo")[,c("adm_c_pid","btr_d_start","ptr_f_cs_ciclo")]
ads.ptr.myco=getPtrDf("ptr_f_cs_myco")[,c("adm_c_pid","btr_d_start","ptr_f_cs_myco")]
ads.ptr.azath=getPtrDf("ptr_f_cs_azath")[,c("adm_c_pid","btr_d_start","ptr_f_cs_azath")]
ads.ptr.cs_other=getPtrDf("ptr_f_cs_other")[,c("adm_c_pid","btr_d_start","ptr_f_cs_other")]
ads.ptr.dupi=getPtrDf("ptr_f_bi_dupi")[,c("adm_c_pid","btr_d_start","ptr_f_bi_dupi")]
ads.ptr.nemo=getPtrDf("ptr_f_bi_nemo")[,c("adm_c_pid","btr_d_start","ptr_f_bi_nemo")]
ads.ptr.lebri=getPtrDf("ptr_f_bi_lebri")[,c("adm_c_pid","btr_d_start","ptr_f_bi_lebri")]
ads.ptr.tralo=getPtrDf("ptr_f_bi_tralo")[,c("adm_c_pid","btr_d_start","ptr_f_bi_tralo")]
ads.ptr.roca=getPtrDf("ptr_f_bi_roca")[,c("adm_c_pid","btr_d_start","ptr_f_bi_roca")]
ads.ptr.omali=getPtrDf("ptr_f_bi_omali")[,c("adm_c_pid","btr_d_start","ptr_f_bi_omali")]
ads.ptr.bi_other=getPtrDf("ptr_f_bi_other")[,c("adm_c_pid","btr_d_start","ptr_f_bi_other")]
ads.ptr.abro=getPtrDf("ptr_f_jak_abro")[,c("adm_c_pid","btr_d_start","ptr_f_jak_abro")]
ads.ptr.bari=getPtrDf("ptr_f_jak_bari")[,c("adm_c_pid","btr_d_start","ptr_f_jak_bari")]
ads.ptr.upad=getPtrDf("ptr_f_jak_upad")[,c("adm_c_pid","btr_d_start","ptr_f_jak_upad")]
ads.ptr.jak_other=getPtrDf("ptr_f_jak_other")[,c("adm_c_pid","btr_d_start","ptr_f_jak_other")]
ads.ptr.list=list(unique(ads.btr[,c("adm_c_pid","btr_d_start")]),ads.ptr.mtx,ads.ptr.ciclo,
                  ads.ptr.myco,ads.ptr.azath,ads.ptr.cs_other,ads.ptr.dupi,ads.ptr.nemo,
                  ads.ptr.lebri,ads.ptr.tralo,ads.ptr.roca,ads.ptr.omali,ads.ptr.bi_other,
                  ads.ptr.abro,ads.ptr.bari,ads.ptr.upad,ads.ptr.jak_other)
ads.ptr.agg=Reduce(function(x,y) merge(x,y,by=c("adm_c_pid","btr_d_start"),
                                       all.x=TRUE),ads.ptr.list)
# replace null variables with 99
ads.ptr.agg[is.na(ads.ptr.agg)]=PTR_NULL_VALUE

## Obj 3 variables

# create dataset for week variables
ads.btr.wk0=getWeekVariables(0)
ads.btr.wk16=getWeekVariables(16)
ads.btr.wk36=getWeekVariables(36)
ads.btr.wk52=getWeekVariables(52)
ads.btr.wk76=getWeekVariables(76)
ads.btr.wk104=getWeekVariables(104)
ads.btr.wk126=getWeekVariables(126)
ads.btr.wk156=getWeekVariables(156)
ads.btr.wkall.list=list(ads.btr.wk0,ads.btr.wk16,ads.btr.wk36,ads.btr.wk52,ads.btr.wk76,
                        ads.btr.wk104,ads.btr.wk126,ads.btr.wk156)
ads.btr.wkall=Reduce(function(x,y) merge(x,y,by=c("adm_c_pid","btr_d_start"),all.x=TRUE),
                     ads.btr.wkall.list)

## create ADS Obj3
ads.obj3.pt1.list=list(ads.adm_c_pid,ads.adm_d_enrol,ads.dem,ads.btr)
ads.obj3.pt1=Reduce(function(x,y) merge(x,y,by="adm_c_pid",all.x=TRUE),ads.obj3.pt1.list)
ads.obj3.pt2.list=list(ads.obj3.pt1,ads.ptr.agg,ads.btr.wkall)
ads.obj3.pt2=Reduce(function(x,y) merge(x,y,by=c("adm_c_pid","btr_d_start"),all.x=TRUE),
                    ads.obj3.pt2.list)

# save ads obj3
write.csv(ads.obj3.pt2,
          "/tmp/obj1.csv",
          na="",row.names=FALSE)


