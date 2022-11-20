##########################################################################################
#The script is to process and plot the T&P response for GPP&growth outputs
#Input data extraction and preparations were performed in R and Arc GIS
##########################################################################################
library("dplyr")
library("ggplot2")
library(forcats)

Response_GPP_TRENDY <- read.csv(".\\Clim_response\\GPP_TRENDY_S2S3_mean_temp_prep_toshp.csv")

Response_GPP_RS <- read.csv(".\\Clim_response\\GPP_RS_mean_temp_prep_pcorr_tras_processed_toshp.csv")

Response_GPP_FLUXCOM <- read.csv(".\\Clim_response\\GPP_FLUXCOM_temp_prep_pcorr_tras_processed_toshp.csv")

Response_growth <- read.csv(".\\Clim_response\\AABI_per_area_all_mean_temp_prep_pcorr_toshp.csv")
#per tree
Response_growth_pt <- read.csv(".\\Clim_response\\AABI_per_tree_temp_prep_pcorr_toshp.csv")

#Unify the cols
Response_GPP_TRENDY1 = data.frame("Corr_Temp" = Response_GPP_TRENDY$Corr_Temp,"Corr_Prep" = Response_GPP_TRENDY$Corr_Prep,"Temp_4_9" = Response_GPP_TRENDY$Temp_4_9,"Prep_4_9" = Response_GPP_TRENDY$Prep_4_9,"Type" = rep("TRENDY",length(Response_GPP_TRENDY$Corr_Temp)))
Response_GPP_RS1 = data.frame("Corr_Temp" = Response_GPP_RS$Corr_Temp,"Corr_Prep" = Response_GPP_RS$Corr_Prep,"Temp_4_9" = Response_GPP_RS$Temp_4_9,"Prep_4_9" = Response_GPP_RS$Prep_4_9,"Type" = rep("RS",length(Response_GPP_RS$Corr_Temp)))
Response_GPP_FLUXCOM1 = data.frame("Corr_Temp" = Response_GPP_FLUXCOM$Corr_Temp,"Corr_Prep" = Response_GPP_FLUXCOM$Corr_Prep,"Temp_4_9" = Response_GPP_FLUXCOM$Temp_4_9,"Prep_4_9" = Response_GPP_FLUXCOM$Prep_4_9,"Type" = rep("FLUXCOM",length(Response_GPP_FLUXCOM$Corr_Temp)))
Response_growth1 = data.frame("Corr_Temp" = Response_growth$Corr_Temp_mean,"Corr_Prep" = Response_growth$Corr_Prep_mean,"Temp_4_9" = Response_growth$Temp_4_9,"Prep_4_9" = Response_growth$Prep_4_9,"Type" = rep("growth",length(Response_growth$Corr_Temp_mean)))
Response_growth_pt1 = data.frame("Corr_Temp" = Response_growth_pt$Corr_Temp,"Corr_Prep" = Response_growth_pt$Corr_Prep,"Temp_4_9" = Response_growth_pt$Temp_4_9,"Prep_4_9" = Response_growth_pt$Prep_4_9,"Type" = rep("growth_pt",length(Response_growth_pt$Corr_Temp)))

#Get the z-score
#Calculate mean
T_mean <- mean(Response_GPP_TRENDY$Temp_4_9)
P_mean <- mean(Response_GPP_TRENDY$Prep_4_9)

#Calculate sd
T_sd <- sd(Response_GPP_TRENDY$Temp_4_9)
P_sd <- sd(Response_GPP_TRENDY$Prep_4_9)

#Calculate z-score

Temp_zscore1 <- (Response_GPP_TRENDY1$Temp_4_9 - T_mean) / T_sd
Prep_zscore1 <- (Response_GPP_TRENDY1$Prep_4_9 - P_mean) / P_sd

Response_GPP_TRENDY1$Temp_zscore <- Temp_zscore1
Response_GPP_TRENDY1$Prep_zscore <- Prep_zscore1

Response_GPP_FLUXCOM1$Temp_zscore <- Temp_zscore1
Response_GPP_FLUXCOM1$Prep_zscore <- Prep_zscore1

Response_GPP_RS1$Temp_zscore <- Temp_zscore1
Response_GPP_RS1$Prep_zscore <- Prep_zscore1

Response_growth1$Temp_zscore <- Temp_zscore1
Response_growth1$Prep_zscore <- Prep_zscore1 

Response_growth_pt1$Temp_zscore <- Temp_zscore1
Response_growth_pt1$Prep_zscore <- Prep_zscore1 

#rbind the dfs
Response_df = rbind(Response_GPP_TRENDY1,Response_GPP_FLUXCOM1,Response_GPP_RS1,Response_growth1,Response_growth_pt1)

#Group data using Temp&Prep zscore 4 groups
{

#Warm & Wet
  Response_df$Zone[Response_df$Temp_zscore >= 0 & Response_df$Prep_zscore >= 0] <- "WW"

# #Warm & Dry
  Response_df$Zone[Response_df$Temp_zscore >= 0 & Response_df$Prep_zscore < 0] <- "WD"

# #Cool & Wet
  Response_df$Zone[Response_df$Temp_zscore < 0 & Response_df$Prep_zscore >= 0] <- "CW"

# #Cool & Dry
  Response_df$Zone[Response_df$Temp_zscore < 0 & Response_df$Prep_zscore < 0] <- "CD"
}
#here write the Response_df out
write.csv(Response_df,".\\Clim_response\\All_response_df.csv")

#Group data based on zone 4 groups
{
  WW_GPP_TRENDY_df = filter(Response_df,Zone == "WW" & Type == "TRENDY") 
  WW_GPP_RS_df = filter(Response_df,Zone == "WW" & Type == "RS") 
  WW_GPP_FLUXCOM_df = filter(Response_df,Zone == "WW" & Type == "FLUXCOM") 
  WW_growth_df = filter(Response_df,Zone == "WW" & Type == "growth") 
  WW_growth_pt_df = filter(Response_df,Zone == "WW" & Type == "growth_pt") 
  
  WD_GPP_TRENDY_df = filter(Response_df,Zone == "WD" & Type == "TRENDY") 
  WD_GPP_RS_df = filter(Response_df,Zone == "WD" & Type == "RS") 
  WD_GPP_FLUXCOM_df = filter(Response_df,Zone == "WD" & Type == "FLUXCOM") 
  WD_growth_df = filter(Response_df,Zone == "WD" & Type == "growth") 
  WD_growth_pt_df = filter(Response_df,Zone == "WD" & Type == "growth_pt") 
  
  CW_GPP_TRENDY_df = filter(Response_df,Zone == "CW" & Type == "TRENDY") 
  CW_GPP_RS_df = filter(Response_df,Zone == "CW" & Type == "RS") 
  CW_GPP_FLUXCOM_df = filter(Response_df,Zone == "CW" & Type == "FLUXCOM") 
  CW_growth_df = filter(Response_df,Zone == "CW" & Type == "growth") 
  CW_growth_pt_df = filter(Response_df,Zone == "CW" & Type == "growth_pt") 
  
  CD_GPP_TRENDY_df = filter(Response_df,Zone == "CD" & Type == "TRENDY") 
  CD_GPP_RS_df = filter(Response_df,Zone == "CD" & Type == "RS") 
  CD_GPP_FLUXCOM_df = filter(Response_df,Zone == "CD" & Type == "FLUXCOM") 
  CD_growth_df = filter(Response_df,Zone == "CD" & Type == "growth") 
  CD_growth_pt_df = filter(Response_df,Zone == "CD" & Type == "growth_pt") 
  
}


#Get mean and sd of responses for each group
{
#4 groups
#updated   
  {
    #WW
    {
      T_response_GPP_TRENDY_WW_mean = mean(WW_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WW_mean = mean(WW_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WW_mean = mean(WW_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WW_mean = mean(WW_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_WW_mean = mean(WW_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WW_mean = mean(WW_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WW_mean = mean(WW_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WW_mean = mean(WW_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WW_mean = mean(WW_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_WW_mean = mean(WW_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_WW_sd = sd(WW_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WW_sd = sd(WW_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WW_sd = sd(WW_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WW_sd = sd(WW_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_WW_sd = sd(WW_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WW_sd = sd(WW_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WW_sd = sd(WW_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WW_sd = sd(WW_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WW_sd = sd(WW_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_WW_sd = sd(WW_growth_pt_df$Corr_Prep,na.rm = T)   
    }
    #WD
    {
      T_response_GPP_TRENDY_WD_mean = mean(WD_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WD_mean = mean(WD_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WD_mean = mean(WD_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WD_mean = mean(WD_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_WD_mean = mean(WD_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WD_mean = mean(WD_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WD_mean = mean(WD_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WD_mean = mean(WD_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WD_mean = mean(WD_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_WD_mean = mean(WD_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_WD_sd = sd(WD_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WD_sd = sd(WD_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WD_sd = sd(WD_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WD_sd = sd(WD_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_WD_sd = sd(WD_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WD_sd = sd(WD_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WD_sd = sd(WD_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WD_sd = sd(WD_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WD_sd = sd(WD_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_WD_sd = sd(WD_growth_pt_df$Corr_Prep,na.rm = T)   
    }    
    #CW
    {
      T_response_GPP_TRENDY_CW_mean = mean(CW_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CW_mean = mean(CW_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CW_mean = mean(CW_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CW_mean = mean(CW_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_CW_mean = mean(CW_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CW_mean = mean(CW_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CW_mean = mean(CW_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CW_mean = mean(CW_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CW_mean = mean(CW_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_CW_mean = mean(CW_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_CW_sd = sd(CW_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CW_sd = sd(CW_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CW_sd = sd(CW_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CW_sd = sd(CW_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_CW_sd = sd(CW_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CW_sd = sd(CW_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CW_sd = sd(CW_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CW_sd = sd(CW_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CW_sd = sd(CW_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_CW_sd = sd(CW_growth_pt_df$Corr_Prep,na.rm = T)   
    }      
    #CD
    {
      T_response_GPP_TRENDY_CD_mean = mean(CD_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CD_mean = mean(CD_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CD_mean = mean(CD_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CD_mean = mean(CD_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_CD_mean = mean(CD_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CD_mean = mean(CD_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CD_mean = mean(CD_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CD_mean = mean(CD_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CD_mean = mean(CD_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_CD_mean = mean(CD_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_CD_sd = sd(CD_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CD_sd = sd(CD_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CD_sd = sd(CD_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CD_sd = sd(CD_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_CD_sd = sd(CD_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CD_sd = sd(CD_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CD_sd = sd(CD_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CD_sd = sd(CD_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CD_sd = sd(CD_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_CD_sd = sd(CD_growth_pt_df$Corr_Prep,na.rm = T)   
    }
  }  
}

#creat output dfs
#4 groups
{
Out_GPP_df <- data.frame(
  Zone <- c("WW","WW","WW","WD","WD","WD","CW","CW","CW","CD","CD","CD"),
  Source <- c("TRENDY","RS","FLUXCOM","TRENDY","RS","FLUXCOM","TRENDY","RS","FLUXCOM","TRENDY","RS","FLUXCOM"),
  T_Mean <- c(T_response_GPP_TRENDY_WW_mean,T_response_GPP_RS_WW_mean,T_response_GPP_FLUXCOM_WW_mean,T_response_GPP_TRENDY_WD_mean,T_response_GPP_RS_WD_mean,T_response_GPP_FLUXCOM_WD_mean,T_response_GPP_TRENDY_CW_mean,T_response_GPP_RS_CW_mean,T_response_GPP_FLUXCOM_CW_mean,T_response_GPP_TRENDY_CD_mean,T_response_GPP_RS_CD_mean,T_response_GPP_FLUXCOM_CD_mean),
  P_Mean <- c(P_response_GPP_TRENDY_WW_mean,P_response_GPP_RS_WW_mean,P_response_GPP_FLUXCOM_WW_mean,P_response_GPP_TRENDY_WD_mean,P_response_GPP_RS_WD_mean,P_response_GPP_FLUXCOM_WD_mean,P_response_GPP_TRENDY_CW_mean,P_response_GPP_RS_CW_mean,P_response_GPP_FLUXCOM_CW_mean,P_response_GPP_TRENDY_CD_mean,P_response_GPP_RS_CD_mean,P_response_GPP_FLUXCOM_CD_mean),
  T_sd <- c(T_response_GPP_TRENDY_WW_sd,T_response_GPP_RS_WW_sd,T_response_GPP_FLUXCOM_WW_sd,T_response_GPP_TRENDY_WD_sd,T_response_GPP_RS_WD_sd,T_response_GPP_FLUXCOM_WD_sd,T_response_GPP_TRENDY_CW_sd,T_response_GPP_RS_CW_sd,T_response_GPP_FLUXCOM_CW_sd,T_response_GPP_TRENDY_CD_sd,T_response_GPP_RS_CD_sd,T_response_GPP_FLUXCOM_CD_sd),
  P_sd <- c(P_response_GPP_TRENDY_WW_sd,P_response_GPP_RS_WW_sd,P_response_GPP_FLUXCOM_WW_sd,P_response_GPP_TRENDY_WD_sd,P_response_GPP_RS_WD_sd,P_response_GPP_FLUXCOM_WD_sd,P_response_GPP_TRENDY_CW_sd,P_response_GPP_RS_CW_sd,P_response_GPP_FLUXCOM_CW_sd,P_response_GPP_TRENDY_CD_sd,P_response_GPP_RS_CD_sd,P_response_GPP_FLUXCOM_CD_sd))
names(Out_GPP_df) <- c("Zone","Source","T_mean","P_mean","T_sd","P_sd")

Out_growth_df <- data.frame(
  Zone <- c("WW","WD","CW","CD"),
  Source <- c("ABI","ABI","ABI","ABI"),
  T_Mean <- c(T_response_growth_WW_mean,T_response_growth_WD_mean,T_response_growth_CW_mean,T_response_growth_CD_mean),
  P_Mean <- c(P_response_growth_WW_mean,P_response_growth_WD_mean,P_response_growth_CW_mean,P_response_growth_CD_mean),
  T_sd <- c(T_response_growth_WW_sd,T_response_growth_WD_sd,T_response_growth_CW_sd,T_response_growth_CD_sd),
  P_sd <- c(P_response_growth_WW_sd,P_response_growth_WD_sd,P_response_growth_CW_sd,P_response_growth_CD_sd))
names(Out_growth_df) <- c("Zone","Source","T_mean","P_mean","T_sd","P_sd")

Out_growth_pt_df <- data.frame(
  Zone <- c("WW","WD","CW","CD"),
  Source <- c("growth_pt","growth_pt","growth_pt","growth_pt"),
  T_Mean <- c(T_response_growth_pt_WW_mean,T_response_growth_pt_WD_mean,T_response_growth_pt_CW_mean,T_response_growth_pt_CD_mean),
  P_Mean <- c(P_response_growth_pt_WW_mean,P_response_growth_pt_WD_mean,P_response_growth_pt_CW_mean,P_response_growth_pt_CD_mean),
  T_sd <- c(T_response_growth_pt_WW_sd,T_response_growth_pt_WD_sd,T_response_growth_pt_CW_sd,T_response_growth_pt_CD_sd),
  P_sd <- c(P_response_growth_pt_WW_sd,P_response_growth_pt_WD_sd,P_response_growth_pt_CW_sd,P_response_growth_pt_CD_sd))
names(Out_growth_pt_df) <- c("Zone","Source","T_mean","P_mean","T_sd","P_sd")

#Combine the out dfs
Out_GPP_TRENDY_df <- base::subset(Out_GPP_df,Source == "TRENDY")
Out_GPP_FR_df <- base::subset(Out_GPP_df,Source == "FLUXCOM" | Source == "RS")
#Out_GPP_df$Source <- "GPP"
#Out_growth_df$Source <-"growth"
#Out_growth_pt_df$Source <- "growth_pt"

Out_df <- rbind(Out_GPP_df,Out_growth_df,Out_growth_pt_df)
Out_df1 <- rbind(Out_GPP_df,Out_growth_df)
Out_df2 <- rbind(Out_GPP_TRENDY_df,Out_growth_df)

Out_GPP_df$T_max <- Out_GPP_df$T_mean + Out_GPP_df$T_sd
Out_GPP_df$T_min <- Out_GPP_df$T_mean - Out_GPP_df$T_sd

Out_GPP_df$P_max <- Out_GPP_df$P_mean + Out_GPP_df$P_sd
Out_GPP_df$P_min <- Out_GPP_df$P_mean - Out_GPP_df$P_sd

Out_df1$T_max <- Out_df1$T_mean + Out_df1$T_sd
Out_df1$T_min <- Out_df1$T_mean - Out_df1$T_sd

Out_df1$P_max <- Out_df1$P_mean + Out_df1$P_sd
Out_df1$P_min <- Out_df1$P_mean - Out_df1$P_sd

Out_df2$T_max <- Out_df2$T_mean + Out_df2$T_sd
Out_df2$T_min <- Out_df2$T_mean - Out_df2$T_sd

Out_df2$P_max <- Out_df2$P_mean + Out_df2$P_sd
Out_df2$P_min <- Out_df2$P_mean - Out_df2$P_sd

#Out_df2$T_max[4]:1.0339039
Out_df2$T_max[4] = 1

Out_GPP_FR_df$T_max <- Out_GPP_FR_df$T_mean + Out_GPP_FR_df$T_sd
Out_GPP_FR_df$T_min <- Out_GPP_FR_df$T_mean - Out_GPP_FR_df$T_sd

Out_GPP_FR_df$P_max <- Out_GPP_FR_df$P_mean + Out_GPP_FR_df$P_sd
Out_GPP_FR_df$P_min <- Out_GPP_FR_df$P_mean - Out_GPP_FR_df$P_sd


}
#Out_df:rbind(Out_GPP_df,Out_growth_df,Out_growth_pt_df)
#Out_df1:rbind(Out_GPP_df,Out_growth_df)
#Out_df2:rbind(Out_GPP_TRENDY_df,Out_growth_df)

#output dfs
#Out_GPP_df Out_GPP_df1 Out_growth_df Out_growth_df1 Out_growth_pt_df Out_growth_pt_df1
#

#boxplot
  test = ggplot()+
    geom_point(Out_GPP_df,mapping = aes(x = T_mean, y = P_mean,color = Source,shape = Zone)) +
    geom_errorbar(Out_GPP_df,mapping = aes(x = T_mean,ymin = P_min, ymax = P_max,color = Source))+
    geom_errorbarh(Out_GPP_df,mapping = aes(y = P_mean,xmin = T_min, xmax = T_max,color = Source))+
    xlim(-1.1,1.1)+
    ylim(-1.1,1.1)
    
#density plot for T and P z-score
#create the x-y axis
#https://stackoverflow.com/questions/17753101/center-x-and-y-axis-with-ggplot2
  library(ggplot2)
  library(magrittr)
  
  # constants
  axis_begin  <- -3
  axis_end    <- 3
  total_ticks <- 7
  
  # DATA ----
  # point to plot
  my_point <- data.frame(x=1,y=1)
  
  # chart junk data
  tick_frame <- 
    data.frame(ticks = seq(axis_begin, axis_end, length.out = total_ticks), 
               zero=0) %>%
    subset(ticks != 0)
  
  lab_frame <- data.frame(lab = seq(axis_begin, axis_end),
                          zero = 0) %>%
    subset(lab != 0)
  
  tick_sz <- (tail(lab_frame$lab, 1) -  lab_frame$lab[1]) / 128
  
  pdf(".\\Clim_response\\Fig3_sub.pdf",width = 4,height = 4)
  
  p_1 <- ggplot(Response_GPP_TRENDY1,aes(x = Temp_zscore, y =Prep_zscore))+
    geom_point(color = "#81c784",size = 2)+
    
    # CHART JUNK
    # y axis line
    geom_segment(x = 0, xend = 0, 
                 y = lab_frame$lab[1], yend = tail(lab_frame$lab, 1),
                 size = 1.5) +
    # x axis line
    geom_segment(y = 0, yend = 0, 
                 x = lab_frame$lab[1], xend = tail(lab_frame$lab, 1),
                 size = 1.5) +
    # x ticks
    geom_segment(data = tick_frame, 
                 aes(x = ticks, xend = ticks, 
                     y = zero, yend = zero + tick_sz)) +
    # y ticks
    geom_segment(data = tick_frame, 
                 aes(x = zero, xend = zero + tick_sz, 
                     y = ticks, yend = ticks)) + 
    
    # labels
    geom_text(data=lab_frame, aes(x=lab, y=zero, label=lab),
              vjust=1.5,size =10) +
    geom_text(data=lab_frame, aes(x=zero, y=lab, label=lab),
              hjust=1.5,size =10)+
    theme_void() 
  
  p_1
  dev.off()
  
#===============================================================================================================
#Density ridgeline plots
#https://www.datanovia.com/en/blog/elegant-visualization-of-density-distribution-in-r-using-ridgeline/
#https://cran.r-project.org/web/packages/ggridges/vignettes/gallery.html
#https://stackoverflow.com/questions/52713272/adding-a-mean-to-geom-density-ridges
#===============================================================================================================

Data_all = read.csv(".\\Clim_response\\All_response_df.csv")

Data_TERENDY_sink = subset(Data_all,Type == "TRENDY" | Type =="growth")  

Data_all_per_area = subset(Data_all,Type == "TRENDY" | Type =="growth" | Type == "RS" |Type == "FLUXCOM")  
#for all per area outputs
{
  #Temp
  pdf(".\\Clim_response\\Fig21.pdf",width = 13.5,height = 6.5)
  
  Corr_Temp = ggplot(Data_all_per_area) +
    geom_density(aes(x=Corr_Temp, fill = Type),alpha = 0.7) +
    facet_grid(. ~ Zone)+
    scale_x_continuous(limits = c(-1.1,1.1),breaks = c(-1.0,-0.5,0,0.5,1.0))+
    scale_y_continuous(limits = c(0,10),breaks = c(0,2.5,5,7.5,10))+
    scale_fill_manual(name = "", breaks = c("growth","FLUXCOM","RS","TRENDY"), labels = c(bquote("AABI"[per_area]),bquote("GPP"[FLUXCOM]),bquote("GPP"[RS]),bquote("GPP"[TRENDY])),values = c("#FDE725FF","#55C667FF","#238A8DFF","#481567FF"))+
    theme_bw()+
    theme(legend.position=c(0.8,0.6),
          legend.text = element_text(size = 22),
          panel.border = element_blank(), 
          panel.spacing.x = unit(0,"line"))+
    
    theme(
      #strip.background = element_blank(),
      strip.text.x = element_text(size = 20)
    )+
    theme(axis.text = element_text(face="bold",size =16),axis.text.x = element_text(face="bold",size =16),axis.text.y = element_text(face="bold",size =16),axis.title.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16)) +
    labs(x = "T_pcor",y="Grid_density")
  
  Corr_Temp
  dev.off()
  
  #Prep
  pdf(".\\Clim_response\\Fig22.pdf",width = 13.5,height = 6.5)
  
  Corr_Prep = ggplot(Data_all_per_area) +
    geom_density(aes(x=Corr_Prep, fill = Type),alpha = 0.7) +
    facet_grid(. ~ Zone)+
    scale_x_continuous(limits = c(-1.1,1.1),breaks = c(-1.0,-0.5,0,0.5,1.0))+
    scale_y_continuous(limits = c(0,8),breaks = c(0,2.5,5,7.5))+
    ylim(0,10)+
    scale_fill_manual(name = "", breaks = c("growth","FLUXCOM","RS","TRENDY"), labels = c(bquote("AABI"[per_area]),bquote("GPP"[FLUXCOM]),bquote("GPP"[RS]),bquote("GPP"[TRENDY])),values = c("#FDE725FF","#55C667FF","#238A8DFF","#481567FF"))+
    theme_bw()+
    theme(legend.position="none",
          legend.text = element_text(size = 22),
          panel.border = element_blank(), 
          panel.spacing.x = unit(0,"line"))+
    
    theme(
      #strip.background = element_blank(),
      strip.text.x = element_text(size = 20)
    )+
    theme(axis.text = element_text(face="bold",size =16),axis.text.x = element_text(face="bold",size =16),axis.text.y = element_text(face="bold",size =16),axis.title.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16)) +
    labs(x = "P_pcor",y="Grid_density")
  
  Corr_Prep
  dev.off()
  
}

  
  