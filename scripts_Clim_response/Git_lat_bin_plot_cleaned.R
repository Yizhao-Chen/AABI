#######################################################################################
#lat distribution of clim response and period Fig.2 cdef
#######################################################################################

library("ggplot2")
library("tidyverse")
library("tidyr")
library(modeest)

#data input
{
  data_temp_trendyS2 = read.csv(".\\Clim_response\\Output_temp\\GPP_TRENDY_S2_temp_pcorr_tras_processed_toshp.csv")
  
  data_prep_trendyS2 = read.csv(".\\Clim_response\\Output_prep\\GPP_TRENDY_S2_prep_pcorr_tras_processed_toshp.csv")
  
  data_temp_trendyS3 = read.csv(".\\Clim_response\\Output_temp\\GPP_TRENDY_S3_temp_pcorr_tras_processed_toshp.csv")
  
  data_prep_trendyS3 = read.csv(".\\Clim_response\\Output_prep\\GPP_TRENDY_S3_prep_pcorr_tras_processed_toshp.csv")  
  
  data_temp_fluxcom = read.csv(".\\Clim_response\\Output_temp\\GPP_FLUXCOM_temp_pcorr_tras_processed_toshp.csv")
  
  data_prep_fluxcom = read.csv(".\\Clim_response\\Output_prep\\GPP_FLUXCOM_prep_pcorr_tras_processed_toshp.csv")
  
  data_temp_RS_mean = read.csv(".\\Clim_response\\Output_temp\\GPP_RS_mean_temp_pcorr_tras_processed_toshp.csv")
  
  data_prep_RS_mean = read.csv(".\\Clim_response\\Output_prep\\GPP_RS_mean_prep_pcorr_tras_processed_toshp.csv")  
  
  data_temp_per_area = read.csv(".\\Clim_response\\Output_temp\\AABI_age_correct_per_area_all_mean_temp_pcorr_tras_processed_toshp.csv")

  data_prep_per_area = read.csv(".\\Clim_response\\Output_prep\\AABI_age_correct_per_area_all_mean_prep_pcorr_tras_processed_toshp.csv")

  data_temp_per_tree = read.csv(".\\Clim_response\\Output_temp\\AABI_age_correct_per_tree_temp_pcorr_tras_processed_toshp.csv")

  data_prep_per_tree = read.csv(".\\Clim_response\\Output_prep\\AABI_age_correct_per_tree_prep_pcorr_tras_processed_toshp.csv")

}

#calculate means for TRENDY
#temp
{
  data_temp_TRENDY_nbr_mean <- data_temp_trendyS2
  #Corr
  df_temp_TRENDY_mean_Corr <- data.frame(data_temp_trendyS2$Corr,data_temp_trendyS3$Corr)
  
  #set -999 to NA
  for (i in 1:length(df_temp_TRENDY_mean_Corr)){
    df_temp_TRENDY_mean_Corr[,i][which(df_temp_TRENDY_mean_Corr[,i] == -999)] = NA
  }
  
  df_temp_TRENDY_mean_Corr$mean <- rowMeans(df_temp_TRENDY_mean_Corr,na.rm = T)
  
  #T_start
  df_temp_TRENDY_mean_T_start <- data.frame(data_temp_trendyS2$T_start,data_temp_trendyS3$T_start)
  
  #set -999 to NA
  for (i in 1:length(df_temp_TRENDY_mean_T_start)){
    df_temp_TRENDY_mean_T_start[,i][which(df_temp_TRENDY_mean_T_start[,i] == -999)] = NA
  }
  #get means
  df_temp_TRENDY_mean_T_start$mean <- rowMeans(df_temp_TRENDY_mean_T_start,na.rm = T)
  #get int with round
  df_temp_TRENDY_mean_T_start$mean = round(df_temp_TRENDY_mean_T_start$mean)
  
  
  #T_end
  df_temp_TRENDY_mean_T_end <- data.frame(data_temp_trendyS2$T_end,data_temp_trendyS3$T_end)
  
  #set -999 to NA
  for (i in 1:length(df_temp_TRENDY_mean_T_end)){
    df_temp_TRENDY_mean_T_end[,i][which(df_temp_TRENDY_mean_T_end[,i] == -999)] = NA
  }
  #get means
  df_temp_TRENDY_mean_T_end$mean <- rowMeans(df_temp_TRENDY_mean_T_end,na.rm = T)
  #get int with round
  df_temp_TRENDY_mean_T_end$mean = round(df_temp_TRENDY_mean_T_end$mean)
  
  #T_mark
  df_temp_TRENDY_mean_T_mark <- data.frame(data_temp_trendyS2$T_mark,data_temp_trendyS3$T_mark)
  
  #set -999 to NA
  for (i in 1:length(df_temp_TRENDY_mean_T_mark)){
    df_temp_TRENDY_mean_T_mark[,i][which(df_temp_TRENDY_mean_T_mark[,i] == -999)] = NA
  }
  #get mode for T_mark
  df_temp_TRENDY_mean_T_mark$mode <- apply(df_temp_TRENDY_mean_T_mark[ ,1:length(df_temp_TRENDY_mean_T_mark)], 1, mfv)
  
  data_temp_TRENDY_nbr_mean$Corr = df_temp_TRENDY_mean_Corr$mean
  data_temp_TRENDY_nbr_mean$T_start = df_temp_TRENDY_mean_T_start$mean
  data_temp_TRENDY_nbr_mean$T_end = df_temp_TRENDY_mean_T_end$mean
  data_temp_TRENDY_nbr_mean$T_mark = df_temp_TRENDY_mean_T_mark$mode
}
#prep
{
  data_prep_TRENDY_nbr_mean <- data_prep_trendyS2
  #Corr
  df_prep_TRENDY_mean_Corr <- data.frame(data_prep_trendyS2$Corr,data_prep_trendyS3$Corr)
  
  #set -999 to NA
  for (i in 1:length(df_prep_TRENDY_mean_Corr)){
    df_prep_TRENDY_mean_Corr[,i][which(df_prep_TRENDY_mean_Corr[,i] == -999)] = NA
  }
  
  df_prep_TRENDY_mean_Corr$mean <- rowMeans(df_prep_TRENDY_mean_Corr,na.rm = T)
  
  #T_start
  df_prep_TRENDY_mean_T_start <- data.frame(data_prep_trendyS2$T_start,data_prep_trendyS3$T_start)
  
  #set -999 to NA
  for (i in 1:length(df_prep_TRENDY_mean_T_start)){
    df_prep_TRENDY_mean_T_start[,i][which(df_prep_TRENDY_mean_T_start[,i] == -999)] = NA
  }
  #get means
  df_prep_TRENDY_mean_T_start$mean <- rowMeans(df_prep_TRENDY_mean_T_start,na.rm = T)
  #get int with round
  df_prep_TRENDY_mean_T_start$mean = round(df_prep_TRENDY_mean_T_start$mean)
  
  
  #T_end
  df_prep_TRENDY_mean_T_end <- data.frame(data_prep_trendyS2$T_end,data_prep_trendyS3$T_end)
  
  #set -999 to NA
  for (i in 1:length(df_prep_TRENDY_mean_T_end)){
    df_prep_TRENDY_mean_T_end[,i][which(df_prep_TRENDY_mean_T_end[,i] == -999)] = NA
  }
  #get means
  df_prep_TRENDY_mean_T_end$mean <- rowMeans(df_prep_TRENDY_mean_T_end,na.rm = T)
  #get int with round
  df_prep_TRENDY_mean_T_end$mean = round(df_prep_TRENDY_mean_T_end$mean)
  
  #T_mark
  df_prep_TRENDY_mean_T_mark <- data.frame(data_prep_trendyS2$T_mark,data_prep_trendyS3$T_mark)
  
  #set -999 to NA
  for (i in 1:length(df_prep_TRENDY_mean_T_mark)){
    df_prep_TRENDY_mean_T_mark[,i][which(df_prep_TRENDY_mean_T_mark[,i] == -999)] = NA
  }
  #get mode for T_mark
  df_prep_TRENDY_mean_T_mark$mode <- apply(df_prep_TRENDY_mean_T_mark[ ,1:length(df_prep_TRENDY_mean_T_mark)], 1, mfv)
  
  data_prep_TRENDY_nbr_mean$Corr = df_prep_TRENDY_mean_Corr$mean
  data_prep_TRENDY_nbr_mean$T_start = df_prep_TRENDY_mean_T_start$mean
  data_prep_TRENDY_nbr_mean$T_end = df_prep_TRENDY_mean_T_end$mean
  data_prep_TRENDY_nbr_mean$T_mark = df_prep_TRENDY_mean_T_mark$mode
}

data_temp_TRENDY_nbr_mean = data.frame(lapply(data_temp_TRENDY_nbr_mean, as.character), stringsAsFactors=FALSE)
data_prep_TRENDY_nbr_mean = data.frame(lapply(data_prep_TRENDY_nbr_mean, as.character), stringsAsFactors=FALSE)


#remove NA & -999 lines
{
  data1_temp_trendyS2 = data_temp_trendyS2[-which(data_temp_trendyS2$T_mark == -999),]
  data1_prep_trendyS2 =   data_prep_trendyS2[-which(data_prep_trendyS2$T_mark == -999),]
  
  data1_temp_trendyS3 = data_temp_trendyS3[-which(data_temp_trendyS3$T_mark == -999),]
  data1_prep_trendyS3 =   data_prep_trendyS3[-which(data_prep_trendyS3$T_mark == -999),]
  
  
  data1_temp_fluxcom = data_temp_fluxcom[!is.na(data_temp_fluxcom$T_mark),]
  data1_prep_fluxcom = data_prep_fluxcom[!is.na(data_prep_fluxcom$T_mark),]
  
  data1_temp_trendy_mean = data_temp_TRENDY_nbr_mean[-which(data_temp_TRENDY_nbr_mean$T_mark == "NA"),]
  data1_prep_trendy_mean = data_prep_TRENDY_nbr_mean[-which(data_prep_TRENDY_nbr_mean$T_mark == "NA"),]  
  
  data1_temp_per_area = data_temp_per_area[-which(data_temp_per_area$T_end == -999),]
  data1_prep_per_area = data_prep_per_area[-which(data_prep_per_area$T_end == -999),]
  data1_temp_per_tree = data_temp_per_tree[-which(data_temp_per_tree$T_end == -999),]
  data1_prep_per_tree = data_prep_per_tree[-which(data_prep_per_tree$T_end == -999),]
  
  data1_temp_RS_mean = data_temp_RS_mean[!is.na(data_temp_RS_mean$T_mark),]
  data1_prep_RS_mean = data_prep_RS_mean[!is.na(data_prep_RS_mean$T_mark),] 
  

}


#bulid subsets for north america only
#subset for period
subset_period <- function(x){
list = list()
for (i in 1:21){
  sub1 = base::subset(x,lat > (26+2*(i-1)) & lat <= (28+2*i))
  sub1_list = c()
for (j in 1:length(sub1$lat)){
  sub1_list = append(sub1_list,c(sub1$T_start[j]:sub1$T_end[j]))
}
list[[i]] = sub1_list
}

bin_df = setNames(do.call(cbind.data.frame, lapply(lapply(list, unlist), `length<-`, max(lengths(list)))), paste0("V", 1:21))
 
bin_df_long = gather(bin_df,bin,out)

outlist = list(bin_df,bin_df_long)
return(outlist)
}

#subset for corr
subset_corr <- function(x){
  list_corr = list()
  for (i in 1:21){
    sub1_corr = base::subset(x,lat > (26+2*(i-1)) & lat <= (28+2*i))
    sub1_list_corr = c()
    for (j in 1:length(sub1_corr$lat)){
      sub1_list_corr = append(sub1_list_corr,sub1_corr$Corr)
    }
    list_corr[[i]] = sub1_list_corr
  }

  bin_df_corr = setNames(do.call(cbind.data.frame, lapply(lapply(list_corr, unlist), `length<-`, max(lengths(list_corr)))), paste0("V", 1:21))
  
  bin_df_corr_long = gather(bin_df_corr,bin_corr,out_corr)
  
  outlist = list(bin_df_corr,bin_df_corr_long)
  return(outlist)
}

#get plotable dfs
#period bin_dfs
{
#TRENDY
bin_df_temp_trendy_period = data.frame(subset_period(data1_temp_trendy_mean)[1])
bin_df_prep_trendy_period = data.frame(subset_period(data1_prep_trendy_mean)[1])
bin_df_long_temp_trendy_period = data.frame(subset_period(data1_temp_trendy_mean)[2])
bin_df_long_prep_trendy_period = data.frame(subset_period(data1_prep_trendy_mean)[2])

bin_df_temp_trendyS2_period = data.frame(subset_period(data1_temp_trendyS2)[1])
bin_df_prep_trendyS2_period = data.frame(subset_period(data1_prep_trendyS2)[1])
bin_df_long_temp_trendyS2_period = data.frame(subset_period(data1_temp_trendyS2)[2])
bin_df_long_prep_trendyS2_period = data.frame(subset_period(data1_prep_trendyS2)[2])

bin_df_temp_trendyS3_period = data.frame(subset_period(data1_temp_trendyS3)[1])
bin_df_prep_trendyS3_period = data.frame(subset_period(data1_prep_trendyS3)[1])
bin_df_long_temp_trendyS3_period = data.frame(subset_period(data1_temp_trendyS3)[2])
bin_df_long_prep_trendyS3_period = data.frame(subset_period(data1_prep_trendyS3)[2])

#FLUXCOM
bin_df_temp_fluxcom_period = data.frame(subset_period(data1_temp_fluxcom)[1])
bin_df_prep_fluxcom_period = data.frame(subset_period(data1_prep_fluxcom)[1])
bin_df_long_temp_fluxcom_period = data.frame(subset_period(data1_temp_fluxcom)[2])
bin_df_long_prep_fluxcom_period = data.frame(subset_period(data1_prep_fluxcom)[2])

#sink per area
bin_df_temp_sink_per_area_period = data.frame(subset_period(data1_temp_per_area)[1])
bin_df_prep_sink_per_area_period = data.frame(subset_period(data1_prep_per_area)[1])
bin_df_long_temp_sink_per_area_period = data.frame(subset_period(data1_temp_per_area)[2])
bin_df_long_prep_sink_per_area_period = data.frame(subset_period(data1_prep_per_area)[2])
#sink per tree
bin_df_temp_sink_per_tree_period = data.frame(subset_period(data1_temp_per_tree)[1])
bin_df_prep_sink_per_tree_period = data.frame(subset_period(data1_prep_per_tree)[1])
bin_df_long_temp_sink_per_tree_period = data.frame(subset_period(data1_temp_per_tree)[2])
bin_df_long_prep_sink_per_tree_period = data.frame(subset_period(data1_prep_per_tree)[2])

#RS_mean
bin_df_temp_RS_mean_period = data.frame(subset_period(data1_temp_RS_mean)[1])
bin_df_prep_RS_mean_period = data.frame(subset_period(data1_prep_RS_mean)[1])
bin_df_long_temp_RS_mean_period = data.frame(subset_period(data1_temp_RS_mean)[2])
bin_df_long_prep_RS_mean_period = data.frame(subset_period(data1_prep_RS_mean)[2])
}

#corr bin_dfs 
{
  #TRENDY
  bin_df_temp_trendy_corr = data.frame(subset_corr(data1_temp_trendy_mean)[1])
  #unknow issue here: solved by write and read..
  write.csv(bin_df_temp_trendy_corr,"bin_df_temp_trendy_corr.csv")
  bin_df_temp_trendy_corr = read.csv("bin_df_temp_trendy_corr.csv")
  bin_df_temp_trendy_corr = bin_df_temp_trendy_corr[,-1]
  bin_df_prep_trendy_corr = data.frame(subset_corr(data1_prep_trendy_mean)[1])
  write.csv(bin_df_prep_trendy_corr,"bin_df_prep_trendy_corr.csv")
  bin_df_prep_trendy_corr = bin_df_prep_trendy_corr[,-1]  
  bin_df_prep_trendy_corr = read.csv("bin_df_prep_trendy_corr.csv",header = T)
  bin_df_long_temp_trendy_corr = data.frame(subset_corr(data1_temp_trendy_mean)[2])
  bin_df_long_prep_trendy_corr = data.frame(subset_corr(data1_prep_trendy_mean)[2])
  
  bin_df_temp_trendyS2_corr = data.frame(subset_corr(data1_temp_trendyS2)[1])
  bin_df_prep_trendyS2_corr = data.frame(subset_corr(data1_prep_trendyS2)[1])
  bin_df_long_temp_trendyS2_corr = data.frame(subset_corr(data1_temp_trendyS2)[2])
  bin_df_long_prep_trendyS2_corr = data.frame(subset_corr(data1_prep_trendyS2)[2])
  
  bin_df_temp_trendyS3_corr = data.frame(subset_corr(data1_temp_trendyS3)[1])
  bin_df_prep_trendyS3_corr = data.frame(subset_corr(data1_prep_trendyS3)[1])
  bin_df_long_temp_trendyS3_corr = data.frame(subset_corr(data1_temp_trendyS3)[2])
  bin_df_long_prep_trendyS3_corr = data.frame(subset_corr(data1_prep_trendyS3)[2])
  
  #FLUXCOM
  bin_df_temp_fluxcom_corr = data.frame(subset_corr(data1_temp_fluxcom)[1])
  bin_df_prep_fluxcom_corr = data.frame(subset_corr(data1_prep_fluxcom)[1])
  bin_df_long_temp_fluxcom_corr = data.frame(subset_corr(data1_temp_fluxcom)[2])
  bin_df_long_prep_fluxcom_corr = data.frame(subset_corr(data1_prep_fluxcom)[2])
  #sink per area
  bin_df_temp_sink_per_area_corr = data.frame(subset_corr(data1_temp_per_area)[1])
  bin_df_prep_sink_per_area_corr = data.frame(subset_corr(data1_prep_per_area)[1])
  bin_df_long_temp_sink_per_area_corr = data.frame(subset_corr(data1_temp_per_area)[2])
  bin_df_long_prep_sink_per_area_corr = data.frame(subset_corr(data1_prep_per_area)[2])
  #sink per tree
  bin_df_temp_sink_per_tree_corr = data.frame(subset_corr(data1_temp_per_tree)[1])
  bin_df_prep_sink_per_tree_corr = data.frame(subset_corr(data1_prep_per_tree)[1])
  bin_df_long_temp_sink_per_tree_corr = data.frame(subset_corr(data1_temp_per_tree)[2])
  bin_df_long_prep_sink_per_tree_corr = data.frame(subset_corr(data1_prep_per_tree)[2])

  #RS mean
  bin_df_temp_RS_mean_corr = data.frame(subset_corr(data1_temp_RS_mean)[1])
  bin_df_prep_RS_mean_corr = data.frame(subset_corr(data1_prep_RS_mean)[1])
  bin_df_long_temp_RS_mean_corr = data.frame(subset_corr(data1_temp_RS_mean)[2])
  bin_df_long_prep_RS_mean_corr = data.frame(subset_corr(data1_prep_RS_mean)[2])
}

#calculate stats.
stats = function(x){
  
  col_mean = apply(x,2,mean,na.rm=TRUE)
  col_sd = apply(x,2,sd,na.rm=TRUE) 
  col_max = col_mean + col_sd
  col_min = col_mean - col_sd
  bin_df_stats = data.frame(col_mean,col_sd,col_max,col_min)  
  return(bin_df_stats)
}

#period stats
{
  #TRENDY
  bin_stats_temp_trendy_period = stats(bin_df_temp_trendy_period)
  bin_stats_prep_trendy_period = stats(bin_df_prep_trendy_period)
  
  bin_stats_temp_trendyS2_period = stats(bin_df_temp_trendyS2_period)
  bin_stats_prep_trendyS2_period = stats(bin_df_prep_trendyS2_period)
  
  bin_stats_temp_trendyS3_period = stats(bin_df_temp_trendyS3_period)
  bin_stats_prep_trendyS3_period = stats(bin_df_prep_trendyS3_period)
  #FLUXCOM
  bin_stats_temp_fluxcom_period = stats(bin_df_temp_fluxcom_period)
  bin_stats_prep_fluxcom_period = stats(bin_df_prep_fluxcom_period)
  #sink per area
  bin_stats_temp_sink_per_area_period = stats(bin_df_temp_sink_per_area_period)
  bin_stats_prep_sink_per_area_period = stats(bin_df_prep_sink_per_area_period)
  #sink per tree
  bin_stats_temp_sink_per_tree_period = stats(bin_df_temp_sink_per_tree_period)
  bin_stats_prep_sink_per_tree_period = stats(bin_df_prep_sink_per_tree_period)
  #RS_mean
  bin_stats_temp_RS_mean_period = stats(bin_df_temp_RS_mean_period)
  bin_stats_prep_RS_mean_period = stats(bin_df_prep_RS_mean_period)  
}

#corr stats
{
  #TRENDY
  bin_stats_temp_trendy_corr = stats(bin_df_temp_trendy_corr)
  bin_stats_prep_trendy_corr = stats(bin_df_prep_trendy_corr)
  bin_stats_prep_trendy_corr = bin_stats_prep_trendy_corr[-1,]
  
  bin_stats_temp_trendyS2_corr = stats(bin_df_temp_trendyS2_corr)
  bin_stats_prep_trendyS2_corr = stats(bin_df_prep_trendyS2_corr)
  
  bin_stats_temp_trendyS3_corr = stats(bin_df_temp_trendyS3_corr)
  bin_stats_prep_trendyS3_corr = stats(bin_df_prep_trendyS3_corr)
  #FLUXCOM
  bin_stats_temp_fluxcom_corr = stats(bin_df_temp_fluxcom_corr)
  bin_stats_prep_fluxcom_corr = stats(bin_df_prep_fluxcom_corr)
  #sink per area
  bin_stats_temp_sink_per_area_corr = stats(bin_df_temp_sink_per_area_corr)
  bin_stats_prep_sink_per_area_corr = stats(bin_df_prep_sink_per_area_corr)
  #sink per tree
  bin_stats_temp_sink_per_tree_corr = stats(bin_df_temp_sink_per_tree_corr)
  bin_stats_prep_sink_per_tree_corr = stats(bin_df_prep_sink_per_tree_corr)
  #RS mean
  bin_stats_temp_RS_mean_corr = stats(bin_df_temp_RS_mean_corr)
  bin_stats_prep_RS_mean_corr = stats(bin_df_prep_RS_mean_corr)
}

#lines
{
  #parameters for plots
  {
    txt_size1 = 50
    txt_size2 = 27.5
    txt_size3 = 40
    txt_size4 = 35
    line_size1 = 4  #main lines
    line_size2 = 2  #hline 
    line_size3 = 2  #axis lines
  }

#all period
  #temp all
  {
    pdf(".\\Clim_response\\Fig.2e.pdf",width = 10,height = 20)
    
    ppp1 <- ggplot()+
      geom_ribbon(data = bin_stats_temp_trendy_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#0D2B4F",alpha = 0.2)+
      geom_ribbon(data = bin_stats_temp_fluxcom_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#6ECBD0",alpha = 0.3)+
      geom_ribbon(data = bin_stats_temp_RS_mean_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#245496",alpha = 0.2)+
      geom_ribbon(data = bin_stats_temp_sink_per_area_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                             ymax= col_max),fill = "#FFD76A",alpha = 0.3)+
      geom_line(data = bin_stats_temp_trendy_period,aes(x = seq(28,68,2), y=col_mean,colour = 'dd'),size = line_size1)+
      geom_line(data = bin_stats_temp_fluxcom_period,aes(x = seq(28,68,2), y=col_mean,colour = 'bb'),size = line_size1)+
      geom_line(data = bin_stats_temp_RS_mean_period,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
      geom_line(data = bin_stats_temp_sink_per_area_period,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+ 
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylab("Month")+
      xlab("")+
      scale_color_manual(name = '',values =c(aa="#FFD76A",bb = "#6ECBD0",cc = "#245496", dd = "#0D2B4F"), labels = c(bquote("AABI"[per_area]),bquote("GPP"[FLUXCOM]),bquote("GPP"[RS]),bquote("GPP"[TRENDY])))+
      scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      scale_y_continuous(limits = c(-12,11.5),breaks=seq(-12, 11.5, 3),labels = c("-Jan","-Apr","-Jul","-Oct","Jan","Apr","Jul","Oct"))+
      #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.175),legend.key.size = unit(1.5, "cm"),legend.spacing.y = unit(2.0, 'cm'),legend.text = element_text(size =txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size3),axis.title.x=element_text(face="bold",size=txt_size4),axis.title.y=element_text(face="bold",size=txt_size4))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    
    ppp1
    
    dev.off()
    
  } 
  #prep all
  {
    pdf(".\\Clim_response\\Fig.2f.pdf",width = 10,height = 20)
    
    ppp2 <- ggplot()+
      geom_ribbon(data = bin_stats_prep_trendy_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                          ymax= col_max),fill = "#0D2B4F",alpha = 0.2)+
      geom_ribbon(data = bin_stats_prep_fluxcom_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                           ymax= col_max),fill = "#6ECBD0",alpha = 0.3)+
      geom_ribbon(data = bin_stats_prep_RS_mean_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                           ymax= col_max),fill = "#245496",alpha = 0.2)+
      geom_ribbon(data = bin_stats_prep_sink_per_area_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#FFD76A",alpha = 0.3)+
      geom_line(data = bin_stats_prep_trendy_period,aes(x = seq(28,68,2), y=col_mean,colour = 'dd'),size = line_size1)+
      geom_line(data = bin_stats_prep_fluxcom_period,aes(x = seq(28,68,2), y=col_mean,colour = 'bb'),size = line_size1)+
      geom_line(data = bin_stats_prep_RS_mean_period,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
      geom_line(data = bin_stats_prep_sink_per_area_period,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+ 
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylab("Month")+
      xlab("")+
      scale_color_manual(name = '',values =c(aa="#FFD76A",bb = "#6ECBD0",cc = "#245496", dd = "#0D2B4F"), labels = c(bquote("AABI"[per_area]),bquote("GPP"[FLUXCOM]),bquote("GPP"[RS]),bquote("GPP"[TRENDY])))+
      scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      scale_y_continuous(limits = c(-12,11.5),breaks=seq(-12, 11.5, 3),labels = c("-Jan","-Apr","-Jul","-Oct","Jan","Apr","Jul","Oct"))+
      #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.175),legend.key.size = unit(1.5, "cm"),legend.spacing.y = unit(2.0, 'cm'),legend.text = element_text(size =txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size3),axis.title.x=element_text(face="bold",size=txt_size4),axis.title.y=element_text(face="bold",size=txt_size4))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    
    ppp2
    
    dev.off()
    
  }  
#all pcor
  #temp all pcor
  {
    pdf(".\\Clim_response\\Fig2c.pdf",width = 10,height = 20)
    
    ppp3 <- ggplot()+
      geom_ribbon(data = bin_stats_temp_trendy_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                          ymax= col_max),fill = "#481567FF",alpha = 0.2)+
      geom_ribbon(data = bin_stats_temp_fluxcom_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                           ymax= col_max),fill = "#55C667FF",alpha = 0.3)+
      geom_ribbon(data = bin_stats_temp_RS_mean_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                           ymax= col_max),fill = "#238A8DFF",alpha = 0.2)+
      geom_ribbon(data = bin_stats_temp_sink_per_area_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#FDE725FF",alpha = 0.3)+
      geom_line(data = bin_stats_temp_trendy_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'dd'),size = line_size1)+
      geom_line(data = bin_stats_temp_fluxcom_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'bb'),size = line_size1)+
      geom_line(data = bin_stats_temp_RS_mean_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
      geom_line(data = bin_stats_temp_sink_per_area_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+ 
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylim(-1.12,1.12)+
      xlab("")+
      ylab("pcor")+
      scale_color_manual(name = '', 
                         values = c(aa="#FDE725FF",bb = "#55C667FF",cc = "#238A8DFF", dd = "#481567FF"), labels = c(bquote("AABI"[per_area]),bquote("GPP"[FLUXCOM]),bquote("GPP"[RS]),bquote("GPP"[TRENDY])))+
      scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.175),legend.key.size = unit(1.5, "cm"),legend.spacing.y = unit(2.0, 'cm'),legend.text = element_text(size =txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size3),axis.title.x=element_text(face="bold",size=txt_size4),axis.title.y=element_text(face="bold",size=txt_size4))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    
    ppp3
    
    dev.off()
    
  }
  #prep all pcor
  {
  pdf(".\\Clim_response\\Fig2d.pdf",width = 10,height = 20)
    
  ppp4 <- ggplot()+
    geom_ribbon(data = bin_stats_prep_trendy_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                      ymax= col_max),fill = "#481567FF",alpha = 0.2)+
    geom_ribbon(data = bin_stats_prep_fluxcom_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                       ymax= col_max),fill = "#55C667FF",alpha = 0.3)+
    geom_ribbon(data = bin_stats_prep_RS_mean_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                       ymax= col_max),fill = "#238A8DFF",alpha = 0.2)+
    geom_ribbon(data = bin_stats_prep_sink_per_area_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                        ymax= col_max),fill = "#FDE725FF",alpha = 0.3)+
    geom_line(data = bin_stats_prep_trendy_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'dd'),size = line_size1)+
    geom_line(data = bin_stats_prep_fluxcom_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'bb'),size = line_size1)+
    geom_line(data = bin_stats_prep_RS_mean_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
    geom_line(data = bin_stats_prep_sink_per_area_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+ 
    geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
    ylim(-1.12,1.12)+
    xlab("")+
    ylab("pcor")+
    scale_color_manual(name = '', 
                       values = c(aa="#FDE725FF",bb = "#55C667FF",cc = "#238A8DFF", dd = "#481567FF"), labels = c(bquote("AABI"[per_area]),bquote("GPP"[FLUXCOM]),bquote("GPP"[RS]),bquote("GPP"[TRENDY])))+
    scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
    #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme_set(theme_classic())+
    theme(legend.position = c(0.5,0.175),legend.key.size = unit(1.5, "cm"),legend.spacing.y = unit(2.0, 'cm'),legend.text = element_text(size =txt_size1,face="bold"))+
    theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size3),axis.title.x=element_text(face="bold",size=txt_size4),axis.title.y=element_text(face="bold",size=txt_size4))+
    theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
    theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
    coord_flip()
  
  ppp4
  
  dev.off()
  }
}
