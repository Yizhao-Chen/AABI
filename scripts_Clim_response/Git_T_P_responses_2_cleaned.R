##########################################################################################
#This script is to get GPP and AABI curve along T&P gradient
#Input data extraction was performed in Arc GIS 10.5
##########################################################################################

library("ggplot2")
library("tidyverse")
library(Metrics)
library("mgcv")

#Comparison of absolute per tree growth and TRENDY GPP
#Temp
  {
Summary_data = read.csv(".\\Clim_response\\summary_GLC_AABI_GPP_Temp1.CSV")

#data binning
# set up cut-off values 
#breaks <- seq(260,306,2)
#breaks for 2 degree
#breaks <- seq(-2,28,2)
 breaks <- c(-2,seq(2,24,2),28)
#breaks for 4 degree
#breaks <- seq(-4,30,4)


# specify interval/bin labels
#temperature
#tags <- c("[260-262)","[262-264)", "[264-266)", "[266-268)", "[268-270)", "[270-272)","[272-274)", "[274-276)","[276-278)","[278-280)","[280-282)","[282-284)","[284-286)","[286-288)","[288-290)","[290-292)","[292-294)","[294-296)","[296-298)","[298-300)","[300-302)","[302-304)","[304-306)")

#breaks for 2 degree
# tags <- c("[-2-0)","[0-2)", "[2-4)", "[4-6)", "[6-8)", "[8-10)","[10-12)", "[12-14)","[14-16)","[16-18)","[18-20)","[20-22)","[22-24)","[24-26)","[26-28)")
# 
# tags1 <- c("-1","1","3","5","7","9","11","13","15","17","19","21","23","25","27")
 
 tags <- c("[-2-2)","[2-4)", "[4-6)", "[6-8)", "[8-10)", "[10-12)","[12-14)", "[14-16)","[16-18)","[18-20)","[20-22)","[22-24)","[24-28)")

 tags1 <- c("0","3","5","7","9","11","13","15","17","19","21","23","26")

#breaks for 4 degree
#tags <- c("[-4-0)","[0-4)", "[4-8)", "[8-12)", "[12-16)", "[16-20)","[20-24)", "[24-28)","[28-32)")

#tags1 <- c("-2","2","6","10","14","18","22","26","30")

# bucketing values into bins
group_tags_T <- cut(Summary_data$T_mean_C, 
                    breaks=breaks, 
                    include.lowest=FALSE, 
                    right=FALSE, 
                    labels=tags)
# inspect bins
summary(group_tags_T)

#Temperature TRENDY
{
  #temp_group <- factor(group_tags,levels = labels,ordered = TRUE)

  # v_TRENDY_T <- Summary_data %>% select(TRENDY_GPP,T_mean_C)
  # vgroup_TRENDY_T <- as_tibble(v_TRENDY_T) %>%
  #   mutate(tag = case_when(
  #     T_mean_C >= -2 & T_mean_C < 0 ~ tags1[1],
  #     T_mean_C >= 0 & T_mean_C < 2 ~ tags1[2],
  #     T_mean_C >= 2 & T_mean_C < 4 ~ tags1[3],
  #     T_mean_C >= 4 & T_mean_C < 6 ~ tags1[4],
  #     T_mean_C >= 6 & T_mean_C < 8 ~ tags1[5],
  #     T_mean_C >= 8 & T_mean_C < 10 ~ tags1[6],
  #     T_mean_C >= 10 & T_mean_C < 12 ~ tags1[7],
  #     T_mean_C >= 12 & T_mean_C < 14 ~ tags1[8],
  #     T_mean_C >= 14 & T_mean_C < 16  ~ tags1[9],
  #     T_mean_C >= 16 & T_mean_C < 18 ~ tags1[10],
  #     T_mean_C >= 18 & T_mean_C < 20 ~ tags1[11],
  #     T_mean_C >= 20 & T_mean_C < 22 ~ tags1[12],
  #     T_mean_C >= 22 & T_mean_C < 24 ~ tags1[13],
  #     T_mean_C >= 24 & T_mean_C < 26 ~ tags1[14],
  #     T_mean_C >= 26 & T_mean_C < 28  ~ tags1[15]
  #   ))
  
  
  v_TRENDY_T <- Summary_data %>% select(TRENDY_GPP,T_mean_C)
  vgroup_TRENDY_T <- as_tibble(v_TRENDY_T) %>%
    mutate(tag = case_when(
      T_mean_C >= -2 & T_mean_C < 2 ~ tags1[1],
      T_mean_C >= 2 & T_mean_C < 4 ~ tags1[2],
      T_mean_C >= 4 & T_mean_C < 6 ~ tags1[3],
      T_mean_C >= 6 & T_mean_C < 8 ~ tags1[4],
      T_mean_C >= 8 & T_mean_C < 10 ~ tags1[5],
      T_mean_C >= 10 & T_mean_C < 12 ~ tags1[6],
      T_mean_C >= 12 & T_mean_C < 14 ~ tags1[7],
      T_mean_C >= 14 & T_mean_C < 16  ~ tags1[8],
      T_mean_C >= 16 & T_mean_C < 18 ~ tags1[9],
      T_mean_C >= 18 & T_mean_C < 20 ~ tags1[10],
      T_mean_C >= 20 & T_mean_C < 22 ~ tags1[11],
      T_mean_C >= 22 & T_mean_C < 24 ~ tags1[12],
      T_mean_C >= 24 & T_mean_C < 28 ~ tags1[13]
    ))
  
}
#Temperature growth
{
  #temp_group <- factor(group_tags,levels = labels,ordered = TRUE)
  
  # v_growth_T <- Summary_data %>% select(North_America_per_tree,T_mean_C)
  # vgroup_growth_T <- as_tibble(v_growth_T) %>% 
  #   mutate(tag = case_when(
  #     T_mean_C >= -2 & T_mean_C < 2 ~ tags1[1],
  #     T_mean_C >= 0 & T_mean_C < 2 ~ tags1[2],
  #     T_mean_C >= 4 & T_mean_C < 4 ~ tags1[3],
  #     T_mean_C >= 8 & T_mean_C < 6 ~ tags1[4],
  #     T_mean_C >= 12 & T_mean_C < 8 ~ tags1[5],
  #     T_mean_C >= 16 & T_mean_C < 10 ~ tags1[6],
  #     T_mean_C >= 20 & T_mean_C < 12 ~ tags1[7],
  #     T_mean_C >= 24 & T_mean_C < 14 ~ tags1[8],
  #     T_mean_C >= 28 & T_mean_C < 16  ~ tags1[9],
  #     T_mean_C >= 16 & T_mean_C < 18 ~ tags1[10],
  #     T_mean_C >= 18 & T_mean_C < 20 ~ tags1[11],
  #     T_mean_C >= 20 & T_mean_C < 22 ~ tags1[12],
  #     T_mean_C >= 22 & T_mean_C < 24 ~ tags1[13],
  #     T_mean_C >= 24 & T_mean_C < 26 ~ tags1[14],
  #     T_mean_C >= 26 & T_mean_C < 28  ~ tags1[15]
  #   ))
  
  
  v_growth_T <- Summary_data %>% select(North_America_per_tree1,T_mean_C)
  vgroup_growth_T <- as_tibble(v_growth_T) %>% 
    mutate(tag = case_when(
      T_mean_C >= -2 & T_mean_C < 2 ~ tags1[1],
      T_mean_C >= 2 & T_mean_C < 4 ~ tags1[2],
      T_mean_C >= 4 & T_mean_C < 6 ~ tags1[3],
      T_mean_C >= 6 & T_mean_C < 8 ~ tags1[4],
      T_mean_C >= 8 & T_mean_C < 10 ~ tags1[5],
      T_mean_C >= 10 & T_mean_C < 12 ~ tags1[6],
      T_mean_C >= 12 & T_mean_C < 14 ~ tags1[7],
      T_mean_C >= 14 & T_mean_C < 16  ~ tags1[8],
      T_mean_C >= 16 & T_mean_C < 18 ~ tags1[9],
      T_mean_C >= 18 & T_mean_C < 20 ~ tags1[10],
      T_mean_C >= 20 & T_mean_C < 22 ~ tags1[11],
      T_mean_C >= 22 & T_mean_C < 24 ~ tags1[12],
      T_mean_C >= 24 & T_mean_C < 28 ~ tags1[13]
    ))
  
}
#Temperature RS
{
  #temp_group <- factor(group_tags,levels = labels,ordered = TRUE)
  
  # v_growth_T <- Summary_data %>% select(North_America_per_tree,T_mean_C)
  # vgroup_growth_T <- as_tibble(v_growth_T) %>% 
  #   mutate(tag = case_when(
  #     T_mean_C >= -2 & T_mean_C < 2 ~ tags1[1],
  #     T_mean_C >= 0 & T_mean_C < 2 ~ tags1[2],
  #     T_mean_C >= 4 & T_mean_C < 4 ~ tags1[3],
  #     T_mean_C >= 8 & T_mean_C < 6 ~ tags1[4],
  #     T_mean_C >= 12 & T_mean_C < 8 ~ tags1[5],
  #     T_mean_C >= 16 & T_mean_C < 10 ~ tags1[6],
  #     T_mean_C >= 20 & T_mean_C < 12 ~ tags1[7],
  #     T_mean_C >= 24 & T_mean_C < 14 ~ tags1[8],
  #     T_mean_C >= 28 & T_mean_C < 16  ~ tags1[9],
  #     T_mean_C >= 16 & T_mean_C < 18 ~ tags1[10],
  #     T_mean_C >= 18 & T_mean_C < 20 ~ tags1[11],
  #     T_mean_C >= 20 & T_mean_C < 22 ~ tags1[12],
  #     T_mean_C >= 22 & T_mean_C < 24 ~ tags1[13],
  #     T_mean_C >= 24 & T_mean_C < 26 ~ tags1[14],
  #     T_mean_C >= 26 & T_mean_C < 28  ~ tags1[15]
  #   ))
  
  
  v_RS_T <- Summary_data %>% select(RS_mean,T_mean_C)
  vgroup_RS_T <- as_tibble(v_RS_T) %>% 
    mutate(tag = case_when(
      T_mean_C >= -2 & T_mean_C < 2 ~ tags1[1],
      T_mean_C >= 2 & T_mean_C < 4 ~ tags1[2],
      T_mean_C >= 4 & T_mean_C < 6 ~ tags1[3],
      T_mean_C >= 6 & T_mean_C < 8 ~ tags1[4],
      T_mean_C >= 8 & T_mean_C < 10 ~ tags1[5],
      T_mean_C >= 10 & T_mean_C < 12 ~ tags1[6],
      T_mean_C >= 12 & T_mean_C < 14 ~ tags1[7],
      T_mean_C >= 14 & T_mean_C < 16  ~ tags1[8],
      T_mean_C >= 16 & T_mean_C < 18 ~ tags1[9],
      T_mean_C >= 18 & T_mean_C < 20 ~ tags1[10],
      T_mean_C >= 20 & T_mean_C < 22 ~ tags1[11],
      T_mean_C >= 22 & T_mean_C < 24 ~ tags1[12],
      T_mean_C >= 24 & T_mean_C < 28 ~ tags1[13]
    ))
  
}
#Temperature FLUXCOM
{
  #temp_group <- factor(group_tags,levels = labels,ordered = TRUE)
  
  # v_growth_T <- Summary_data %>% select(North_America_per_tree,T_mean_C)
  # vgroup_growth_T <- as_tibble(v_growth_T) %>% 
  #   mutate(tag = case_when(
  #     T_mean_C >= -2 & T_mean_C < 2 ~ tags1[1],
  #     T_mean_C >= 0 & T_mean_C < 2 ~ tags1[2],
  #     T_mean_C >= 4 & T_mean_C < 4 ~ tags1[3],
  #     T_mean_C >= 8 & T_mean_C < 6 ~ tags1[4],
  #     T_mean_C >= 12 & T_mean_C < 8 ~ tags1[5],
  #     T_mean_C >= 16 & T_mean_C < 10 ~ tags1[6],
  #     T_mean_C >= 20 & T_mean_C < 12 ~ tags1[7],
  #     T_mean_C >= 24 & T_mean_C < 14 ~ tags1[8],
  #     T_mean_C >= 28 & T_mean_C < 16  ~ tags1[9],
  #     T_mean_C >= 16 & T_mean_C < 18 ~ tags1[10],
  #     T_mean_C >= 18 & T_mean_C < 20 ~ tags1[11],
  #     T_mean_C >= 20 & T_mean_C < 22 ~ tags1[12],
  #     T_mean_C >= 22 & T_mean_C < 24 ~ tags1[13],
  #     T_mean_C >= 24 & T_mean_C < 26 ~ tags1[14],
  #     T_mean_C >= 26 & T_mean_C < 28  ~ tags1[15]
  #   ))
  
  
  v_FLUXCOM_T <- Summary_data %>% select(FLUXCOM_GPP,T_mean_C)
  vgroup_FLUXCOM_T <- as_tibble(v_FLUXCOM_T) %>% 
    mutate(tag = case_when(
      T_mean_C >= -2 & T_mean_C < 2 ~ tags1[1],
      T_mean_C >= 2 & T_mean_C < 4 ~ tags1[2],
      T_mean_C >= 4 & T_mean_C < 6 ~ tags1[3],
      T_mean_C >= 6 & T_mean_C < 8 ~ tags1[4],
      T_mean_C >= 8 & T_mean_C < 10 ~ tags1[5],
      T_mean_C >= 10 & T_mean_C < 12 ~ tags1[6],
      T_mean_C >= 12 & T_mean_C < 14 ~ tags1[7],
      T_mean_C >= 14 & T_mean_C < 16  ~ tags1[8],
      T_mean_C >= 16 & T_mean_C < 18 ~ tags1[9],
      T_mean_C >= 18 & T_mean_C < 20 ~ tags1[10],
      T_mean_C >= 20 & T_mean_C < 22 ~ tags1[11],
      T_mean_C >= 22 & T_mean_C < 24 ~ tags1[12],
      T_mean_C >= 24 & T_mean_C < 28 ~ tags1[13]
    ))
}
}

#Prep
  {
Summary_data1 = read.csv(".\\Clim_response\\summary_GLC_AABI_GPP_Prep1.CSV")

    #data binning
    # set up cut-off values 
    breaks <- c(0,seq(120,780,60),900)
    #breaks for 4 degree
    #breaks <- seq(-4,30,4)
    
    
    # specify interval/bin labels
    #temperature
    # tags1 <- c("60","120","180","240","300","360","420","480","540","600","660","720","780","840","900")
    tags <- c("[0-120)","[120-180)", "[180-240)", "[240-300)", "[300-360)", "[360-420)","[420-480)", "[480-540)","[540-600)","[600-660)","[660-720)","[720-780)","[780-900)")

    tags1 <- c("60","150","210","270","330","390","450","510","570","630","690","750","840")
    #breaks for 4 degree
    # tags <- c("[-4-0)","[0-4)", "[4-8)", "[8-12)", "[12-16)", "[16-20)","[20-24)", "[24-28)","[28-32)")
    # 
    # tags1 <- c("-2","2","6","10","14","18","22","26","30")
    
    # bucketing values into bins
    group_tags_P <- cut(Summary_data1$P_4n183, 
                        breaks=breaks, 
                        include.lowest=FALSE, 
                        right=FALSE, 
                        labels=tags)
    # inspect bins
    summary(group_tags_P)
    
    #Prep TRENDY
    {
      #temp_group <- factor(group_tags,levels = labels,ordered = TRUE)
      
      # v_TRENDY_P <- Summary_data1 %>% select(TRENDY_GPP,P_4n183)
      # vgroup_TRENDY_P <- as_tibble(v_TRENDY_P) %>% 
      #   mutate(tag = case_when(
      #     P_4n183 >= 30 & P_4n183 < 90 ~ tags1[1],
      #     P_4n183 >= 90 & P_4n183 < 150 ~ tags1[2],
      #     P_4n183 >= 150 & P_4n183 < 210 ~ tags1[3],
      #     P_4n183 >= 210 & P_4n183 < 270 ~ tags1[4],
      #     P_4n183 >= 270 & P_4n183 < 330 ~ tags1[5],
      #     P_4n183 >= 330 & P_4n183 < 390 ~ tags1[6],
      #     P_4n183 >= 390 & P_4n183 < 450 ~ tags1[7],
      #     P_4n183 >= 450 & P_4n183 < 510 ~ tags1[8],
      #     P_4n183 >= 510 & P_4n183 < 570  ~ tags1[9],
      #     P_4n183 >= 570 & P_4n183 < 630 ~ tags1[10],
      #     P_4n183 >= 630 & P_4n183 < 690 ~ tags1[11],
      #     P_4n183 >= 690 & P_4n183 < 750 ~ tags1[12],
      #     P_4n183 >= 750 & P_4n183 < 810 ~ tags1[13],
      #     P_4n183 >= 810 & P_4n183 < 870 ~ tags1[14],
      #     P_4n183 >= 870 & P_4n183 < 930  ~ tags1[15]    
      #   ))
      
      v_TRENDY_P <- Summary_data1 %>% select(TRENDY_GPP,P_4n183)
      vgroup_TRENDY_P <- as_tibble(v_TRENDY_P) %>% 
        mutate(tag = case_when(
          P_4n183 >= 0 & P_4n183 < 120 ~ tags1[1],
          P_4n183 >= 120 & P_4n183 < 180 ~ tags1[2],
          P_4n183 >= 180 & P_4n183 < 240 ~ tags1[3],
          P_4n183 >= 240 & P_4n183 < 300 ~ tags1[4],
          P_4n183 >= 300 & P_4n183 < 360 ~ tags1[5],
          P_4n183 >= 360 & P_4n183 < 420 ~ tags1[6],
          P_4n183 >= 420 & P_4n183 < 480 ~ tags1[7],
          P_4n183 >= 480 & P_4n183 < 540 ~ tags1[8],
          P_4n183 >= 540 & P_4n183 < 600  ~ tags1[9],
          P_4n183 >= 600 & P_4n183 < 660 ~ tags1[10],
          P_4n183 >= 660 & P_4n183 < 720 ~ tags1[11],
          P_4n183 >= 720 & P_4n183 < 780 ~ tags1[12],
          P_4n183 >= 780 & P_4n183 < 900 ~ tags1[13]
        ))
      
    }
    #Prep growth
    {
      v_growth_P <- Summary_data1 %>% select(North_America_per_tree1,P_4n183)
      vgroup_growth_P <- as_tibble(v_growth_P) %>% 
        mutate(tag = case_when(
          P_4n183 >= 0 & P_4n183 < 120 ~ tags1[1],
          P_4n183 >= 120 & P_4n183 < 180 ~ tags1[2],
          P_4n183 >= 180 & P_4n183 < 240 ~ tags1[3],
          P_4n183 >= 240 & P_4n183 < 300 ~ tags1[4],
          P_4n183 >= 300 & P_4n183 < 360 ~ tags1[5],
          P_4n183 >= 360 & P_4n183 < 420 ~ tags1[6],
          P_4n183 >= 420 & P_4n183 < 480 ~ tags1[7],
          P_4n183 >= 480 & P_4n183 < 540 ~ tags1[8],
          P_4n183 >= 540 & P_4n183 < 600  ~ tags1[9],
          P_4n183 >= 600 & P_4n183 < 660 ~ tags1[10],
          P_4n183 >= 660 & P_4n183 < 720 ~ tags1[11],
          P_4n183 >= 720 & P_4n183 < 780 ~ tags1[12],
          P_4n183 >= 780 & P_4n183 < 900 ~ tags1[13]
      
        ))
      
    }
    #Prep RS
    {
      #temp_group <- factor(group_tags,levels = labels,ordered = TRUE)
      
      # v_TRENDY_P <- Summary_data1 %>% select(TRENDY_GPP,P_4n183)
      # vgroup_TRENDY_P <- as_tibble(v_TRENDY_P) %>% 
      #   mutate(tag = case_when(
      #     P_4n183 >= 30 & P_4n183 < 90 ~ tags1[1],
      #     P_4n183 >= 90 & P_4n183 < 150 ~ tags1[2],
      #     P_4n183 >= 150 & P_4n183 < 210 ~ tags1[3],
      #     P_4n183 >= 210 & P_4n183 < 270 ~ tags1[4],
      #     P_4n183 >= 270 & P_4n183 < 330 ~ tags1[5],
      #     P_4n183 >= 330 & P_4n183 < 390 ~ tags1[6],
      #     P_4n183 >= 390 & P_4n183 < 450 ~ tags1[7],
      #     P_4n183 >= 450 & P_4n183 < 510 ~ tags1[8],
      #     P_4n183 >= 510 & P_4n183 < 570  ~ tags1[9],
      #     P_4n183 >= 570 & P_4n183 < 630 ~ tags1[10],
      #     P_4n183 >= 630 & P_4n183 < 690 ~ tags1[11],
      #     P_4n183 >= 690 & P_4n183 < 750 ~ tags1[12],
      #     P_4n183 >= 750 & P_4n183 < 810 ~ tags1[13],
      #     P_4n183 >= 810 & P_4n183 < 870 ~ tags1[14],
      #     P_4n183 >= 870 & P_4n183 < 930  ~ tags1[15]    
      #   ))
      
      v_RS_P <- Summary_data1 %>% select(RS_mean,P_4n183)
      vgroup_RS_P <- as_tibble(v_RS_P) %>% 
        mutate(tag = case_when(
          P_4n183 >= 0 & P_4n183 < 120 ~ tags1[1],
          P_4n183 >= 120 & P_4n183 < 180 ~ tags1[2],
          P_4n183 >= 180 & P_4n183 < 240 ~ tags1[3],
          P_4n183 >= 240 & P_4n183 < 300 ~ tags1[4],
          P_4n183 >= 300 & P_4n183 < 360 ~ tags1[5],
          P_4n183 >= 360 & P_4n183 < 420 ~ tags1[6],
          P_4n183 >= 420 & P_4n183 < 480 ~ tags1[7],
          P_4n183 >= 480 & P_4n183 < 540 ~ tags1[8],
          P_4n183 >= 540 & P_4n183 < 600  ~ tags1[9],
          P_4n183 >= 600 & P_4n183 < 660 ~ tags1[10],
          P_4n183 >= 660 & P_4n183 < 720 ~ tags1[11],
          P_4n183 >= 720 & P_4n183 < 780 ~ tags1[12],
          P_4n183 >= 780 & P_4n183 < 900 ~ tags1[13]
        ))
      
    }    
    #Prep FLUXCOM
    {
      #temp_group <- factor(group_tags,levels = labels,ordered = TRUE)
      
      # v_TRENDY_P <- Summary_data1 %>% select(TRENDY_GPP,P_4n183)
      # vgroup_TRENDY_P <- as_tibble(v_TRENDY_P) %>% 
      #   mutate(tag = case_when(
      #     P_4n183 >= 30 & P_4n183 < 90 ~ tags1[1],
      #     P_4n183 >= 90 & P_4n183 < 150 ~ tags1[2],
      #     P_4n183 >= 150 & P_4n183 < 210 ~ tags1[3],
      #     P_4n183 >= 210 & P_4n183 < 270 ~ tags1[4],
      #     P_4n183 >= 270 & P_4n183 < 330 ~ tags1[5],
      #     P_4n183 >= 330 & P_4n183 < 390 ~ tags1[6],
      #     P_4n183 >= 390 & P_4n183 < 450 ~ tags1[7],
      #     P_4n183 >= 450 & P_4n183 < 510 ~ tags1[8],
      #     P_4n183 >= 510 & P_4n183 < 570  ~ tags1[9],
      #     P_4n183 >= 570 & P_4n183 < 630 ~ tags1[10],
      #     P_4n183 >= 630 & P_4n183 < 690 ~ tags1[11],
      #     P_4n183 >= 690 & P_4n183 < 750 ~ tags1[12],
      #     P_4n183 >= 750 & P_4n183 < 810 ~ tags1[13],
      #     P_4n183 >= 810 & P_4n183 < 870 ~ tags1[14],
      #     P_4n183 >= 870 & P_4n183 < 930  ~ tags1[15]    
      #   ))
      
      v_FLUXCOM_P <- Summary_data1 %>% select(FLUXCOM,P_4n183)
      vgroup_FLUXCOM_P <- as_tibble(v_FLUXCOM_P) %>% 
        mutate(tag = case_when(
          P_4n183 >= 0 & P_4n183 < 120 ~ tags1[1],
          P_4n183 >= 120 & P_4n183 < 180 ~ tags1[2],
          P_4n183 >= 180 & P_4n183 < 240 ~ tags1[3],
          P_4n183 >= 240 & P_4n183 < 300 ~ tags1[4],
          P_4n183 >= 300 & P_4n183 < 360 ~ tags1[5],
          P_4n183 >= 360 & P_4n183 < 420 ~ tags1[6],
          P_4n183 >= 420 & P_4n183 < 480 ~ tags1[7],
          P_4n183 >= 480 & P_4n183 < 540 ~ tags1[8],
          P_4n183 >= 540 & P_4n183 < 600  ~ tags1[9],
          P_4n183 >= 600 & P_4n183 < 660 ~ tags1[10],
          P_4n183 >= 660 & P_4n183 < 720 ~ tags1[11],
          P_4n183 >= 720 & P_4n183 < 780 ~ tags1[12],
          P_4n183 >= 780 & P_4n183 < 900 ~ tags1[13]
        ))
      
    }
}
  
#====================================================================================
#this part of the code is to check the goodness of the stat_smooth fitting
line <- ggplot() +
  #stat_smooth(data = vgroup_FLUXCOM_T, aes(x = as.numeric(tag),y=FLUXCOM_GPP),n=827)
  #stat_smooth(data = vgroup_growth_T, aes(x = as.numeric(tag),y=North_America_per_tree1),n=827)
  #stat_smooth(data = vgroup_RS_T, aes(x = as.numeric(tag),y=RS_mean),n=827)
  #stat_smooth(data = vgroup_TRENDY_T, aes(x = as.numeric(tag),y=TRENDY_GPP),n=827)
  #stat_smooth(data = vgroup_FLUXCOM_P, aes(x = as.numeric(tag),y=FLUXCOM),n=827)
stat_smooth(data = vgroup_growth_P, aes(x = as.numeric(tag),y=North_America_per_tree1),n=827)
#stat_smooth(data = vgroup_RS_P, aes(x = as.numeric(tag),y=RS_mean),n=827)
#stat_smooth(data = vgroup_TRENDY_P, aes(x = as.numeric(tag),y=TRENDY_GPP),n=827)

#test with high value excluded
{
  line1 <- ggplot() +
    stat_smooth(data = vgroup_growth_T, aes(x = as.numeric(tag),y=North_America_per_tree1),n=827)
  
  line2 <- ggplot() +
    stat_smooth(data = vgroup_growth_P, aes(x = as.numeric(tag),y=North_America_per_tree1),n=827) 
  
  vgroup_growth_T1 = vgroup_growth_T %>%
    filter(vgroup_growth_T <40)

  vgroup_growth_P1 = vgroup_growth_P %>%
    filter(vgroup_growth_P <40)

  line11 <- ggplot() +
    stat_smooth(data = vgroup_growth_T1, aes(x = as.numeric(tag),y=North_America_per_tree1),n=826)

  line22 <- ggplot() +
    stat_smooth(data = vgroup_growth_P1, aes(x = as.numeric(tag),y=North_America_per_tree1),n=826)

  
  ggplot_build(line1)
  vgroup_growth_T = vgroup_growth_T[order(vgroup_growth_T$T_mean_C),]
  vgroup_growth_T$AABI_fitted = ggplot_build(line1)$data[[1]]$y
  vgroup_growth_T = vgroup_growth_T %>%
    filter(vgroup_growth_T <40)
  
  ggplot_build(line11)
  vgroup_growth_T1 = vgroup_growth_T1[order(vgroup_growth_T1$T_mean_C),]
  vgroup_growth_T1$AABI_fitted = ggplot_build(line11)$data[[1]]$y
  
  cor(vgroup_growth_T$AABI_fitted,vgroup_growth_T1$AABI_fitted)
  
  ggplot_build(line2)
  vgroup_growth_P = vgroup_growth_P[order(vgroup_growth_P$P_4n183),]
  vgroup_growth_P$AABI_fitted = ggplot_build(line2)$data[[1]]$y
  vgroup_growth_P = vgroup_growth_P %>%
    filter(vgroup_growth_P <40)
  
  ggplot_build(line22)
  vgroup_growth_P1 = vgroup_growth_P1[order(vgroup_growth_P1$P_4n183),]
  vgroup_growth_P1$AABI_fitted = ggplot_build(line22)$data[[1]]$y
  
  cor(vgroup_growth_P$AABI_fitted,vgroup_growth_P1$AABI_fitted)

}

#Temp
{
#FLUXCOM
ggplot_build(line)
vgroup_FLUXCOM_T = vgroup_FLUXCOM_T[order(vgroup_FLUXCOM_T$T_mean_C),]
vgroup_FLUXCOM_T$FLUXCOM_GPP_fitted = ggplot_build(line)$data[[1]]$y
cor(vgroup_FLUXCOM_T$FLUXCOM_GPP_fitted,vgroup_FLUXCOM_T$FLUXCOM_GPP)
rmse(vgroup_FLUXCOM_T$FLUXCOM_GPP_fitted,vgroup_FLUXCOM_T$FLUXCOM_GPP)
#AABI
ggplot_build(line)
vgroup_growth_T = vgroup_growth_T[order(vgroup_growth_T$T_mean_C),]
vgroup_growth_T$AABI_fitted = ggplot_build(line)$data[[1]]$y
cor(vgroup_growth_T$AABI_fitted,vgroup_growth_T$North_America_per_tree1)
rmse(vgroup_growth_T$AABI_fitted,vgroup_growth_T$North_America_per_tree1)
#AABI exclude high value
ggplot_build(line)
vgroup_growth_T1 = vgroup_growth_T1[order(vgroup_growth_T1$T_mean_C),]
vgroup_growth_T1$AABI_fitted = ggplot_build(line)$data[[1]]$y
cor(vgroup_growth_T1$AABI_fitted,vgroup_growth_T1$North_America_per_tree1)
rmse(vgroup_growth_T1$AABI_fitted,vgroup_growth_T1$North_America_per_tree1)
#RS
ggplot_build(line)
vgroup_RS_T = vgroup_RS_T[order(vgroup_RS_T$T_mean_C),]
vgroup_RS_T$RS_GPP_fitted = ggplot_build(line)$data[[1]]$y
cor(vgroup_RS_T$RS_GPP_fitted,vgroup_RS_T$RS_mean)
rmse(vgroup_RS_T$RS_GPP_fitted,vgroup_RS_T$RS_mean)
#TRENDY
ggplot_build(line)
vgroup_TRENDY_T = vgroup_TRENDY_T[order(vgroup_TRENDY_T$T_mean_C),]
vgroup_TRENDY_T$TRENDY_GPP_fitted = ggplot_build(line)$data[[1]]$y
cor(vgroup_TRENDY_T$TRENDY_GPP_fitted,vgroup_TRENDY_T$TRENDY_GPP)
rmse(vgroup_TRENDY_T$TRENDY_GPP_fitted,vgroup_TRENDY_T$TRENDY_GPP)
}
#Prep
{
  #FLUXCOM
  ggplot_build(line)
  vgroup_FLUXCOM_P = vgroup_FLUXCOM_P[order(vgroup_FLUXCOM_P$P_4n183),]
  vgroup_FLUXCOM_P$FLUXCOM_GPP_fitted = ggplot_build(line)$data[[1]]$y
  cor(vgroup_FLUXCOM_P$FLUXCOM_GPP_fitted,vgroup_FLUXCOM_P$FLUXCOM)
  rmse(vgroup_FLUXCOM_P$FLUXCOM_GPP_fitted,vgroup_FLUXCOM_P$FLUXCOM)  
  #AABI
  ggplot_build(line)
  vgroup_growth_P = vgroup_growth_P[order(vgroup_growth_P$P_4n183),]
  vgroup_growth_P$AABI_fitted = ggplot_build(line)$data[[1]]$y
  cor(vgroup_growth_P$AABI_fitted,vgroup_growth_P$North_America_per_tree1)
  rmse(vgroup_growth_P$AABI_fitted,vgroup_growth_P$North_America_per_tree1)
  #AABI exclude high value
  ggplot_build(line)
  vgroup_growth_P1 = vgroup_growth_P1[order(vgroup_growth_P1$T_mean_C),]
  vgroup_growth_P1$AABI_fitted = ggplot_build(line)$data[[1]]$y
  cor(vgroup_growth_P1$AABI_fitted,vgroup_growth_P1$North_America_per_tree1)
  rmse(vgroup_growth_P1$AABI_fitted,vgroup_growth_P1$North_America_per_tree1)
  #RS
  ggplot_build(line)
  vgroup_RS_P = vgroup_RS_P[order(vgroup_RS_P$P_4n183),]
  vgroup_RS_P$RS_GPP_fitted = ggplot_build(line)$data[[1]]$y
  cor(vgroup_RS_P$RS_GPP_fitted,vgroup_RS_P$RS_mean)
  rmse(vgroup_RS_P$RS_GPP_fitted,vgroup_RS_P$RS_mean)
  #TRENDY
  ggplot_build(line)
  vgroup_TRENDY_P = vgroup_TRENDY_P[order(vgroup_TRENDY_P$P_4n183),]
  vgroup_TRENDY_P$TRENDY_GPP_fitted = ggplot_build(line)$data[[1]]$y
  cor(vgroup_TRENDY_P$TRENDY_GPP_fitted,vgroup_TRENDY_P$TRENDY_GPP)
  rmse(vgroup_TRENDY_P$TRENDY_GPP_fitted,vgroup_TRENDY_P$TRENDY_GPP)
}
#===================================================================================

#Dual y-axis plot

pdf(".\\Clim_response\\Fig3a.pdf",width = 12,height = 6)
p111 <- ggplot() + 
  geom_point(data = vgroup_FLUXCOM_T,aes(x = as.numeric(T_mean_C),y = FLUXCOM_GPP,color = "b"),alpha =0.3)+
  geom_point(data = vgroup_RS_T,aes(x = as.numeric(T_mean_C),y = RS_mean,color = "c"),alpha =0.3)+
  geom_point(data = vgroup_TRENDY_T,aes(x = as.numeric(T_mean_C),y = TRENDY_GPP,color = "d"),alpha =0.3)+
  geom_point(data = vgroup_growth_T,aes(x = as.numeric(T_mean_C),y = North_America_per_tree1/20,color = "a"),alpha =0.3)+
  stat_smooth(data = vgroup_FLUXCOM_T, aes(x = as.numeric(tag),y=FLUXCOM_GPP,color="b"),n=827,alpha = 0.5,size = 1.25, show.legend =TRUE)+
  stat_smooth(data = vgroup_RS_T, aes(x = as.numeric(tag),y=RS_mean,color="c"),n=827,alpha = 0.5,size = 1.25, show.legend =TRUE)+
  stat_smooth(data = vgroup_TRENDY_T, aes(x = as.numeric(tag),y=TRENDY_GPP,color ="d"),n=827,alpha = 0.5,size = 1.25, show.legend =TRUE)+
  stat_smooth(data = vgroup_growth_T, aes(x = as.numeric(tag),y=North_America_per_tree1/20,color ="a"),n=827,alpha = 0.5,size = 1.25, show.legend =TRUE)+
  scale_x_continuous(breaks = c(0,5,10,15,20,25))+
  scale_y_continuous(limits = c(0,2.6),breaks = c(0,0.5,1.0,1.5,2.0,2.5),sec.axis = sec_axis(~.*20, name = "AABI"["per_tree"]~"/"~"kg/tree",breaks = c(0,10,20,30,40,50)))+
  labs(x= expression("T"["grow"]~"/"~degree*"C"),y="GPP"~"/"~"kg/m"^2)+
  scale_colour_manual(name="", values=c("a"="#F57f17","b"="#7AD151FF","c" = "#1976D2","d"="#311B92"),labels = c(expression("AABI"["per_tree"]),expression("GPP"["FLUXCOM"]),expression("GPP"["RS"]),expression("GPP"["TRENDY"])))+
  theme_bw()+
  theme(legend.position = "bottom",legend.direction = "horizontal",legend.text = element_text(size =18,face="bold"))+
  theme(axis.text = element_text(size =26),axis.text.x = element_text(size =26),axis.text.y = element_text(size =26),axis.title.x=element_text(size=26),axis.title.y=element_text(size=26))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))

p111
dev.off()


pdf(".\\Clim_response\\Fig3b.pdf",width = 12,height = 6)
p222 <- ggplot() + 
  geom_point(data = vgroup_FLUXCOM_P,aes(x = as.numeric(P_4n183),y = FLUXCOM,color = "b"),alpha =0.3)+
  geom_point(data = vgroup_RS_P,aes(x = as.numeric(P_4n183),y = RS_mean,color = "c"),alpha =0.3)+
  geom_point(data = vgroup_TRENDY_P,aes(x = as.numeric(P_4n183),y = TRENDY_GPP,color = "d"),alpha =0.3)+
  geom_point(data = vgroup_growth_P,aes(x = as.numeric(P_4n183),y = North_America_per_tree1/20,color = "a"),alpha =0.3)+
  stat_smooth(data = vgroup_FLUXCOM_P, aes(x = as.numeric(tag),y=FLUXCOM,color="b",linetype = "bb"),n=827,alpha = 0.5,size = 1.25, show.legend =TRUE)+
  stat_smooth(data = vgroup_RS_P, aes(x = as.numeric(tag),y=RS_mean,color="c",linetype = "cc"),n=827,alpha = 0.5,size = 1.25, show.legend =TRUE)+
  stat_smooth(data = vgroup_TRENDY_P, aes(x = as.numeric(tag),y=TRENDY_GPP,color ="d",linetype = "dd"),n=827,alpha = 0.5,size = 1.25, show.legend =TRUE)+
  stat_smooth(data = vgroup_growth_P, aes(x = as.numeric(tag),y=North_America_per_tree1/20,color ="a",linetype = "aa"),n=827,alpha = 0.5,size = 1.25, show.legend =TRUE)+
  scale_x_continuous(breaks = c(50,200,400,600,800))+
  scale_y_continuous(limits = c(0,2.6),breaks = c(0,0.5,1.0,1.5,2.0,2.5),sec.axis = sec_axis(~.*20, name = "AABI"["per_tree"]~"/"~"kg/tree",breaks = c(0,10,20,30,40,50)))+
  labs(x= expression("P"["grow"]~"/"~"mm"),y="GPP"~"/"~"kg/m"^2)+
  scale_colour_manual(name="", values=c("a"="#F57f17","b"="#7AD151FF","c" = "#1976D2","d"="#311B92"),labels = c(expression("AABI"["per_tree"]),expression("GPP"["FLUXCOM"]),expression("GPP"["RS"]),expression("GPP"["TRENDY"])))+
  scale_linetype_manual(values = c(rep("longdash", 4)),guide = "none")+
  theme_bw()+
  theme(legend.position = "bottom",legend.direction = "horizontal",legend.text = element_text(size =18,face="bold"))+
  theme(axis.text = element_text(size =26),axis.text.x = element_text(size =26),axis.text.y = element_text(size =26),axis.title.x=element_text(size=26),axis.title.y=element_text(size=26))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))

p222
dev.off()

