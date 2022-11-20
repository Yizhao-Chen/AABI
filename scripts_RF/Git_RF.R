##########################################################################################
#The script is to perform random forest model fitting, validation and map creation
##########################################################################################
#Load dependencies
#----------------------------------------------------------------------------------------
install.packages("pacman")
pacman::p_load(ranger,randomForest,spm,ggplot2,dplR,ggthemes,RColorBrewer,tuneRanger,
               viridis,MASS)
#----------------------------------------------------------------------------------------

#Get the working directory
#----------------------------------------------------------------------------------------
workDir <- getwd ()
#----------------------------------------------------------------------------------------

#Specify local workspace
#----------------------------------------------------------------------------------------
setwd(".\\RF\\")
#----------------------------------------------------------------------------------------

input_rf = read.csv(paste0(getwd(), "\\input_data_age_corrected.csv"))

input_rf_cleaned = input_rf
#age bias corrected
input_rf_cleaned = input_rf_cleaned[,-206]
input_rf_cleaned = input_rf_cleaned[,-(1:2)]
#no age bias correction
input_rf_cleaned1 = input_rf[,-207]
input_rf_cleaned1 = input_rf_cleaned1[,-(1:2)]

site_all = data.frame(input_rf$site)
year_all = data.frame(input_rf$Year)

#train with the entire dataset age bias corrected
{rgr_all = ranger(Target_1_age_cor~.,data= input_rf_cleaned, mtry = 100, importance = 'impurity')
}

#train with the entire dataset no age bias corrected
{rgr1_all = ranger(Target_1~.,data= input_rf_cleaned1, mtry = 100, importance = 'impurity')
}

#split train and test dataset age bias corrected
{
  set.seed(100)
  ind = sample(2,nrow(input_rf),replace = TRUE,prob = c(0.9,0.1))

  train = input_rf[ind==1,]
  test = input_rf[ind==2,]

  train_site = train$site
  test_site = test$site

  train = train[,-206]
  train = train[,-(1:2)]

  test = test[,-206]
  test = test[,-(1:2)]

  rgr_val = ranger(Target_1_age_cor~.,data= train, mtry = 100, importance = 'impurity')

  #make a prediction
  pred_test = predict(rgr_val,data=test)
}

#plot the train & prediction data validation FigS5 c and d
{
  #train data df
  train_df = data.frame(site = train_site,pred_train = rgr$predictions,train = train$Target)
  train_df_agg = aggregate(train_df,by = list(train_df$site),FUN=mean)
  
  #test data df
  pred_df = data.frame(site = test_site,pred_test = pred_test$predictions,test = test$Target)
  pred_df_agg = aggregate(pred_df,by = list(pred_df$site),FUN=mean)
  
  train_df$density <- get_density(train_df$pred_train, train_df$train, n = 100)
  
  pdf(".\\FigS5c.pdf",width = 6,height = 6)
  
  p_train <- ggplot() +
    geom_point(data = train_df, aes(x =pred_train, y =train, color = density))+ scale_color_viridis() +
    geom_smooth(data = train_df,aes(x =pred_train, y =train),method = 'lm',formula = y~x,colour="black",se=TRUE)+
    xlim(0,110)+
    ylim(0,110)+
    ylab("Obs_train (kg per tree)")+
    xlab("Pred_train (kg per tree)")+
    theme(legend.position = c(0.9,0.2))+
    theme(axis.text = element_text(face="bold",size =16),axis.text.x = element_text(face="bold",size =16),axis.text.y = element_text(face="bold",size =16),axis.title.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16))
  
  p_train
  dev.off()
  
  pdf(".\\FigS5d.pdf",width = 6,height = 6)
  pred_df$density <- get_density(pred_df$pred_test, pred_df$test, n = 100)
  p_test <- ggplot() +
    geom_point(data = pred_df, aes(x =pred_test, y =test, color = density))+ scale_color_viridis() +
    geom_smooth(data = pred_df,aes(x =pred_test, y =test),method = 'lm',formula = y~x,colour="black",se=TRUE)+
    xlim(0,110)+
    ylim(0,110)+
    ylab("Obs_test (kg per tree)")+
    xlab("Pred_test (kg per tree)")+
    theme(legend.position = c(0.9,0.2))+
    theme(axis.text = element_text(face="bold",size =16),axis.text.x = element_text(face="bold",size =16),axis.text.y = element_text(face="bold",size =16),axis.title.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16))
  
  p_test
  dev.off()
}

#split train and test dataset no age bias correction
{
  set.seed(100)
  ind1 = sample(2,nrow(input_rf_cleaned1),replace = TRUE,prob = c(0.9,0.1))

  train1 = input_rf_cleaned1[ind1==1,]
  test1 = input_rf_cleaned1[ind1==2,]

  rgr1 = ranger(Target_1~.,data= train1, mtry = 100, importance = 'impurity')
}

#10 fold cross validation for age bias corrected output
{
  n <- 100 # number of iterations, 60 to 100 is recommended.
  VEcv <- NULL
  rmse <- NULL
  mae <- NULL
  for (i in 1:n) {
    rgcv1 <- rgcv(input_rf_cleaned[,1:203], input_rf_cleaned[, 204], mtry =100, min.node.size =5, verbose = TRUE, predacc = "ALL")
    VEcv [i] <- rgcv1$vecv
    rmse [i] <- rgcv1$rmse
    mae [i] <- rgcv1$mae
  }
  write.csv(VEcv,".\\Cross_validation\\VEcv.csv")
  write.csv(rmse,".\\Cross_validation\\rmse.csv")
}

#plot the r2 and rmse for 10 fold cross validation output
#FigS5 f and g
{
  input_r2 = VEcv
  input_rmse = rmse

  r2 = NULL
  r2 = input_r2$x/100

  pdf(".\\FigS5f.pdf",width = 10,height = 10)

  mar.default <- c(1,1,1,1) + 0.1
  par(mar = mar.default + c(4, 4, 0, 0)) 

  plot(r2 ~ c(1:100), xlab = "Number of iteration", ylab = expression(bold(R^2)),pch = 19,cex = 2,cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,font = 2,font.lab=2)
  points(cumsum(r2) / c(1:100) ~ c(1:100), col = 2,pch =19,cex = 2)
  abline(h = mean(r2), col = 'blue', lwd = 4)

  dev.off()

  pdf(".\\FigS5g.pdf",width = 10,height = 10)

  mar.default <- c(1,1,1,1) + 0.1
  par(mar = mar.default + c(4, 4, 0, 0)) 

  plot(input_rmse$x ~ c(1:100), xlab = "Number of iteration", ylab = "rmse(kg biomass per tree)",pch = 19,cex = 2,cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,font = 2,font.lab = 2)
  points(cumsum(input_rmse$x) / c(1:100) ~ c(1:100), col = 2,pch =19,cex = 2)
  abline(h = mean(input_rmse$x), col = 'blue', lwd = 2)

  dev.off()
}

#make site prediction and plot site validations Fig.S5e
{
  #IAV tests
  IAV_root = ".\\IAV_test\\Site_input\\"
  IAV_out_root = ".\\IAV_test\\Predict\\" 
  flist = list.files(IAV_root)
  tmp<-strsplit(flist,split=".",fixed=TRUE)
  slist<-unlist(lapply(tmp,head,1)) 
  
  for (i in 1:length(slist)){
      sinput = read.csv(paste(IAV_root,slist[i],".csv",sep = ""))

      sinput = sinput[,-(1:2)]
      test = predict(rgr_all,sinput)
      sinput$predict = test$predictions
      sinput$Year = seq(1982,(1982 + length(rownames(sinput))-1),1)
      write.csv(sinput,paste(IAV_out_root,slist[i],"_pred.csv",sep = ""))
  }
  
#############################################################################################
#Get Pearson correlation output using .\\IAV_test\\rf_corr_site_iav.py  
#############################################################################################
  
  hist_input = read.csv(".\\IAV_test\\Test\\stat_summary_uscanada_site_delta_bio_iav.csv")
  
  pdf(".\\FigS5e.pdf",width = 6,height = 4.5)
  
  p3<-ggplot(hist_input, aes(x=rsqu_nt)) + 
    geom_histogram(color="black", fill="white",size = 1)+
    geom_vline(aes(xintercept=mean(rsqu_nt)),
                color="dark red", linetype="dashed", size=1)+
    xlab(expression("R"^2))+
    ylab("Site count")+
    ylim(0,150)+
    theme(axis.text = element_text(face="bold",size =16),axis.text.x = element_text(face="bold",size =16),axis.text.y = element_text(face="bold",size =16),axis.title.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16))

  p3
  dev.off()
  
}

#make map prediction
{
  #bias correct file mapping
  for (i in 1982:2010){
    map_input = read.csv(paste(".\\map_creation_",i,"_soil_LAI_added_nan_filled.csv",sep=""))
    #remove the pointid column
    map_input = map_input[,-1]
    
    pred = predict(rgr_all,map_input)
    write.csv(pred$predictions,paste(".\\predict",i,"_bias_correct.csv",sep=""))
  }
  
  #no bias correct file mapping
  for (i in 1982:2010){
    map_input = read.csv(paste(".\\map_creation_",i,"_soil_LAI_added_nan_filled.csv",sep=""))
    #remove the pointid column
    map_input = map_input[,-1]
    
    pred = predict(rgr1_all,map_input)
    write.csv(pred$predictions,paste(".\\predict",i,".csv",sep=""))
  }
}

#################################################################################################
#The predict* files were then converted to tif files with lat/lon and used to create datasets
#The predict* files with lat/lon were deposited in .\\predict\\AABI_per_tree_point\\ and 
#.\\predict\\AABI_per_tree_bias_corrected_point\\
#################################################################################################

#Robustness test
{
#check and plot the site number distribution over years
{
  #plot the site number distribution for each year
  site_num = c(sum(input_rf$Year == 1982),sum(input_rf$Year == 1983),sum(input_rf$Year == 1984),sum(input_rf$Year == 1985),sum(input_rf$Year == 1986),sum(input_rf$Year == 1987),sum(input_rf$Year == 1988),sum(input_rf$Year == 1989),sum(input_rf$Year == 1990),sum(input_rf$Year == 1991),sum(input_rf$Year == 1992),sum(input_rf$Year == 1993),sum(input_rf$Year == 1994),sum(input_rf$Year == 1995),sum(input_rf$Year == 1996),sum(input_rf$Year == 1997),sum(input_rf$Year == 1998),sum(input_rf$Year == 1999),sum(input_rf$Year == 2000),sum(input_rf$Year == 2001),sum(input_rf$Year == 2002),sum(input_rf$Year == 2003),sum(input_rf$Year == 2004),sum(input_rf$Year == 2005),sum(input_rf$Year == 2006),sum(input_rf$Year == 2007),sum(input_rf$Year == 2008),sum(input_rf$Year == 2009),sum(input_rf$Year == 2010))
  site_num_df = data.frame("Year" = c(1982:2010),"Site_num" = site_num)
  

  p_site_num <- ggplot(data = site_num_df,aes(x=Year,y=Site_num,fill="red"))+
    xlim(1980,2011)+
    geom_bar(stat="identity")+
    theme(legend.position="none")
  
  p_site_num

  }
#remove one year
{
  for (j in 1982:2001){
    input_rbst = input_rf[-which(input_rf$Year == j),]
      
    input_rbst = input_rbst[,-206]
    input_rbst = input_rbst[,-(1:2)]

    rgr_rbst = ranger(Target_1_age_cor~.,data= input_rbst, mtry = 100, importance = 'impurity')
    
    #map creation
    out = NULL
    for (i in 1982:2010){
      map_input = read.csv(paste(".\\map_creation_",i,"_soil_LAI_added_nan_filled.csv",sep=""))
      #remove the pointid column
      map_input = map_input[,-1]
      
      pred = predict(rgr_rbst,map_input)
      out[(i-1981)] = mean(pred$predictions)
    }
    write.csv(out,paste(".\\predict_rbst_test_summary_no",j,".csv",sep=""))
  }
}
#remove years after 2002
{
  input_rbst = input_rf[-which(input_rf$Year == "2002" | input_rf$Year == "2003" | input_rf$Year == "2004" |input_rf$Year == "2005" | input_rf$Year == "2006" | input_rf$Year == "2007" | input_rf$Year == "2008" | input_rf$Year == "2009" | input_rf$Year == "2010"),]
  
  input_rbst = input_rbst[,-206]
  input_rbst = input_rbst[,-(1:2)]
  
  rgr_rbst1 = ranger(Target_1_age_cor~.,data= input_rbst, mtry = 100, importance = 'impurity')
  
  #map creation
  out = NULL
  for (i in 1982:2010){
    #data source:
    map_input = read.csv(paste(".\\map_creation_",i,"_soil_LAI_added_nan_filled.csv",sep=""))
    #remove the pointid column
    map_input = map_input[,-1]
    
    pred = predict(rgr_rbst1,map_input)
    out[(i-1981)] = mean(pred$predictions)
  }
  write.csv(out,paste(".\\robustness_test\\predict_rbst_test_summary_after2002_removed.csv",sep=""))
}
#plot the robustness test output Extended Data Fig.9a
{
  input_rbst_ensemble = read.csv(".\\RF\\robustness_test\\predict_rbst_test_summary.csv")
  input_rbst_ensemble$Year = c(1982:2010)
  input_rbst_scaled = scale(input_rbst_ensemble[,1:(length(input_rbst_ensemble)-1)],center = TRUE,scale = TRUE)
  input_rbst_scaled_df = data.frame(input_rbst_scaled)
  input_rbst_scaled_mean = apply(input_rbst_scaled_df[,1:ncol(input_rbst_scaled_df)],1,mean,na.rm=TRUE)
  input_rbst_scaled_rsd = apply(input_rbst_scaled_df[,1:ncol(input_rbst_scaled_df)],1,sd,na.rm=TRUE)
  input_rbst_scaled_df$mean = input_rbst_scaled_mean
  input_rbst_scaled_df$sd = input_rbst_scaled_rsd
  input_rbst_scaled_df$min = input_rbst_scaled_mean - input_rbst_scaled_rsd
  input_rbst_scaled_df$max = input_rbst_scaled_mean + input_rbst_scaled_rsd
  input_rbst_scaled_df$Year = c(1982:2010)
  
  input_rbst_scaled_df_de <- input_rbst_scaled_df
  for (i in 1:(ncol(input_rbst_scaled_df_de)-1)){
    output_lm <- lm(input_rbst_scaled_df_de[,i]~input_rbst_scaled_df_de[,(ncol(input_rbst_scaled_df_de))],na.action=na.exclude)
    output_ret <- residuals(output_lm)
    input_rbst_scaled_df_de[,i] <- output_ret
  } 
  
  input_rbst_scaled_df_de_mean = apply(input_rbst_scaled_df_de[,1:(ncol(input_rbst_scaled_df_de)-2)],1,mean,na.rm=TRUE)
  input_rbst_scaled_df_de_rsd = apply(input_rbst_scaled_df_de[,1:(ncol(input_rbst_scaled_df_de)-2)],1,sd,na.rm=TRUE)
  input_rbst_scaled_df_de$mean = input_rbst_scaled_df_de_mean
  input_rbst_scaled_df_de$sd = input_rbst_scaled_df_de_rsd
  input_rbst_scaled_df_de$min = input_rbst_scaled_df_de_mean - input_rbst_scaled_df_de_rsd
  input_rbst_scaled_df_de$max = input_rbst_scaled_df_de_mean + input_rbst_scaled_df_de_rsd
  input_rbst_scaled_df_de$Year = c(1982:2010)
  
  
  pdf(".\\robustness_test\\FigS9a.pdf",width = 9,height = 6)
  p_rbst <- ggplot()+
    geom_ribbon(data = input_rbst_scaled_df_de,aes(x=Year,ymin= min, 
                                                   ymax= max),fill = "lightblue",alpha = 0.4)+
    geom_line(data = input_rbst_scaled_df_de,aes(x=Year, 
                                                 y=mean,color = "b"),linetype="solid",size =1)+
    geom_line(data = input_rbst_scaled_df_de,aes(x=Year, 
                                                 y=no_02_10,color = "c"),linetype="solid",size =1)+
    geom_line(data = input_rbst_scaled_df_de,aes(x=Year,y= all,color = "a"),linetype="solid",size = 1)+
    ylim(-3,3)+
    ylab("z-score")+
    xlab("Year")+
    #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
    scale_color_manual(name = '', 
                       values =c("a"="lightcoral","b" = "royalblue4","c"="green4"), labels = c('All','Mean_1year_less','No_02_10'))+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = "bottom",legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  p_rbst
  dev.off()
  
}
}

#Uncertainty quantification
{
  pred_rf_uncert = data.frame("num" = c(1:388208))
  
  for (k in 1:100){
    assign(paste0("pred_bootstrap_",k),pred_rf_uncert)
  }
  
  for (i in 1982:2010){
    map_input = read.csv(paste(".\\map_creation_",i,"_soil_LAI_added_nan_filled.csv",sep=""))
    #remove the pointid column
    map_input = map_input[,-1]
    #out_df = pred_rf_uncert
    #Uncertainty from the rf algorithm
    for (j in 1:100){
      ind_rf_uncert = sample(2,nrow(input_rf_cleaned),replace = TRUE,prob = c(0.9,0.1))
      train_rf_uncert = input_rf_cleaned[ind_rf_uncert==1,]
      #train_rf_uncert1 = train_rf_uncert[,-j]
      rgr_rf_uncert = ranger(Target_1_age_cor~.,data= train_rf_uncert1, mtry = 100, importance = 'impurity')
      #map_input1 = map_input[,-j]
      pred_rf_uncert1 = predict(rgr_rf_uncert,map_input)
      #out_df = pred_rf_uncert
      assign(paste0("pred_bootstrap_",k),cbind(get(paste0("pred_bootstrap_",k)),pred_rf_uncert1$predictions))
    }
  }
  
  #Uncertainty from the Target estimation
  pred_rf_uncert = data.frame("num" = c(1:388208))
  
  for (k in 1:100){
    assign(paste0("pred_bootstrap_",k),pred_rf_uncert)
  }
  
  for (i in 1982:2010){
    map_input = read.csv(paste(".\\map_creation_",i,"_soil_LAI_added_nan_filled.csv",sep=""))
    #remove the pointid column
    map_input = map_input[,-1]
    #out_df = pred_rf_uncert
    #Uncertainty from the rf algorithm
    for (j in 1:203){
      input_rf_uncert <- input_rf_cleaned[,-j]
      rgr_var_uncert = ranger(Target_1_age_cor~.,data= input_rf_uncert, mtry = 100, importance = 'impurity')
      pred_var_uncert = predict(rgr_var_uncert,map_input)

      assign(paste0("pred_",j),cbind(get(paste0("pred_",j)),pred_var_uncert$predictions))
    }
  }

#####################################################################################################
#The outputs of the two uncertainty sources were summarized in
#.\\RF\\predict_bootstrap_mean_output.csv
#.\\RF\\predict_var1_mean_output.csv
#.\\RF\\predict_var2_mean_output.csv
#The baseline output is the mean prediction of the AABI per tree bias correct dataset from 1982 to 2010
#####################################################################################################
  
  #calculate the absolute difference
  map_baseline = read.csv(".\\RF\\predict_baseline_mean_output.csv")
  map_bootstrap = read.csv(".\\RF\\predict_bootstrap_mean_output.csv")
  map_var1 = read.csv(".\\RF\\predict_var1_mean_output.csv")
  map_var2 = read.csv(".\\RF\\predict_var2_mean_output.csv")
  
  map_out_boot = map_baseline[,c(2:3)]
  #bootstrap
  for (i in 3:length(map_bootstrap)){
    diff = abs(map_baseline[,33] - map_bootstrap[,i])
    map_out_boot[,i] = diff
  }
  map_out_boot_temp = map_out_boot[,-c(1:2)]
  map_out_boot$mean = rowMeans(map_out_boot_temp)
  map_out_boot_mean = data.frame("lat" = map_out_boot$lat, "lon" = map_out_boot$lon,  "mean" = map_out_boot$mean)
  write.csv(map_out_boot,".\\RF\\map_out_boot_full.csv")
  write.csv(map_out_boot_mean,".\\RF\\map_out_boot_mean.csv")
  
  #var1
  map_out_var1 = map_baseline[,c(2:3)]
  for (i in 3:length(map_var1)){
    diff = abs(map_baseline[,33] - map_var1[,i])
    map_out_var1[,i] = diff
  }
  names(map_out_var1)[c(3:length(map_out_var1))] = names(map_var1)[c(3:length(map_var1))]
  
  #write.csv(map_out_boot,"map_out_boot_full.csv")
  #write.csv(map_out_boot_mean,"map_out_boot_mean.csv")
  
  #var2
  map_out_var2 = map_baseline[,c(2:3)]
  for (i in 3:length(map_var2)){
    diff = abs(map_baseline[,33] - map_var2[,i])
    map_out_var2[,i] = diff
  }
  names(map_out_var2)[c(3:length(map_out_var2))] = names(map_var2)[c(3:length(map_var2))]
  
  #var all
  map_out_var_all = cbind(map_out_var1,map_out_var2[,-c(1:2)])
  map_out_var_all_temp = map_out_var_all[,-c(1:2)]
  map_out_var_all$mean = rowMeans(map_out_var_all_temp)
  map_out_var_all_mean = data.frame("lat" = map_out_var_all$lat, "lon" = map_out_var_all$lon,  "mean" = map_out_var_all$mean)  
  col_mean = colMeans(map_out_var_all[,-c(1:2)])
  write.csv(map_out_var_all,".\\RF\\map_out_var_all_full.csv")
  write.csv(map_out_var_all_mean,".\\RF\\map_out_var_all_mean.csv")
  }

###########################################################################################
#.\\RF\\map_out_boot_mean.csv
#.\\RF\\map_out_var_all_mean.csv
#were used to plot Extended data Fig.9b and Fig.9c
###########################################################################################

##################################################################################
#Affiliated functions
{
#from https://slowkow.com/notes/ggplot2-color-by-density/
# Get density of points in 2 dimensions.
# @param x A numeric vector.
# @param y A numeric vector.
# @param n Create a square n by n grid to compute density.
# @return The density within each square.
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

}
##################################################################################