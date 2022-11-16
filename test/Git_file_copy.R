#Copy sites from itrdb repository

filename_input = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Deep_learning\\random_forest\\site_2022_11_13.csv")

filename_list = list(filename_input$name)
rwl_filepath = "D:\\MEGA\\Live_cases\\Hybrid\\Tree_ring_data_collection_NOAA_ITRDB\\Appendix S1\\Cleaned_datasets\\itrdb-v720-cleaned-rwl\\usa_TRW_no_species_cleaned\\"
diff_path = "D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\TRW2BIOMASS\\dia_bio_updated_2022_9_13\\"
out_filepath = "D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\TRW2BIOMASS\\dia_bio_updated_2022_11_13\\usa\\"
out_filepath1 = "D:\\MEGA\\Live_cases\\Hybrid\\Tree_ring_data_collection_NOAA_ITRDB\\Final_sites_1329_2022_11_13\\usa_diff\\"

#
filenames1 = list()
filenames3 = list()

filenames <- dir(rwl_filepath)
for (i in 1:length(filenames)){
  filenames1[i] = strsplit(filenames[i],"[.]")[[1]][1]
}

filenames2 <- dir(diff_path)
for (i in 1:length(filenames2)){
  filenames3[i] = strsplit(filenames2[i],"[.]")[[1]][1]
  filenames3[i] = strsplit(filenames3[[i]],"_")[1][[1]][1]
}


`%notin%` <- Negate(`%in%`)

for (i in 1:length(filename_list[[1]])){
  if (filename_list[[1]][i] %in% filenames1){
    if (filename_list[[1]][i] %notin% filenames3){
      #filenames2 = paste0(filename_list[[1]][i],".rwl")
      filenames4 = paste0(filename_list[[1]][i],".rwl")
    file.copy(from = paste0(rwl_filepath,filenames4),to = file.path(paste0(out_filepath1,filenames4)))
    #print(filename_list[[1]][i])
  }
}
}


#########################################################################
#delete files
#########################################################################
filename_input = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Deep_learning\\random_forest\\site_2022_11_13.csv")

filename_list = list(filename_input$name)
del_bio_filepath = "D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\TRW2BIOMASS\\dia_bio_updated_2022_11_13\\"
  
filenames5 <- dir(del_bio_filepath)
filenames6 = list()
for (i in 1:length(filenames5)){
  filenames6[i] = strsplit(filenames5[i],"[.]")[[1]][1]
  filenames6[i] = strsplit(filenames6[[i]],"_")[1][[1]][1]
}

for (i in 1:length(filenames6)){
    if (filenames6[i][[1]] %notin% filename_list[[1]]){
      #filenames2 = paste0(filename_list[[1]][i],".rwl")
      #filenames7 = paste0(filename_list[[1]][i],"_age_mean.csv")
      to_be_deleted <- list.files(del_bio_filepath, pattern = filenames6[i][[1]])
      file.remove(paste0(del_bio_filepath,to_be_deleted))
      #print(filename_list[[1]][i])
    }
  }
