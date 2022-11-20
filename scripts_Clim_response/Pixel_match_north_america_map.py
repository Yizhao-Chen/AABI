#########################################################################################################
#Script to match the pixels from env_corr to lat/lon file for mapping
#########################################################################################################
import pandas as pd


#reference point csv
shp_csv_sink_per_tree = pd.read_csv(".\\"
                      "reference_point.CSV")
shp_csv_sink_per_tree.index = shp_csv_sink_per_tree['POINTID']


root_temp = ".\\Clim_response\\Output_temp\\"
root_prep = ".\\Clim_response\\Output_prep\\"

root_out = ".\\Clim_response\\"



#temp
#trendy
temp_trendy_csv = pd.read_csv(root_temp + "trendy_temp_pcorr_tras_processed.csv")
#fluxcom
temp_fluxcom_csv = pd.read_csv(root_temp + "fluxcom_temp_pcorr_tras_processed.csv")
#sink age correct
temp_sink_age_correct_csv = pd.read_csv(root_temp + "AABI_age_correct_per_area_all_mean_temp_pcorr_tras_processed.csv")
#RS_mean
temp_RS_mean_csv = pd.read_csv(root_temp + "GPP_RS_mean_age_correct_temp_pcorr_tras_processed.csv")
#sink per tree
temp_sink_per_tree_csv = pd.read_csv(root_temp + "AABI_age_correct_per_tree_temp_pcorr_tras_processed.csv")


#convert index to point id, i.e., Variable in the corr files
temp_trendy_csv.index = temp_trendy_csv['Variable']
temp_fluxcom_csv.index = temp_fluxcom_csv['Variable']
temp_sink_age_correct_csv.index = temp_sink_age_correct_csv['Variable']
temp_RS_mean_csv.index = temp_RS_mean_csv['Variable']
temp_sink_per_tree_csv.index = temp_sink_per_tree_csv['Variable']

#prep
#trendy
prep_trendy_csv = pd.read_csv(root_prep + "trendy_prep_pcorr_tras_processed.csv")
#fluxcom
prep_fluxcom_csv = pd.read_csv(root_prep + "fluxcom_prep_pcorr_tras_processed.csv")
#sink_age_correct
prep_sink_age_correct_csv = pd.read_csv(root_prep + "AABI_age_correct_per_area_all_mean_prep_pcorr_tras_processed.csv")
#RS_mean
prep_RS_mean_csv = pd.read_csv(root_prep + "GPP_RS_mean_age_correct_prep_pcorr_tras_processed.csv")
#sink per tree
prep_sink_per_tree_csv = pd.read_csv(root_prep + "AABI_age_correct_per_tree_prep_pcorr_tras_processed.csv")


#convert index to point id, i.e., Variable in the corr files
prep_trendy_csv.index = prep_trendy_csv['Variable']
prep_fluxcom_csv.index = prep_fluxcom_csv['Variable']
prep_sink_age_correct_csv.index = prep_sink_age_correct_csv['Variable']
prep_RS_mean_csv.index = prep_RS_mean_csv['Variable']
prep_sink_per_tree_csv.index = prep_sink_per_tree_csv['Variable']


#create lists
list_temp_trendy = list(temp_trendy_csv['Variable'])
list_temp_fluxcom =  list(temp_fluxcom_csv['Variable'])
list_temp_sink_age = list(temp_sink_age_correct_csv['Variable'])
list_temp_RS_mean = list(temp_RS_mean_csv['Variable'])
list_temp_sink_per_tree = list(temp_sink_per_tree_csv['Variable'])

list_prep_trendy = list(prep_trendy_csv['Variable'])
list_prep_fluxcom =  list(prep_fluxcom_csv['Variable'])
list_prep_sink_age = list(prep_sink_age_correct_csv['Variable'])
list_prep_RS_mean = list(prep_RS_mean_csv['Variable'])
list_prep_sink_per_tree = list(prep_sink_per_tree_csv['Variable'])

#list_shp = list(shp_csv['Point_id'])
list_shp = list(shp_csv_sink_per_tree['POINTID'])
#temp
#trendy
for i in range(len(list_temp_trendy)):
    if list_temp_trendy[i] in list_shp:
        shp_csv_trendy['Corr'].at[list_temp_trendy[i]] = temp_trendy_csv['maximal_calculated_metric'].at[list_temp_trendy[i]]
        shp_csv_trendy['T_start'].at[list_temp_trendy[i]] = temp_trendy_csv['start'].at[list_temp_trendy[i]]
        shp_csv_trendy['T_end'].at[list_temp_trendy[i]] = temp_trendy_csv['end'].at[list_temp_trendy[i]]

shp_csv_trendy.to_csv(root_out + "Point_temp_trendy_pcorr_tras_processed_toshp.csv")
shp_csv_trendy['Corr'] = -999.0
shp_csv_trendy['T_start'] = -999.0
shp_csv_trendy['T_end'] = -999.0

#fluxcom
for i in range(len(list_temp_fluxcom)):
    if list_temp_fluxcom[i] in list_shp:
        shp_csv['Corr'].at[list_temp_fluxcom[i]] = temp_fluxcom_csv['maximal_calculated_metric'].at[list_temp_fluxcom[i]]
        shp_csv['T_start'].at[list_temp_fluxcom[i]] = temp_fluxcom_csv['start'].at[list_temp_fluxcom[i]]
        shp_csv['T_end'].at[list_temp_fluxcom[i]] = temp_fluxcom_csv['end'].at[list_temp_fluxcom[i]]

shp_csv.to_csv(root_out + "Point_temp_fluxcom_pcorr_tras_processed_toshp.csv")
shp_csv['Corr'] = -999.0
shp_csv['T_start'] = -999.0
shp_csv['T_end'] = -999.0

#sink age correct
shp_csv_sink_per_tree['Corr'] = -999.0
shp_csv_sink_per_tree['T_start'] = -999.0
shp_csv_sink_per_tree['T_end'] = -999.0

for i in range(len(list_temp_sink_age)):
    if list_temp_sink_age[i] in list_shp:
        shp_csv_sink_per_tree['Corr'].at[list_temp_sink_age[i]] = temp_sink_age_correct_csv['maximal_calculated_metric'].at[list_temp_sink_age[i]]
        shp_csv_sink_per_tree['T_start'].at[list_temp_sink_age[i]] = temp_sink_age_correct_csv['start'].at[list_temp_sink_age[i]]
        shp_csv_sink_per_tree['T_end'].at[list_temp_sink_age[i]] = temp_sink_age_correct_csv['end'].at[list_temp_sink_age[i]]

shp_csv_sink_per_tree.to_csv(root_out + "AABI_age_correct_per_area_all_mean_temp_pcorr_tras_processed_toshp.csv")
shp_csv_sink_per_tree['Corr'] = -999.0
shp_csv_sink_per_tree['T_start'] = -999.0
shp_csv_sink_per_tree['T_end'] = -999.0

#RS_mean
shp_csv_rs_mean['Corr'] = -999.0
shp_csv_rs_mean['T_start'] = -999.0
shp_csv_rs_mean['T_end'] = -999.0

for i in range(len(list_temp_RS_mean)):
    if list_temp_RS_mean[i] in list_shp:
        shp_csv_rs_mean['Corr'].at[list_temp_RS_mean[i]] = temp_RS_mean_csv['maximal_calculated_metric'].at[list_temp_RS_mean[i]]
        shp_csv_rs_mean['T_start'].at[list_temp_RS_mean[i]] = temp_RS_mean_csv['start'].at[list_temp_RS_mean[i]]
        shp_csv_rs_mean['T_end'].at[list_temp_RS_mean[i]] = temp_RS_mean_csv['end'].at[list_temp_RS_mean[i]]

shp_csv_rs_mean.to_csv(root_out + "GPP_RS_mean_temp_pcorr_tras_processed_toshp.csv")

#sink per tree
shp_csv_sink_per_tree['Corr'] = -999.0
shp_csv_sink_per_tree['T_start'] = -999.0
shp_csv_sink_per_tree['T_end'] = -999.0

for i in range(len(list_temp_sink_per_tree)):
    if list_temp_sink_per_tree[i] in list_shp:
        shp_csv_sink_per_tree['Corr'].at[list_temp_sink_per_tree[i]] = temp_sink_per_tree_csv['maximal_calculated_metric'].at[list_temp_sink_per_tree[i]]
        shp_csv_sink_per_tree['T_start'].at[list_temp_sink_per_tree[i]] = temp_sink_per_tree_csv['start'].at[list_temp_sink_per_tree[i]]
        shp_csv_sink_per_tree['T_end'].at[list_temp_sink_per_tree[i]] = temp_sink_per_tree_csv['end'].at[list_temp_sink_per_tree[i]]

shp_csv_sink_per_tree.to_csv(root_out + "AABI_age_correct_per_tree_temp_pcorr_tras_processed_toshp.csv")

#prep
#trendy
for i in range(len(list_prep_trendy)):
    if list_prep_trendy[i] in list_shp:
        shp_csv_trendy['Corr'].at[list_prep_trendy[i]] = prep_trendy_csv['maximal_calculated_metric'].at[list_prep_trendy[i]]
        shp_csv_trendy['T_start'].at[list_prep_trendy[i]] = prep_trendy_csv['start'].at[list_prep_trendy[i]]
        shp_csv_trendy['T_end'].at[list_prep_trendy[i]] = prep_trendy_csv['end'].at[list_prep_trendy[i]]

shp_csv_trendy.to_csv(root_out + "trendy\\Point_prep_trendy_pcorr_tras_processed_toshp.csv")
shp_csv_trendy['Corr'] = -999.0
shp_csv_trendy['T_start'] = -999.0
shp_csv_trendy['T_end'] = -999.0


#fluxcom
for i in range(len(list_prep_fluxcom)):
    if list_prep_fluxcom[i] in list_shp:
        shp_csv['Corr'].at[list_prep_fluxcom[i]] = prep_fluxcom_csv['maximal_calculated_metric'].at[list_prep_fluxcom[i]]
        shp_csv['T_start'].at[list_prep_fluxcom[i]] = prep_fluxcom_csv['start'].at[list_prep_fluxcom[i]]
        shp_csv['T_end'].at[list_prep_fluxcom[i]] = prep_fluxcom_csv['end'].at[list_prep_fluxcom[i]]

shp_csv.to_csv(root_out + "fluxcom\\Point_prep_fluxcom_pcorr_tras_processed_toshp.csv")
shp_csv['Corr'] = -999.0
shp_csv['T_start'] = -999.0
shp_csv['T_end'] = -999.0

#sink age correct
shp_csv_sink_per_tree['Corr'] = -999.0
shp_csv_sink_per_tree['T_start'] = -999.0
shp_csv_sink_per_tree['T_end'] = -999.0

for i in range(len(list_prep_sink_age)):
    if list_prep_sink_age[i] in list_shp:
        shp_csv_sink_per_tree['Corr'].at[list_prep_sink_age[i]] = prep_sink_age_correct_csv['maximal_calculated_metric'].at[list_prep_sink_age[i]]
        shp_csv_sink_per_tree['T_start'].at[list_prep_sink_age[i]] = prep_sink_age_correct_csv['start'].at[list_prep_sink_age[i]]
        shp_csv_sink_per_tree['T_end'].at[list_prep_sink_age[i]] = prep_sink_age_correct_csv['end'].at[list_prep_sink_age[i]]

shp_csv_sink_per_tree.to_csv(root_out + "AABI_age_correct_per_area_all_mean_prep_pcorr_tras_processed_toshp.csv")

shp_csv_sink_per_tree['Corr'] = -999.0
shp_csv_sink_per_tree['T_start'] = -999.0
shp_csv_sink_per_tree['T_end'] = -999.0

#RS_mean
shp_csv_rs_mean['Corr'] = -999.0
shp_csv_rs_mean['T_start'] = -999.0
shp_csv_rs_mean['T_end'] = -999.0

for i in range(len(list_prep_RS_mean)):
    if list_prep_RS_mean[i] in list_prep_RS_mean:
        shp_csv_rs_mean['Corr'].at[list_prep_RS_mean[i]] = prep_RS_mean_csv['maximal_calculated_metric'].at[list_prep_RS_mean[i]]
        shp_csv_rs_mean['T_start'].at[list_prep_RS_mean[i]] = prep_RS_mean_csv['start'].at[list_prep_RS_mean[i]]
        shp_csv_rs_mean['T_end'].at[list_prep_RS_mean[i]] = prep_RS_mean_csv['end'].at[list_prep_RS_mean[i]]

shp_csv_rs_mean.to_csv(root_out + "GPP_RS_mean_prep_pcorr_tras_processed_toshp.csv")

#sink per tree
shp_csv_sink_per_tree['Corr'] = -999.0
shp_csv_sink_per_tree['T_start'] = -999.0
shp_csv_sink_per_tree['T_end'] = -999.0

for i in range(len(list_prep_sink_per_tree)):
    if list_prep_sink_per_tree[i] in list_prep_sink_per_tree:
        shp_csv_sink_per_tree['Corr'].at[list_prep_sink_per_tree[i]] = prep_sink_per_tree_csv['maximal_calculated_metric'].at[list_prep_sink_per_tree[i]]
        shp_csv_sink_per_tree['T_start'].at[list_prep_sink_per_tree[i]] = prep_sink_per_tree_csv['start'].at[list_prep_sink_per_tree[i]]
        shp_csv_sink_per_tree['T_end'].at[list_prep_sink_per_tree[i]] = prep_sink_per_tree_csv['end'].at[list_prep_sink_per_tree[i]]

shp_csv_sink_per_tree.to_csv(root_out + "AABI_age_correct_per_tree_prep_pcorr_tras_processed_toshp.csv")

shp_csv_sink_per_tree['Corr'] = -999.0
shp_csv_sink_per_tree['T_start'] = -999.0
shp_csv_sink_per_tree['T_end'] = -999.0
