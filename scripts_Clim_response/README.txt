Climate response analysis and plot

1. Git_Env_corr_North_America_map.R: process clim and GPP/AABI point data and get climte response statistics

Inputs: .\\Spatial_analysis\\Clim_response\\CRU_NCEP_prep_1_1984_2010.csv
            .\\Spatial_analysis\\Clim_response\\CRU_NCEP_temp_1_1984_2010.csv
            .\\Spatial_analysis\\Clim_response\\AABI_age_bias_corrected_GLC_detrend
            .\\Spatial_analysis\\Clim_response\\AABI_age_bias_corrected_GLC_per_tree_detrend
            .\\Spatial_analysis\\Clim_response\\GPP_FLUXCOM_GLC_detrend
            .\\Spatial_analysis\\Clim_response\\GPP_RS_mean_GLC_detrend
            .\\Spatial_analysis\\Clim_response\\GPP_TRENDYS2_GLC_detrend
            .\\Spatial_analysis\\Clim_response\\GPP_TRENDYS3_GLC_detrend

Outputs:	.\\Spatial_analysis\\Clim_response\\Output_temp\\*pcorr.csv
	.\\Spatial_analysis\\Clim_response\\Output_prep\\*pcorr.csv

2.  Trans_Concat_climate_corr_North_America_map_Multiple_month.py
Inputs:	.\\Spatial_analysis\\Clim_response\\Output_temp\\*pcorr.csv
	.\\Spatial_analysis\\Clim_response\\Output_prep\\*pcorr.csv

Outputs:	.\\Spatial_analysis\\Clim_response\\Output_temp\\*pcorr_tras_processed.csv
	.\\Spatial_analysis\\Clim_response\\Output_prep\\*pcorr_tras_processed.csv

3. Pixel_match_north_america_map.py
Inputs: 	.\\Spatial_analysis\\Clim_response\\Output_temp\\*pcorr_tras_processed.csv
	.\\Spatial_analysis\\Clim_response\\Output_prep\\*pcorr_tras_processed.csv

Outputs:  .\\Spatial_analysis\\Clim_response\\*toshp.csv
	.\\Spatial_analysis\\Clim_response\\*toshp.csv

The *toshp.csv files were then converted to rasters using Arc GIS 10.5 and plot *ED Fig.4*

4. Git_T_P_responses_cleaned.R: plot Fig.2a-b

5. Git_lat_bin_plot_cleaned.R: plot Fig.2c-f

6. Git_T_P_responses_2_cleaned.R: plot Fig.3a-b


