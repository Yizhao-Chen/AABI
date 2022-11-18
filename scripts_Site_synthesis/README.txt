Steps to perform the site-level synthesis (Related output: Extended Data Fig.2):

1. Collect and compare FLUXNET sites GPP data and the corresponding growth observations from various sources (FLUXNET comparison), as summarized in Data_synthesis_summary.xlsx.
2. Compare spatial explicit GPP data with ITRDB, Gentree and Tropical sites output (RS comparison).

Data involved: Spatial explicit GPP datasets: BEPS,EC-LUE,GPPinf (1982-2010); Tree ring data from ITRDB and Gentree; Inventory data from six Tropical sites.

Data processing:
Spatial homogenous test: 
Get the multi-year mean maps of the GPP datasets and calculate the spatial explicit CV values (focal statistics in Arc GIS 10.5); extract the pixels of of the growth observation sites and select the spatially homogenous ones (CV < 0.4). 
Extract the pixels: GIS_cv.py; Extract the low cv sites: Extract_lowCV_sites.py.

Correlation analysis:
1). Get the detrended correlationships between GPP and tree ring width. Extract the pixel values of GPP datasets: GIS_GPP_indexes.py;  correlation analysis: dat_compile_output_csv_only.py; add site info: ; get same sites output from three comparisons: Get_the_common_sites.py
2). Add flags for significance and convert to shp files mannually. 

Output summary:  
Plot FLUXNET comparison output using Sigmaplot 14.0
Plot global map using Arc GIS 10.5.
Plot code for the lat distribution: plot_spatial_distribution.py.
