Analysis code for the paper "Expanded urban heat extent with flattened gradients driven by urban expansion and greening"

Overview 
This study applied open-accesed remote sensing data and R programing for data analysis.

Data:
This study used multiple MODIS products and GAIA impervious surface areas (SIA) data as follows:
MODIS daily LST data downloaded from https://e4ftl01.cr.usgs.gov/MOLA/MYD11A1.006
MODIS monthly EVI data downloaded from https://e4ftl01.cr.usgs.gov/MOLA/MYD13A3.006
MODIS yearly LULC data downloaded from: https://e4ftl01.cr.usgs.gov/MOTA/MCD12Q1.006
GAIA Yearly ISA data downloaded from http://data.ess.tsinghua.edu.cn/gaia.html

Code:
The data was processed, analyzed, and plotted using R 4.3.3 in four steps
step 1: city cluster and buffer buffer (1.1-1.3)
step 2: Trend analysis (2.2-3.2, 6.4-6.5, 7.1-7.2)
step 3: ATC model fitting (5.0-6.3)
step 4: statistical analysis and ploting (7.0, 8s)

System requirements: Hardware requirements: A standard computer with enough RAM to support the in-memory operations is required for the installation and operations with R platform.
The ATC fitting at pixel scale is done through paralleling computing in HPC using library("doParallel") and library("foreach")
Software requirements: The R 4.3.3 install package is accessible on the website: [https://cran.r-project.org/bin/windows/base/old/4.3.3/]

Installation guide: The details about R installation is accessible at：[https://cran.r-project.org/doc/manuals/r-release/R-intro.html]

Instruction for use: Include the data and code in one folder.
