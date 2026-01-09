



#Set working directory and working envir-----
library(openxlsx)
library(stringr)
library(tidyverse)
library(zoo)
library(ineq)
library(cowplot)
library(RColorBrewer)
library(viridis)
library(ggpubr)
library(ggh4x)
library(wesanderson)
library(ggrepel)

setwd("C:\\Users\\xiang\\OneDrive\\C_Database\\GTAP 11\\GTAP_11b")
path <- getwd()
pathdata <- str_c(path,"/Data/Global SAM blocks_GTAP 11b_2017_160reg65secs")
pathdata2 <- str_c(path,"/Data/CSV files for 2017_GTAP 11b raw data")
pathdata3 <- str_c(path,"/Data/GTAP 2017_CO2 account")
pathdata4 <- str_c("H:/My Drive/Carbon Price Project/Data")
pathout <- str_c(path,"/Analysis_GTAP SAM_20241125");dir.create(pathout)
pathout2 <- str_c(pathout,"/Tax scenarios and revenue_Nov2025");dir.create(pathout2)
pathout3 <- str_c(pathout,"/Effect_Tax+Social assistance_Nov2025");dir.create(pathout3)
pathout4 <- str_c(pathout,"/Effect_Tax+Social assistance+Global transfer_Nov2025");dir.create(pathout4)
pathout5 <- str_c(pathout,"/Results summary and figs_Nov2025");dir.create(pathout5)
pathcode <- str_c(path,"/Code/R code/Nature_R3")


source(str_c(pathcode,"/Module 1_Read and clean basic blocks_v2.R"))
source(str_c(pathcode,"/Module 2_Labor income and transfer payments_v5.R"))
source(str_c(pathcode,"/Module 3_Carbon tax scenarios_v9_LES.R"))
source(str_c(pathcode,"/Module 4_Social assistance scenarios_v4.R"))
source(str_c(pathcode,"/Module 5_Global transfer scenarios_v2.R"))
source(str_c(pathcode,"/Module 6_Effects_Tax+SA_v5_recyshares_v3.R"))
source(str_c(pathcode,"/Module 7_Effects_TaxSA+GlobalFund_v5_recyshares.R"))
source(str_c(pathcode,"/Module 8_Tradeoff_poverty_fund_v2.R"))
source(str_c(pathcode,"/Module 9_Results summary_v3_recyshares.R"))
source(str_c(pathcode,"/Module 10_Visualization_v5_recyshares100.R"))
source(str_c(pathcode,"/Module 10_Visualization_v4_recyshares80.R"))
source(str_c(pathcode,"/Module 10_Visualization_v4_recyshares30.R"))
# source(str_c(pathcode,"/Module 10_Visualization_v4_recyshares20.R"))
  
