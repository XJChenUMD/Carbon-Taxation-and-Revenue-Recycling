#The potential of carbon taxation with revenue recycling for reducing poverty and inequality 
#Main file
#2022-12-06
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu


#Path
#=============
Sys.setlocale("LC_ALL","zh_CN.UTF-8")
# memory.limit(size = 20000)
library(stringr)
library(ineq)
library(tidyverse)

setwd("I:\\我的云端硬盘\\Carbon Price Project")
path <- getwd()
pathdata <- str_c(path,"/Data")
pathcode <- str_c(path,"/Code/Code_Long Paper_230204")
pathout <- str_c(path,"/results/Carbon taxatation and recycling_230204");dir.create(pathout)
pathout2 <- str_c(pathout,"/Tax scenarios and revenue");dir.create(pathout2)
pathout3 <- str_c(pathout,"/Effect_Tax+Social assistance");dir.create(pathout3)
pathout4 <- str_c(pathout,"/Effect_Tax+Social assistance+Global transfer");dir.create(pathout4)
pathout5 <- str_c(pathout,"/Results summary and figs");dir.create(pathout5)
#=============


#Modules
#=============
#Step 1: expenditure, carbon footprint by income groups
source(str_c(pathcode,"/","Module 1_Expenditure, carbon footprint by income bins.R"))

#Step 2: carbon tax scenarios
source(str_c(pathcode,"/","Module 2_Carbon tax scenarios.R"))
# source(str_c(pathcode,"/","Module 2_Carbon tax scenarios_income groups.R"))

#Step 3: social assistance scenarios
source(str_c(pathcode,"/","Module 3_Social assistance scenarios.R"))

#Step 4: global climate finance scenarios
source(str_c(pathcode,"/","Module 4_Global transfer scenarios.R"))

#Step 5: The poverty and inequality effect of combinations of tax scenarios (7) and assistance scenarios (5)
source(str_c(pathcode,"/","Module 5_Effects_Tax+SA.R"))

#Step 6: The poverty and inequality effect of tax+SA (2) and global transfer (6)
source(str_c(pathcode,"/","Module 6_Effects_TaxSA+GlobalFund.R"))

#Step 7: Trade off for poverty reduction between Global Climate Fund (0-1000) and Carbon Tax Level (50-150)
source(str_c(pathcode,"/","Module 7_Tradeoff_poverty_taxORfund.R"))

#Step 8: Results summary for visualization
source(str_c(pathcode,"/","Module 8_Results summary.R")) 

#Step 9: Visualization
source(str_c(pathcode,"/","Module 9_Visualization.R")) 
#=============


#Step 6_v2 (all combinations: The poverty and inequality effect of tax+SA (2) and global transfer (6)
source(str_c(pathcode,"/","Module 6_Effects_TaxSA+GlobalFund_all scenarios.R"))
