#Module 3: social assistance scenarios
#2022-12-06
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu

# 1.	Without recycling 
# 2.	lump-sum
# 3.	Current social assistance programs
# 4.	Social assistance during covid-19 period
# 5.	PMT

load(str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))

SP_Scenarios <- array(0,dim = c(201,length(Reg_Inclu),5),
                      dimnames = list(str_c("Bin",0:200), Reg_Inclu,
                                      c("Null","Universal","SP_current","SP_covid","PMT")))

# 1.	Without recycling 
#--------------------
SP_Scenarios[,,1] <- 0
#--------------------

# 2.	lump-sum 
#--------------------
SP_Scenarios[,,2] <- 1
#--------------------

# 3.	Current social assistance programs
# 4.	Social assistance during covid-19 period
# 5.	PMT
#--------------------
SP_bins <- readxl::read_xlsx(str_c(pathdata,"/final_data_SP scenarios.xlsx"),
                             sheet = 1)
SP_Scen <- c("coverage_S2","coverage_S3","coverage_S4")
SP_num <-  which(colnames(SP_bins) %in% SP_Scen)

for (z in 1:length(SP_Scen)) {
  Cover_raw <- apply(SP_bins[,SP_num[z]],2,as.vector)/100
  dim(Cover_raw) <- c(201,length(Reg_WBCD))
  colnames(Cover_raw) <- Reg_WBCD
  SP_Scenarios[,,2+z]  <- Cover_raw[,match(Reg_corr$WBCD_reg,Reg_WBCD)] 
}
#--------------------
save(SP_Scenarios,
     file = str_c(pathout,"/Social assistance scenarios.Rdata"))

rm(SP_Scenarios,SP_Scen,SP_num,SP_bins,Cover_raw,z,
   
   Footprint_perCap_detail,Footprint_detail_global,
   # Footprint_perCap_detail_global,Expenditure_detail_global,Exp_perCap_detail_global,
   Expenditure_detail,Exp_perCap_detail,Footprint_detail,Expenditure_perCap,
   Population,Emission_CB,Emission_PB,regsecnam, regnam, secnam,
   Reg_corr, Reg_corr_Full,Reg_Inclu, Reg_WBCD, 
   Leontief, EMc,N,G,GN)
gc()