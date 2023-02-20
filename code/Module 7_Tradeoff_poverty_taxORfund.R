#Module 7: Trade off for poverty reduction between Global Climate Fund (0-1000) and Carbon Tax Level (50-150)
#2022-12-26
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu

#Only focus on extreme poverty


load(str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))
load(str_c(pathout,"/Social assistance scenarios.Rdata"))
load(str_c(pathout,"/Climate finance scenarios.Rdata"))
load(str_c(pathout2,"/",50," Tax scenarios and revenue.Rdata"))
load(str_c(pathout3,"/",50,"-", 100,
           "Global poverty under TAX+SA scenarios",".Rdata"))
load(str_c(pathout4,"/",50,"-", 100,
           "Global poverty under TAX+SA+GF scenarios",".Rdata"))


#Poverty reduction under different tax level (based on the best real Tax+SA+GF)
#=============
Tax_Level <- c(5:200)
Poverty_taxlevel <- array(NA,dim = c(length(Tax_Level),6))
colnames(Poverty_taxlevel) <- c("TaxLevel","PPop_ipl_ori", "PR_ipl_ori",
                                "PPop_ipl_tax", "PR_ipl_tax", "PR_ipl_chg")

Best_Tax2 <- which(dimnames(Tax_Scenarios)[[3]] %in% Best_Real_GF[1])
Best_SA2 <- which(dimnames(SP_Scenarios)[[3]] %in% Best_Real_GF[2])
Best_GF2 <- which(colnames(ClimateFund) %in% Best_Real_GF[3])

for(i in 1:length(Tax_Level)){
  t1 <- Sys.time()
  Poverty_taxlevel[i,1] <- Tax_Level[i]
  #Poverty definition
  #------------
  npl <- pracma::repmat(NPL_2017,201,1)
  Expenditure_perCap <- t(apply(Exp_perCap_detail, 2, colSums, na.rm=T))
  Poor <- array(0,dim = dim(Population))
  
  d <- Expenditure_perCap < npl
  d[is.na(d)] <- FALSE# define the poverty
  Poor[d] <- Population[d]
  
  #original poverty calculation
  Poverty_taxlevel[i,2] <- sum(Poor)#Poverty headcount under ipl
  Poverty_taxlevel[i,3] <- sum(Poor)/sum(Population)#Poverty rate under ipl
  #------------
  
  #Tax+SP scenarios
  #------------
  #tax
  Cost_perCap<- array(0,dim = dim(Population))
  for (r in 1:length(Reg_Inclu)) {
    Cost_perCap[,r] <- t(Footprint_perCap_detail[,,r])%*%Tax_Scenarios[,r,Best_Tax2]*(Tax_Level[i]/50)
  }
  Expen_tax <- Expenditure_perCap - Cost_perCap
  
  #social assistance
  Pop_tar <- Population*SP_Scenarios[,,Best_SA2]
  Pop_untar <- Population-Pop_tar
  
  Benifit_tar <- (Tax_revenue[,Best_Tax2]*(Tax_Level[i]/50)+ClimateFund[,Best_GF2])/colSums(Pop_tar)*10^6#$ Tax revenue adjusted by climate fund

  Expen_perCap_untar <- Expen_tax
  Expen_perCap_tar <- Expen_tax + pracma::repmat(Benifit_tar,201,1)
  
  #poverty calculation
  d <- Expen_perCap_untar < npl
  d[is.na(d)] <- FALSE
  Poor_SP_untar <- array(0,dim = dim(Expenditure_perCap))
  Poor_SP_untar[d] <- Pop_untar[d]
  
  d <- Expen_perCap_tar < npl
  d[is.na(d)] <- FALSE
  Poor_SP_tar <- array(0,dim = dim(Expenditure_perCap))
  Poor_SP_tar[d] <- Pop_tar[d]
  
  Poverty_taxlevel[i,4] <- sum(Poor_SP_untar + Poor_SP_tar)
  Poverty_taxlevel[i,5] <- sum(Poor_SP_untar + Poor_SP_tar)/sum(Population)
  
  Poverty_taxlevel[i,6] <- Poverty_taxlevel[i,4]-Poverty_taxlevel[i,2]
  #------------
  print(str_c("Time cost: ", Best_Tax2,dimnames(Tax_Scenarios)[[3]][Best_Tax2],"-",
              Best_SA2,dimnames(SP_Scenarios)[[3]][Best_SA2],"-",colnames(ClimateFund)[Best_GF2],
              "---",Tax_Level[i],"---",
              round(Sys.time()-t1,2)))
}
#=============



#Poverty reduction under different scale of global fund (based on the best real Tax+SA+GF)
#=============
Global_Fund <- seq(0,1000,10)

Poverty_GF <- array(NA,dim = c(length(Global_Fund),6))
colnames(Poverty_GF) <- c("TaxLevel","PPop_ipl_ori", "PR_ipl_ori",
                          "PPop_ipl_tax","PR_ipl_tax","PPop_ipl_chg")

for(i in 1:length(Global_Fund)){
  t1 <- Sys.time()
  Poverty_GF[i,1] <- Global_Fund[i]
  #Poverty definition
  #------------
  npl <- pracma::repmat(NPL_2017,201,1)
  Expenditure_perCap <- t(apply(Exp_perCap_detail, 2, colSums))
  Poor <- array(0,dim = dim(Population))
  
  d <- Expenditure_perCap < npl
  d[is.na(d)] <- FALSE# define the poverty
  Poor[d] <- Population[d]
  
  #original poverty calculation
  Poverty_GF[i,2] <- sum(Poor)#Poverty headcount under ipl
  Poverty_GF[i,3] <- sum(Poor)/sum(Population)#Poverty rate under ipl
  #------------
  
  #Tax+SP scenarios
  #------------
  #tax
  Cost_perCap<- array(0,dim = dim(Population))
  for (r in 1:length(Reg_Inclu)) {
    Cost_perCap[,r] <- t(Footprint_perCap_detail[,,r])%*%Tax_Scenarios[,r,Best_Tax2]
  }
  Expen_tax <- Expenditure_perCap - Cost_perCap
  
  #social assistance
  Pop_tar <- Population*SP_Scenarios[,,Best_SA2]
  Pop_untar <- Population-Pop_tar
  
  Benifit_tar <- (Tax_revenue[,Best_Tax2]+ClimateFund[,Best_GF2]*(Global_Fund[i]/100))/colSums(Pop_tar)*10^6#$ Tax revenue adjusted by climate fund
 
  Expen_perCap_untar <- Expen_tax
  Expen_perCap_tar <- Expen_tax + pracma::repmat(Benifit_tar,201,1)
  
  #poverty calculation
  d <- Expen_perCap_untar < npl
  d[is.na(d)] <- FALSE
  Poor_SP_untar <- array(0,dim = dim(Expenditure_perCap))
  Poor_SP_untar[d] <- Pop_untar[d]
  
  d <- Expen_perCap_tar < npl
  d[is.na(d)] <- FALSE
  Poor_SP_tar <- array(0,dim = dim(Expenditure_perCap))
  Poor_SP_tar[d] <- Pop_tar[d]
  
  Poverty_GF[i,4] <- sum(Poor_SP_untar + Poor_SP_tar)
  Poverty_GF[i,5] <- sum(Poor_SP_untar + Poor_SP_tar)/sum(Population)
  
  Poverty_GF[i,6] <- Poverty_GF[i,4]-Poverty_GF[i,2]
  #------------
  print(str_c("Time cost: ", Best_Tax2,dimnames(Tax_Scenarios)[[3]][Best_Tax2],"-",
              Best_SA2,dimnames(SP_Scenarios)[[3]][Best_SA2],"-",
              Best_GF2,colnames(ClimateFund)[Best_GF2],
              "---",Global_Fund[i],"---",
              round(Sys.time()-t1,2)))
}
#=============
save(Poverty_GF,Poverty_taxlevel,Best_GF2,Best_SA2,Best_Tax2,
     file = str_c(pathout4,"/",dimnames(Tax_Scenarios)[[3]][Best_Tax2],"-",
                  dimnames(SP_Scenarios)[[3]][Best_SA2],"-",
                  colnames(ClimateFund)[Best_GF2],"-",
                  "Poverty_Tax_Global fund_Tradeoff",".Rdata"))


rm(Global_PovInq_SATAX_GF,Best_PMT_GF,Best_Real_GF,Global_PovInq_SATAX,Best_PMT,Best_Real,
   Global_Fund,Tax_Level,
   Expen_perCap_untar,Expen_perCap_tar,Expen_tax,
   Cost_perCap,Benifit_tar,Pop_untar,Pop_tar,Poor_SP_tar,Poor_SP_untar,
   
   Tax_Scenarios, Tax_revenue, Level, Elasticity,Elasticity_raw,
   
   SP_Scenarios, ClimateFund,Poverty.Gap,Poverty.Headcount,PR_2017,NPL_2017,developed,developing,
   npl,Poor,
   
   d,Footprint_InvGov_detail,InvGov_detail,Poverty_GF,Poverty_taxlevel,
   Best_GF2,Best_SA,Best_SA2,Best_Tax,Best_Tax2,i,r,Tax_reg_P,Tax_reg_C,
   
   Footprint_perCap_detail,Footprint_detail_global,
   Expenditure_detail,Exp_perCap_detail,Footprint_detail,Expenditure_perCap,
   Population,Emission_CB,Emission_PB,regsecnam, regnam, secnam,
   Reg_corr, Reg_corr_Full,Reg_Inclu, Reg_WBCD, 
   Leontief, EMc,N,G,GN)
gc()
