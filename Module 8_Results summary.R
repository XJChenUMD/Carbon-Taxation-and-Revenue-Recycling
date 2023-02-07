#Module 8: Results summary for visualization
#2022-12-26
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu


load(str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))
load(str_c(pathout,"/Social assistance scenarios.Rdata"))
load(str_c(pathout,"/Climate finance scenarios.Rdata"))
load(str_c(pathout2,"/",50," Tax scenarios and revenue.Rdata"))

load(str_c(pathout3,"/",50,"-", 30,
           "Global poverty under TAX+SA scenarios",".Rdata"))
Global_PovInq_SATAX_30 <- Global_PovInq_SATAX

load(str_c(pathout3,"/",25,"-", 100,
           "Global poverty under TAX+SA scenarios",".Rdata"))
Global_PovInq_SATAX_25tax <- Global_PovInq_SATAX

load(str_c(pathout3,"/",75,"-", 100,
           "Global poverty under TAX+SA scenarios",".Rdata"))
Global_PovInq_SATAX_75tax <- Global_PovInq_SATAX

load(str_c(pathout3,"/",100,"-", 100,
           "Global poverty under TAX+SA scenarios",".Rdata"))
Global_PovInq_SATAX_100tax <- Global_PovInq_SATAX

load(str_c(pathout3,"/",50,"-", 100,
           "Global poverty under TAX+SA scenarios",".Rdata"))
load(str_c(pathout4,"/",50,"-", 100,
           "Global poverty under TAX+SA+GF scenarios",".Rdata"))
#Fig1: Tax scenarios setting.
#===================
# Tax scenarios 
#---------------
Reg <- unique(Reg_corr$region_class_new)
#Tax level-CBDR pro-based/con-based
CBDR <- array(NA, dim=c(length(Reg),3))
colnames(CBDR) <- c("Region","Tax_C","Tax_P")
CBDR[,1] <- unique(Reg_corr$region_class_new)
CBDR[,2] <- rowsum(Tax_reg_C*colSums(Emission_CB),Reg_corr$region_class_new,reorder = F)/
  rowsum(colSums(Emission_CB),Reg_corr$region_class_new,reorder = F)
CBDR[,3] <- rowsum(Tax_reg_P*colSums(Emission_PB),Reg_corr$region_class_new,reorder = F)/
  rowsum(colSums(Emission_PB),Reg_corr$region_class_new,reorder = F)

#Tax level-Luxury (sectoral difference)
Luxury <- array(NA, dim=c(length(secnam),3))
colnames(Luxury) <- c("Secnam","Elas","TaxRate")
Luxury[,1] <- secnam
Luxury[,2] <- rowMeans(Elasticity_raw)
Luxury[,3] <- rowMeans(Tax_Scenarios[,,3])
 
#Tax revenue-amount
Revenue <- rowsum(Tax_revenue[,1:7],Reg_corr$region_class_new,reorder = F)#million

#Tax revenue-share of GDP
RevenuetoGDP <- rowsum(Tax_revenue[,1:7],Reg_corr$region_class_new,reorder = F)/
                   pracma::repmat(rowsum(Tax_revenue[,8],Reg_corr$region_class_new,reorder = F),1,7)
#------------


#Tax burden by decile_country level
#----------
DEC_name <- c("G10","G20","G30","G40","G50","G60","G70","G80","G90","G100")
DEC_num <- seq(0.1,1,0.1)

Population_decile <- array(NA,dim = c(length(DEC_name),length(Reg_Inclu)),
                           dimnames = list(DEC_name,Reg_Inclu))
Expenditure_decile <- Population_decile
Carbon_cost_decile <- array(NA,dim = c(length(DEC_name),length(Reg_Inclu),dim(Tax_Scenarios)[3]),
                            dimnames = list(DEC_name,Reg_Inclu,dimnames(Tax_Scenarios)[[3]]))
Cost_perEXP_decile <- Carbon_cost_decile
Cost_perCap_decile <- Carbon_cost_decile
  
for (i in 1:dim(Tax_Scenarios)[3]) {
  load(str_c(pathout3,"/",50,"-", 
             dimnames(Tax_Scenarios)[[3]][i],"-",100,
             dimnames(SP_Scenarios)[[3]][1],".Rdata"))
  for (q in 1:length(Reg_Inclu)) {
    a <- trunc(sum(trunc(Population[,q]/100))/10)#Population for each decile
    B <- rep.int(Expenditure_perCap[,q],trunc(Population[,q]/100))#change the unit for pop to boost calculation
    C <- rep.int(Cost_perCap[,q],trunc(Population[,q]/100))
    
    for (p in 1:length(DEC_name)){
      Population_decile[p,q] <- a*100
      Expenditure_decile[p,q] <- sum(B[((p-1)*a+1):(p*a)])*100
      Carbon_cost_decile[p,q,i] <- sum(C[((p-1)*a+1):(p*a)])*100
    }
  }
  Cost_perEXP_decile[,,i] <-  Carbon_cost_decile[,,i]/Expenditure_decile#unit: 1
  Cost_perCap_decile[,,i] <- Carbon_cost_decile[,,i] /Population_decile#unit: $
}
Expenditure_perCap_decile  <- Expenditure_decile/Population_decile#unit: $
rm(a,B,C);gc()
#----------

#Tax burden by decile_regional level
#----------
Population_decile_agg <- array(NA,dim = c(length(DEC_name),length(Reg)),
                               dimnames = list(DEC_name,Reg))
Expenditure_decile_agg <- Population_decile_agg
Carbon_cost_decile_agg <- array(NA,dim = c(length(DEC_name),length(Reg),dim(Tax_Scenarios)[3]),
                                dimnames = list(DEC_name,Reg,dimnames(Tax_Scenarios)[[3]]))
Cost_perEXP_decile_agg <- Carbon_cost_decile_agg 
Cost_perCap_decile_agg <- Carbon_cost_decile_agg 

for (i in 1:dim(Tax_Scenarios)[3]) {
  load(str_c(pathout3,"/",50,"-", 
             dimnames(Tax_Scenarios)[[3]][i],"-",100,
             dimnames(SP_Scenarios)[[3]][1],".Rdata"))
  
  Population_agg <- t(rowsum(t(Population),Reg_corr$region_class_new,na.rm = T,reorder = F))
  Carbon_cost_agg <- t(rowsum(t(Cost_perCap*Population),Reg_corr$region_class_new,na.rm = T,reorder = F))
  Expenditure_agg <- t(rowsum(t(Expenditure_perCap*Population),Reg_corr$region_class_new,na.rm = T,reorder = F))
  
  for (q in 1:length(Reg)) {
    a <- trunc(sum(trunc(Population_agg[,q]/100))/10)#Population for each decile
    B <- rep.int(Expenditure_agg[,q]/Population_agg[,q],trunc(Population_agg[,q]/100))#change the unit for pop to boost calculation
    C <- rep.int(Carbon_cost_agg[,q]/Population_agg[,q],trunc(Population_agg[,q]/100))

    for (p in 1:length(DEC_name)){
      Population_decile_agg[p,q] <- a*100
      m = (p-1)*a+1; n = p*a
      Expenditure_decile_agg[p,q] <- sum(B[m:n])*100
      Carbon_cost_decile_agg[p,q,i] <- sum(C[m:n])*100
    }
  }
  Cost_perEXP_decile_agg[,,i] <-  Carbon_cost_decile_agg[,,i]/Expenditure_decile_agg#unit: 1
  Cost_perCap_decile_agg[,,i] <- Carbon_cost_decile_agg[,,i] /Population_decile_agg#unit: $
}
Expenditure_perCap_decile_agg  <- Expenditure_decile_agg/Population_decile_agg#unit: $
rm(a,B,C);gc()


Name_Tax <- dimnames(Cost_perEXP_decile_agg)[[3]]
A <- as.data.frame(Cost_perEXP_decile_agg[,,1])
A$Group <- rownames(Cost_perEXP_decile_agg[,,1])
A %>% pivot_longer(-Group,
                   names_to = "Region",
                   values_to = Name_Tax[1]) ->X1

A <- as.data.frame(Cost_perEXP_decile_agg[,,2]) 
A$Group <- rownames(Cost_perEXP_decile_agg[,,2])
A %>% pivot_longer(-Group,
                   names_to = "Region",
                   values_to = Name_Tax[2]) ->X2

A <- as.data.frame(Cost_perEXP_decile_agg[,,3]) 
A$Group <- rownames(Cost_perEXP_decile_agg[,,3])
A %>% pivot_longer(-Group,
                   names_to = "Region",
                   values_to = Name_Tax[3]) ->X3

A <- as.data.frame(Cost_perEXP_decile_agg[,,4]) 
A$Group <- rownames(Cost_perEXP_decile_agg[,,4])
A %>% pivot_longer(-Group,
                   names_to = "Region",
                   values_to = Name_Tax[4]) ->X4

A <- as.data.frame(Cost_perEXP_decile_agg[,,5]) 
A$Group <- rownames(Cost_perEXP_decile_agg[,,5])
A %>% pivot_longer(-Group,
                   names_to = "Region",
                   values_to = Name_Tax[5]) ->X5

A <- as.data.frame(Cost_perEXP_decile_agg[,,6]) 
A$Group <- rownames(Cost_perEXP_decile_agg[,,6])
A %>% pivot_longer(-Group,
                   names_to = "Region",
                   values_to = Name_Tax[6]) ->X6

Cost_perEXP_All <- merge(merge(merge(merge(X1,X2),X3),X4),merge(X5,X6))
rm(A,X1,X2,X3,X4,X5,X6,Name_Tax)
#----------
#===================

#Fig2: Recycling scenarios setting.
#===================
#Social assistance scenarios
#---------------
Cover_decile_agg <- array(NA,dim = c(length(DEC_name),length(Reg),dim(SP_Scenarios)[3]),
                          dimnames = list(DEC_name,Reg,dimnames(SP_Scenarios)[[3]]))

for (i in 1:dim(SP_Scenarios)[3]){
  Pop_tar <- Population*SP_Scenarios[,,i]
  Pop_tar_agg <- t(rowsum(t(Pop_tar),Reg_corr$region_class_new,na.rm = T,reorder = F))
  Cover_agg <- Pop_tar_agg/Population_agg
  Pop_tar_decile_agg <- array(NA,dim = c(length(DEC_name),length(Reg)),
                              dimnames = list(DEC_name,Reg))
  
  for (q in 1:length(Reg)) {
    a <- trunc(sum(trunc(Population_agg[,q]/100))/10)#Population for each decile
    B <- rep.int(Cover_agg[,q],trunc(Population_agg[,q]/100))#change the unit for pop to boost calculation
    for (p in 1:length(DEC_name)) {Pop_tar_decile_agg[p,q] <- sum(B[((p-1)*a+1):(p*a)],na.rm = T)*100}
  }
  rm(a,B);gc()
  
  Cover_decile_agg[,,i] <- Pop_tar_decile_agg/Population_decile_agg*100
}

Name_SP <- dimnames(Cover_decile_agg)[[3]]
A <- as.data.frame(Cover_decile_agg[,,2])
A$Group <- rownames(Cover_decile_agg[,,2])
A %>% pivot_longer(-Group,
                   names_to = "Region",
                   values_to = Name_SP[2]) ->X1

A <- as.data.frame(Cover_decile_agg[,,3])
A$Group <- rownames(Cover_decile_agg[,,3])
A %>% pivot_longer(-Group,
                   names_to = "Region",
                   values_to = Name_SP[3]) ->X2

A <- as.data.frame(Cover_decile_agg[,,4])
A$Group <- rownames(Cover_decile_agg[,,4])
A %>% pivot_longer(-Group,
                   names_to = "Region",
                   values_to = Name_SP[4]) ->X3

A <- as.data.frame(Cover_decile_agg[,,5])
A$Group <- rownames(Cover_decile_agg[,,5])
A %>% pivot_longer(-Group,
                   names_to = "Region",
                   values_to = Name_SP[5]) ->X4

Cover_All <- merge(merge(merge(X1,X2),X3),X4)
rm(A,X1,X2,X3,X4,Name_SP)
#---------------

#Global fund scenarios
#---------------
Reg_Fund <- rowsum(ClimateFund,Reg_corr$region_class_new,reorder = F)
#---------------
#===================

#Fig3: TAX+SA scenarios outcome.
#===================
#Already prepared: Global_PovInq_SATAX
#===================

#Fig4: TAX+SA+GF scenarios outcome.
#===================
#Already prepared: Global_PovInq_SATAX_GF

#Regional level: poverty and inequality within country
#----------
Best_Tax2 <- which(dimnames(Tax_Scenarios)[[3]] %in% Best_Real_GF[1])
Best_SA2 <- which(dimnames(SP_Scenarios)[[3]] %in% Best_Real_GF[2])
Best_GF2 <- which(colnames(ClimateFund) %in% Best_Real_GF[3])

#Four scenarios: Null, tax without recycling, best tax + SA; best TAX+SA+GF
Reg_Outcome <- array(NA,dim = c(length(Reg),7,4),
                     dimnames = list(Reg,
                                     c("PPop_npl_tax", "PR_npl_tax","PPop_ipl_tax", "PR_ipl_tax",
                                       "Gini_within_tax","PPop_npl_chg","PPop_ipl_chg"),
                                     c("Null","Tax","TaxSA","TaxSAGF")))

load(str_c(pathout3,"/",50,"-", 
           "Null","-",100,"Null",".Rdata"))
OUT_TAXSP <- as.data.frame(OUT_TAXSP)
Reg_Outcome[,1,1] <- rowsum(OUT_TAXSP$PPop_npl_tax,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,2,1] <- Reg_Outcome[,1,1]/rowsum(OUT_TAXSP$Pop,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,3,1] <- rowsum(OUT_TAXSP$PPop_ipl_tax,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,4,1] <- Reg_Outcome[,3,1]/rowsum(OUT_TAXSP$Pop,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,6,1] <- Reg_Outcome[,1,1]-rowsum(OUT_TAXSP$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,7,1] <- Reg_Outcome[,3,1]-rowsum(OUT_TAXSP$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)
for (r in 1:length(Reg)) {
  a <- Reg_corr$region_class_new %in% Reg[r]
  Reg_Outcome[r,5,1] <- weighted.mean(OUT_TAXSP$Gini_tax[a],OUT_TAXSP$Pop[a],na.rm = T)
}


load(str_c(pathout3,"/",50,"-", 
           dimnames(Tax_Scenarios)[[3]][Best_Tax2],"-",100,"Null",".Rdata"))
OUT_TAXSP <- as.data.frame(OUT_TAXSP)
Reg_Outcome[,1,2] <- rowsum(OUT_TAXSP$PPop_npl_tax,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,2,2] <- Reg_Outcome[,1,2]/rowsum(OUT_TAXSP$Pop,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,3,2] <- rowsum(OUT_TAXSP$PPop_ipl_tax,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,4,2] <- Reg_Outcome[,3,2]/rowsum(OUT_TAXSP$Pop,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,6,2] <- Reg_Outcome[,1,2]-rowsum(OUT_TAXSP$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,7,2] <- Reg_Outcome[,3,2]-rowsum(OUT_TAXSP$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)
for (r in 1:length(Reg)) {
  a <- Reg_corr$region_class_new %in% Reg[r]
  Reg_Outcome[r,5,2] <- weighted.mean(OUT_TAXSP$Gini_tax[a],OUT_TAXSP$Pop[a],na.rm = T)
}

load(str_c(pathout3,"/",50,"-", 
           dimnames(Tax_Scenarios)[[3]][Best_Tax2],"-",100,
           dimnames(SP_Scenarios)[[3]][Best_SA2],".Rdata"))
OUT_TAXSP <- as.data.frame(OUT_TAXSP)
Reg_Outcome[,1,3] <- rowsum(OUT_TAXSP$PPop_npl_tax,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,2,3] <- Reg_Outcome[,1,3]/rowsum(OUT_TAXSP$Pop,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,3,3] <- rowsum(OUT_TAXSP$PPop_ipl_tax,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,4,3] <- Reg_Outcome[,3,3]/rowsum(OUT_TAXSP$Pop,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,6,3] <- Reg_Outcome[,1,3]-rowsum(OUT_TAXSP$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,7,3] <- Reg_Outcome[,3,3]-rowsum(OUT_TAXSP$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)
for (r in 1:length(Reg)) {
  a <- Reg_corr$region_class_new %in% Reg[r]
  Reg_Outcome[r,5,3] <- weighted.mean(OUT_TAXSP$Gini_tax[a],OUT_TAXSP$Pop[a],na.rm = T)
}

load(str_c(pathout4,"/",50,"-", 
           dimnames(Tax_Scenarios)[[3]][Best_Tax2],"-",100,
           dimnames(SP_Scenarios)[[3]][Best_SA2],"-",
           colnames(ClimateFund)[Best_GF2],".Rdata"))
OUT_TAXSP <- as.data.frame(OUT_TAXSP_GF)
Reg_Outcome[,1,4] <- rowsum(OUT_TAXSP$PPop_npl_tax,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,2,4] <- Reg_Outcome[,1,4]/rowsum(OUT_TAXSP$Pop,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,3,4] <- rowsum(OUT_TAXSP$PPop_ipl_tax,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,4,4] <- Reg_Outcome[,3,4]/rowsum(OUT_TAXSP$Pop,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,5,4] <- weighted.mean(OUT_TAXSP$Gini_tax,OUT_TAXSP$Pop,na.rm = T)
Reg_Outcome[,6,4] <- Reg_Outcome[,1,4]-rowsum(OUT_TAXSP$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)
Reg_Outcome[,7,4] <- Reg_Outcome[,3,4]-rowsum(OUT_TAXSP$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)
for (r in 1:length(Reg)) {
  a <- Reg_corr$region_class_new %in% Reg[r]
  Reg_Outcome[r,5,4] <- weighted.mean(OUT_TAXSP$Gini_tax[a],OUT_TAXSP$Pop[a],na.rm = T)
}
#----------

#Different tax level and climate fund
#Already prepared: Poverty_GF,Poverty_taxlevel
load(str_c(pathout4,"/",dimnames(Tax_Scenarios)[[3]][Best_Tax2],"-",
           dimnames(SP_Scenarios)[[3]][Best_SA2],"-",
           colnames(ClimateFund)[Best_GF2],"-",
           "Poverty_Tax_Global fund_Tradeoff",".Rdata"))
#===================


#Fig S2 & S3: national based consumption and production tax.
#===================
a <- cbind(Tax_reg_C,Tax_reg_P)
colnames(a) <- c("CB","PB")
write.csv(a,
          file = str_c(pathout,"/national tax level ($pertCO2).csv"))
#===================

#Fig S4: sectoral elasticity and tax
#===================
#elasticity
write.csv(Elasticity_raw,file = str_c(pathout,"/Elasticity_raw.csv"))
#tax ($/tCO2)
Coun_Sec_Tax <- Tax_Scenarios[,,3]
write.csv(Tax_Scenarios[,,3],
          file = str_c(pathout,"/Luxury tax level by country and sector ($pertCO2).csv"))
#===================


#Fig S5: national poverty under the best policy
#===================
load(str_c(pathout4,"/",50,"-", 
      dimnames(Tax_Scenarios)[[3]][Best_Tax2],"-",100,
      dimnames(SP_Scenarios)[[3]][Best_SA2],"-",
      colnames(ClimateFund)[Best_GF2],".Rdata"))
#===================


save(CBDR,Luxury,Revenue,RevenuetoGDP,Cost_perEXP_All,Tax_reg_C,Tax_reg_P,Elasticity_raw,
     Cost_perCap_decile_agg,Cost_perEXP_decile_agg,Coun_Sec_Tax,OUT_TAXSP_GF,
     Cost_perCap_decile,Cost_perEXP_decile,
     Cover_decile_agg,Reg_Fund,Reg,Cover_All,
     Global_PovInq_SATAX,Global_PovInq_SATAX_30,Global_PovInq_SATAX_GF,Reg_Outcome,
     Poverty_GF,Poverty_taxlevel,Best_PMT,Best_PMT_GF,Best_Real_GF,
     Global_PovInq_SATAX_25tax,Global_PovInq_SATAX_75tax,Global_PovInq_SATAX_100tax,
     file = str_c(pathout5,"/Results summary.Rdata"))

rm(CBDR,Luxury,Revenue,RevenuetoGDP,Cost_perEXP_All,
   Cost_perCap_decile_agg,Cost_perEXP_decile_agg,
   Cover_decile_agg,Reg_Fund,
   Reg_Outcome,Cover_All,
   Cover_agg,Expenditure_agg,Expenditure_decile,Expenditure_decile_agg,
   Expenditure_perCap_decile,Expenditure_perCap_decile_agg,Pop_tar_agg,
   Pop_tar_decile_agg,Population_agg,Population_decile_agg,
   Carbon_cost_agg,Carbon_cost_decile_agg,Carbon_cost_decile,
   Cost_perCap_decile,Cost_perEXP_decile,Population_decile,
   DEC_num,DEC_name,Reg,
   
   OUT_TAXSP,OUT_TAXSP_GF,
   
   Poverty_GF,Poverty_taxlevel,Best_GF2,Best_SA2,Best_Tax2,
   
   Global_PovInq_SATAX_GF,Best_PMT_GF,Best_Real_GF,Global_PovInq_SATAX,Best_PMT,Best_Real,
   
   Expen_perCap_untar,Expen_perCap_tar,Expen_tax,
   Cost_perCap,Benifit_tar,Pop_untar,Pop_tar,Poor_SP_tar,Poor_SP_untar,
   
   Tax_Scenarios, Tax_revenue, Level, Elasticity,Elasticity_raw,Tax_reg_C,Tax_reg_P,
   
   SP_Scenarios, ClimateFund,Poverty.Gap,Poverty.Headcount,PR_2017,NPL_2017,developed,developing,
   
   Footprint_perCap_detail,Footprint_detail_global,
   # Footprint_perCap_detail_global,Expenditure_detail_global,Exp_perCap_detail_global,
   Expenditure_detail,Exp_perCap_detail,Footprint_detail,Expenditure_perCap,
   Population,Emission_CB,Emission_PB,regsecnam, regnam, secnam,
   Reg_corr, Reg_corr_Full,Reg_Inclu, Reg_WBCD, 
   Leontief, EMc,N,G,GN)
gc()
