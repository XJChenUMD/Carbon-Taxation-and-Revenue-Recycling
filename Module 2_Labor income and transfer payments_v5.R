


#Elements belong to the transfer payments matrix
#Saving Sf_new, Sg_new, Sp_new;  Income Tax: Td_new
#We do not consider the international transfer.


load(str_c(pathout,"/GTAP_11b_2017_Elements for SAM.Rdata"))


#Part 1: Carbon footprint by products in GTAP 2017
#======================
#Calculation the carbon footprint of products
#-------------------
EMc <- FCO2/TotOutput
EMc[is.na(EMc)] <- 0;EMc[is.infinite(EMc)] <- 0
Carbon_footprint <- EMc %*% B#MT/M$ = T/$
names(Carbon_footprint) <- str_c(rep(regnam, each = N),rep(secnam, G))
Leontief <-  B
FD <- Final_Trade
FD_reg <- array(0,dim = c(GN,G),dimnames = list(regsecnam,regnam))
for (r in 1:G) {
  m <- (r-1)*3+1;n <- r*3
  FD_reg[,r] <- rowSums(FD[,m:n],na.rm = T)
}

VA_mat <- rbind(F_new,ProdTax,TrdTaxSub,FactorTax)
fac <- c("2Capital","1Lab","1Lab","2Capital","3Tax","3Tax","3Tax")
VA_mat <- rowsum(VA_mat,fac,reorder = T)
# VA_mat[which(VA_mat < 0)] <- 10^-5
Vc_mat <- t(t(VA_mat)/TotOutput)
VA_multiplier_Lab <- diag(Vc_mat[1,])%*%Leontief
VA_multiplier_Cap <- diag(Vc_mat[2,])%*%Leontief
VA_multiplier_Tax <- diag(Vc_mat[3,])%*%Leontief

Saving_mat <- rbind(Sp_new,Sf_new,Sg_new)
rownames(Saving_mat) <- instnam
colnames(Saving_mat) <- regnam

rm(A, B, VST_vec,QT_new,SVC_new,
   Final_Trade,Inter_Trade,Import_share,
   Te_new,Tm_new,Ts_new,Tz_new,F_new,Sp_new,Sg_new,Sf_new,
   ProdTax,TrdTaxSub,FactorTax,TrdMargin);gc()
#=====================


# Part 2: expenditure and population by income bins
#======================
#load consumption data and match table
#----------------
Con_200group <-openxlsx::read.xlsx(str_c(pathdata4,"/Consumption decile data/All_consumption_by_200_groups.xlsx"),
                                   sheet = 1)#113 countries

Sector_corr <- openxlsx::read.xlsx (str_c(pathdata4,"/Consumption decile data/Consumption item and sector mapping_EXIOBASE1.xlsx"),
                                    sheet = 6)
rownames(Sector_corr) <- Sector_corr[,1]
Sector_corr <- Sector_corr[,-1]

Reg_WBCD <- unique((Con_200group$ctry))
Reg_WBCD <- Reg_WBCD[!is.na(Reg_WBCD)]
Con_200group$ctry <- rep(Reg_WBCD, each = 201)

Reg_corr_Full <- openxlsx::read.xlsx (str_c(pathdata4,
                                           "/Reg match table_GTAP_WBCD_WBGDP_GTAP11b160reg_v7.xlsx"),
                                     sheet = 1)
Reg_corr <- Reg_corr_Full[which(Reg_corr_Full$Included %in% "Y"),]
Reg_Inclu <- Reg_corr$WBGDPcode
#----------------


#Expenditure and population by bins: would be used in scenarios analysis
#----------------
Expenditure_detail_global <- array(NA, dim = c(GN,201,length(Reg_Inclu)),
                                   dimnames = list(regsecnam, str_c("Bin",0:200), Reg_corr$WBGDPreg))
Exp_perCap_detail_global <- Expenditure_detail_global
Footprint_detail_global <- Expenditure_detail_global
Footprint_perCap_detail_global <- Expenditure_detail_global
# 3D: sectors, bins, countries; GN*201*168

Expenditure_detail <- array(NA, dim = c(N,201,length(Reg_Inclu)),
                            dimnames = list(secnam, str_c("Bin",0:200), Reg_corr$WBGDPreg))
Exp_perCap_detail <- Expenditure_detail
Footprint_detail <- Expenditure_detail
Footprint_perCap_detail <- Expenditure_detail
# 3D: sectors, bins, countries; N*201*168

Inv_detail_global <- array(NA, dim = c(GN,length(Reg_Inclu)),
                              dimnames = list(regsecnam, Reg_corr$WBGDPreg))
Gov_detail_global <- array(NA, dim = c(GN,length(Reg_Inclu)),
                              dimnames = list(regsecnam, Reg_corr$WBGDPreg))
CF_IncluDirect <- Inv_detail_global
Footprint_Inv_detail_global <- Inv_detail_global

Inv_detail <- array(NA, dim = c(N,length(Reg_Inclu)),
                       dimnames = list(secnam, Reg_corr$WBGDPreg))
Footprint_Inv_detail <- Inv_detail

Footprint_Gov_detail_global <- Gov_detail_global

Gov_detail <- array(NA, dim = c(N,length(Reg_Inclu)),
                    dimnames = list(secnam, Reg_corr$WBGDPreg))
Footprint_Gov_detail <- Gov_detail

Population <- array(NA, dim = c(201,length(Reg_Inclu)),
                    dimnames = list(str_c("Bin",0:200), Reg_corr$WBGDPreg))

VA_mat_detail <- array(NA, dim = c(3,N,length(Reg_Inclu)),
                       dimnames = list(rownames(VA_mat),secnam,Reg_corr$WBGDPreg))
Saving_mat_detail <- array(NA, dim = c(3,length(Reg_Inclu)),
                          dimnames = list(rownames(Saving_mat),Reg_corr$WBGDPreg))
Td_vec <- rep(NA,length(Reg_Inclu))


Emission_CB <- array(NA, dim = c(length(secnam),length(Reg_Inclu)),
                     dimnames = list(secnam, Reg_corr$WBGDPreg))#Consumption-based emission, impact tax revenue.
Emission_PB <- array(NA, dim = c(length(secnam),length(Reg_Inclu)),
                     dimnames = list(secnam, Reg_corr$WBGDPreg))#Production-based emission, impact tax revenue.
#----------------

#update WBCD for European countries and Japan
#1. EuroStat
#--------------
# Str_5quin_Euro <- readxl::read_xlsx(str_c(pathdata4,"/Expenditure update data/EuroSTAT_consumption data_2015.xlsx"),
#                                     sheet = 1)#32 countries
Str_5quin_Euro <- openxlsx::read.xlsx(str_c(pathdata4,"/Expenditure update data/EuroSTAT_consumption data_2015.xlsx"),
                                    sheet = 1)
Reg_Euro <- unique(Str_5quin_Euro$`GEO/COICOP`)

# Exp_5quin_Euro <- readxl::read_xlsx(str_c(pathdata4,"/Expenditure update data/EuroSTAT_consumption data_2015.xlsx"),
#                                     sheet = 2)#32 countries
# Exp_5quin_Euro %>% pivot_longer(-`GEO (Labels)`,
#                                 names_to = "Quintile",
#                                 values_to = "Exp") -> Exp_5quin_Euro

Exp_5quin_Euro <- openxlsx::read.xlsx(str_c(pathdata4,"/Expenditure update data/EuroSTAT_consumption data_2015.xlsx"),
                                    sheet = 2)
Exp_5quin_Euro %>% pivot_longer(-`GEO.(Labels)`,
                                names_to = "Quintile",
                                values_to = "Exp") -> Exp_5quin_Euro

Con_5quin_Euro_Scale <- Str_5quin_Euro[,5:16]*(Exp_5quin_Euro$Exp/rowSums(Str_5quin_Euro[,5:16],na.rm = T))
Con_5quin_Euro <- Con_5quin_Euro_Scale
for (cc in 1:length(Reg_Euro)) {
  m = (cc-1)*5+1;n = cc*5
  Con_5quin_Euro[m:n,]<- t(t(Con_5quin_Euro_Scale[m:n,])/colSums(Con_5quin_Euro_Scale[m:n,],na.rm = T))
}


# Sector_corr_Euro <- xlsx::read.xlsx (str_c(pathdata4,"/Expenditure update data/EuroSTAT_consumption data_2015.xlsx"),
#                                      sheetName = "C12_G65")#12 sectors to 65 sectors
Sector_corr_Euro <- openxlsx::read.xlsx(str_c(pathdata4,"/Expenditure update data/EuroSTAT_consumption data_2015.xlsx"),
                                     sheet = 3)#12 sectors to 65 sectors
rownames(Sector_corr_Euro) <- Sector_corr_Euro[,1]
Sector_corr_Euro <- Sector_corr_Euro[,-c(1:2)]
Con_5quin_Euro_65 <- as.matrix(Con_5quin_Euro) %*% as.matrix(Sector_corr_Euro)
#--------------

#2. Japan
#--------------
Con_10dec_Japan <- openxlsx::read.xlsx(str_c(pathdata4,"/Expenditure update data/Japan_decile_consumption_2017.xlsx"),
                                    sheet = 1)#Japan

Sector_corr_Japan <- openxlsx::read.xlsx(str_c(pathdata4,"/Expenditure update data/Japan_decile_consumption_2017.xlsx"),
                                      sheet = 2)

rownames(Sector_corr_Japan) <- Sector_corr_Japan[,1]
Sector_corr_Japan <- Sector_corr_Japan[,-c(1:2)]
Con_10dec_Japan_65 <- as.matrix(Con_10dec_Japan[,2:24]) %*% as.matrix(Sector_corr_Japan)
#--------------

#Countries list need to be updated
REG_update <- c(Reg_Euro,"Japan")#Missing data for China

for (r in 1:length(Reg_Inclu)) {
  t1 <- Sys.time()
  
  #consumption 
  #---------------
  tar <- which(Reg_WBCD %in% Reg_corr$WBCD_reg[r])
  m = (tar-1)*201+1;n = tar*201
  Group200_Sec33 <- Con_200group[m:n,]
  Group200_Sec65 <- as.matrix(Group200_Sec33[,4:36]) %*% as.matrix(Sector_corr)
  
  #Make bins distribution adjustment
  if (sum(REG_update==Reg_corr$WBGDPreg[r]) !=0) {
    if (Reg_corr$WBGDPreg[r] == "Japan"){
      Pop_r <- Group200_Sec33$pop/sum(Group200_Sec33$pop,na.rm = T)*Reg_corr$Pop_2017_WB[r]
      
      Dec_num <- seq(0.1,1,0.1)
      Con_10dec_Old <- array(NA, dim = c(10,65))
      
      a <- trunc(sum(trunc(Pop_r/100))/10)
      for (g in 1:length(secnam)) {
        B <- rep.int(Group200_Sec65[,g]/Pop_r,trunc(Pop_r/100))
        for (p in 1:length(Dec_num)){
          m = (p-1)*a+1; n = p*a
          Con_10dec_Old[p,g] <- sum(B[m:n])*100
        }
      }
      
      Adjust <- Con_10dec_Japan_65/Con_10dec_Old 
      
      for (g in 1:length(secnam)) {
        B <- rep.int(Group200_Sec65[,g]/Pop_r,trunc(Pop_r/100))
        for (p in 1:length(Dec_num)){
          m = (p-1)*a+1; n = p*a
          B[m:n] <- Adjust[p,g]*B[m:n]
        }
        ss <- rowsum(B,rep.int(1:201,trunc(Pop_r/100)),reorder = F)*100
        Group200_Sec65[as.numeric(rownames(ss)),g] <- ss
      }
      Group200_Sec65[is.nan(Group200_Sec65)] <- 0
      
    }else{
      New_quin <- Con_5quin_Euro_65[which(Str_5quin_Euro$`GEO/COICOP` %in% Reg_corr$WBGDPreg[r]),]
      Pop_r <- Group200_Sec33$pop/sum(Group200_Sec33$pop)*Reg_corr$Pop_2017_WB[r]
      
      Quin_num <- seq(0.2,1,0.2)
      Con_5quin_Old <- array(NA, dim = c(5,65))
      
      a <- trunc(sum(trunc(Pop_r/100))/5)
      for (g in 1:length(secnam)) {
        B <- rep.int(Group200_Sec65[,g]/Pop_r,trunc(Pop_r/100))
        for (p in 1:length(Quin_num)){
          m = (p-1)*a+1; n = p*a
          Con_5quin_Old[p,g] <- sum(B[m:n])*100
        }
      }
      
      Adjust <- New_quin/Con_5quin_Old 
      
      for (g in 1:length(secnam)) {
        B <- rep.int(Group200_Sec65[,g]/Pop_r,trunc(Pop_r/100))
        for (p in 1:length(Quin_num)){
          m = (p-1)*a+1; n = p*a
          B[m:n] <- Adjust[p,g]*B[m:n]
        }
        ss <- rowsum(B,rep.int(1:201,trunc(Pop_r/100)),reorder = F)*100
        Group200_Sec65[as.numeric(rownames(ss)),g] <- ss
      }
      Group200_Sec65[is.nan(Group200_Sec65)] <- 0
    }
  }
  
  #consumption in GTAP
  #---------------
  Con_total <- FD[,(Reg_corr$GTAP.NO[r]-1)*3+1]#+1 means Household consumption
  Inv_total <- FD[,(Reg_corr$GTAP.NO[r]-1)*3+2]#+2 means investment
  Gov_total <- FD[,(Reg_corr$GTAP.NO[r]-1)*3+3]#+3 means government expenditure
  #Consumption by bins 
  Expenditure_detail_global[,,r] <- t(pracma::repmat(Group200_Sec65,1,G)*pracma::repmat(Con_total,201,1))
  #Expenditure by sectors and bins. unit: M$
  Inv_detail_global[,r] <- Inv_total
  Gov_detail_global[,r] <- Gov_total
  
  y = (Reg_corr$GTAP.NO[r]-1)*N+1;z = Reg_corr$GTAP.NO[r]*N
  VA_mat_detail[,,r] <- VA_mat[,y:z]
  Saving_mat_detail[,r] <- Saving_mat[,Reg_corr$GTAP.NO[r]]
  Td_vec[r] <- Td_new[Reg_corr$GTAP.NO[r]]
  #---------------
  
  
  #consumption-based/production-based emission (decide revenue)
  #----------
  #indirect emission
  a <- FD_reg[,Reg_corr$GTAP.NO[r]]*Carbon_footprint
  dim(a) <- c(N,G)
  indir_em <- rowSums(a,na.rm = T)
  
  #direct emission from energy use
  CO2_dir <- PCO2
  sec_dir <- hhco2Sec
  hh_em <- rep(0,65); hh_em[match(sec_dir,secnam)] <- CO2_dir[,Reg_corr$GTAP.NO[r]]
  Emission_CB[,r] <- indir_em + hh_em#MT  
  
  #direct industrial emissions
  p = (Reg_corr$GTAP.NO[r]-1)*N+1; q = Reg_corr$GTAP.NO[r]*N
  indus_em <- FCO2[p:q]
  # Emission_PB[,r] <- indus_em + hh_em#MT
  Emission_PB[,r] <- indus_em#MT
  
  #household direct emissions intensity
  con_agg <- rowSums(rowsum(Expenditure_detail_global[,,r],
                            rep(secnam,G),reorder = F,na.rm = T),na.rm = T)[match(sec_dir,secnam)]
  CO2_dir_inten <- rep(0,65)
  CO2_dir_inten[match(sec_dir,secnam)] <- CO2_dir[,Reg_corr$GTAP.NO[r]]/con_agg
  CO2_dir_inten[is.na(CO2_dir_inten)] <- 0;CO2_dir_inten[is.infinite(CO2_dir_inten)] <- 0
  #----------
  
  #Breakdown the carbon emission and consumption of aggregated regions
  if (substr(Reg_corr$GTAP.reg[r],1,1) == "X") {
    share_GDP <- Reg_corr_Full$GDP_2017_WB[which(Reg_corr_Full$WBGDPcode %in% Reg_corr$WBGDPcode[r])]/
      sum(Reg_corr_Full$GDP_2017_WB[which(Reg_corr_Full$GTAP.reg %in% Reg_corr$GTAP.reg[r])],na.rm = T)
    
    Expenditure_detail_global[,,r] <- Expenditure_detail_global[,,r]*share_GDP
    Inv_detail_global[,r] <- Inv_total*share_GDP
    Gov_detail_global[,r] <- Gov_total*share_GDP
    
    #break down VA matrix
    VA_mat_detail[,,r] <- VA_mat_detail[,,r]*share_GDP
    Saving_mat_detail[,r] <- Saving_mat_detail[,r]*share_GDP
    Td_vec[r] <- Td_vec[r]*share_GDP
    
    share_CO2 <- Reg_corr_Full$WB_CO2_2017[which(Reg_corr_Full$WBGDPcode %in% Reg_corr$WBGDPcode[r])]/
      sum(Reg_corr_Full$WB_CO2_2017[which(Reg_corr_Full$GTAP.reg %in% Reg_corr$GTAP.reg[r])],na.rm = T)
      
    Emission_CB[,r] <- Emission_CB[,r]*share_CO2
    #even we calculate consumption-based co2, we still use this since most of emission come from local.
    #Assumption for similar trade pattern for countries within aggregated regions.
    Emission_PB[,r] <- Emission_PB[,r]*share_CO2
  }
  
  #Carbon footprint, indirect and direct, determine carbon tax burden
  Population[,r] <- Group200_Sec33$pop/sum(Group200_Sec33$pop,na.rm = T)*Reg_corr$Pop_2017_WB[r]# unit:1
  Exp_perCap_detail_global[,,r] <- t(t(Expenditure_detail_global[,,r])/Population[,r])*10^6# USD$
  Exp_perCap_detail_global[,,r][is.na(Exp_perCap_detail_global[,,r])] <- 0
  Exp_perCap_detail_global[,,r][is.infinite(Exp_perCap_detail_global[,,r])] <- 0
  
  CF_IncluDirect[,r] <- Carbon_footprint+rep(CO2_dir_inten,G)
  
  Footprint_detail_global[,,r] <- Expenditure_detail_global[,,r]*t(pracma::repmat(CF_IncluDirect[,r],201,1))
  Footprint_perCap_detail_global[,,r] <- t(t(Footprint_detail_global[,,r])/Population[,r])*10^6# ton
  Footprint_perCap_detail_global[,,r][is.na(Footprint_perCap_detail_global[,,r])] <- 0
  Footprint_perCap_detail_global[,,r][is.infinite(Footprint_perCap_detail_global[,,r])] <- 0
  
  Footprint_Inv_detail_global[,r] <- Inv_detail_global[,r]*CF_IncluDirect[,r]
  Footprint_Gov_detail_global[,r] <- Gov_detail_global[,r]*CF_IncluDirect[,r]
  
  #Aggregate to 65 sectors
  Expenditure_detail[,,r] <- rowsum(Expenditure_detail_global[,,r],rep(secnam,G),reorder = F, na.rm = T)
  Exp_perCap_detail[,,r] <- rowsum(Exp_perCap_detail_global[,,r],rep(secnam,G),reorder = F,na.rm = T)
  Footprint_detail[,,r] <- rowsum(Footprint_detail_global[,,r],rep(secnam,G),reorder = F,na.rm = T)
  Footprint_perCap_detail[,,r] <- rowsum(Footprint_perCap_detail_global[,,r],rep(secnam,G),reorder = F,na.rm = T)
  Inv_detail[,r] <- rowsum(Inv_detail_global[,r],rep(secnam,G),reorder = F,na.rm = T)
  Gov_detail[,r] <- rowsum(Gov_detail_global[,r],rep(secnam,G),reorder = F,na.rm = T)
  
  Footprint_Inv_detail[,r] <- rowsum(Footprint_Inv_detail_global[,r],rep(secnam,G),reorder = F,na.rm = T)
  Footprint_Gov_detail[,r] <- rowsum(Footprint_Gov_detail_global[,r],rep(secnam,G),reorder = F,na.rm = T)
  print(str_c("Time cost---",r,"---",Reg_Inclu[r],"---",round(Sys.time()-t1,2)))
}
#----------------


Expenditure_perCap <- t(apply(Exp_perCap_detail, 2, colSums, na.rm=T))

Gnam <- rownames(Expenditure_perCap)

#Balance countries' transfer payment account one by one.
#----------------
Transfer_mat <- array(NA, dim = c(length(Gnam)+2,3,length(Reg_Inclu)),
                      dimnames = list(c(Gnam,"Firms","Gov"),
                                      c("Lab","Cap","Tax"),
                                      Reg_Inclu))

LISSheetName <- openxlsx::getSheetNames(str_c(pathdata4,"/LIS/Dataset_LIS_4AprbyDaniele.xlsx"))

for (r in 1:length(Reg_Inclu)) {
  LIS_raw <- openxlsx::read.xlsx(str_c(pathdata4,"/LIS/Dataset_LIS_4AprbyDaniele.xlsx"),
                                 sheet = which(LISSheetName %in% Reg_corr$LIS_reg[r]))
  
  LIS <- as.data.frame(array(0,dim = c(100,3),dimnames = list(1:100,c("Lab","Cap","Tax"))))
  
  LIS$Lab <- as.numeric(LIS_raw$`pc_hil~r`[2:101])
  LIS$Cap <- as.numeric(LIS_raw$`pc_hic~l`[2:101])
  LIS$Tax <- as.numeric(LIS_raw$`pc_hit~r`[2:101])
  
  if (sum(Population[,r]) < 10^7) {
    popcum <- cumsum(Population[,r])
    LABfull <- rep(LIS$Lab, each = ceiling(sum(Population[,r])/100))
    CAPfull <- rep(LIS$Cap, each = ceiling(sum(Population[,r])/100))
    TAXfull <- rep(LIS$Tax, each = ceiling(sum(Population[,r])/100))
  } else {
    popcum <- cumsum(Population[,r])/100
    LABfull <- rep(LIS$Lab, each = ceiling(sum(Population[,r])/100/100))*100
    CAPfull <- rep(LIS$Cap, each = ceiling(sum(Population[,r])/100/100))*100
    TAXfull <- rep(LIS$Tax, each = ceiling(sum(Population[,r])/100/100))*100
  }
  
  LIS_tar <- as.data.frame(array(0,dim = c(201,3),dimnames = list(Gnam,c("Lab","Cap","Tax"))))
  
  for (q in 1:201) {
    if(q == 1) {  
      if (popcum[q] != 0) {
        LIS_tar$Lab[q] <- sum(LABfull[1:popcum[q]])
        LIS_tar$Cap[q] <- sum(CAPfull[1:popcum[q]])
        LIS_tar$Tax[q] <- sum(TAXfull[1:popcum[q]])
      }
    } else {
      if((popcum[q] - popcum[q-1]) != 0){
        LIS_tar$Lab[q] <- sum(LABfull[popcum[q-1]:popcum[q]])
        LIS_tar$Cap[q] <- sum(CAPfull[popcum[q-1]:popcum[q]])
        LIS_tar$Tax[q] <- sum(TAXfull[popcum[q-1]:popcum[q]])
      }
    }
  }
  
  Tar_mat <- Transfer_mat[,,r]
  Tar_mat[203,2] <- Td_vec[r]
  Tar_mat[203,3] <- sum(Gov_detail_global[,r])-Td_vec[r]
  Tar_mat[202,1] <- Saving_mat_detail[1,r]
  Tar_mat[202,2] <- Saving_mat_detail[2,r]
  Tar_mat[202,3] <- Saving_mat_detail[3,r]
  
  #Constraints
  HouExp <- colSums(Expenditure_detail_global[,,r])
  scale_fac <- (sum(HouExp) + sum(Gov_detail_global[,r]) + sum(Inv_detail_global[,r]))/
    sum(VA_mat_detail[,,r])
  
  HouToHou <- sum(VA_mat_detail[1,,r])*scale_fac - Tar_mat[202,1]
  FirmToHou <- sum(VA_mat_detail[2,,r])*scale_fac - Tar_mat[202,2] - Tar_mat[203,2]
  GovToHou <- sum(VA_mat_detail[3,,r])*scale_fac - Tar_mat[203,3] - Tar_mat[202,3] 
  Tar_cons <- c(HouToHou,FirmToHou,GovToHou)
  
  if(sum(Tar_cons < 0) != 0) {#Money transfer to household can not be zero
    Tar_cons[which(Tar_cons <0)] <- 1#set a small value
  }
  
  Tar_mat[1:201,1:3] <- as.matrix(LIS_tar)%*%diag(Tar_cons/colSums(LIS_tar))
  Tar_mat[is.na(Tar_mat)] <- 0
  
  Col_cons <- rowSums(VA_mat_detail[,,r])*scale_fac
  Row_cons <- c(HouExp,sum(Inv_detail_global[,r]),sum(Gov_detail_global[,r]))
  
  #Use RAS to rebalance
  while(mean(abs(rowSums(Tar_mat)-Row_cons)/sum(Row_cons)) > 0.001) {
    #row adjustment
    Tar_mat <- t(t(Tar_mat)%*%diag(Row_cons/rowSums(Tar_mat)))
    Tar_mat[is.nan(Tar_mat)] <- 0
    
    #col adjustment
    Tar_mat <- Tar_mat%*%diag(Col_cons/colSums(Tar_mat))
    Tar_mat[is.nan(Tar_mat)] <- 0
    
    print(str_c(r,Reg_Inclu[r],"---",
                round(mean(abs(rowSums(Tar_mat)-Row_cons)/sum(Row_cons))*100,8)))
  }
  
  Transfer_mat[,,r] <- t(t(Tar_mat)/colSums(Tar_mat))
}
#----------------


#Save results
#----------------
save(Saving_mat_detail,VA_mat_detail,Td_vec,Transfer_mat,
     VA_multiplier_Lab,VA_multiplier_Cap,VA_multiplier_Tax,
     Reg_corr_Full,Footprint_detail,
     Footprint_perCap_detail,
     # Expenditure_detail,Inv_detail,Gov_detail,Carbon_footprint,
     Footprint_detail_global,
     Exp_perCap_detail,Expenditure_perCap,
     Inv_detail_global,Gov_detail_global,
     Expenditure_detail_global,
     Population,Emission_CB,Emission_PB,regsecnam, regnam, secnam,Gnam,
     Reg_corr, Reg_Inclu, Reg_WBCD, 
     Leontief, EMc,N,G,GN,
     CF_IncluDirect,
     file = str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))
#---------------
#======================


rm(list = ls()[-which(ls() %in% c("path","pathout","pathout2","pathout3","pathout4","pathout5",
                                  "pathdata3","pathdata4","pathcode"))])
gc()