#Module 1: Expenditure, carbon footprint by income bins.
#For 201 bins. 65 sectors.
#2022-12-06
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu


#Part 1: Carbon footprint by products in GTAP 2017
#======================
#MRIO and CO2
#-------------------
#Load GTAP data sets
flnm<-c(str_c(pathdata,"/GTAP MRIO 2017/2017 GTAP10 MRIO origin basic element.RData"))
load(flnm)
regnam <- toupper(regnam)

#Reading  carbon emissions data  Unit: MT CO2
CO2 <- read.csv(str_c(pathdata,"/GTAP MRIO 2017/GTAP V11 2017 carbon emission.csv"))
CO2 <- apply(CO2[,-1], 2, as.numeric)
dim(CO2) <- c(1,GN)
#-------------------

#Calculation the carbon footprint of products
#-------------------
EMc <- CO2/X
EMc[is.na(EMc)] <- 0;EMc[is.infinite(EMc)] <- 0
Carbon_footprint <- EMc %*% B#MT/M$ = T/$

names(Carbon_footprint) <- str_c(rep(regnam, each = N),rep(secnam, G))
#-------------------
Leontief <-  B
rm(A, B, VST, VA, Vc, Y, X);gc()
#=====================


# Part 2: expenditure and population by income bins
#======================
#load consumption data and match table
#----------------
Con_200group <- readxl::read_xlsx(str_c(pathdata,"/Consumption decile data/All_consumption_by_200_groups.xlsx"),
                                  sheet = 1)#113 countries

Sector_corr <- xlsx::read.xlsx (str_c(pathdata,"/Consumption decile data/Consumption item and sector mapping_EXIOBASE1.xlsx"),
                                sheetName = "conc34_GTAP65")
rownames(Sector_corr) <- Sector_corr[,1]
Sector_corr <- Sector_corr[,-1]

Reg_WBCD <- unique((Con_200group$ctry))
Reg_WBCD <- Reg_WBCD[!is.na(Reg_WBCD)]
Con_200group$ctry <- rep(Reg_WBCD, each = 201)

Reg_corr_Full <- readxl::read_excel (str_c(pathdata,"/Reg match table_GTAP_WBCD_WBGDP_v3.xlsx"),
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

InvGov_detail_global <- array(NA, dim = c(GN,length(Reg_Inclu)),
                              dimnames = list(regsecnam, Reg_corr$WBGDPreg))
Footprint_InvGov_detail_global <- InvGov_detail_global

InvGov_detail <- array(NA, dim = c(N,length(Reg_Inclu)),
                       dimnames = list(secnam, Reg_corr$WBGDPreg))
Footprint_InvGov_detail <- InvGov_detail

Population <- array(NA, dim = c(201,length(Reg_Inclu)),
                    dimnames = list(str_c("Bin",0:200), Reg_corr$WBGDPreg))

Emission_CB <- array(NA, dim = c(length(secnam),length(Reg_Inclu)),
                      dimnames = list(secnam, Reg_corr$WBGDPreg))#Consumption-based emission, impact tax revenue.
Emission_PB <- array(NA, dim = c(length(secnam),length(Reg_Inclu)),
                     dimnames = list(secnam, Reg_corr$WBGDPreg))#Production-based emission, impact tax revenue.
#----------------

#update WBCD for European countries and Japan
#1. EuroStat
#--------------
Str_5quin_Euro <- readxl::read_xlsx(str_c(pathdata,"/Expenditure update data/EuroSTAT_consumption data_2015.xlsx"),
                                    sheet = 1)#32 countries
Reg_Euro <- unique(Str_5quin_Euro$`GEO/COICOP`)

Exp_5quin_Euro <- readxl::read_xlsx(str_c(pathdata,"/Expenditure update data/EuroSTAT_consumption data_2015.xlsx"),
                                    sheet = 2)#32 countries

Exp_5quin_Euro %>% pivot_longer(-`GEO (Labels)`,
                                names_to = "Quintile",
                                values_to = "Exp") -> Exp_5quin_Euro

Con_5quin_Euro_Scale <- Str_5quin_Euro[,5:16]*(Exp_5quin_Euro$Exp/rowSums(Str_5quin_Euro[,5:16],na.rm = T))
Con_5quin_Euro <- Con_5quin_Euro_Scale
for (cc in 1:length(Reg_Euro)) {
  m = (cc-1)*5+1;n = cc*5
  Con_5quin_Euro[m:n,]<- t(t(Con_5quin_Euro_Scale[m:n,])/colSums(Con_5quin_Euro_Scale[m:n,],na.rm = T))
}


Sector_corr_Euro <- xlsx::read.xlsx (str_c(pathdata,"/Expenditure update data/EuroSTAT_consumption data_2015.xlsx"),
                                     sheetName = "C12_G65")#12 sectors to 65 sectors
rownames(Sector_corr_Euro) <- Sector_corr_Euro[,1]
Sector_corr_Euro <- Sector_corr_Euro[,-c(1:2)]
Con_5quin_Euro_65 <- as.matrix(Con_5quin_Euro) %*% as.matrix(Sector_corr_Euro)
#--------------

#2. Japan
#--------------
Con_10dec_Japan <- xlsx::read.xlsx (str_c(pathdata,"/Expenditure update data/Japan_decile_consumption_2017.xlsx"),
                                    sheetName = "Consumption")#Japan

Sector_corr_Japan <- xlsx::read.xlsx (str_c(pathdata,"/Expenditure update data/Japan_decile_consumption_2017.xlsx"),
                                      sheetName = "Sector_corr")#23 sectors to 65 sectors
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
  Con_total <- FD[,(Reg_corr$GTAP.NO[r]-1)*3+2]#+2 means Household consumption
  InvGov_total <- FD[,(Reg_corr$GTAP.NO[r]-1)*3+1]+FD[,(Reg_corr$GTAP.NO[r]-1)*3+3]
  #Consumption by bins 
  Expenditure_detail_global[,,r] <- t(pracma::repmat(Group200_Sec65,1,G)*pracma::repmat(Con_total,201,1))
  #Expenditure by sectors and bins. unit: M$
  InvGov_detail_global[,r] <- InvGov_total
  #---------------
  
  
  #consumption-based/production-based emission (decide revenue)
  #----------
  #indirect emission
  a <- FD_reg[,Reg_corr$GTAP.NO[r]]*Carbon_footprint
  dim(a) <- c(N,G)
  indir_em <- rowSums(a,na.rm = T)
  
  #direct emission from energy use
  CO2_dir <- read.csv(str_c(pathdata,"/GTAP MRIO 2017/household direct emission_GTAP_2017.csv"))
  sec_dir <- substr(CO2_dir[,1],3,5)
  CO2_dir <- apply(CO2_dir[,-1], 2, as.numeric)
  hh_em <- rep(0,65); hh_em[match(sec_dir,secnam)] <- CO2_dir[,Reg_corr$GTAP.NO[r]]
  Emission_CB[,r] <- indir_em + hh_em#MT  
  
  #direct industrial emissions
  p = (Reg_corr$GTAP.NO[r]-1)*N+1; q = Reg_corr$GTAP.NO[r]*N
  indus_em <- CO2[p:q]
  Emission_PB[,r] <- indus_em + hh_em#MT
  
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
    InvGov_detail_global[,r] <- InvGov_total*share_GDP
    
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

  Footprint_detail_global[,,r] <- Expenditure_detail_global[,,r]*
    t(pracma::repmat(Carbon_footprint,201,1)+pracma::repmat(rep(CO2_dir_inten,G),201,1))
  Footprint_perCap_detail_global[,,r] <- t(t(Footprint_detail_global[,,r])/Population[,r])*10^6# ton
  Footprint_perCap_detail_global[,,r][is.na(Footprint_perCap_detail_global[,,r])] <- 0
  Footprint_perCap_detail_global[,,r][is.infinite(Footprint_perCap_detail_global[,,r])] <- 0
  
  Footprint_InvGov_detail_global[,r] <- InvGov_detail_global[,r]*(Carbon_footprint+rep(CO2_dir_inten,G))
  
  #Aggregate to 65 sectors
  Expenditure_detail[,,r] <- rowsum(Expenditure_detail_global[,,r],rep(secnam,G),reorder = F, na.rm = T)
  Exp_perCap_detail[,,r] <- rowsum(Exp_perCap_detail_global[,,r],rep(secnam,G),reorder = F,na.rm = T)
  Footprint_detail[,,r] <- rowsum(Footprint_detail_global[,,r],rep(secnam,G),reorder = F,na.rm = T)
  Footprint_perCap_detail[,,r] <- rowsum(Footprint_perCap_detail_global[,,r],rep(secnam,G),reorder = F,na.rm = T)
  InvGov_detail[,r] <- rowsum(InvGov_detail_global[,r],rep(secnam,G),reorder = F,na.rm = T)
  Footprint_InvGov_detail[,r] <- rowsum(Footprint_InvGov_detail_global[,r],rep(secnam,G),reorder = F,na.rm = T)
  print(str_c("Time cost---",r,"---",Reg_Inclu[r],"---",round(Sys.time()-t1,2)))
}
#----------------

Expenditure_perCap <- t(apply(Exp_perCap_detail, 2, colSums, na.rm=T))

#Save results
#----------------
save(Footprint_perCap_detail,Footprint_detail_global,Footprint_InvGov_detail,
     Expenditure_detail,Exp_perCap_detail,Footprint_detail,Expenditure_perCap,InvGov_detail,
     Population,Emission_CB,Emission_PB,regsecnam, regnam, secnam,
     Reg_corr, Reg_corr_Full,Reg_Inclu, Reg_WBCD, 
     Leontief, EMc,N,G,GN,
     file = str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))

rm(Carbon_footprint, CO2, CO2_dir,CO2_dir_inten,Con_10dec_Old,Con_10dec_Japan,
Con_10dec_Japan_65,Con_200group,Con_5quin_Euro,Emission_PB,
Con_5quin_Euro_65,Con_5quin_Euro_Scale,Con_5quin_Old, con_agg,
Con_total, Exp_5quin_Euro,FD,Group200_Sec33,Group200_Sec65,               
hh_em,indus_em,New_quin,Pop_r,Quin_num, indir_em,            
Reg_Euro,REG_update,sec_dir,Sector_corr,Sector_corr_Euro,Sector_corr_Japan,share_CO2, share_GDP,                
Str_5quin_Euro, Dec_num, B, N,G,GN,Expenditure_perCap,InvGov_total,p,q,
Adjust,ss,a,cc,flnm,g,m,n,r,t1,tar,
Expenditure_detail_global,Exp_perCap_detail_global,Footprint_detail_global,InvGov_detail_global,Footprint_InvGov_detail,
Expenditure_detail,Exp_perCap_detail,Footprint_detail,InvGov_detail,Footprint_InvGov_detail_global,
Population,Emission_CB,regsecnam, regnam, secnam,Leontief, EMc,FD_reg,
Reg_corr, Reg_corr_Full,Reg_Inclu, Reg_WBCD,
Footprint_perCap_detail,Footprint_perCap_detail_global)
gc()
#---------------
#======================
