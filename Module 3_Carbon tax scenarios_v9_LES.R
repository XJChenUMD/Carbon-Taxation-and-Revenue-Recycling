

#Module 3: carbon tax scenarios
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu

# 1.	Global universal tax. production tax
# 2.	Global universal tax. consumption tax
# 3.	Luxury tax. Different tax level between sectors (Yannick’s method), remain a uniform national averagely tax.
# 4.	Countries have different carbon tax rates decided by income level and PB-CO2(affordable)  
# 5.	Countries have different carbon tax rates decided by income level and CB-CO2(affordable) . 
# 6.	Countries have different carbon tax rates (decide national average tax rate) + Luxury tax (decide sectoral rates).
# 7.  No tax.
# Calculate the price changes by products and demand, CO2 response to carbon tax.
# Also, calculate the tax revenue under each scenario.

#Carbon budget: CO2 in 2023 is 36.8 Gt CO2
# remaining budget for 1.5 degree is 250 Gt CO2 (r =0.853) 
# remaining budget for 2 degree is 120 Gt CO2 (r =0.973)
# total emission in 2017 fr GTAP 11b, 32971.25 MT CO2.
# Yearly decrement for 1.5 goal :4847 Mt;  for 2 goal: 890 Mt


#Load the data from the results in Module 2
load(str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))
regnam <- toupper(regnam)
#--------------

# estimate the expenditure elasticity
# --------------
Elasticity_raw <- array(0,dim = c(N,length(Reg_Inclu)))
rownames(Elasticity_raw) <- secnam;colnames(Elasticity_raw) <- Reg_Inclu

for (r in 1:length(Reg_Inclu)) {
  Exp_Sec <- Exp_perCap_detail[,,r]
  Exp <- colSums(Exp_perCap_detail[,,r],na.rm = T)
  for (j in 1:N) {
    if (sum(Exp_Sec[j,],na.rm = T) != 0){
      a <- Exp_Sec[j,]
      data <- data.frame(cbind(log(a),log(Exp)))
      data[is.na(data) | data=="Inf"| data=="-Inf"] = NaN
      equation <- lm(data = data, X1 ~ X2)
      Elasticity_raw[j,r] <- equation$coefficients[2]
    }
  }
}
write.csv(Elasticity_raw,file = str_c(pathout2,"/Elasticity_raw.csv"))

Elasticity <- Elasticity_raw
Elasticity[Elasticity<=0] <- 0.1#treatment for negative elasticity
#--------------


#calculate some summary statistics to save time---------
FinalDemand_ori <- apply(Expenditure_detail_global, c(1,3), sum) + Gov_detail_global + Inv_detail_global

DemandTot_ori <- array(0,dim = c(203,length(Reg_Inclu)),
                       dimnames = list(c(Gnam,"Firms","Gov"),Reg_Inclu))

#Locate the "minimum demand" (committed consumption)
#people live below the extreme poverty line ware unable to adjust their consumption structure
#not the extreme one, but the bottom one in each country
Minumum <- array(0,dim = dim(FinalDemand_ori),dimnames = dimnames(FinalDemand_ori))
Minumum_reg <- array(0,dim = c(203,length(Reg_Inclu)))
for (r in 1:length(Reg_Inclu)) {
  a <- which(colSums(Expenditure_detail_global[,,r]) > 0)[1]
  Minumum[,r] <- Expenditure_detail_global[,a,r]/Population[a,r]*sum(Population[,r])
  Minumum_reg[1:201,r] <- sum(Expenditure_detail_global[,a,r])/Population[a,r]*Population[,r]
}
Mimunum_sum <- colSums(Minumum)

INT2 <- array(0,dim = c(GN,203,length(Reg_Inclu)),
                      dimnames = list(regsecnam,c( Gnam,"Firms","Gov"),Reg_Inclu))

for (r in 1:length(Reg_Inclu)) {
  int <- cbind(Expenditure_detail_global[,,r],Inv_detail_global[,r],Gov_detail_global[,r])
  xx <- array(0,dim = c(GN, length(Gnam)+2))
  xx[,1:length(Gnam)] <- t(pracma::repmat((Minumum[,r]/sum(Population[,r])),length(Gnam),1))%*%diag(Population[,r])
  INT2[,,r] <- t(t(int-xx)/(colSums(int)-Minumum_reg[,r]))
  DemandTot_ori[,r] <- colSums(int)
}
INT2[is.nan(INT2)] <- 0
INT2[is.infinite(INT2)] <- 0
#---------


Tax_Level <- c(0:180)#US$/ton
# Tax_Level <- seq(0,180,15)#US$/ton
Tax_level_nam <- str_c("L",Tax_Level)

#define output variable
#-----------
Tax_Scenarios <- array(0,dim = c(N,length(Reg_Inclu),7,length(Tax_Level)),
                       dimnames = list(secnam, Reg_Inclu,
                                       c("Universal_P","Universal_C","Luxury","CBDR_P","CBDR_C","Luxury&CBDR_C","Null"),
                                       Tax_level_nam))
#"Universal_P" and "CBDR_P" are production tax; 
#"Universal_C","CBDR_C","Luxury" and "Luxury&CBDR" are consumption tax;
# All tax scenarios transit to a consumption tax to improve the code efficiency for calculating tax burden

Tax_revenue <- array(0,dim = c(length(Reg_Inclu),8,length(Tax_Level)),
                     dimnames = list(Reg_Inclu,
                                     c("Universal_P","Universal_C","Luxury","CBDR_P","CBDR_C","Luxury&CBDR","Null","GDP"),
                                     Tax_level_nam))

Price_Response <- array(0,dim = c(GN,length(Reg_Inclu),7,length(Tax_Level)),
                        dimnames = list(regsecnam, Reg_Inclu,
                                        c("Universal_P","Universal_C","Luxury","CBDR_P","CBDR_C","Luxury&CBDR_C","Null"),
                                        Tax_level_nam))

CO2_Response <- array(0,dim = c(7,length(Tax_Level)),
                      dimnames = list(c("Universal_P","Universal_C","Luxury","CBDR_P","CBDR_C","Luxury&CBDR_C","Null"),
                                      Tax_level_nam))

CO2_Reg_Response <- array(0,dim = c(length(Reg_Inclu),7,length(Tax_Level)),
                      dimnames = list(Reg_Inclu,c("Universal_P","Universal_C","Luxury","CBDR_P","CBDR_C","Luxury&CBDR_C","Null"),
                                      Tax_level_nam))

DemandTot_new <- array(0,dim = c(203,length(Reg_Inclu),7,length(Tax_Level)),
                       dimnames = list(c(Gnam,"Firms","Gov"),Reg_Inclu,
                                       c("Universal_P","Universal_C","Luxury","CBDR_P","CBDR_C","Luxury&CBDR_C","Null"),Tax_level_nam))

VA_Tot_Chg <- array(0,dim = c(3,length(Reg_Inclu),7,length(Tax_Level)),
                    dimnames = list(c("Lab","Capital","Tax"),Reg_Inclu,
                                    c("Universal_P","Universal_C","Luxury","CBDR_P","CBDR_C","Luxury&CBDR_C","Null"),Tax_level_nam))


Tax_reg_P <- array(0,dim = c(length(Reg_Inclu),length(Tax_Level)),
                   dimnames = list(Reg_Inclu,Tax_level_nam))

Tax_reg_C <- Tax_reg_P
#-----------

for (k in 1:length(Tax_Level)) {
  t1 <- Sys.time()
  print(str_c("Begin calculation for ",Tax_Level[k]," $/tCO2"))

  Level <- Tax_Level[k]
  Tax_revenue[,8,k] <- Reg_corr$GDP_2017_WB/10^6#M$
  
  #Part 1: Global universal tax. production tax
  #--------------
  Tax_S1 <- array(Level,dim = c(N,length(Reg_Inclu)),
                  dimnames = list(secnam,Reg_corr$WBGDPreg))
  Tax_Scenarios[,,1,k] <- Tax_S1
  Tax_revenue[,1,k] <- colSums(Emission_PB,na.rm = T)*Level#M$
  
  Price_Response[,,1,k] <- CF_IncluDirect*Level+1
  FinalDemand_New <- Minumum + (FinalDemand_ori-Minumum)/(CF_IncluDirect*Level+1)
 
  FinalDemand_chg <- rowSums(FinalDemand_New)-rowSums(FinalDemand_ori)
  INT <- rbind(rowSums(VA_multiplier_Lab%*%FinalDemand_chg),
               rowSums(VA_multiplier_Cap%*%FinalDemand_chg),
               rowSums(VA_multiplier_Tax%*%FinalDemand_chg))
  
  VA_mat_detail_chg <- VA_mat_detail
  DemandTot_new_int <- array(0,dim = c(203,length(Reg_Inclu)))
  CO2_Reg_Response_int <- c()
  for (r in 1:length(Reg_Inclu)) {
    y = (Reg_corr$GTAP.NO[r]-1)*N+1;z = Reg_corr$GTAP.NO[r]*N
    VA_mat_detail_chg[,,r] <- INT[,y:z]
    if (substr(Reg_corr$GTAP.reg[r],1,1) == "X") {
      share_GDP <- Reg_corr_Full$GDP_2017_WB[which(Reg_corr_Full$WBGDPcode %in% Reg_corr$WBGDPcode[r])]/
        sum(Reg_corr_Full$GDP_2017_WB[which(Reg_corr_Full$GTAP.reg %in% Reg_corr$GTAP.reg[r])],na.rm = T)
      VA_mat_detail_chg[,,r] <- VA_mat_detail_chg[,,r]*share_GDP
    }
    DemandTot_new_int[,r] <- DemandTot_ori[,r] +
      rowSums(Transfer_mat[,,r]%*%diag(rowSums(VA_mat_detail_chg[,,r])))
    CO2_Reg_Response_int[r] <- sum((Minumum[,r]+
                                     INT2[,,r]%*%(DemandTot_new_int[,r]-Minumum_reg[,r])/
                                     (CF_IncluDirect[,r]*Level+1))*CF_IncluDirect[,r])
  }
  
  VA_Tot_Chg[,,1,k] <- apply(VA_mat_detail_chg,3,rowSums)
  DemandTot_new[,,1,k] <- DemandTot_new_int
  CO2_Reg_Response[,1,k] <- CO2_Reg_Response_int
  #--------------
  
  #Part 2: Global universal tax. consumption tax
  #--------------
  Tax_S2 <- array(Level,dim = c(N,length(Reg_Inclu)))
  Tax_Scenarios[,,2,k] <- Tax_S2
  Tax_revenue[,2,k] <- colSums(Emission_CB,na.rm = T)*Level#M$
  
  Price_Response[,,2,k] <- CF_IncluDirect*Level+1
  FinalDemand_New <- Minumum + (FinalDemand_ori-Minumum)/(CF_IncluDirect*Level+1)
  
  FinalDemand_chg <- rowSums(FinalDemand_New)-rowSums(FinalDemand_ori)
  INT <- rbind(rowSums(VA_multiplier_Lab%*%FinalDemand_chg),
               rowSums(VA_multiplier_Cap%*%FinalDemand_chg),
               rowSums(VA_multiplier_Tax%*%FinalDemand_chg))

  VA_mat_detail_chg <- VA_mat_detail
  DemandTot_new_int <- array(0,dim = c(203,length(Reg_Inclu)))
  CO2_Reg_Response_int <- c()
  for (r in 1:length(Reg_Inclu)) {
    y = (Reg_corr$GTAP.NO[r]-1)*N+1;z = Reg_corr$GTAP.NO[r]*N
    VA_mat_detail_chg[,,r] <- INT[,y:z]
    if (substr(Reg_corr$GTAP.reg[r],1,1) == "X") {
      share_GDP <- Reg_corr_Full$GDP_2017_WB[which(Reg_corr_Full$WBGDPcode %in% Reg_corr$WBGDPcode[r])]/
        sum(Reg_corr_Full$GDP_2017_WB[which(Reg_corr_Full$GTAP.reg %in% Reg_corr$GTAP.reg[r])],na.rm = T)
      VA_mat_detail_chg[,,r] <- VA_mat_detail_chg[,,r]*share_GDP
    }
    DemandTot_new_int[,r] <- DemandTot_ori[,r] +
      rowSums(Transfer_mat[,,r]%*%diag(rowSums(VA_mat_detail_chg[,,r])))
    CO2_Reg_Response_int[r] <- sum((Minumum[,r]+
                                     INT2[,,r]%*%(DemandTot_new_int[,r]-Minumum_reg[,r])/
                                     (CF_IncluDirect[,r]*Level+1))*CF_IncluDirect[,r])
  }
  
  VA_Tot_Chg[,,2,k] <- apply(VA_mat_detail_chg,3,rowSums)
  DemandTot_new[,,2,k] <- DemandTot_new_int
  CO2_Reg_Response[,2,k] <- CO2_Reg_Response_int
  #--------------
  
  #Part 3: Luxury tax. consumption tax
  #--------------
  Tax_S3 <- array(0,dim = c(N,length(Reg_Inclu)),
                  dimnames = list(secnam,Reg_corr$WBGDPreg))
  
  if (Level != 0){
    for (r in 1:length(Reg_Inclu)) {
      Tax_S3[,r] <- Level*Elasticity[,r]*
        (Level/weighted.mean(Level*Elasticity[,r],Emission_CB[,r]))
    }
  }
  
  Tax_Scenarios[,,3,k] <- Tax_S3
  Tax_revenue[,3,k] <- colSums(Tax_S3*Emission_CB,na.rm = T)

  LevelbyProduct <- array(0, dim = dim(CF_IncluDirect))
  for (r in 1:length(Reg_Inclu)) {
    LevelbyProduct[,r] <- rep(Tax_S3[,r], G)
  }
  
  Price_Response[,,3,k] <- CF_IncluDirect*LevelbyProduct+1
  FinalDemand_New <- Minumum + (FinalDemand_ori-Minumum)/(CF_IncluDirect*LevelbyProduct+1)
  
  FinalDemand_chg <- rowSums(FinalDemand_New)-rowSums(FinalDemand_ori)
  INT <- rbind(rowSums(VA_multiplier_Lab%*%FinalDemand_chg),
               rowSums(VA_multiplier_Cap%*%FinalDemand_chg),
               rowSums(VA_multiplier_Tax%*%FinalDemand_chg))
  
  VA_mat_detail_chg <- VA_mat_detail
  DemandTot_new_int <- array(0,dim = c(203,length(Reg_Inclu)))
  CO2_Reg_Response_int <- c()
  for (r in 1:length(Reg_Inclu)) {
    y = (Reg_corr$GTAP.NO[r]-1)*N+1;z = Reg_corr$GTAP.NO[r]*N
    VA_mat_detail_chg[,,r] <- INT[,y:z]
    if (substr(Reg_corr$GTAP.reg[r],1,1) == "X") {
      share_GDP <- Reg_corr_Full$GDP_2017_WB[which(Reg_corr_Full$WBGDPcode %in% Reg_corr$WBGDPcode[r])]/
        sum(Reg_corr_Full$GDP_2017_WB[which(Reg_corr_Full$GTAP.reg %in% Reg_corr$GTAP.reg[r])],na.rm = T)
      VA_mat_detail_chg[,,r] <- VA_mat_detail_chg[,,r]*share_GDP
    }
    DemandTot_new_int[,r] <- DemandTot_ori[,r] +
      rowSums(Transfer_mat[,,r]%*%diag(rowSums(VA_mat_detail_chg[,,r])))
    CO2_Reg_Response_int[r] <- sum((Minumum[,r]+
                                     INT2[,,r]%*%(DemandTot_new_int[,r]-Minumum_reg[,r])/
                                     (CF_IncluDirect[,r]*LevelbyProduct[,r]+1))*CF_IncluDirect[,r])
  }
  
  VA_Tot_Chg[,,3,k] <- apply(VA_mat_detail_chg,3,rowSums)
  DemandTot_new[,,3,k] <- DemandTot_new_int
  CO2_Reg_Response[,3,k] <- CO2_Reg_Response_int
  #--------------
  
  #Part 4: Countries have different carbon tax rates. production tax
  #--------------
  Tax_S4 <- array(0,dim = c(N,length(Reg_Inclu)),
                  dimnames = list(secnam,Reg_corr$WBGDPreg))
  
  #Assign same tax level for countries with same development level. Same level, same tax.
  #The GDP per capita and PB emission by development level.
  a <-Reg_corr
  a$Emi_CB <- colSums(Emission_CB)
  a$Emi_PB <- colSums(Emission_PB)
  a$EXP <- colSums((Expenditure_perCap*Population))
  a %>% group_by(WB_IncomeGroup) %>% summarise(GDP_perCap = sum(GDP_2017_WB)/sum(Pop_2017_WB),
                                               EXP_perCap = sum(EXP)/sum(Pop_2017_WB),
                                               Emi_CB = sum(Emi_CB),
                                               Emi_PB = sum(Emi_PB)) -> b 
  if (Level != 0){
    c <- Level*b$GDP_perCap*(Level/weighted.mean(Level*b$GDP_perCap,b$Emi_PB))
    Tax_reg_P[,k] <-  c[match(Reg_corr$WB_IncomeGroup,b$WB_IncomeGroup)] 
  }
  
  #Construct tax vector matched with GTAP, production tax
  Tax_Gtap <- rep(0,GN)
  for (r in 1:G) {
    p <- (r-1)*N+1; q <- r*N
    a <- which(Reg_corr$GTAP.reg %in% regnam[r])
    Tax_Gtap[p:q] <- weighted.mean(Tax_reg_P[a,k],Reg_corr$WB_CO2_2017[a],na.rm = T)
  }
  Tax_Gtap[is.na(Tax_Gtap)] <- Level
  
  #Transit to the consumption based tax for 168 countries
  for (r in 1:length(Reg_Inclu)) {
    tax <- Tax_Gtap
    a <- which(regnam %in% Reg_corr$GTAP.reg[r])
    p <- (a-1)*N+1; q <- a*N
    tax[p:q] <- Tax_reg_P[r,k]
    Tax_detail_global <- tax*rowSums(Footprint_detail_global[,,r],na.rm = T)
    dim(Tax_detail_global) <- c(N,G)
    Tax_S4[,r] <- rowSums(Tax_detail_global,na.rm = T)/rowSums(Footprint_detail[,,r],na.rm = T)
  }
  Tax_S4[is.na(Tax_S4)] <- 0
  Tax_Scenarios[,,4,k] <- Tax_S4
  Tax_revenue[,4,k] <- Tax_reg_P[,k]*colSums(Emission_PB,na.rm = T)

  LevelbyProduct <- array(0, dim = dim(CF_IncluDirect))
  for (r in 1:length(Reg_Inclu)) {
    LevelbyProduct[,r] <- rep(Tax_S4[,r], G)
  }
  
  Price_Response[,,4,k] <- CF_IncluDirect*LevelbyProduct+1
  FinalDemand_New <- Minumum + (FinalDemand_ori-Minumum)/(CF_IncluDirect*LevelbyProduct+1)
  
  FinalDemand_chg <- rowSums(FinalDemand_New)-rowSums(FinalDemand_ori)
  INT <- rbind(rowSums(VA_multiplier_Lab%*%FinalDemand_chg),
               rowSums(VA_multiplier_Cap%*%FinalDemand_chg),
               rowSums(VA_multiplier_Tax%*%FinalDemand_chg))
  
  VA_mat_detail_chg <- VA_mat_detail
  DemandTot_new_int <- array(0,dim = c(203,length(Reg_Inclu)))
  CO2_Reg_Response_int <- c()
  for (r in 1:length(Reg_Inclu)) {
    y = (Reg_corr$GTAP.NO[r]-1)*N+1;z = Reg_corr$GTAP.NO[r]*N
    VA_mat_detail_chg[,,r] <- INT[,y:z]
    if (substr(Reg_corr$GTAP.reg[r],1,1) == "X") {
      share_GDP <- Reg_corr_Full$GDP_2017_WB[which(Reg_corr_Full$WBGDPcode %in% Reg_corr$WBGDPcode[r])]/
        sum(Reg_corr_Full$GDP_2017_WB[which(Reg_corr_Full$GTAP.reg %in% Reg_corr$GTAP.reg[r])],na.rm = T)
      VA_mat_detail_chg[,,r] <- VA_mat_detail_chg[,,r]*share_GDP
    }
    DemandTot_new_int[,r] <- DemandTot_ori[,r] +
      rowSums(Transfer_mat[,,r]%*%diag(rowSums(VA_mat_detail_chg[,,r])))
    CO2_Reg_Response_int[r] <- sum((Minumum[,r]+
                                     INT2[,,r]%*%(DemandTot_new_int[,r]-Minumum_reg[,r])/
                                     (CF_IncluDirect[,r]*LevelbyProduct[,r]+1))*CF_IncluDirect[,r])
  }
  
  VA_Tot_Chg[,,4,k] <- apply(VA_mat_detail_chg,3,rowSums)
  DemandTot_new[,,4,k] <- DemandTot_new_int
  CO2_Reg_Response[,4,k] <- CO2_Reg_Response_int
  #--------------
  
  #Part 5: Countries have different carbon tax rates. consumption tax
  #--------------
  Tax_S5 <- array(0,dim = c(N,length(Reg_Inclu)))
  
  if (Level != 0){
    c <- Level*b$EXP_perCap*(Level/weighted.mean(Level*b$EXP_perCap,b$Emi_CB))
    Tax_reg_C[,k] <-  c[match(Reg_corr$WB_IncomeGroup,b$WB_IncomeGroup)] 
    for (r in 1:length(Reg_Inclu)) {
      Tax_S5[,r] <- Tax_reg_C[r,k]
    }
  }
  
  Tax_Scenarios[,,5,k] <- Tax_S5
  Tax_revenue[,5,k] <- colSums(Tax_S5*Emission_CB,na.rm = T)
  
  LevelbyProduct <- array(0, dim = dim(CF_IncluDirect))
  for (r in 1:length(Reg_Inclu)) {
    LevelbyProduct[,r] <- rep(Tax_S5[,r], G)
  }

  Price_Response[,,5,k] <- CF_IncluDirect*LevelbyProduct+1
  FinalDemand_New <- Minumum + (FinalDemand_ori-Minumum)/(CF_IncluDirect*LevelbyProduct+1)

  FinalDemand_chg <- rowSums(FinalDemand_New)-rowSums(FinalDemand_ori)
  INT <- rbind(rowSums(VA_multiplier_Lab%*%FinalDemand_chg),
               rowSums(VA_multiplier_Cap%*%FinalDemand_chg),
               rowSums(VA_multiplier_Tax%*%FinalDemand_chg))
  
  VA_mat_detail_chg <- VA_mat_detail
  DemandTot_new_int <- array(0,dim = c(203,length(Reg_Inclu)))
  CO2_Reg_Response_int <- c()
  for (r in 1:length(Reg_Inclu)) {
    y = (Reg_corr$GTAP.NO[r]-1)*N+1;z = Reg_corr$GTAP.NO[r]*N
    VA_mat_detail_chg[,,r] <- INT[,y:z]
    if (substr(Reg_corr$GTAP.reg[r],1,1) == "X") {
      share_GDP <- Reg_corr_Full$GDP_2017_WB[which(Reg_corr_Full$WBGDPcode %in% Reg_corr$WBGDPcode[r])]/
        sum(Reg_corr_Full$GDP_2017_WB[which(Reg_corr_Full$GTAP.reg %in% Reg_corr$GTAP.reg[r])],na.rm = T)
      VA_mat_detail_chg[,,r] <- VA_mat_detail_chg[,,r]*share_GDP
    }
    DemandTot_new_int[,r] <- DemandTot_ori[,r] +
      rowSums(Transfer_mat[,,r]%*%diag(rowSums(VA_mat_detail_chg[,,r])))
    CO2_Reg_Response_int[r] <- sum((Minumum[,r]+
                                     INT2[,,r]%*%(DemandTot_new_int[,r]-Minumum_reg[,r])/
                                     (CF_IncluDirect[,r]*LevelbyProduct[,r]+1))*CF_IncluDirect[,r])
  }
  
  VA_Tot_Chg[,,5,k] <- apply(VA_mat_detail_chg,3,rowSums)
  DemandTot_new[,,5,k] <- DemandTot_new_int
  CO2_Reg_Response[,5,k] <- CO2_Reg_Response_int
  #--------------
  
  #Part 6: Countries have different carbon tax rates+Luxury tax.
  #--------------
  Tax_S6 <- array(0,dim = c(N,length(Reg_Inclu)))
  
  if (Level != 0){
    for (r in 1:length(Reg_Inclu)) {
      Tax_S6[,r] <- Tax_reg_C[r,k]*Elasticity[,r]*
        ( Tax_reg_C[r,k]/weighted.mean(Tax_reg_C[r,k]*Elasticity[,r],Emission_CB[,r]))
    }
  }
  
  Tax_Scenarios[,,6,k] <- Tax_S6
  Tax_revenue[,6,k] <- colSums(Tax_S6*Emission_CB,na.rm = T)
  
  LevelbyProduct <- array(0, dim = dim(CF_IncluDirect))
  for (r in 1:length(Reg_Inclu)) {
    LevelbyProduct[,r] <- rep(Tax_S6[,r], G)
  }
  
  Price_Response[,,6,k] <- CF_IncluDirect*LevelbyProduct+1
  FinalDemand_New <- Minumum + (FinalDemand_ori-Minumum)/(CF_IncluDirect*LevelbyProduct+1)
  
  FinalDemand_chg <- rowSums(FinalDemand_New)-rowSums(FinalDemand_ori)
  INT <- rbind(rowSums(VA_multiplier_Lab%*%FinalDemand_chg),
               rowSums(VA_multiplier_Cap%*%FinalDemand_chg),
               rowSums(VA_multiplier_Tax%*%FinalDemand_chg))
  
  VA_mat_detail_chg <- VA_mat_detail
  DemandTot_new_int <- array(0,dim = c(203,length(Reg_Inclu)))
  CO2_Reg_Response_int <- c()
  for (r in 1:length(Reg_Inclu)) {
    y = (Reg_corr$GTAP.NO[r]-1)*N+1;z = Reg_corr$GTAP.NO[r]*N
    VA_mat_detail_chg[,,r] <- INT[,y:z]
    if (substr(Reg_corr$GTAP.reg[r],1,1) == "X") {
      share_GDP <- Reg_corr_Full$GDP_2017_WB[which(Reg_corr_Full$WBGDPcode %in% Reg_corr$WBGDPcode[r])]/
        sum(Reg_corr_Full$GDP_2017_WB[which(Reg_corr_Full$GTAP.reg %in% Reg_corr$GTAP.reg[r])],na.rm = T)
      VA_mat_detail_chg[,,r] <- VA_mat_detail_chg[,,r]*share_GDP
    }
    DemandTot_new_int[,r] <- DemandTot_ori[,r] +
      rowSums(Transfer_mat[,,r]%*%diag(rowSums(VA_mat_detail_chg[,,r])))
    CO2_Reg_Response_int[r] <- sum((Minumum[,r]+
                                     INT2[,,r]%*%(DemandTot_new_int[,r]-Minumum_reg[,r])/
                                     (CF_IncluDirect[,r]*LevelbyProduct[,r]+1))*CF_IncluDirect[,r])
  }
  
  VA_Tot_Chg[,,6,k] <- apply(VA_mat_detail_chg,3,rowSums)
  DemandTot_new[,,6,k] <- DemandTot_new_int
  CO2_Reg_Response[,6,k] <- CO2_Reg_Response_int
  #--------------
  
  #Part 7: No tax
  #--------------
  Tax_S7 <- array(0,dim = c(N,length(Reg_Inclu)))
  Tax_Scenarios[,,7,k] <- Tax_S7
  Tax_revenue[,7,k] <- colSums(Emission_PB,na.rm = T)*0#M$
  Price_Response[,,7,k] <- 1
  
  FinalDemand_New <- FinalDemand_ori
  DemandTot_new[,,7,k] <- DemandTot_ori
  VA_Tot_Chg[,,7,k] <- 0
  CO2_Reg_Response[,7,k] <- colSums(FinalDemand_New*CF_IncluDirect)
  #--------------
  
  CO2_Response[,k] <- colSums(CO2_Reg_Response[,,k])
  
  print(str_c("Time cost------------",Tax_Level[k],"-----------------",round(Sys.time()-t1,2)))
}

#Identify the required tax level
#-------------
Tar15 <- CO2_Response[7,1]*0.853#1.5 degree goal
CO2_Effect15 <- CO2_Response-Tar15
TarLevel15 <- as.numeric(substr(colnames(CO2_Effect15)[apply(abs(CO2_Effect15), 1, which.min)],2,3))

Tar20 <- CO2_Response[7,1]*0.973#2 degree goal
CO2_Effect20 <- CO2_Response-Tar20
TarLevel20 <- as.numeric(substr(colnames(CO2_Effect20)[apply(abs(CO2_Effect20), 1, which.min)],2,3))
#------------

save(Tax_Scenarios, Tax_revenue, CO2_Response,CO2_Reg_Response,Price_Response,
     DemandTot_new, DemandTot_ori,VA_Tot_Chg,
     Mimunum_sum,Minumum_reg,Minumum,
     Tax_Level, Tax_level_nam,
     Tax_reg_C,Tax_reg_P,Elasticity,Elasticity_raw,
     TarLevel15,TarLevel20,CO2_Effect20,CO2_Effect15,Tar15,Tar20,
     file = str_c(pathout2,"/Tax scenarios_price and CO2 response by tax level.Rdata"))

rm(list = ls()[-which(ls() %in% c("path","pathout","pathout2","pathout3","pathout4","pathout5",
                                  "pathdata3","pathdata4","pathcode"))])
gc()
