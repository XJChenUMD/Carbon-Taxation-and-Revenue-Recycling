#Module 2: carbon tax scenarios
#2022-12-06
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu

# 1.	Global universal tax, US50$/ton. production tax
# 2.	Global universal tax, US50$/ton. consumption tax
# 3.	Luxury tax. Different tax level between sectors (Yannick’s method), remain a US50$/ton national averagely.
# 4.	Countries have different carbon tax rates decided by income level and PB-CO2(affordable) , remain a US50$/ton global averagely. 
# 5.	Countries have different carbon tax rates decided by income level and CB-CO2(affordable) , remain a US50$/ton global averagely. 
# 6.	Countries have different carbon tax rates (decide national average tax rate) + Luxury tax (decide sectoral rates).
# 7.  No tax.
# Also, calculate the tax revenue under each scenario.



#Load the data from the results in Module 1
load(str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))
Tax_Level <- c(25,50,75,100)#US$/ton

for (k in 1:length(Tax_Level)) {
  t1 <- Sys.time()
  Level <- Tax_Level[k]
  
  Tax_Scenarios <- array(0,dim = c(N,length(Reg_Inclu),7),
                         dimnames = list(secnam, Reg_Inclu,
                                         c("Universal_P","Universal_C","Luxury","CBDR_P","CBDR_C","Luxury&CBDR_C","Null")))
  #"Universal_P" and "CBDR_P" are production tax; 
  #"Universal_C","CBDR_C","Luxury" and "Luxury&CBDR" are consumption tax;
  # All tax scenarios transit to a consumption tax to improve the code efficiency for calculating tax burden
  
  Tax_revenue <- array(0,dim = c(length(Reg_Inclu),8),
                       dimnames = list(Reg_Inclu,
                                       c("Universal_P","Universal_C","Luxury","CBDR_P","CBDR_C","Luxury&CBDR","Null","GDP")))
  Tax_revenue[,8] <- Reg_corr$GDP_2017_WB/10^6#M$
  
  #Part 1: Global universal tax. production tax
  #--------------
  Tax_S1 <- array(Level,dim = c(N,length(Reg_Inclu)))
  Tax_Scenarios[,,1] <- Tax_S1
  Tax_revenue[,1] <- colSums(Emission_PB,na.rm = T)*Level#M$
  #--------------
  
  #Part 2: Global universal tax. consumption tax
  #--------------
  Tax_S2 <- array(Level,dim = c(N,length(Reg_Inclu)))
  Tax_Scenarios[,,2] <- Tax_S2
  Tax_revenue[,2] <- colSums(Emission_CB,na.rm = T)*Level#M$
  #--------------
  
  
  #Part 3: Luxury tax. consumption tax
  #--------------
  Tax_S3 <- array(0,dim = c(N,length(Reg_Inclu)))
  
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
  
  for (r in 1:length(Reg_Inclu)) {
    Tax_S3[,r] <- Level*Elasticity[,r]*
      (Level/weighted.mean(Level*Elasticity[,r],Emission_CB[,r]))
  }
  
  Tax_Scenarios[,,3] <- Tax_S3
  Tax_revenue[,3] <- colSums(Tax_S3*Emission_CB,na.rm = T)
  #--------------
  
  
  #Part 4: Countries have different carbon tax rates. production tax
  #--------------
  Tax_S4 <- array(0,dim = c(N,length(Reg_Inclu)))
  
  #decide by GDP per capita and production-based CO2
  # GDP_perCap <- Reg_corr$GDP_2017_WB/Reg_corr$Pop_2017_WB
  # Tax_reg_P <- Level*GDP_perCap*(Level/weighted.mean(Level*GDP_perCap,colSums(Emission_PB,na.rm = T)))
  
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
  c <- Level*b$GDP_perCap*(Level/weighted.mean(Level*b$GDP_perCap,b$Emi_PB))
  Tax_reg_P <-  c[match(Reg_corr$WB_IncomeGroup,b$WB_IncomeGroup)] 
  
  
  #Construct tax vector matched with GTAP, production tax
  Tax_Gtap <- rep(0,GN)
  for (r in 1:G) {
    p <- (r-1)*N+1; q <- r*N
    a <- which(Reg_corr$GTAP.reg %in% regnam[r])
    Tax_Gtap[p:q] <- weighted.mean(Tax_reg_P[a],Reg_corr$WB_CO2_2017[a],na.rm = T)
  }
  Tax_Gtap[is.na(Tax_Gtap)] <- Level
  
  #Transit to the consumption based tax for 168 countries
  for (r in 1:length(Reg_Inclu)) {
    tax <- Tax_Gtap
    a <- which(regnam %in% Reg_corr$GTAP.reg[r])
    p <- (a-1)*N+1; q <- a*N
    tax[p:q] <- Tax_reg_P[r]
    Tax_detail_global <- tax*rowSums(Footprint_detail_global[,,r],na.rm = T)
    dim(Tax_detail_global) <- c(N,G)
    Tax_S4[,r] <- rowSums(Tax_detail_global,na.rm = T)/rowSums(Footprint_detail[,,r],na.rm = T)
  }
  Tax_S4[is.na(Tax_S4)] <- 0
  Tax_Scenarios[,,4] <- Tax_S4
  Tax_revenue[,4] <- Tax_reg_P*colSums(Emission_PB,na.rm = T)
  #--------------
  
  
  #Part 5: Countries have different carbon tax rates. consumption tax
  #--------------
  Tax_S5 <- array(0,dim = c(N,length(Reg_Inclu)))
  
  #decide by EXP per capita and con-based CO2
  # EXP_perCap <- colSums((Expenditure_perCap*Population))/colSums(Population)
  # Tax_reg_C <- Level*EXP_perCap*(Level/weighted.mean(Level*EXP_perCap,colSums(Emission_CB,na.rm = T)))
  
  c <- Level*b$EXP_perCap*(Level/weighted.mean(Level*b$EXP_perCap,b$Emi_CB))
  Tax_reg_C <-  c[match(Reg_corr$WB_IncomeGroup,b$WB_IncomeGroup)] 
  
  
  for (r in 1:length(Reg_Inclu)) {
    Tax_S5[,r] <- Tax_reg_C[r]
  }
  
  Tax_Scenarios[,,5] <- Tax_S5
  Tax_revenue[,5] <- colSums(Tax_S5*Emission_CB,na.rm = T)
  #--------------
  
  
  #Part 6: Countries have different carbon tax rates+Luxury tax.
  #--------------
  Tax_S6 <- array(0,dim = c(N,length(Reg_Inclu)))
  
  for (r in 1:length(Reg_Inclu)) {
    Tax_S6[,r] <- Tax_reg_C[r]*Elasticity[,r]*
      ( Tax_reg_C[r]/weighted.mean(Tax_reg_C[r]*Elasticity[,r],Emission_CB[,r]))
  }
  
  Tax_Scenarios[,,6] <- Tax_S6
  Tax_revenue[,6] <- colSums(Tax_S6*Emission_CB,na.rm = T)
  #--------------
  
  
  #Part 7: No tax
  #--------------
  Tax_S7 <- array(0,dim = c(N,length(Reg_Inclu)))
  Tax_Scenarios[,,7] <- Tax_S7
  Tax_revenue[,7] <- colSums(Emission_PB,na.rm = T)*0#M$
  #--------------
  
  print(str_c("Time cost---",Tax_Level[k],"---",round(Sys.time()-t1,2)))
  
  save(Tax_Scenarios, Tax_revenue, Level, Elasticity,Elasticity_raw,Tax_reg_C,Tax_reg_P,
       file = str_c(pathout2,"/",Level," Tax scenarios and revenue.Rdata"))
}

rm(data,equation, Exp_Sec,Tax_detail_global,Tax_S1,Tax_S2,Tax_S3,Tax_S4,Tax_S5,
   Tax_S6,Tax_S7,Exp, GDP_perCap, tax, j, p ,q,r, Tax_Gtap,Tax_reg_C,Tax_reg_P,Tax_Level,
   Elasticity,Elasticity_raw,Tax_revenue,
   
   Footprint_perCap_detail,Footprint_detail_global,
   # Footprint_perCap_detail_global,Expenditure_detail_global,Exp_perCap_detail_global,
   Expenditure_detail,Exp_perCap_detail,Footprint_detail,Expenditure_perCap,
   Population,Emission_CB,Emission_PB,regsecnam, regnam, secnam,
   Reg_corr, Reg_corr_Full,Reg_Inclu, Reg_WBCD, 
   Leontief, EMc,N,G,GN)
gc()