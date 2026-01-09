#Module 5: The poverty and inequality effect of combinations of tax scenarios (4) and assistance scenarios (5)
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu


load(str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))
load(str_c(pathout,"/Social assistance scenarios.Rdata"))
load(str_c(pathout2,"/Tax scenarios_price and CO2 response by tax level.Rdata"))

#Prepare nesscessy variable----------
INT2 <- array(0,dim = c(GN,201,length(Reg_Inclu)), dimnames = list(regsecnam,Gnam,Reg_Inclu))
for (r in 1:length(Reg_Inclu)) {
  INT2[,,r] <- t(t(Expenditure_detail_global[,,r])/colSums(Expenditure_detail_global[,,r]))
}
INT2[is.nan(INT2)] <- 0

rm(Saving_mat_detail,VA_mat_detail,Td_vec,Transfer_mat,
   Footprint_detail_global,
   Footprint_detail,Exp_perCap_detail,
   Gov_detail_global,
   Emission_CB,Emission_PB,Reg_corr_Full, Reg_WBCD,Leontief, EMc)
gc()
#------------


#Set various recycling share
# Recy <- c(0.2,0.3,0.8,1)
# Recynam <- c("20percent","30percent","80percent","100percent")
Recy <- c(0.3,0.8,1)
Recynam <- c("30percent","80percent","100percent")
# Recy <- c(0.8)
# Recynam <- c("80percent")

for (z in 1:length(Recy)) {#loop for various recycling share
  #Define aggregated output variable----------
  CO2_Response_Recy <- array(0,dim = c(dim(Tax_Scenarios)[3],length(Tax_Level),dim(SP_Scenarios)[3]),
                             dimnames = list(dimnames(Tax_Scenarios)[[3]],Tax_level_nam,
                                             dimnames(SP_Scenarios)[[3]]))
  
  OUT_TAXSP <- array(NA, dim = c(length(Reg_Inclu),13,
                                 dim(Tax_Scenarios)[3],length(Tax_Level),dim(SP_Scenarios)[3]),
                     dimnames = list(Reg_corr$WBGDPreg, 
                                     c("Pop",
                                       "PPop_npl_ori","PR_npl_ori","PPop_ipl_ori","PR_ipl_ori",
                                       "Gini_ori",
                                       "PPop_npl_tax", "PR_npl_tax","PPop_ipl_tax", "PR_ipl_tax",
                                       "Gini_tax",
                                       "CO2_ori","CO2_tax"),
                                     dimnames(Tax_Scenarios)[[3]],Tax_level_nam,dimnames(SP_Scenarios)[[3]]))
  #------------
  
  for (k in 1:length(Tax_Level)) {
    #loop for different tax scenarios and social protection scenarios
    t1 <- Sys.time()
    print(str_c(Recynam[z],"--","Begin: ",k,"--", Tax_Level[k],"$/tCO2"))
    
    for (i in 1:dim(Tax_Scenarios)[3]) {
      t2 <- Sys.time()
      
      #Redefine Poverty-focused scenario's coverage rate, add on 11/06/2024
      SP_Scenarios[,,6] <- pvt_tarfc(Tax_revenue[,i,k]*Recy[z],Fund_required)
      write.csv(SP_Scenarios[,,6],
                str_c(pathout3,"/Poverty focused Coverage rate--",Tax_level_nam[k],"-",
                      dimnames(Tax_Scenarios)[[3]][i],"-",Recynam[z],".csv"))
      
      
      for (j in 1:dim(SP_Scenarios)[3]) {
        #Here we calculate the emission as consumption-based emission.
        
        #loop for different poverty line setting
        NPL_setting <- c("WB_PR","WB_2.15")
        for (npl_setting in 1:length(NPL_setting)){
          OUT_TAXSP[,1,i,k,j] <- colSums(Population)#Population
          
          #Poverty definition
          #------------
          if (npl_setting == 1) PR_2017 <- Reg_corr$WB_PR/100#"WB_PR" 
          if (npl_setting == 2) PR_2017 <- Reg_corr$WB_2.15/100#"WB_2.15" 
          pr <- pracma::repmat(PR_2017,201,1)
          pop <- apply(t(t(Population)/colSums(Population)),2,cumsum)
          position <- apply(abs(pr-pop),2, which.min)
          NPL_2017 <- c()
          
          for (r in 1 :length(Reg_Inclu)) {
            xx <- pr[,r]-pop[,r]
            
            if (xx[position[r]] > 0) {
              share <- xx[position[r]]/(xx[position[r]]-xx[position[r]+1])
              NPL_2017[r] <- Expenditure_perCap[position[r],r]+ 
                share*(Expenditure_perCap[position[r]+1,r]-Expenditure_perCap[position[r],r])
            } else{
              share <- xx[position[r]-1]/(xx[position[r]-1]-xx[position[r]])
              if (sum(share) != 0) {
                NPL_2017[r] <- Expenditure_perCap[position[r]-1,r]+ 
                  share*(Expenditure_perCap[position[r],r]-Expenditure_perCap[position[r]-1,r])
              }else{
                NPL_2017[r] <- Expenditure_perCap[position[r],r]
              }
            }
          }
          
          npl <- round(pracma::repmat(NPL_2017,201,1),5)
          #------------
          
          #Baseline
          #------------
          Poor <- array(0,dim = dim(Population))
          
          for (r in 1 :length(Reg_Inclu)) {
            expend_col <- Expenditure_perCap[, r]
            pop_col <- Population[, r]
            order_idx <- order(expend_col)
            expend_sorted <- expend_col[order_idx]
            pop_sorted <- pop_col[order_idx]
            expend_sorted[expend_sorted == 0] <- NA
            xx <- npl[, r] - expend_sorted
            position <- which.min(abs(xx))
            
            if (xx[position] > 0) {
              share <- xx[position] / (xx[position] - xx[position + 1])
              Poor_sorted <- rep(0, length(expend_col))
              Poor_sorted[1:position] <- pop_sorted[1:position]
              Poor_sorted[position + 1] <- pop_sorted[position + 1] * share
            } else if (position > 1) {
              share <- xx[position - 1] / (xx[position - 1] - xx[position])
              Poor_sorted <- rep(0, length(expend_col))
              Poor_sorted[1:(position - 1)] <- pop_sorted[1:(position - 1)]
              Poor_sorted[position] <- pop_sorted[position] * share
            } else {
              Poor_sorted <- rep(0, length(expend_col))  # fallback for rare edge cases
            }
            Poor[, r] <- Poor_sorted[order(order_idx)]
          }
          Poor[is.na(Poor)] <- 0#For no extreme poverty
          
          #poverty calculation
          if (npl_setting == 1) {
            OUT_TAXSP[,2,i,k,j] <- colSums(Poor)#Poverty headcount under npl
            OUT_TAXSP[,3,i,k,j] <- colSums(Poor)/colSums(Population)#Poverty rate under npl
          }else{
            OUT_TAXSP[,4,i,k,j] <- colSums(Poor)#Poverty headcount under ipl
            OUT_TAXSP[,5,i,k,j] <- colSums(Poor)/colSums(Population)#Poverty rate under ipl
          }
          #------------
          
          #Tax+SP scenarios
          #------------
          #tax
          Cost_perCap<- array(0,dim = dim(Population))
          
          for (r in 1:length(Reg_Inclu)) {
            Cost_perCap[,r] <- t(Footprint_perCap_detail[,,r])%*%Tax_Scenarios[,r,i,k]
          }
          
          Expen_tax <- DemandTot_new[1:length(Gnam),,i,k]*10^6/Population - Cost_perCap
          Inc_effect <- (DemandTot_ori[1:length(Gnam), ] - DemandTot_new[1:length(Gnam),,i,k])*10^6/Population
          
          #social assistance
          Pop_tar <- Population*SP_Scenarios[,,j]
          Pop_untar <- Population-Pop_tar
          
          Benifit_tar <- Tax_revenue[,i,k]*Recy[z]/colSums(Pop_tar)*10^6#$
          Benifit_tar[is.nan(Benifit_tar)] <- 0;Benifit_tar[is.infinite(Benifit_tar)] <- 0
          
          #Export benifit per capita
          if (Tax_Level[k] == 60 | Tax_Level[k] == 120 ) {
            write.csv(Benifit_tar,
                      file = str_c(pathout3,"/",Tax_level_nam[k],"-",
                                   dimnames(Tax_Scenarios)[[3]][i],"-",
                                   dimnames(SP_Scenarios)[[3]][j],Recynam[z],"Benifit per capita.csv"))
          }             
          
          Expen_perCap_untar <- Expen_tax
          Expen_perCap_tar <- Expen_tax + pracma::repmat(Benifit_tar,201,1)
          Expen_perCap_tar[is.na(Expen_perCap_tar)] <- 0
          Expen_perCap_untar[is.na(Expen_perCap_untar)] <- 0
          
          
          #poverty calculation
          Poor_SP_untar <- array(0,dim = dim(Expenditure_perCap))
          
          for (r in 1 :length(Reg_Inclu)) {
            expend_col <- Expen_perCap_untar[, r]
            pop_col <- Pop_untar[, r]
            
            order_idx <- order(expend_col)
            expend_sorted <- expend_col[order_idx]
            pop_sorted <- pop_col[order_idx]
            expend_sorted[expend_sorted == 0] <- NA
            xx <- npl[, r] - expend_sorted
            position <- which.min(abs(xx))
            
            if (xx[position] > 0) {
              share <- xx[position] / (xx[position] - xx[position + 1])
              Poor_SP_sorted <- rep(0, length(expend_col))
              Poor_SP_sorted[1:position] <- pop_sorted[1:position]
              Poor_SP_sorted[position + 1] <- pop_sorted[position + 1] * share
            } else if (position > 1) {
              share <- xx[position - 1] / (xx[position - 1] - xx[position])
              Poor_SP_sorted <- rep(0, length(expend_col))
              Poor_SP_sorted[1:(position - 1)] <- pop_sorted[1:(position - 1)]
              Poor_SP_sorted[position] <- pop_sorted[position] * share
            } else {
              Poor_SP_sorted <- rep(0, length(expend_col))  # fallback for rare edge cases
            }
            Poor_SP_untar[, r] <- Poor_SP_sorted[order(order_idx)]
          }
          Poor_SP_untar[is.na( Poor_SP_untar)] <- 0#For no extreme poverty
          
          
          Poor_SP_tar <- array(0,dim = dim(Expenditure_perCap))
          
          for (r in 1 :length(Reg_Inclu)) {
            expend_col <- Expen_perCap_tar[, r]
            pop_col <- Pop_tar[, r]
            
            order_idx <- order(expend_col)
            expend_sorted <- expend_col[order_idx]
            pop_sorted <- pop_col[order_idx]
            expend_sorted[expend_sorted == 0] <- NA
            xx <- npl[, r] - expend_sorted
            position <- which.min(abs(xx))
            
            if (xx[position] > 0) {
              share <- xx[position] / (xx[position] - xx[position + 1])
              Poor_SP_sorted <- rep(0, length(expend_col))
              Poor_SP_sorted[1:position] <- pop_sorted[1:position]
              Poor_SP_sorted[position + 1] <- pop_sorted[position + 1] * share
            } else if (position > 1) {
              share <- xx[position - 1] / (xx[position - 1] - xx[position])
              if (is.na(share)) {share = 0}
              Poor_SP_sorted <- rep(0, length(expend_col))
              Poor_SP_sorted[1:(position - 1)] <- pop_sorted[1:(position - 1)]
              Poor_SP_sorted[position] <- pop_sorted[position] * share
            } else {
              Poor_SP_sorted <- rep(0, length(expend_col))  # fallback for rare edge cases
            }
            Poor_SP_tar[, r] <- Poor_SP_sorted[order(order_idx)]
          }
          Poor_SP_tar[is.na(Poor_SP_tar)] <- 0#For no extreme poverty
          
          
          if (npl_setting == 1) {#npl
            OUT_TAXSP[,7,i,k,j] <- colSums(Poor_SP_untar + Poor_SP_tar)
            OUT_TAXSP[,8,i,k,j] <- colSums(Poor_SP_untar + Poor_SP_tar)/colSums(Population)
          }else{#ipl
            OUT_TAXSP[,9,i,k,j] <- colSums(Poor_SP_untar + Poor_SP_tar)
            OUT_TAXSP[,10,i,k,j] <- colSums(Poor_SP_untar + Poor_SP_tar)/colSums(Population)
          }
          #------------
        }
        
        #Inequality
        #-------------
        #original inequality
        for (r in 1:length(Reg_Inclu)) {
          CumShareEXP <- cumsum(Expenditure_perCap[,r]*Population[,r])/sum(Expenditure_perCap[,r]*Population[,r])
          SharePop <- Population[,r]/sum(Population[,r])
          OUT_TAXSP[r,6,i,k,j] <- (1-2*sum(CumShareEXP*SharePop))*
            (length(SharePop)/(length(SharePop)-1))
        }
        
        #inequality under tax+SP
        for (r in 1:length(Reg_Inclu)) {
          X <- order(c(Expen_perCap_tar[,r],Expen_perCap_untar[,r]))
          exp.order <- c(Expen_perCap_tar[,r],Expen_perCap_untar[,r])[X]
          pop.order <- c(Pop_tar[,r],Pop_untar[,r])[X]
          CumShareEXP <- cumsum(exp.order*pop.order)/sum(exp.order*pop.order)
          CumShareEXP[is.nan(CumShareEXP)] <- 0
          SharePop <- pop.order/sum(pop.order)
          OUT_TAXSP[r,11,i,k,j] <- (1-2*sum(CumShareEXP*SharePop))*
            (length(SharePop)/(length(SharePop)-1))
        }
        
        rm(X,CumShareEXP,SharePop,exp.order,pop.order);gc()
        #-------------
        
        #Emission effect 
        #-------------
        #original emission and emission under tax+SP
        OUT_TAXSP[,12,i,k,j] <- CO2_Reg_Response[,7,k]
        
        RecyAmount <- pracma::repmat(Benifit_tar,201,1)*Pop_tar/10^6
        
        CO2_Recycle <- c()
        for (r in 1:length(Reg_Inclu)) {
          CO2_Recycle[r] <- t(INT2[,,r]%*%RecyAmount[,r]/
                                Price_Response[,r,i,k])%*%CF_IncluDirect[,r]
        }
        
        OUT_TAXSP[,13,i,k,j]  <- CO2_Reg_Response[,i,k]+CO2_Recycle
        
        CO2_Response_Recy[i,k,j] <- sum(OUT_TAXSP[,13,i,k,j])
        #-------------
        
        save(Expen_perCap_untar,Expen_perCap_tar,
             Expen_tax,Cost_perCap,Inc_effect,CO2_Recycle,
             Benifit_tar,Pop_untar,Pop_tar,Poor_SP_tar,Poor_SP_untar,
             file = str_c(pathout3,"/",Tax_level_nam[k],"-",
                          dimnames(Tax_Scenarios)[[3]][i],"-",
                          dimnames(SP_Scenarios)[[3]][j],Recynam[z],".Rdata"))
      }
      
      print(str_c(Recynam[z],"--",dimnames(Tax_Scenarios)[[3]][i],"--",round(Sys.time()-t2,2)))
    }
    
    print(str_c(Recynam[z],"--","Time cost: ",k,"--", Tax_Level[k],"$/tCO2--",round(Sys.time()-t1,2)))
  }
  
  
  #Identify the policy combination required for 2 degree goal-------------
  CO2_Effect20_Recy <- CO2_Response_Recy-Tar20
  CO2_Effect15_Recy <- CO2_Response_Recy-Tar15
  
  #find the tax level required under each combination
  RequireTax20 <- array(0, dim = c(dim(Tax_Scenarios)[3]-1,dim(SP_Scenarios)[3]),
                        dimnames = list(dimnames(Tax_Scenarios)[[3]][1:6],dimnames(SP_Scenarios)[[3]]))
  RequireTax15 <- RequireTax20 
  
  IPL_PovertyOutcome20 <- RequireTax20; IPL_PovertyOutcome15 <- RequireTax20 
  NPL_PovertyOutcome20 <- RequireTax20; NPL_PovertyOutcome15 <- RequireTax20 
  LocalGiniOutcome20 <- RequireTax20; LocalGiniOutcome15 <- RequireTax20 
  InterGiniOutcome20 <- RequireTax20; InterGiniOutcome15 <- RequireTax20 
  
  for (i in 1:(dim(Tax_Scenarios)[3]-1)) {
    for (j in 1:dim(SP_Scenarios)[3]) {
      #2 degree--------------
      RequireTax20[i,j] <- Tax_Level[which.min(abs(CO2_Effect20_Recy[i,,j]))]
      Posi <- which.min(abs(CO2_Effect20_Recy[i,,j]))
      OUTCOME20 <- as.data.frame(OUT_TAXSP[,,i,Posi ,j])
      
      IPL_PovertyOutcome20[i,j] <- sum(OUTCOME20$PPop_ipl_tax- OUTCOME20$PPop_ipl_ori)/10^6#Million headcount
      NPL_PovertyOutcome20[i,j] <- sum(OUTCOME20$PPop_npl_tax- OUTCOME20$PPop_npl_ori)/10^6#Million headcount
      
      LocalGiniOutcome20[i,j] <- (weighted.mean(OUTCOME20$Gini_tax,OUTCOME20$Pop)-
                                    weighted.mean(OUTCOME20$Gini_ori,OUTCOME20$Pop))/weighted.mean(OUTCOME20$Gini_ori,OUTCOME20$Pop)
      
      load(str_c(pathout3,"/L",RequireTax20[i,j],"-",
                 dimnames(Tax_Scenarios)[[3]][i],"-",
                 dimnames(SP_Scenarios)[[3]][j],Recynam[z],".Rdata"))
      
      InterGiniOutcome20[i,j] <- (Gini(colSums(Expen_perCap_untar*Pop_untar+Expen_perCap_tar*Pop_tar,na.rm = T)/colSums(Population,na.rm = T),
                                       corr  = T)-Gini(colSums(Expenditure_perCap*Population,na.rm = T)/colSums(Population,na.rm = T),
                                                       corr  = T))/Gini(colSums(Expenditure_perCap*Population,na.rm = T)/colSums(Population,na.rm = T),
                                                                        corr  = T)
      #----------
      
      #1.5 degree-------
      RequireTax15[i,j] <- Tax_Level[which.min(abs(CO2_Effect15_Recy[i,,j]))]
      Posi <- which.min(abs(CO2_Effect15_Recy[i,,j]))
      OUTCOME15 <- as.data.frame(OUT_TAXSP[,,i,Posi,j])
      
      IPL_PovertyOutcome15[i,j] <- sum(OUTCOME15$PPop_ipl_tax- OUTCOME15$PPop_ipl_ori)/10^6#Million headcount
      NPL_PovertyOutcome15[i,j] <- sum(OUTCOME15$PPop_npl_tax- OUTCOME15$PPop_npl_ori)/10^6#Million headcount
      
      LocalGiniOutcome15[i,j] <- (weighted.mean(OUTCOME15$Gini_tax,OUTCOME15$Pop)-
                                    weighted.mean(OUTCOME15$Gini_ori,OUTCOME15$Pop))/weighted.mean(OUTCOME15$Gini_ori,OUTCOME15$Pop)
      
      load(str_c(pathout3,"/L",RequireTax15[i,j],"-",
                 dimnames(Tax_Scenarios)[[3]][i],"-",
                 dimnames(SP_Scenarios)[[3]][j],Recynam[z],".Rdata"))
      
      InterGiniOutcome15[i,j] <- (Gini(colSums(Expen_perCap_untar*Pop_untar+Expen_perCap_tar*Pop_tar,na.rm = T)/colSums(Population,na.rm = T),
                                       corr  = T)-Gini(colSums(Expenditure_perCap*Population,na.rm = T)/colSums(Population,na.rm = T),
                                                       corr  = T))/Gini(colSums(Expenditure_perCap*Population,na.rm = T)/colSums(Population,na.rm = T),
                                                                        corr  = T)
      #----------
    }
  }
  
  GlobalBest20 <- as.vector(arrayInd(which.min(IPL_PovertyOutcome20[,1:5]), .dim = dim(IPL_PovertyOutcome20[,1:5])))
  GlobalBest15 <- as.vector(arrayInd(which.min(IPL_PovertyOutcome15[,1:5]), .dim = dim(IPL_PovertyOutcome15[,1:5])))
  
  
  save(OUT_TAXSP, CO2_Response_Recy, CO2_Effect15_Recy,CO2_Effect15_Recy,
       RequireTax20,RequireTax15,
       IPL_PovertyOutcome20,NPL_PovertyOutcome20,LocalGiniOutcome20,InterGiniOutcome20,
       IPL_PovertyOutcome15,NPL_PovertyOutcome15,LocalGiniOutcome15,InterGiniOutcome15,
       GlobalBest20,GlobalBest15,
       file = str_c(pathout3,"/Poverty, Ineq, Emission outcome by tax, sp, recy",Recynam[z],".Rdata"))
}

rm(list = ls()[-which(ls() %in% c("path","pathout","pathout2","pathout3","pathout4","pathout5",
                                  "pathdata3","pathdata4","pathcode"))])
gc()
