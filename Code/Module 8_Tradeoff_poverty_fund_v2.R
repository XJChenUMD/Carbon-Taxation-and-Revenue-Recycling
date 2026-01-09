#Module 7: The poverty and inequality effect of tax+SA (2) and global transfer (4)
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu

#Choose two ideal TAX+SA combination from Module 5 based on the extreme poverty effect.
#One for actual; one for ideal (based on PerfectTargeted)

load(str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))
load(str_c(pathout,"/Social assistance scenarios.Rdata"))
load(str_c(pathout2,"/Tax scenarios_price and CO2 response by tax level.Rdata"))
load(str_c(pathout,"/Climate finance scenarios.Rdata"))

#Prepare nesscessy variable----------
INT2 <- array(0,dim = c(GN,201,length(Reg_Inclu)), dimnames = list(regsecnam,Gnam,Reg_Inclu))
for (r in 1:length(Reg_Inclu)) {
  INT2[,,r] <- t(t(Expenditure_detail_global[,,r])/colSums(Expenditure_detail_global[,,r]))
}
INT2[is.nan(INT2)] <- 0

rm(Saving_mat_detail,VA_mat_detail,Td_vec,Transfer_mat,
   Footprint_detail_global,
   Footprint_detail,Exp_perCap_detail,Gov_detail_global,
   Emission_CB,Emission_PB,Reg_corr_Full, Reg_WBCD,Leontief, EMc)
gc()
#------------

load(str_c(pathout3,"/Poverty, Ineq, Emission outcome by tax, sp, recy100percent.Rdata"))
load(str_c(pathout4,"/Global poverty under TAX+SA+GF scenarios100percent.Rdata"))

TarTaxScen <- which(dimnames(Tax_Scenarios)[[3]] %in% "Luxury")
TarRecyScen <- which(dimnames(SP_Scenarios)[[3]] %in% "SP_covid")
TarGlobRecy <- which(dimnames(ClimateFund)[[2]] %in%  "His.PoverPop")
TarTaxLevel <- which(Tax_Level %in% RequireTax15_Glo[2,TarGlobRecy])

Global_Fund <- seq(0,1200,1)
Global_Fundname <- str_c("S",seq(0,1200,1))

#Define aggregated output variable----------
OUT_TAXSP_GF_size <- array(NA, dim = c(length(Reg_Inclu),11,
                                       length(Global_Fund)),
                           dimnames = list(Reg_corr$WBGDPreg, 
                                           c("Pop",
                                             "PPop_npl_ori","PR_npl_ori","PPop_ipl_ori","PR_ipl_ori","Gini_ori",
                                             "PPop_npl_tax", "PR_npl_tax","PPop_ipl_tax", "PR_ipl_tax","Gini_tax"),
                                           Global_Fundname))
#------------


#Combine the tax+SP and Global transfer scenarios
#----------------
for (g in  1:length(Global_Fund)) {
  t1 <- Sys.time()
  print(str_c(g,"--", Global_Fund [g],"billion USD"))
  
  #loop for different poverty line setting
  NPL_setting <- c("WB_PR","WB_2.15")
  for (npl_setting in 1:length(NPL_setting)){
    OUT_TAXSP_GF_size[,1,g] <- colSums(Population)#Population
    
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
      OUT_TAXSP_GF_size[,2,g] <- colSums(Poor)#Poverty headcount under npl
      OUT_TAXSP_GF_size[,3,g] <- colSums(Poor)/colSums(Population)#Poverty rate under npl
    }else{
      OUT_TAXSP_GF_size[,4,g] <- colSums(Poor)#Poverty headcount under ipl
      OUT_TAXSP_GF_size[,5,g] <- colSums(Poor)/colSums(Population)#Poverty rate under ipl
    }
    #------------
    
    #Tax+SP scenarios
    #------------
    #tax
    Cost_perCap<- array(0,dim = dim(Population))
    for (r in 1:length(Reg_Inclu)) {
      Cost_perCap[,r] <- t(Footprint_perCap_detail[,,r])%*%Tax_Scenarios[,r,TarTaxScen,TarTaxLevel]
    }
    
    Expen_tax <- DemandTot_new[1:length(Gnam),,TarTaxScen,TarTaxLevel]*10^6/Population - Cost_perCap
    Inc_effect <- (DemandTot_ori[1:length(Gnam), ] - DemandTot_new[1:length(Gnam),,TarTaxScen,TarTaxLevel])*10^6/Population
    
    
    #social assistance
    Pop_tar <- Population*SP_Scenarios[,,TarRecyScen]
    Pop_untar <- Population-Pop_tar
    
    Benifit_tar <- (Tax_revenue[,TarTaxScen,TarTaxLevel]+ClimateFund[,TarGlobRecy]*Global_Fund[g]/100)/
      colSums(Pop_tar)*10^6#$ Tax revenue adjusted by climate fund
    Benifit_tar[is.nan(Benifit_tar)] <- 0;Benifit_tar[is.infinite(Benifit_tar)] <- 0
    
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
      OUT_TAXSP_GF_size[,7,g] <- colSums(Poor_SP_untar + Poor_SP_tar)
      OUT_TAXSP_GF_size[,8,g] <- colSums(Poor_SP_untar + Poor_SP_tar)/colSums(Population)
    }else{#ipl
      OUT_TAXSP_GF_size[,9,g] <- colSums(Poor_SP_untar + Poor_SP_tar)
      OUT_TAXSP_GF_size[,10,g] <- colSums(Poor_SP_untar + Poor_SP_tar)/colSums(Population)
    }
    #------------
  }
  
  #Inequality
  #-------------
  #original inequality
  for (r in 1:length(Reg_Inclu)) {
    CumShareEXP <- cumsum(Expenditure_perCap[,r]*Population[,r])/sum(Expenditure_perCap[,r]*Population[,r])
    SharePop <- Population[,r]/sum(Population[,r])
    OUT_TAXSP_GF_size[r,6,g] <- (1-2*sum(CumShareEXP*SharePop))*(length(SharePop)/(length(SharePop)-1))
  }
  
  #inequality under tax+SP
  for (r in 1:length(Reg_Inclu)) {
    X <- order(c(Expen_perCap_tar[,r],Expen_perCap_untar[,r]))
    exp.order <- c(Expen_perCap_tar[,r],Expen_perCap_untar[,r])[X]
    pop.order <- c(Pop_tar[,r],Pop_untar[,r])[X]
    CumShareEXP <- cumsum(exp.order*pop.order)/sum(exp.order*pop.order)
    CumShareEXP[is.nan(CumShareEXP)] <- 0
    SharePop <- pop.order/sum(pop.order)
    OUT_TAXSP_GF_size[r,11,g] <- (1-2*sum(CumShareEXP*SharePop))*(length(SharePop)/(length(SharePop)-1))
  }
  
  rm(X,CumShareEXP,SharePop,exp.order,pop.order);gc()
  #-------------
  
  print(str_c("Time cost: ",g,"--", Global_Fund [g],"billion USD",round(Sys.time()-t1,2)))
  
}
#----------------


#Aggregate OUT_TAXSP_GF_size------
Tradeoff <- as.data.frame(apply(OUT_TAXSP_GF_size, 2, colSums))

Tradeoff$Size <- Global_Fund

Tradeoff <- Tradeoff %>% select(Pop,PPop_ipl_ori,PPop_npl_ori,PPop_ipl_tax,PPop_npl_tax,Size)
#--------


save(Tradeoff,
     file = str_c(pathout4,"/Global poverty under various climate fund size.Rdata"))

rm(list = ls()[-which(ls() %in% c("path","pathout","pathout2","pathout3","pathout4","pathout5",
                                  "pathdata3","pathdata4","pathcode"))])
gc()
