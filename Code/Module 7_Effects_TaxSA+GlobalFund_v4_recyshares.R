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


# Recy <- c(0.2, 0.3,0.8,1)
# Recynam <- c("20percent","30percent","80percent","100percent")
Recy <- c(0.3,0.8,1)
Recynam <- c("30percent","80percent","100percent")
#Recy <- c(1)
#Recynam <- c("100percent")

for (z in 1:length(Recy)) {#loop for various recycling share
  load(str_c(pathout3,"/Poverty, Ineq, Emission outcome by tax, sp, recy",Recynam[z],".Rdata"))
  
  TarTaxScen <- which(dimnames(Tax_Scenarios)[[3]] %in% 
                        unique(c(dimnames(Tax_Scenarios)[[3]][GlobalBest15[1]],
                                 dimnames(Tax_Scenarios)[[3]][GlobalBest20[1]])))
  TarRecyScen <- which(dimnames(SP_Scenarios)[[3]] %in% 
                         unique(c(dimnames(SP_Scenarios)[[3]][GlobalBest15[2]],
                                  dimnames(SP_Scenarios)[[3]][GlobalBest20[2]],"SP_covid","PerfectTargeted")))
  
  #Define aggregated output variable----------
  OUT_TAXSP_GF <- array(NA, dim = c(length(Reg_Inclu),13,
                                    length(TarTaxScen),length(TarRecyScen),dim(ClimateFund)[2],
                                    length(Tax_Level)),
                        dimnames = list(Reg_corr$WBGDPreg, 
                                        c("Pop",
                                          "PPop_npl_ori","PR_npl_ori","PPop_ipl_ori","PR_ipl_ori","Gini_ori",
                                          "PPop_npl_tax", "PR_npl_tax","PPop_ipl_tax", "PR_ipl_tax","Gini_tax",
                                          "CO2_ori","CO2_tax"),
                                        dimnames(Tax_Scenarios)[[3]][TarTaxScen],dimnames(SP_Scenarios)[[3]][TarRecyScen],
                                        dimnames(ClimateFund)[[2]],
                                        Tax_level_nam))
  
  CO2_Response_GloRecy <- array(0,dim = c(length(TarTaxScen),length(TarRecyScen),dim(ClimateFund)[2],
                                          length(Tax_Level)),
                                dimnames = list( dimnames(Tax_Scenarios)[[3]][TarTaxScen],dimnames(SP_Scenarios)[[3]][TarRecyScen],
                                                 dimnames(ClimateFund)[[2]],
                                                 Tax_level_nam))
  #------------
  
  
  #Combine the tax+SP and Global transfer scenarios
  #----------------
  for (k in  1:length(Tax_Level)) {
    t1 <- Sys.time()
    print(str_c(Recynam[z],"--","Global recycling Begin: ",k,"--", Tax_Level[k],"$/tCO2"))
    
    for (w in 1:dim(ClimateFund)[2]) {
      for (i in 1:length(TarTaxScen) ) {
        for (j in 1:length(TarRecyScen)) {
          #loop for different poverty line setting
          NPL_setting <- c("WB_PR","WB_2.15")
          for (npl_setting in 1:length(NPL_setting)){
            OUT_TAXSP_GF[,1,i,j,w,k] <- colSums(Population)#Population
            
            #Poverty definition
            #------------
            if (npl_setting == 1) PR_2017 <- Reg_corr$WB_PR/100#"WB_PR" 
            if (npl_setting == 2) PR_2017 <- Reg_corr$WB_2.15/100#"WB_2.15" 
            pr <- pracma::repmat(PR_2017,201,1)
            pop <- apply(t(t(Population)/colSums(Population)),2,cumsum)
            position <- apply(abs(pr-pop),2, which.min)
            NPL_2017 <- c()
            for (r in 1 :length(Reg_Inclu)) {
              NPL_2017[r] <- Expenditure_perCap[position[r],r]
            }
            
            npl <- round(pracma::repmat(NPL_2017,201,1),5)
            #------------
            
            #Baseline
            #------------
            Poor <- array(0,dim = dim(Population))
            d <- round(Expenditure_perCap,5) <= npl
            d[is.na(d)] <- FALSE# define the poverty
            Poor[d] <- Population[d]
            
            #poverty calculation
            if (npl_setting == 1) {
              OUT_TAXSP_GF[,2,i,j,w,k] <- colSums(Poor)#Poverty headcount under npl
              OUT_TAXSP_GF[,3,i,j,w,k] <- colSums(Poor)/colSums(Population)#Poverty rate under npl
            }else{
              OUT_TAXSP_GF[,4,i,j,w,k] <- colSums(Poor)#Poverty headcount under ipl
              OUT_TAXSP_GF[,5,i,j,w,k] <- colSums(Poor)/colSums(Population)#Poverty rate under ipl
            }
            #------------
            
            #Tax+SP scenarios
            #------------
            #tax
            Cost_perCap<- array(0,dim = dim(Population))
            for (r in 1:length(Reg_Inclu)) {
              Cost_perCap[,r] <- t(Footprint_perCap_detail[,,r])%*%Tax_Scenarios[,r,TarTaxScen[i],k]
            }
            
            Expen_tax <- DemandTot_new[1:length(Gnam),,TarTaxScen[i],k]*10^6/Population - Cost_perCap
            Inc_effect <- (DemandTot_ori[1:length(Gnam), ] - DemandTot_new[1:length(Gnam),,TarTaxScen[i],k])*10^6/Population
            
            
            #social assistance
            
            #Redefine Poverty-focused scenario's coverage rate, add on 11/06/2024
            if(TarRecyScen[j] == 6){
              SP_Scenarios[,,6] <- pvt_tarfc(Tax_revenue[,TarTaxScen[i],k]*Recy[z]+ClimateFund[,w],Fund_required)
            }
            
            Pop_tar <- Population*SP_Scenarios[,,TarRecyScen[j]]
            Pop_untar <- Population-Pop_tar
            
            Benifit_tar <- (Tax_revenue[,TarTaxScen[i],k]*Recy[z]+ClimateFund[,w])/colSums(Pop_tar)*10^6#$ Tax revenue adjusted by climate fund
            Benifit_tar[is.nan(Benifit_tar)] <- 0;Benifit_tar[is.infinite(Benifit_tar)] <- 0
            
            Expen_perCap_untar <- Expen_tax
            Expen_perCap_tar <- Expen_tax + pracma::repmat(Benifit_tar,201,1)
            Expen_perCap_tar[is.na(Expen_perCap_tar)] <- 0
            Expen_perCap_untar[is.na(Expen_perCap_untar)] <- 0
            
            #poverty calculation
            d <- round(Expen_perCap_untar,5) <= npl
            d[is.na(d)] <- FALSE
            Poor_SP_untar <- array(0,dim = dim(Expenditure_perCap))
            Poor_SP_untar[d] <- Pop_untar[d]
            
            d <- round(Expen_perCap_tar,5) <= npl
            d[is.na(d)] <- FALSE
            Poor_SP_tar <- array(0,dim = dim(Expenditure_perCap))
            Poor_SP_tar[d] <- Pop_tar[d]
            
            if (npl_setting == 1) {#npl
              OUT_TAXSP_GF[,7,i,j,w,k] <- colSums(Poor_SP_untar + Poor_SP_tar)
              OUT_TAXSP_GF[,8,i,j,w,k] <- colSums(Poor_SP_untar + Poor_SP_tar)/colSums(Population)
            }else{#ipl
              OUT_TAXSP_GF[,9,i,j,w,k] <- colSums(Poor_SP_untar + Poor_SP_tar)
              OUT_TAXSP_GF[,10,i,j,w,k] <- colSums(Poor_SP_untar + Poor_SP_tar)/colSums(Population)
            }
            #------------
          }
          
          #Inequality
          #-------------
          #original inequality
          for (r in 1:length(Reg_Inclu)) {
            CumShareEXP <- cumsum(Expenditure_perCap[,r]*Population[,r])/sum(Expenditure_perCap[,r]*Population[,r])
            SharePop <- Population[,r]/sum(Population[,r])
            OUT_TAXSP_GF[r,6,i,j,w,k] <- (1-2*sum(CumShareEXP*SharePop))*(length(SharePop)/(length(SharePop)-1))
          }
          
          #inequality under tax+SP
          for (r in 1:length(Reg_Inclu)) {
            X <- order(c(Expen_perCap_tar[,r],Expen_perCap_untar[,r]))
            exp.order <- c(Expen_perCap_tar[,r],Expen_perCap_untar[,r])[X]
            pop.order <- c(Pop_tar[,r],Pop_untar[,r])[X]
            CumShareEXP <- cumsum(exp.order*pop.order)/sum(exp.order*pop.order)
            CumShareEXP[is.nan(CumShareEXP)] <- 0
            SharePop <- pop.order/sum(pop.order)
            OUT_TAXSP_GF[r,11,i,j,w,k] <- (1-2*sum(CumShareEXP*SharePop))*(length(SharePop)/(length(SharePop)-1))
          }
          
          rm(X,CumShareEXP,SharePop,exp.order,pop.order);gc()
          #-------------
          
          
          #Emission effect based on consumption response
          #-------------
          #original emission and emission under tax+SP
          OUT_TAXSP_GF[,12,i,j,w,k] <- CO2_Reg_Response[,7,k]
          
          RecyAmount <- pracma::repmat(Benifit_tar,201,1)*Pop_tar/10^6
          
          CO2_Recycle <- c()
          for (r in 1:length(Reg_Inclu)) {
            CO2_Recycle[r] <- t(INT2[,,r]%*%RecyAmount[,r]/
                                  Price_Response[,r,TarTaxScen[i],k])%*%CF_IncluDirect[,r]
          }
          
          OUT_TAXSP_GF[,13,i,j,w,k]  <- CO2_Reg_Response[,TarTaxScen[i],k]+CO2_Recycle
          CO2_Response_GloRecy[i,j,w,k] <- sum(OUT_TAXSP_GF[,13,i,j,w,k])
          #-------------
          
          save(Expen_perCap_untar,Expen_perCap_tar,
               Expen_tax,Cost_perCap,Inc_effect,CO2_Recycle,
               Benifit_tar,Pop_untar,Pop_tar,Poor_SP_tar,Poor_SP_untar,
               file = str_c(pathout4,"/",Tax_level_nam[k],"-",
                            dimnames(Tax_Scenarios)[[3]][TarTaxScen[i]],"-",
                            dimnames(SP_Scenarios)[[3]][TarRecyScen[j]],"-",
                            colnames(ClimateFund)[w],Recynam[z],".Rdata"))
        }
      }
    }
    
    print(str_c(Recynam[z],"--","Time cost: ",k,"--", Tax_Level[k],"$/tCO2--",round(Sys.time()-t1,2)))
    
  }
  #----------------
  
  
  
  #Identify the policy combination required for 2 degree goal-------------
  CO2_Effect20_GloRecy <- CO2_Response_GloRecy-Tar20
  CO2_Effect15_GloRecy <- CO2_Response_GloRecy-Tar15
  
  #find the tax level required under each combination
  RequireTax20_Glo <- array(0, dim = c(dim(CO2_Response_GloRecy)[2],dim(CO2_Response_GloRecy)[3]),
                            dimnames = list(dimnames(CO2_Response_GloRecy)[[2]],dimnames(CO2_Response_GloRecy)[[3]]))
  RequireTax15_Glo <- RequireTax20_Glo
  
  IPL_PovertyOutcome20_Glo <- RequireTax20_Glo; IPL_PovertyOutcome15_Glo <- RequireTax20_Glo 
  NPL_PovertyOutcome20_Glo <- RequireTax20_Glo; NPL_PovertyOutcome15_Glo <- RequireTax20_Glo 
  LocalGiniOutcome20_Glo <- RequireTax20_Glo; LocalGiniOutcome15_Glo <- RequireTax20_Glo 
  InterGiniOutcome20_Glo <- RequireTax20_Glo; InterGiniOutcome15_Glo <- RequireTax20_Glo 
  
  for (i in 1:dim(CO2_Response_GloRecy)[2]) {
    for (j in 1:dim(CO2_Response_GloRecy)[3]) {
      #2 degree--------------
      RequireTax20_Glo[i,j] <- Tax_Level[which.min(abs(CO2_Effect20_GloRecy[1,i,j,]))]
      Posi <- which.min(abs(CO2_Effect20_GloRecy[1,i,j,]))
      OUTCOME20_Glo <- as.data.frame(OUT_TAXSP_GF[,,1,i ,j,Posi])
      
      IPL_PovertyOutcome20_Glo[i,j] <- sum(OUTCOME20_Glo$PPop_ipl_tax- OUTCOME20_Glo$PPop_ipl_ori)/10^6#Million headcount
      NPL_PovertyOutcome20_Glo[i,j] <- sum(OUTCOME20_Glo$PPop_npl_tax- OUTCOME20_Glo$PPop_npl_ori)/10^6#Million headcount
      
      LocalGiniOutcome20_Glo[i,j] <- (weighted.mean(OUTCOME20_Glo$Gini_tax,OUTCOME20_Glo$Pop)-
                                        weighted.mean(OUTCOME20_Glo$Gini_ori,OUTCOME20_Glo$Pop))/
        weighted.mean(OUTCOME20_Glo$Gini_ori,OUTCOME20_Glo$Pop)
      
      load(str_c(pathout4,"/L",RequireTax20_Glo[i,j],"-",
                 dimnames(Tax_Scenarios)[[3]][TarTaxScen],"-",
                 dimnames(CO2_Response_GloRecy)[[2]][i],"-",
                 dimnames(CO2_Response_GloRecy)[[3]][j],Recynam[z],".Rdata"))
      
      InterGiniOutcome20_Glo[i,j] <- (Gini(colSums(Expen_perCap_untar*Pop_untar+Expen_perCap_tar*Pop_tar,na.rm = T)/colSums(Population,na.rm = T),
                                           corr  = T)-Gini(colSums(Expenditure_perCap*Population,na.rm = T)/colSums(Population,na.rm = T),
                                                           corr  = T))/Gini(colSums(Expenditure_perCap*Population,na.rm = T)/colSums(Population,na.rm = T),
                                                                            corr  = T)
      #----------
      
      #1.5 degree-------
      RequireTax15_Glo[i,j] <- Tax_Level[which.min(abs(CO2_Effect15_GloRecy[1,i,j,]))]
      Posi <- which.min(abs(CO2_Effect15_GloRecy[1,i,j,]))
      OUTCOME15_Glo <- as.data.frame(OUT_TAXSP_GF[,,1,i ,j,Posi])
      
      IPL_PovertyOutcome15_Glo[i,j] <- sum(OUTCOME15_Glo$PPop_ipl_tax- OUTCOME15_Glo$PPop_ipl_ori)/10^6#Million headcount
      NPL_PovertyOutcome15_Glo[i,j] <- sum(OUTCOME15_Glo$PPop_npl_tax- OUTCOME15_Glo$PPop_npl_ori)/10^6#Million headcount
      
      LocalGiniOutcome15_Glo[i,j] <- (weighted.mean(OUTCOME15_Glo$Gini_tax,OUTCOME15_Glo$Pop)-
                                        weighted.mean(OUTCOME15_Glo$Gini_ori,OUTCOME15_Glo$Pop))/
        weighted.mean(OUTCOME15_Glo$Gini_ori,OUTCOME15_Glo$Pop)
      
      load(str_c(pathout4,"/L",RequireTax15_Glo[i,j],"-",
                 dimnames(Tax_Scenarios)[[3]][TarTaxScen],"-",
                 dimnames(CO2_Response_GloRecy)[[2]][i],"-",
                 dimnames(CO2_Response_GloRecy)[[3]][j],Recynam[z],".Rdata"))
      
      InterGiniOutcome15_Glo[i,j] <- (Gini(colSums(Expen_perCap_untar*Pop_untar+Expen_perCap_tar*Pop_tar,na.rm = T)/colSums(Population,na.rm = T),
                                           corr  = T)-Gini(colSums(Expenditure_perCap*Population,na.rm = T)/colSums(Population,na.rm = T),
                                                           corr  = T))/Gini(colSums(Expenditure_perCap*Population,na.rm = T)/colSums(Population,na.rm = T),
                                                                            corr  = T)
      #----------
    }
  }
  
  GlobalBest20_GloRec <- as.vector(arrayInd(which.min(IPL_PovertyOutcome20_Glo[1:2,]), .dim = dim(IPL_PovertyOutcome20_Glo[1:2,])))
  GlobalBest15_GloRec <- as.vector(arrayInd(which.min(IPL_PovertyOutcome15_Glo[1:2,]), .dim = dim(IPL_PovertyOutcome15_Glo[1:2,])))
  #----------------
  
  
  save(OUT_TAXSP_GF, CO2_Response_GloRecy,TarRecyScen,TarTaxScen,
       CO2_Effect15_GloRecy,CO2_Effect15_GloRecy,
       RequireTax20_Glo,RequireTax15_Glo,
       IPL_PovertyOutcome20_Glo,NPL_PovertyOutcome20_Glo,LocalGiniOutcome20_Glo,InterGiniOutcome20_Glo,
       IPL_PovertyOutcome15_Glo,NPL_PovertyOutcome15_Glo,LocalGiniOutcome15_Glo,InterGiniOutcome15_Glo,
       GlobalBest20_GloRec,GlobalBest15_GloRec,
       file = str_c(pathout4,"/Global poverty under TAX+SA+GF scenarios",Recynam[z],".Rdata"))
  
}

rm(list = ls()[-which(ls() %in% c("path","pathout","pathout2","pathout3","pathout4","pathout5",
                                  "pathdata3","pathdata4","pathcode"))])
gc()
