#Module 6: The poverty and inequality effect of tax+SA (2) and global transfer (4)
#2022-12-25
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu
#Add consumption response on Jan. 29, 2023

pathout6 <- str_c(pathout,"/All combinations");dir.create(pathout6)
load(str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))
load(str_c(pathout,"/Social assistance scenarios.Rdata"))
load(str_c(pathout,"/Climate finance scenarios.Rdata"))

Tax_Level <- c(25,50,75,100)#US$/ton
Fund_Size <-  c(200,300,400,500)

#Loop for different tax level
for (k in 1:length(Tax_Level)) {
  print(str_c("Begin---", Tax_Level[k],"US$/ton"))
  Level <- Tax_Level[k]
  load(str_c(pathout2,"/",Level," Tax scenarios and revenue.Rdata"))
  
  
  for (z in 1:length(Fund_Size)) {#230123 add a loop for different fund size
  #Combine the tax+SP and Global transfer scenarios
  #----------------
  Global_PovInq_SATAX_GF <- array(NA,dim = c((dim(Tax_Scenarios)[3])*(dim(SP_Scenarios)[3])*(dim(ClimateFund)[2]),21))#aggregate results to global level
  colnames(Global_PovInq_SATAX_GF) <- c("Tax","SA","GF","Pop",
                                        "PPop_npl_ori","PR_npl_ori","PPop_ipl_ori","PR_ipl_ori",
                                        "PPop_npl_tax", "PR_npl_tax","PPop_ipl_tax", "PR_ipl_tax",
                                        "Gini_within_ori","Gini_btw_ori","Gini_within_tax","Gini_btw_tax",
                                        "PPop_npl_chg","PPop_ipl_chg",
                                        "CO2_ori","CO2_tax","CO2_chg")
  Global_PovInq_SATAX_GF <- as.data.frame(Global_PovInq_SATAX_GF)
  
  
  for (w in 1:dim(ClimateFund)[2]) {
    for (i in 1:dim(Tax_Scenarios)[3]) {
      for (j in 1:dim(SP_Scenarios)[3]) {
        t1 <- Sys.time()
        
        OUT_TAXSP_GF <- array(NA, dim = c(length(Reg_Inclu),13),
                              dimnames = list(Reg_corr$WBGDPreg, 
                                              c("Pop",
                                                "PPop_npl_ori","PR_npl_ori","PPop_ipl_ori","PR_ipl_ori","Gini_ori",
                                                "PPop_npl_tax", "PR_npl_tax","PPop_ipl_tax", "PR_ipl_tax","Gini_tax",
                                                "CO2_ori","CO2_tax")))
        OUT_TAXSP_GF <- as.data.frame(OUT_TAXSP_GF)
        
        #loop for different poverty line setting
        NPL_setting <- c("WB_PR","WB_2.15")
        for (npl_setting in 1:length(NPL_setting)){
          OUT_TAXSP_GF[,1] <- colSums(Population)#Population
          
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
          
          npl <- pracma::repmat(NPL_2017,201,1)
          #------------
          
          #Baseline
          #------------
          Poor <- array(0,dim = dim(Population))
          d <- Expenditure_perCap < npl
          d[is.na(d)] <- FALSE# define the poverty
          Poor[d] <- Population[d]
          
          #poverty calculation
          if (npl_setting == 1) {
            OUT_TAXSP_GF[,2] <- colSums(Poor)#Poverty headcount under npl
            OUT_TAXSP_GF[,3] <- colSums(Poor)/colSums(Population)#Poverty rate under npl
          }else{
            OUT_TAXSP_GF[,4] <- colSums(Poor)#Poverty headcount under ipl
            OUT_TAXSP_GF[,5] <- colSums(Poor)/colSums(Population)#Poverty rate under ipl
          }
          #------------
          
          #Tax+SP scenarios
          #------------
          #tax
          Cost_perCap<- array(0,dim = dim(Population))
          for (r in 1:length(Reg_Inclu)) {
            Cost_perCap[,r] <- t(Footprint_perCap_detail[,,r])%*%Tax_Scenarios[,r,i]
          }
          Expen_tax <- Expenditure_perCap - Cost_perCap
          
          #social assistance
          Pop_tar <- Population*SP_Scenarios[,,j]
          Pop_untar <- Population-Pop_tar
          
          Benifit_tar <- (Tax_revenue[,i]+ClimateFund[,w]*(Fund_Size[z]/100))/colSums(Pop_tar)*10^6#$ Tax revenue adjusted by climate fund
          
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
          
          if (npl_setting == 1) {#npl
            OUT_TAXSP_GF[,7] <- colSums(Poor_SP_untar + Poor_SP_tar)
            OUT_TAXSP_GF[,8] <- colSums(Poor_SP_untar + Poor_SP_tar)/colSums(Population)
          }else{#ipl
            OUT_TAXSP_GF[,9] <- colSums(Poor_SP_untar + Poor_SP_tar)
            OUT_TAXSP_GF[,10] <- colSums(Poor_SP_untar + Poor_SP_tar)/colSums(Population)
          }
          #------------
        }
        
        #Inequality
        #-------------
        #original inequality
        for (r in 1:length(Reg_Inclu)) {
          CumShareEXP <- cumsum(Expenditure_perCap[,r]*Population[,r])/sum(Expenditure_perCap[,r]*Population[,r])
          SharePop <- Population[,r]/sum(Population[,r])
          OUT_TAXSP_GF[r,6] <- (1-2*sum(CumShareEXP*SharePop))*(length(SharePop)/(length(SharePop)-1))
        }
        
        #inequality under tax+SP
        for (r in 1:length(Reg_Inclu)) {
          X <- order(c(Expen_perCap_tar[,r],Expen_perCap_untar[,r]))
          exp.order <- c(Expen_perCap_tar[,r],Expen_perCap_untar[,r])[X]
          pop.order <- c(Pop_tar[,r],Pop_untar[,r])[X]
          CumShareEXP <- cumsum(exp.order*pop.order)/sum(exp.order*pop.order)
          SharePop <- pop.order/sum(pop.order)
          OUT_TAXSP_GF[r,11] <- (1-2*sum(CumShareEXP*SharePop))*(length(SharePop)/(length(SharePop)-1))
        }
        
        rm(X,CumShareEXP,SharePop,exp.order,pop.order);gc()
        #-------------
        
        #Emission effect based on consumption response (Jan. 29, 2023)
        #-------------
        #original emission and emission under tax+SP
        PopShare_tar <- Pop_tar/Population
        PopShare_untar <- Pop_untar/Population
        Expen_perCap_tar_chgrate <- Expen_perCap_tar/Expenditure_perCap-1
        Expen_perCap_untar_chgrate <- Expen_perCap_untar/Expenditure_perCap-1
        InvGov_detail_tax <- InvGov_detail-Footprint_InvGov_detail*Tax_Scenarios[,,i]
        InvGov_chgrate <- InvGov_detail_tax/InvGov_detail-1
        for (r in 1:length(Reg_Inclu)) {
          Exp_reponse_chgrate_tar <- pracma::repmat(Expen_perCap_tar_chgrate[,r],65,1)*t(pracma::repmat(Elasticity[,r],201,1))
          Exp_reponse_chgrate_untar <- pracma::repmat(Expen_perCap_untar_chgrate[,r],65,1)*t(pracma::repmat(Elasticity[,r],201,1))
          InvGov_reponse_chgrate <- InvGov_chgrate[,r]*Elasticity[,r]
          
          OUT_TAXSP_GF[r,12] <- sum(Footprint_detail[,,r])+sum(Footprint_InvGov_detail[,r])
          OUT_TAXSP_GF[r,13] <- sum(Footprint_detail[,,r]%*%diag(PopShare_tar[,r])*Exp_reponse_chgrate_tar+
                                   Footprint_detail[,,r]%*%diag(PopShare_untar[,r])*Exp_reponse_chgrate_untar,na.rm = T)+
            sum(Footprint_InvGov_detail[,r]*InvGov_reponse_chgrate,na.rm = T)+
            sum(Footprint_detail[,,r])+sum(Footprint_InvGov_detail[,r])
        }
        #-------------
        
        save(OUT_TAXSP_GF, Expen_perCap_untar,Expen_perCap_tar,Expen_tax,
             Cost_perCap,Benifit_tar,Pop_untar,Pop_tar,Poor_SP_tar,Poor_SP_untar,
             file = str_c(pathout6,"/",Tax_Level[k],"-", 
                          dimnames(Tax_Scenarios)[[3]][i],"-",
                          dimnames(SP_Scenarios)[[3]][j],"-",
                          colnames(ClimateFund)[w],".Rdata"))
        
        print(str_c("Time cost: ", i,dimnames(Tax_Scenarios)[[3]][i],"-",
                    j,dimnames(SP_Scenarios)[[3]][j],"-",w,colnames(ClimateFund)[w],
                    round(Sys.time()-t1,2)))
        
        
        #Aggregate results to global level
        #-------------
        s <- (w-1)*(dim(Tax_Scenarios)[3])*(dim(SP_Scenarios)[3])+(i-1)*(dim(SP_Scenarios)[3])+j
        
        Global_PovInq_SATAX_GF[s,1] <- dimnames(Tax_Scenarios)[[3]][i]
        Global_PovInq_SATAX_GF[s,2] <- dimnames(SP_Scenarios)[[3]][j]
        Global_PovInq_SATAX_GF[s,3] <- colnames(ClimateFund)[w]
        
        Global_PovInq_SATAX_GF[s,4] <- sum(OUT_TAXSP_GF$Pop,na.rm = T)#population
        Global_PovInq_SATAX_GF[s,5] <- sum(OUT_TAXSP_GF$PPop_npl_ori,na.rm = T)
        Global_PovInq_SATAX_GF[s,6] <- sum(OUT_TAXSP_GF$PPop_npl_ori,na.rm = T)/ Global_PovInq_SATAX_GF[s,4]
        Global_PovInq_SATAX_GF[s,7] <- sum(OUT_TAXSP_GF$PPop_ipl_ori,na.rm = T)
        Global_PovInq_SATAX_GF[s,8] <- sum(OUT_TAXSP_GF$PPop_ipl_ori,na.rm = T)/ Global_PovInq_SATAX_GF[s,4]
        Global_PovInq_SATAX_GF[s,9] <- sum(OUT_TAXSP_GF$PPop_npl_tax,na.rm = T)
        Global_PovInq_SATAX_GF[s,10] <- sum(OUT_TAXSP_GF$PPop_npl_tax,na.rm = T)/ Global_PovInq_SATAX_GF[s,4]
        Global_PovInq_SATAX_GF[s,11] <- sum(OUT_TAXSP_GF$PPop_ipl_tax,na.rm = T)
        Global_PovInq_SATAX_GF[s,12] <- sum(OUT_TAXSP_GF$PPop_ipl_tax,na.rm = T)/ Global_PovInq_SATAX_GF[s,4]
        
        Global_PovInq_SATAX_GF[s,13] <- weighted.mean(OUT_TAXSP_GF$Gini_ori,OUT_TAXSP_GF$Pop,na.rm = T)
        Global_PovInq_SATAX_GF[s,14] <- Gini(colSums(Expenditure_perCap*Population,na.rm = T)/colSums(Population,na.rm = T),corr  = T)
        Global_PovInq_SATAX_GF[s,15] <- weighted.mean(OUT_TAXSP_GF$Gini_tax,OUT_TAXSP_GF$Pop,na.rm = T)
        Global_PovInq_SATAX_GF[s,16] <- Gini(colSums(Expen_perCap_untar*Pop_untar+Expen_perCap_tar*Pop_tar,na.rm = T)/colSums(Population,na.rm = T),corr  = T)
        Global_PovInq_SATAX_GF[s,17] <- Global_PovInq_SATAX_GF[s,9]-Global_PovInq_SATAX_GF[s,5]
        Global_PovInq_SATAX_GF[s,18] <- Global_PovInq_SATAX_GF[s,11]-Global_PovInq_SATAX_GF[s,7]
        Global_PovInq_SATAX_GF[s,19] <- sum(OUT_TAXSP_GF$CO2_ori,na.rm = T)
        Global_PovInq_SATAX_GF[s,20] <- sum(OUT_TAXSP_GF$CO2_tax,na.rm = T)
        Global_PovInq_SATAX_GF[s,21] <- Global_PovInq_SATAX_GF[s,20]-Global_PovInq_SATAX_GF[s,19]
        #-------------
      }
    }
  }
  Global_PovInq_SATAX_GF <- Global_PovInq_SATAX_GF[-which(Global_PovInq_SATAX_GF$SA %in% "Null"),]
  
  
  #criteria: global extreme headcount
  Global_PovInq_SATAX_GF %>% filter(SA == "PMT") -> X
  Best_PMT_GF <- c(X[which.min(X$PR_ipl_tax),1:3])
  Global_PovInq_SATAX_GF %>% filter(SA != "PMT") -> X
  Best_Real_GF <- c(X[which.min(X$PR_ipl_tax),1:3])
  print(str_c("Best solution under PMT: ",Best_PMT_GF[1]," + ",Best_PMT_GF[2]," + ",Best_PMT_GF[3]))
  print(str_c("Best solution under Real: ",Best_Real_GF[1]," + ",Best_Real_GF[2]," + ",Best_Real_GF[3]))
  
  save(Global_PovInq_SATAX_GF,Best_PMT_GF,Best_Real_GF,
       file = str_c(pathout6,"/",Tax_Level[k],"Tax-", Fund_Size[z],"Fund-", 
                    "Global poverty under TAX+SA+GF scenarios_ALL scenarios",".Rdata"))
  #----------------
  }
}

rm(Best_PMT,Best_Real,Best_SA,Best_Tax,ClimateFund,
   Global_PovInq_SATAX,Global_PovInq_SATAX_GF,OUT_TAXSP_GF,
   Best_PMT_GF,Best_Real_GF,X,
   
   OUT_TAXSP,pr,Pop_untar,Pop_tar,pop,Poor,Poor_SP_tar,Poor_SP_untar,
   npl,NPL_2017,PR_2017,d,Cost_perCap,Benifit_tar,position,Tax_Level,npl_setting,NPL_setting,
   Expen_perCap_tar,Expen_perCap_untar,Expen_tax,
   
   Tax_Scenarios, Tax_revenue, Level, Elasticity,Elasticity_raw,
   
   SP_Scenarios,
   
   Footprint_perCap_detail,Footprint_detail_global,
   Expenditure_detail,Exp_perCap_detail,Footprint_detail,Expenditure_perCap,
   Population,Emission_CB,Emission_PB,regsecnam, regnam, secnam,
   Reg_corr, Reg_corr_Full,Reg_Inclu, Reg_WBCD, 
   Leontief, EMc,N,G,GN)
gc()