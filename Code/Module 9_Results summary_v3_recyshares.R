#Module 8: Results summary
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu

load(str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))
load(str_c(pathout2,"/Tax scenarios_price and CO2 response by tax level.Rdata"))
load(str_c(pathout,"/Social assistance scenarios.Rdata"))
load(str_c(pathout,"/Climate finance scenarios.Rdata"))
load(str_c(pathout4,"/Global poverty under various climate fund size.Rdata"))


# Recy <- c(0.2,0.3,0.8,1)
# Recynam <- c("20percent","30percent","80percent","100percent")
Recy <- c(0.3,0.8,1)
Recynam <- c("30percent","80percent","100percent")

for (z in 1:length(Recy)) {#loop for various recycling share
  load(str_c(pathout3,"/Poverty, Ineq, Emission outcome by tax, sp, recy",Recynam[z],".Rdata"))
  load(str_c(pathout4,"/Global poverty under TAX+SA+GF scenarios",Recynam[z],".Rdata"))
  
  #Figure: Tax level required for climate goals; Global total poverty, ineq outcome 
  #================
  #Under domestic policy mix-------
  RequireTax20 <- as.data.frame(RequireTax20); RequireTax20$Tax <- rownames(RequireTax20)
  RequireTax15 <- as.data.frame(RequireTax15); RequireTax15$Tax <- rownames(RequireTax15)
  
  NPL_PovertyOutcome20 <- as.data.frame(NPL_PovertyOutcome20); NPL_PovertyOutcome20$Tax <- rownames(NPL_PovertyOutcome20)
  NPL_PovertyOutcome15 <- as.data.frame(NPL_PovertyOutcome15); NPL_PovertyOutcome15$Tax <- rownames(NPL_PovertyOutcome15)
  
  IPL_PovertyOutcome20 <- as.data.frame(IPL_PovertyOutcome20); IPL_PovertyOutcome20$Tax <- rownames(IPL_PovertyOutcome20)
  IPL_PovertyOutcome15 <- as.data.frame(IPL_PovertyOutcome15); IPL_PovertyOutcome15$Tax <- rownames(IPL_PovertyOutcome15)
  
  LocalGiniOutcome20 <- as.data.frame(LocalGiniOutcome20); LocalGiniOutcome20$Tax <- rownames(LocalGiniOutcome20)
  LocalGiniOutcome15 <- as.data.frame(LocalGiniOutcome15); LocalGiniOutcome15$Tax <- rownames(LocalGiniOutcome15)
  
  InterGiniOutcome20 <- as.data.frame(InterGiniOutcome20); InterGiniOutcome20$Tax <- rownames(InterGiniOutcome20)
  InterGiniOutcome15 <- as.data.frame(InterGiniOutcome15); InterGiniOutcome15$Tax <- rownames(InterGiniOutcome15)
  
  
  GlobalOutcome_dome <- rbind(RequireTax20 %>% pivot_longer(-Tax, names_to = "SP") %>% mutate(Vari = "TaxLevel",Goal = "2.0degree"),
                              RequireTax15 %>% pivot_longer(-Tax, names_to = "SP") %>% mutate(Vari = "TaxLevel",Goal = "1.5degree"),
                              NPL_PovertyOutcome20 %>% pivot_longer(-Tax, names_to = "SP") %>% mutate(Vari = "NPLchg",Goal = "2.0degree"),
                              NPL_PovertyOutcome15 %>% pivot_longer(-Tax, names_to = "SP") %>% mutate(Vari = "NPLchg",Goal = "1.5degree"),
                              IPL_PovertyOutcome20 %>% pivot_longer(-Tax, names_to = "SP") %>% mutate(Vari = "IPLchg",Goal = "2.0degree"),
                              IPL_PovertyOutcome15 %>% pivot_longer(-Tax, names_to = "SP") %>% mutate(Vari = "IPLchg",Goal = "1.5degree"),
                              LocalGiniOutcome20 %>% pivot_longer(-Tax, names_to = "SP") %>% mutate(Vari = "LocalGiniChg",Goal = "2.0degree"),
                              LocalGiniOutcome15 %>% pivot_longer(-Tax, names_to = "SP") %>% mutate(Vari = "LocalGiniChg",Goal = "1.5degree"),
                              InterGiniOutcome20 %>% pivot_longer(-Tax, names_to = "SP") %>% mutate(Vari = "InterGiniChg",Goal = "2.0degree"),
                              InterGiniOutcome15 %>% pivot_longer(-Tax, names_to = "SP") %>% mutate(Vari = "InterGiniChg",Goal = "1.5degree"))
  #----------
  
  #Under global policy mix-------
  RequireTax20_Glo <- as.data.frame(RequireTax20_Glo); RequireTax20_Glo$SP <- rownames(RequireTax20_Glo)
  RequireTax15_Glo <- as.data.frame(RequireTax15_Glo); RequireTax15_Glo$SP <- rownames(RequireTax15_Glo)
  
  NPL_PovertyOutcome20_Glo <- as.data.frame(NPL_PovertyOutcome20_Glo); NPL_PovertyOutcome20_Glo$SP <- rownames(NPL_PovertyOutcome20_Glo)
  NPL_PovertyOutcome15_Glo <- as.data.frame(NPL_PovertyOutcome15_Glo); NPL_PovertyOutcome15_Glo$SP <- rownames(NPL_PovertyOutcome15_Glo)
  
  IPL_PovertyOutcome20_Glo <- as.data.frame(IPL_PovertyOutcome20_Glo); IPL_PovertyOutcome20_Glo$SP <- rownames(IPL_PovertyOutcome20_Glo)
  IPL_PovertyOutcome15_Glo <- as.data.frame(IPL_PovertyOutcome15_Glo); IPL_PovertyOutcome15_Glo$SP <- rownames(IPL_PovertyOutcome15_Glo)
  
  LocalGiniOutcome20_Glo <- as.data.frame(LocalGiniOutcome20_Glo); LocalGiniOutcome20_Glo$SP <- rownames(LocalGiniOutcome20_Glo)
  LocalGiniOutcome15_Glo <- as.data.frame(LocalGiniOutcome15_Glo); LocalGiniOutcome15_Glo$SP <- rownames(LocalGiniOutcome15_Glo)
  
  InterGiniOutcome20_Glo <- as.data.frame(InterGiniOutcome20_Glo); InterGiniOutcome20_Glo$SP <- rownames(InterGiniOutcome20_Glo)
  InterGiniOutcome15_Glo <- as.data.frame(InterGiniOutcome15_Glo); InterGiniOutcome15_Glo$SP <- rownames(InterGiniOutcome15_Glo)
  
  
  GlobalOutcome_GloRec <- rbind(RequireTax20_Glo %>% pivot_longer(-SP, names_to = "GF") %>% mutate(Vari = "TaxLevel",Goal = "2.0degree"),
                                RequireTax15_Glo %>% pivot_longer(-SP, names_to = "GF") %>% mutate(Vari = "TaxLevel",Goal = "1.5degree"),
                                NPL_PovertyOutcome20_Glo %>% pivot_longer(-SP, names_to = "GF") %>% mutate(Vari = "NPLchg",Goal = "2.0degree"),
                                NPL_PovertyOutcome15_Glo %>% pivot_longer(-SP, names_to = "GF") %>% mutate(Vari = "NPLchg",Goal = "1.5degree"),
                                IPL_PovertyOutcome20_Glo %>% pivot_longer(-SP, names_to = "GF") %>% mutate(Vari = "IPLchg",Goal = "2.0degree"),
                                IPL_PovertyOutcome15_Glo %>% pivot_longer(-SP, names_to = "GF") %>% mutate(Vari = "IPLchg",Goal = "1.5degree"),
                                LocalGiniOutcome20_Glo %>% pivot_longer(-SP, names_to = "GF") %>% mutate(Vari = "LocalGiniChg",Goal = "2.0degree"),
                                LocalGiniOutcome15_Glo %>% pivot_longer(-SP, names_to = "GF") %>% mutate(Vari = "LocalGiniChg",Goal = "1.5degree"),
                                InterGiniOutcome20_Glo %>% pivot_longer(-SP, names_to = "GF") %>% mutate(Vari = "InterGiniChg",Goal = "2.0degree"),
                                InterGiniOutcome15_Glo %>% pivot_longer(-SP, names_to = "GF") %>% mutate(Vari = "InterGiniChg",Goal = "1.5degree"))
  
  GlobalOutcome_GloRec$Tax <- dimnames(Tax_Scenarios)[[3]][TarTaxScen]
  #----------
  #================
  
  
  #Figure: Uneven burden by deciles under different tax scenarios.
  #===========================
  #Use 60 $/ton show the carbon burden by decile
  DEC_name <- c("G10","G20","G30","G40","G50","G60","G70","G80","G90","G100")
  DEC_num <- seq(0.1,1,0.1)
  
  #National level: can be used for results check
  #-------------
  Population_decile <- array(NA,dim = c(length(DEC_name),length(Reg_Inclu)),
                             dimnames = list(DEC_name,Reg_Inclu))
  Expenditure_decile <- Population_decile
  Carbon_cost_decile <- array(NA,dim = c(length(DEC_name),length(Reg_Inclu),dim(Tax_Scenarios)[3]),
                              dimnames = list(DEC_name,Reg_Inclu,dimnames(Tax_Scenarios)[[3]]))
  Cost_perEXP_decile <- Carbon_cost_decile
  Cost_perCap_decile <- Carbon_cost_decile
  Income_cost_decile <- Carbon_cost_decile
  Income_cost_perEXP_decile <- Carbon_cost_decile
  Income_cost_perCap_decile <- Carbon_cost_decile
  
  Transfer_Str_Decile <- array(NA,dim = c(length(DEC_name),3,length(Reg_Inclu)),
                               dimnames = list(DEC_name,c("Lab","Cap","Tax"),Reg_Inclu))
  
  for (i in 1:(dim(Tax_Scenarios)[3]-1)) {
    load(str_c(pathout3,"/L",60,"-",
               dimnames(Tax_Scenarios)[[3]][i],"-",
               dimnames(SP_Scenarios)[[3]][1],Recynam[z],".Rdata"))
    
    for (q in 1:length(Reg_Inclu)) {
      a <- trunc(sum(trunc(Population[,q]/100))/10)#Population for each decile
      B <- rep.int(Expenditure_perCap[,q],trunc(Population[,q]/100))#change the unit for pop to boost calculation
      C <- rep.int(Cost_perCap[,q],trunc(Population[,q]/100))#Carbon Cost
      D <- rep.int(Inc_effect[,q],trunc(Population[,q]/100))#Income Cost
      E1 <- rep.int(Transfer_mat[1:length(Gnam),1,q],trunc(Population[,q]/100))
      E2 <- rep.int(Transfer_mat[1:length(Gnam),2,q],trunc(Population[,q]/100))
      E3 <- rep.int(Transfer_mat[1:length(Gnam),3,q],trunc(Population[,q]/100))
      
      for (p in 1:length(DEC_name)){
        Population_decile[p,q] <- a*100
        Expenditure_decile[p,q] <- sum(B[((p-1)*a+1):(p*a)])*100
        Carbon_cost_decile[p,q,i] <- sum(C[((p-1)*a+1):(p*a)])*100
        Income_cost_decile[p,q,i] <- sum(D[((p-1)*a+1):(p*a)])*100
        Transfer_Str_Decile[p,1,q] <- sum(E1[((p-1)*a+1):(p*a)])*100/length(E1)
        Transfer_Str_Decile[p,2,q] <- sum(E2[((p-1)*a+1):(p*a)])*100/length(E2)
        Transfer_Str_Decile[p,3,q] <- sum(E3[((p-1)*a+1):(p*a)])*100/length(E3)
      }
    }
    Cost_perEXP_decile[,,i] <-  Carbon_cost_decile[,,i]/Expenditure_decile#unit: 1
    Cost_perCap_decile[,,i] <- Carbon_cost_decile[,,i] /Population_decile#unit: $
    Income_cost_perEXP_decile[,,i] <- Income_cost_decile[,,i]/Expenditure_decile#unit: 1
    Income_cost_perCap_decile[,,i] <- Income_cost_decile[,,i] /Population_decile#unit: $
  }
  
  Tot_cost_perEXP_decile <- Income_cost_perEXP_decile+Cost_perEXP_decile
  #-------------
  
  #Prepare national fig data
  #------------
  INT <- Tot_cost_perEXP_decile
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(Tot_cost_perEXP_decile)[[2]],dim(Tot_cost_perEXP_decile)[3]),
                         rep(dimnames(Tot_cost_perEXP_decile)[[3]], each = dim(Tot_cost_perEXP_decile)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Group <- dimnames(Tot_cost_perEXP_decile)[[1]]
  INT %>% pivot_longer(-Group, names_to = "Country_Tax") %>% 
    separate(col = Country_Tax, "::", into = c("Country","TaxScenario")) %>% 
    mutate(Vari = "Total_Effect") -> Tot_cost_PerExp_Figdata
  
  
  INT <- Income_cost_perEXP_decile
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(Income_cost_perEXP_decile)[[2]],dim(Income_cost_perEXP_decile)[3]),
                         rep(dimnames(Income_cost_perEXP_decile)[[3]], each = dim(Income_cost_perEXP_decile)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Group <- dimnames(Income_cost_perEXP_decile)[[1]]
  INT %>% pivot_longer(-Group, names_to = "Country_Tax") %>% 
    separate(col = Country_Tax, "::", into = c("Country","TaxScenario"))  %>% 
    mutate(Vari = "Income_Effect") -> Inc_cost_PerExp_Figdata
  
  
  INT <- Cost_perEXP_decile
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(Cost_perEXP_decile)[[2]],dim(Cost_perEXP_decile)[3]),
                         rep(dimnames(Cost_perEXP_decile)[[3]], each = dim(Cost_perEXP_decile)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Group <- dimnames(Cost_perEXP_decile)[[1]]
  INT %>% pivot_longer(-Group, names_to = "Country_Tax") %>% 
    separate(col = Country_Tax, "::", into = c("Country","TaxScenario"))  %>% 
    mutate(Vari = "Price_Effect") -> Carbon_cost_PerExp_Figdata
  
  Data_Unevenburden <- rbind(Tot_cost_PerExp_Figdata,Inc_cost_PerExp_Figdata,Carbon_cost_PerExp_Figdata)
  #-------------
  
  #Region level
  #-------------
  Reg <- unique(Reg_corr$region_class_new)
  Population_decile_agg <- array(NA,dim = c(length(DEC_name),length(Reg)),
                                 dimnames = list(DEC_name,Reg))
  Expenditure_decile_agg <- Population_decile_agg
  Carbon_cost_decile_agg <- array(NA,dim = c(length(DEC_name),length(Reg),dim(Tax_Scenarios)[3]),
                                  dimnames = list(DEC_name,Reg,dimnames(Tax_Scenarios)[[3]]))
  Cost_perEXP_decile_agg <- Carbon_cost_decile_agg 
  Cost_perCap_decile_agg <- Carbon_cost_decile_agg 
  Income_cost_decile_agg <- Carbon_cost_decile_agg
  Income_cost_perEXP_decile_agg <- Carbon_cost_decile_agg
  Income_cost_perCap_decile_agg <- Carbon_cost_decile_agg
  
  for (i in 1:(dim(Tax_Scenarios)[3]-1)) {
    load(str_c(pathout3,"/L",60,"-",
               dimnames(Tax_Scenarios)[[3]][i],"-",
               dimnames(SP_Scenarios)[[3]][1],Recynam[z],".Rdata"))
    
    Population_agg <- t(rowsum(t(Population),Reg_corr$region_class_new,na.rm = T,reorder = F))
    Expenditure_agg <- t(rowsum(t(Expenditure_perCap*Population),Reg_corr$region_class_new,na.rm = T,reorder = F))
    
    Carbon_cost_agg <- t(rowsum(t(Cost_perCap*Population),Reg_corr$region_class_new,na.rm = T,reorder = F))
    Income_cost_agg <- t(rowsum(t(Inc_effect*Population),Reg_corr$region_class_new,na.rm = T,reorder = F))
    
    for (q in 1:length(Reg)) {
      a <- trunc(sum(trunc(Population_agg[,q]/100))/10)#Population for each decile
      B <- rep.int(Expenditure_agg[,q]/Population_agg[,q],trunc(Population_agg[,q]/100))#change the unit for pop to boost calculation
      C <- rep.int(Carbon_cost_agg[,q]/Population_agg[,q],trunc(Population_agg[,q]/100))
      D <- rep.int(Income_cost_agg[,q]/Population_agg[,q],trunc(Population_agg[,q]/100))
      
      for (p in 1:length(DEC_name)){
        Population_decile_agg[p,q] <- a*100
        m = (p-1)*a+1; n = p*a
        Expenditure_decile_agg[p,q] <- sum(B[m:n])*100
        Carbon_cost_decile_agg[p,q,i] <- sum(C[m:n])*100
        Income_cost_decile_agg[p,q,i] <- sum(D[m:n])*100
      }
    }
    Cost_perEXP_decile_agg[,,i] <-  Carbon_cost_decile_agg[,,i]/Expenditure_decile_agg#unit: 1
    Cost_perCap_decile_agg[,,i] <- Carbon_cost_decile_agg[,,i] /Population_decile_agg#unit: $
    Income_cost_perEXP_decile_agg[,,i] <- Income_cost_decile_agg[,,i]/Expenditure_decile_agg#unit: 1
    Income_cost_perCap_decile_agg[,,i] <- Income_cost_decile_agg[,,i] /Population_decile_agg#unit: $
  }
  Expenditure_perCap_decile_agg  <- Expenditure_decile_agg/Population_decile_agg#unit: $
  Tot_cost_perEXP_decile_agg <- Income_cost_perEXP_decile_agg+Cost_perEXP_decile_agg
  
  rm(a,B,C,D);gc()
  #-------------
  
  #Prepare regional fig data
  #------------
  INT <- Tot_cost_perEXP_decile_agg
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(Tot_cost_perEXP_decile_agg)[[2]],dim(Tot_cost_perEXP_decile_agg)[3]),
                         rep(dimnames(Tot_cost_perEXP_decile_agg)[[3]], each = dim(Tot_cost_perEXP_decile_agg)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Group <- dimnames(Tot_cost_perEXP_decile_agg)[[1]]
  INT %>% pivot_longer(-Group, names_to = "Reg_Tax") %>% 
    separate(col = Reg_Tax, "::", into = c("Reg","TaxScenario")) %>% 
    mutate(Vari = "Total_Effect") -> Tot_cost_PerExp_Figdata
  
  
  INT <- Income_cost_perEXP_decile_agg
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(Income_cost_perEXP_decile_agg)[[2]],dim(Income_cost_perEXP_decile_agg)[3]),
                         rep(dimnames(Income_cost_perEXP_decile_agg)[[3]], each = dim(Income_cost_perEXP_decile_agg)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Group <- dimnames(Income_cost_perEXP_decile_agg)[[1]]
  INT %>% pivot_longer(-Group, names_to = "Reg_Tax") %>% 
    separate(col = Reg_Tax, "::", into = c("Reg","TaxScenario"))  %>% 
    mutate(Vari = "Income_Effect") -> Inc_cost_PerExp_Figdata
  
  
  INT <- Cost_perEXP_decile_agg
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(Cost_perEXP_decile_agg)[[2]],dim(Cost_perEXP_decile_agg)[3]),
                         rep(dimnames(Cost_perEXP_decile_agg)[[3]], each = dim(Cost_perEXP_decile_agg)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Group <- dimnames(Cost_perEXP_decile_agg)[[1]]
  INT %>% pivot_longer(-Group, names_to = "Reg_Tax") %>% 
    separate(col = Reg_Tax, "::", into = c("Reg","TaxScenario"))  %>% 
    mutate(Vari = "Price_Effect") -> Carbon_cost_PerExp_Figdata
  
  Data_Unevenburden_agg <- rbind(Tot_cost_PerExp_Figdata,Inc_cost_PerExp_Figdata,Carbon_cost_PerExp_Figdata)
  #-------------
  #================
  
  
  #Figure: Social protection rate
  #================
  Cover_decile_agg <- array(NA,dim = c(length(DEC_name),length(Reg),dim(SP_Scenarios)[3]+2),
                            dimnames = list(DEC_name,Reg,c(dimnames(SP_Scenarios)[[3]],"PerfectTargeted60","PerfectTargeted90")))
  
  for (i in 1:(dim(SP_Scenarios)[3]+2)){
    if(i <=5){
      Pop_tar <- Population*SP_Scenarios[,,i]
    }
    if (i == 6) {
      Pop_tar <- Population*read.csv(str_c(pathout3,"/Poverty focused Coverage rate--L30-Universal_P-",Recynam[z],".csv"),row.names = 1)
    }
    if (i == 7) {
      Pop_tar <- Population*read.csv(str_c(pathout3,"/Poverty focused Coverage rate--L60-Universal_P-",Recynam[z],".csv"),row.names = 1)
    }
    if (i == 8) {
      Pop_tar <- Population*read.csv(str_c(pathout3,"/Poverty focused Coverage rate--L90-Universal_P-",Recynam[z],".csv"),row.names = 1)
    }
    
    
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
  
  
  INT <- Cover_decile_agg
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(Cover_decile_agg)[[2]],dim(Cover_decile_agg)[3]),
                         rep(dimnames(Cover_decile_agg)[[3]], each = dim(Cover_decile_agg)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Group <- dimnames(Cover_decile_agg)[[1]]
  INT %>% pivot_longer(-Group, names_to = "Reg_SP") %>% 
    separate(col = Reg_SP, "::", into = c("Reg","SPScenario")) -> SPCover_Reg
  #================
  
  
  #Figure: regional outcome under global best policy mix
  #================
  Best_Tax <- "Luxury"  
  Best_SA <- "SP_covid"
  Best_GF20 <- "His.PoverGap"
  Best_GF15 <- "His.PoverGap"
  
  Reg_Outcome_20 <- array(0,dim = c(length(Reg),10,4),
                          dimnames = list(Reg,
                                          c("PPop_npl_tax", "PR_npl_tax","PPop_ipl_tax", "PR_ipl_tax",
                                            "Gini_within_tax","PPop_npl_chg","PPop_ipl_chg",
                                            "PPop_npl_chgrate","PPop_ipl_chgrate","PPop_Gini_chgrate"),
                                          c("Null","Tax","TaxSA","TaxSAGF")))
  Reg_Outcome_15 <- Reg_Outcome_20
  
  #Four scenarios: Null, tax without recycling, best tax + SA; best TAX+SA+GF
  # 2.0 degree---------
  TarLevel <- RequireTax20[which(rownames(RequireTax20) %in% Best_Tax),which(colnames(RequireTax20) %in% "Null")]
  OUT_Tax_20 <- as.data.frame(OUT_TAXSP[,,which(dimnames(OUT_TAXSP)[[3]] %in% Best_Tax),
                                        which(Tax_Level %in% TarLevel),which(dimnames(OUT_TAXSP)[[5]] %in% "Null")])
  
  TarLevel <- RequireTax20[which(rownames(RequireTax20) %in% Best_Tax),which(colnames(RequireTax20) %in% Best_SA)]
  OUT_TaxSA_20 <- as.data.frame(OUT_TAXSP[,,which(dimnames(OUT_TAXSP)[[3]] %in% Best_Tax),
                                          which(Tax_Level %in% TarLevel),which(dimnames(OUT_TAXSP)[[5]] %in% Best_SA)])
  
  TarLevel <- RequireTax20_Glo[which(rownames(RequireTax20_Glo) %in% Best_SA),which(colnames(RequireTax20_Glo) %in% Best_GF20)]
  OUT_TaxSAGF_20 <- as.data.frame(OUT_TAXSP_GF[,,,which(dimnames(OUT_TAXSP_GF)[[4]] %in% Best_SA),
                                               which(dimnames(OUT_TAXSP_GF)[[5]] %in% Best_GF20),
                                               which(Tax_Level %in% TarLevel)])
  
  #null
  Reg_Outcome_20[,1,1] <- rowsum(OUT_Tax_20$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,2,1] <-  Reg_Outcome_20[,1,1]/rowsum(OUT_Tax_20$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,3,1] <- rowsum(OUT_Tax_20$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,4,1] <-  Reg_Outcome_20[,3,1]/rowsum(OUT_Tax_20$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,6,1] <-  Reg_Outcome_20[,1,1]-rowsum(OUT_Tax_20$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,7,1] <-  Reg_Outcome_20[,3,1]-rowsum(OUT_Tax_20$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)
  for (r in 1:length(Reg)) {
    a <- Reg_corr$region_class_new %in% Reg[r]
    Reg_Outcome_20[r,5,1] <- weighted.mean(OUT_Tax_20$Gini_tax[a],OUT_Tax_20$Pop[a],na.rm = T)
  }
  
  #Tax
  Reg_Outcome_20[,1,2] <- rowsum(OUT_Tax_20$PPop_npl_tax,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,2,2] <- Reg_Outcome_20[,1,2]/rowsum(OUT_Tax_20$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,3,2] <- rowsum(OUT_Tax_20$PPop_ipl_tax,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,4,2] <- Reg_Outcome_20[,3,2]/rowsum(OUT_Tax_20$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,6,2] <- Reg_Outcome_20[,1,2]-rowsum(OUT_Tax_20$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,7,2] <- Reg_Outcome_20[,3,2]-rowsum(OUT_Tax_20$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,8,2] <- Reg_Outcome_20[,1,2]/rowsum(OUT_Tax_20$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)-1
  Reg_Outcome_20[,9,2] <- Reg_Outcome_20[,3,2]/rowsum(OUT_Tax_20$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)-1
  for (r in 1:length(Reg)) {
    a <- Reg_corr$region_class_new %in% Reg[r]
    Reg_Outcome_20[r,5,2] <- weighted.mean(OUT_Tax_20$Gini_tax[a],OUT_Tax_20$Pop[a],na.rm = T)
    Reg_Outcome_20[r,10,2] <- weighted.mean(OUT_Tax_20$Gini_tax[a],OUT_Tax_20$Pop[a],na.rm = T)/
      weighted.mean(OUT_Tax_20$Gini_ori[a],OUT_Tax_20$Pop[a],na.rm = T)-1
  }
  
  #TaxSA
  Reg_Outcome_20[,1,3] <- rowsum(OUT_TaxSA_20$PPop_npl_tax,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,2,3] <- Reg_Outcome_20[,1,3]/rowsum(OUT_TaxSA_20$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,3,3] <- rowsum(OUT_TaxSA_20$PPop_ipl_tax,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,4,3] <- Reg_Outcome_20[,3,3]/rowsum(OUT_TaxSA_20$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,6,3] <- Reg_Outcome_20[,1,3]-rowsum(OUT_TaxSA_20$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,7,3] <- Reg_Outcome_20[,3,3]-rowsum(OUT_TaxSA_20$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,8,3] <- Reg_Outcome_20[,1,3]/rowsum(OUT_TaxSA_20$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)-1
  Reg_Outcome_20[,9,3] <- Reg_Outcome_20[,3,3]/rowsum(OUT_TaxSA_20$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)-1
  for (r in 1:length(Reg)) {
    a <- Reg_corr$region_class_new %in% Reg[r]
    Reg_Outcome_20[r,5,3] <- weighted.mean(OUT_TaxSA_20$Gini_tax[a],OUT_TaxSA_20$Pop[a],na.rm = T)
    Reg_Outcome_20[r,10,3] <- weighted.mean(OUT_TaxSA_20$Gini_tax[a],OUT_TaxSA_20$Pop[a],na.rm = T)/
      weighted.mean(OUT_TaxSA_20$Gini_ori[a],OUT_TaxSA_20$Pop[a],na.rm = T)-1
  }
  
  #TaxSAGF
  Reg_Outcome_20[,1,4] <- rowsum(OUT_TaxSAGF_20$PPop_npl_tax,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,2,4] <- Reg_Outcome_20[,1,4]/rowsum(OUT_TaxSAGF_20$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,3,4] <- rowsum(OUT_TaxSAGF_20$PPop_ipl_tax,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,4,4] <- Reg_Outcome_20[,3,4]/rowsum(OUT_TaxSAGF_20$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,5,4] <- weighted.mean(OUT_TaxSAGF_20$Gini_tax,OUT_TaxSAGF_20$Pop,na.rm = T)
  Reg_Outcome_20[,6,4] <- Reg_Outcome_20[,1,4]-rowsum(OUT_TaxSAGF_20$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,7,4] <- Reg_Outcome_20[,3,4]-rowsum(OUT_TaxSAGF_20$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_20[,8,4] <- Reg_Outcome_20[,1,4]/rowsum(OUT_TaxSAGF_20$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)-1
  Reg_Outcome_20[,9,4] <- Reg_Outcome_20[,3,4]/rowsum(OUT_TaxSAGF_20$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)-1
  for (r in 1:length(Reg)) {
    a <- Reg_corr$region_class_new %in% Reg[r]
    Reg_Outcome_20[r,5,4] <- weighted.mean(OUT_TaxSAGF_20$Gini_tax[a],OUT_TaxSAGF_20$Pop[a],na.rm = T)
    Reg_Outcome_20[r,10,4] <- weighted.mean(OUT_TaxSAGF_20$Gini_tax[a],OUT_TaxSAGF_20$Pop[a],na.rm = T)/
      weighted.mean(OUT_TaxSAGF_20$Gini_ori[a],OUT_TaxSAGF_20$Pop[a],na.rm = T)-1
  }
  
  INT <- Reg_Outcome_20
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(Reg_Outcome_20)[[2]],dim(Reg_Outcome_20)[3]),
                         rep(dimnames(Reg_Outcome_20)[[3]], each = dim(Reg_Outcome_20)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Reg <- dimnames(Reg_Outcome_20)[[1]]
  INT %>% pivot_longer(-Reg, names_to = "Vari_Scenarios") %>% 
    separate(col = Vari_Scenarios, "::", into = c("Vari","Scenario")) -> Reg_Outcome_20_Figdata
  
  # 1.5 degree-----------
  TarLevel <- RequireTax15[which(rownames(RequireTax15) %in% Best_Tax),which(colnames(RequireTax15) %in% "Null")]
  OUT_Tax_15 <- as.data.frame(OUT_TAXSP[,,which(dimnames(OUT_TAXSP)[[3]] %in% Best_Tax),
                                        which(Tax_Level %in% TarLevel),which(dimnames(OUT_TAXSP)[[5]] %in% "Null")])
  
  TarLevel <- RequireTax15[which(rownames(RequireTax15) %in% Best_Tax),which(colnames(RequireTax15) %in% Best_SA)]
  OUT_TaxSA_15 <- as.data.frame(OUT_TAXSP[,,which(dimnames(OUT_TAXSP)[[3]] %in% Best_Tax),
                                          which(Tax_Level %in% TarLevel),which(dimnames(OUT_TAXSP)[[5]] %in% Best_SA)])
  
  TarLevel <- RequireTax15_Glo[which(rownames(RequireTax15_Glo) %in% Best_SA),which(colnames(RequireTax15_Glo) %in% Best_GF15)]
  OUT_TaxSAGF_15 <- as.data.frame(OUT_TAXSP_GF[,,,which(dimnames(OUT_TAXSP_GF)[[4]] %in% Best_SA),
                                               which(dimnames(OUT_TAXSP_GF)[[5]] %in% Best_GF15),
                                               which(Tax_Level %in% TarLevel)])
  
  #null
  Reg_Outcome_15[,1,1] <- rowsum(OUT_Tax_15$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,2,1] <-  Reg_Outcome_15[,1,1]/rowsum(OUT_Tax_15$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,3,1] <- rowsum(OUT_Tax_15$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,4,1] <-  Reg_Outcome_15[,3,1]/rowsum(OUT_Tax_15$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,6,1] <-  Reg_Outcome_15[,1,1]-rowsum(OUT_Tax_15$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,7,1] <-  Reg_Outcome_15[,3,1]-rowsum(OUT_Tax_15$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)
  for (r in 1:length(Reg)) {
    a <- Reg_corr$region_class_new %in% Reg[r]
    Reg_Outcome_15[r,5,1] <- weighted.mean(OUT_Tax_15$Gini_tax[a],OUT_Tax_15$Pop[a],na.rm = T)
  }
  
  #Tax
  Reg_Outcome_15[,1,2] <- rowsum(OUT_Tax_15$PPop_npl_tax,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,2,2] <- Reg_Outcome_15[,1,2]/rowsum(OUT_Tax_15$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,3,2] <- rowsum(OUT_Tax_15$PPop_ipl_tax,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,4,2] <- Reg_Outcome_15[,3,2]/rowsum(OUT_Tax_15$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,6,2] <- Reg_Outcome_15[,1,2]-rowsum(OUT_Tax_15$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,7,2] <- Reg_Outcome_15[,3,2]-rowsum(OUT_Tax_15$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,8,2] <- Reg_Outcome_15[,1,2]/rowsum(OUT_Tax_15$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)-1
  Reg_Outcome_15[,9,2] <- Reg_Outcome_15[,3,2]/rowsum(OUT_Tax_15$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)-1
  for (r in 1:length(Reg)) {
    a <- Reg_corr$region_class_new %in% Reg[r]
    Reg_Outcome_15[r,5,2] <- weighted.mean(OUT_Tax_15$Gini_tax[a],OUT_Tax_15$Pop[a],na.rm = T)
    Reg_Outcome_15[r,10,2] <- weighted.mean(OUT_Tax_15$Gini_tax[a],OUT_Tax_15$Pop[a],na.rm = T)/
      weighted.mean(OUT_Tax_15$Gini_ori[a],OUT_Tax_15$Pop[a],na.rm = T)-1
  }
  
  #TaxSA
  Reg_Outcome_15[,1,3] <- rowsum(OUT_TaxSA_15$PPop_npl_tax,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,2,3] <- Reg_Outcome_15[,1,3]/rowsum(OUT_TaxSA_15$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,3,3] <- rowsum(OUT_TaxSA_15$PPop_ipl_tax,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,4,3] <- Reg_Outcome_15[,3,3]/rowsum(OUT_TaxSA_15$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,6,3] <- Reg_Outcome_15[,1,3]-rowsum(OUT_TaxSA_15$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,7,3] <- Reg_Outcome_15[,3,3]-rowsum(OUT_TaxSA_15$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,8,3] <- Reg_Outcome_15[,1,3]/rowsum(OUT_TaxSA_15$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)-1
  Reg_Outcome_15[,9,3] <- Reg_Outcome_15[,3,3]/rowsum(OUT_TaxSA_15$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)-1
  for (r in 1:length(Reg)) {
    a <- Reg_corr$region_class_new %in% Reg[r]
    Reg_Outcome_15[r,5,3] <- weighted.mean(OUT_TaxSA_15$Gini_tax[a],OUT_TaxSA_15$Pop[a],na.rm = T)
    Reg_Outcome_15[r,10,3] <- weighted.mean(OUT_TaxSA_15$Gini_tax[a],OUT_TaxSA_15$Pop[a],na.rm = T)/
      weighted.mean(OUT_TaxSA_15$Gini_ori[a],OUT_TaxSA_15$Pop[a],na.rm = T)-1
  }
  
  #TaxSAGF
  Reg_Outcome_15[,1,4] <- rowsum(OUT_TaxSAGF_15$PPop_npl_tax,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,2,4] <- Reg_Outcome_15[,1,4]/rowsum(OUT_TaxSAGF_15$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,3,4] <- rowsum(OUT_TaxSAGF_15$PPop_ipl_tax,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,4,4] <- Reg_Outcome_15[,3,4]/rowsum(OUT_TaxSAGF_15$Pop,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,5,4] <- weighted.mean(OUT_TaxSAGF_15$Gini_tax,OUT_TaxSAGF_15$Pop,na.rm = T)
  Reg_Outcome_15[,6,4] <- Reg_Outcome_15[,1,4]-rowsum(OUT_TaxSAGF_15$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,7,4] <- Reg_Outcome_15[,3,4]-rowsum(OUT_TaxSAGF_15$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)
  Reg_Outcome_15[,8,4] <- Reg_Outcome_15[,1,4]/rowsum(OUT_TaxSAGF_15$PPop_npl_ori,Reg_corr$region_class_new,reorder = F)-1
  Reg_Outcome_15[,9,4] <- Reg_Outcome_15[,3,4]/rowsum(OUT_TaxSAGF_15$PPop_ipl_ori,Reg_corr$region_class_new,reorder = F)-1
  for (r in 1:length(Reg)) {
    a <- Reg_corr$region_class_new %in% Reg[r]
    Reg_Outcome_15[r,5,4] <- weighted.mean(OUT_TaxSAGF_15$Gini_tax[a],OUT_TaxSAGF_15$Pop[a],na.rm = T)
    Reg_Outcome_15[r,10,4] <- weighted.mean(OUT_TaxSAGF_15$Gini_tax[a],OUT_TaxSAGF_15$Pop[a],na.rm = T)/
      weighted.mean(OUT_TaxSAGF_15$Gini_ori[a],OUT_TaxSAGF_15$Pop[a],na.rm = T)-1
  }
  
  INT <- Reg_Outcome_15
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(Reg_Outcome_15)[[2]],dim(Reg_Outcome_15)[3]),
                         rep(dimnames(Reg_Outcome_15)[[3]], each = dim(Reg_Outcome_15)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Reg <- dimnames(Reg_Outcome_15)[[1]]
  INT %>% pivot_longer(-Reg, names_to = "Vari_Scenarios") %>% 
    separate(col = Vari_Scenarios, "::", into = c("Vari","Scenario")) -> Reg_Outcome_15_Figdata
  #================
  
  
  #Figure: National level domestic best recycling policy
  #================
  X_IPL <- OUT_TAXSP[,which(dimnames(OUT_TAXSP)[[2]] %in% "PR_ipl_tax"),
                     which(dimnames(OUT_TAXSP)[[3]] %in% "Luxury"),which(Tax_Level %in% 60),
                     which(dimnames(OUT_TAXSP)[[5]]%in% c("Cash","SP_current","SP_covid"))]
  
  
  write.csv(X_IPL,str_c(pathout5,"/","National best policy mix_raw results_luxury 60",Recynam[z],".csv"),row.names = F)
  
  Nation_BestRecy <- as.data.frame(array(NA,dim = c(length(Reg_Inclu),2)))
  colnames(Nation_BestRecy) <- c("Country","BestRec")
  Nation_BestRecy$Country <- Reg_corr$WBGDPreg
  Nation_BestRecy$BestRec <- colnames(X_IPL)[apply(X_IPL, 1, which.min)]
  #================
  
  
  #Figure: Tax level by nation and sectors (also elas), tax revenue and gdp; 60$/ton
  #================
  #Tax level-CBDR pro-based/con-based-------
  CBDR <- as.data.frame(array(NA, dim=c(length(Reg),3)))
  colnames(CBDR) <- c("Region","Tax_C","Tax_P")
  CBDR[,1] <- unique(Reg_corr$region_class_new)
  CBDR[,2] <- rowsum(Tax_reg_C[,which(Tax_Level %in% 60)]*colSums(Emission_CB),Reg_corr$region_class_new,reorder = F)/
    rowsum(colSums(Emission_CB),Reg_corr$region_class_new,reorder = F)
  CBDR[,3] <- rowsum(Tax_reg_P[,which(Tax_Level %in% 60)]*colSums(Emission_PB),Reg_corr$region_class_new,reorder = F)/
    rowsum(colSums(Emission_PB),Reg_corr$region_class_new,reorder = F)
  
  #Tax level-Luxury (sectoral difference)---------
  Luxury <- as.data.frame(array(NA, dim=c(length(secnam),3)))
  colnames(Luxury) <- c("Secnam","Elas","TaxRate")
  Luxury[,1] <- secnam
  Luxury[,2] <- rowMeans(Elasticity_raw)
  Luxury[,3] <- rowMeans(Tax_Scenarios[,,3,which(Tax_Level %in% 60)])
  Country_sec_level_lux <- Tax_Scenarios[,,3,which(Tax_Level %in% 60)]
  write.csv(Country_sec_level_lux,
            str_c(pathout5,"/", "Country_sector_level_lux_60.csv"))
  
  #Tax revenue-amount--------
  Revenue <- rowsum(Tax_revenue[,1:7,which(Tax_Level %in% 60)],Reg_corr$region_class_new,reorder = F)#million
  Revenue <- as.data.frame(Revenue[,1:6])
  
  #Tax revenue-share of GDP
  RevenuetoGDP <- rowsum(Tax_revenue[,1:7,which(Tax_Level %in% 60)],Reg_corr$region_class_new,reorder = F)/
    pracma::repmat(rowsum(Tax_revenue[,8,which(Tax_Level %in% 60)],Reg_corr$region_class_new,reorder = F),1,7)
  
  Revenue$Reg <- rownames(Revenue)
  Revenue %>% pivot_longer(-Reg,
                           names_to = "Type") %>% 
    mutate(Vari = "Revenue")-> X
  
  RevenuetoGDP <- as.data.frame(RevenuetoGDP[,1:6])
  RevenuetoGDP$Reg <- rownames(RevenuetoGDP)
  RevenuetoGDP %>% pivot_longer(-Reg,
                                names_to = "Type") %>% 
    mutate(Vari = "RevenueShare")-> Y
  
  RevenueAnalysis <- rbind(X,Y)
  rm(X,Y);gc()
  
  Revenue_nation <- Tax_revenue[,1:7,which(Tax_Level %in% 60)]
  write.csv(Revenue_nation,
            str_c(pathout5,"/", "Revenue_nation_60.csv"))
  #================
  
  #Figure: Global recycling scenarios
  #================
  Reg_Fund <- rowsum(ClimateFund,Reg_corr$region_class_new,reorder = F)
  write.csv(ClimateFund,
            str_c(pathout5,"/", "Reg_Fund_100.csv"))
  #================
  
  
  #Figure: By tax level global outcome 
  #================
  #Tax level and emission reduction (all domestic combination)
  INT <- CO2_Response_Recy
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[c(2,3)]))
  colnames(INT) <- str_c(rep(dimnames(CO2_Response_Recy)[[2]],dim(CO2_Response_Recy)[3]),
                         rep(dimnames(CO2_Response_Recy)[[3]], each = dim(CO2_Response_Recy)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Tax <- dimnames(CO2_Response_Recy)[[1]]
  INT %>% pivot_longer(-Tax, names_to = "TaxLevel_SP") %>% 
    separate(col = TaxLevel_SP, "::", into = c("TaxLevel","SP")) %>% 
    mutate(Vari = "CO2_Effect") %>% 
    mutate(TaxLevel = as.numeric(substr(TaxLevel ,2, nchar(TaxLevel)))) -> Glo_CO2_Figdata
  
  #Tax level and national/extreme poverty (all domestic combination)
  NPL_Response_Recy <- array(NA, dim = dim(CO2_Response_Recy),dimnames = dimnames(CO2_Response_Recy))
  IPL_Response_Recy <- NPL_Response_Recy
  for (i in 1:dim(CO2_Response_Recy)[1]) {
    for (j in 1:dim(CO2_Response_Recy)[2]) {
      for (k in 1:dim(CO2_Response_Recy)[3]) {
        NPL_Response_Recy[i,j,k] <- sum(OUT_TAXSP[,which(dimnames(OUT_TAXSP)[[2]] %in% "PPop_npl_tax"),i,j,k])/10^6 #Million headcount
        # - sum(OUT_TAXSP[,which(dimnames(OUT_TAXSP)[[2]] %in% "PPop_npl_ori"),i,j,k])/10^6
        
        IPL_Response_Recy[i,j,k] <- sum(OUT_TAXSP[,which(dimnames(OUT_TAXSP)[[2]] %in% "PPop_ipl_tax"),i,j,k])/10^6 #Million headcount
        # - sum(OUT_TAXSP[,which(dimnames(OUT_TAXSP)[[2]] %in% "PPop_ipl_ori"),i,j,k])/10^6
      }
    }
  }
  
  
  INT <- NPL_Response_Recy
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[c(2,3)]))
  colnames(INT) <- str_c(rep(dimnames(NPL_Response_Recy)[[2]],dim(NPL_Response_Recy)[3]),
                         rep(dimnames(NPL_Response_Recy)[[3]], each = dim(NPL_Response_Recy)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Tax <- dimnames(NPL_Response_Recy)[[1]]
  INT %>% pivot_longer(-Tax, names_to = "TaxLevel_SP") %>% 
    separate(col = TaxLevel_SP, "::", into = c("TaxLevel","SP")) %>% 
    mutate(Vari = "NPL_Effect") %>% 
    mutate(TaxLevel = as.numeric(substr(TaxLevel ,2, nchar(TaxLevel)))) -> Glo_NPL_Figdata
  
  
  INT <- IPL_Response_Recy
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[c(2,3)]))
  colnames(INT) <- str_c(rep(dimnames(IPL_Response_Recy)[[2]],dim(IPL_Response_Recy)[3]),
                         rep(dimnames(IPL_Response_Recy)[[3]], each = dim(IPL_Response_Recy)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Tax <- dimnames(IPL_Response_Recy)[[1]]
  INT %>% pivot_longer(-Tax, names_to = "TaxLevel_SP") %>% 
    separate(col = TaxLevel_SP, "::", into = c("TaxLevel","SP")) %>% 
    mutate(Vari = "IPL_Effect") %>% 
    mutate(TaxLevel = as.numeric(substr(TaxLevel ,2, nchar(TaxLevel)))) -> Glo_IPL_Figdata
  
  Data_AllTaxLevel <- rbind(Glo_CO2_Figdata,Glo_NPL_Figdata,Glo_IPL_Figdata)
  #================
  
  
  GlobalOutcome_dome$PovertyRateLabel <- NA
  GlobalOutcome_dome$PovertyRateLabel[GlobalOutcome_dome$Vari %in% "NPLchg"] <- GlobalOutcome_dome$value[GlobalOutcome_dome$Vari %in% "NPLchg"]/(colSums(OUT_Tax_15)[2]/10^6)*100
  GlobalOutcome_dome$PovertyRateLabel[GlobalOutcome_dome$Vari %in% "IPLchg"] <- GlobalOutcome_dome$value[GlobalOutcome_dome$Vari %in% "IPLchg"]/(colSums(OUT_Tax_15)[4]/10^6)*100
  
  GlobalOutcome_GloRec$PovertyRateLabel <- NA
  GlobalOutcome_GloRec$PovertyRateLabel[GlobalOutcome_GloRec$Vari %in% "NPLchg"] <- 
    GlobalOutcome_GloRec$value[GlobalOutcome_GloRec$Vari %in% "NPLchg"]/(colSums(OUT_Tax_15)[2]/10^6)*100
  GlobalOutcome_GloRec$PovertyRateLabel[GlobalOutcome_GloRec$Vari %in% "IPLchg"] <- 
    GlobalOutcome_GloRec$value[GlobalOutcome_GloRec$Vari %in% "IPLchg"]/(colSums(OUT_Tax_15)[4]/10^6)*100
  
  GloNPLheadcount <- colSums(OUT_Tax_15)[2]/10^6
  GloIPLheadcount <- colSums(OUT_Tax_15)[4]/10^6
  
  GlobalOutcome_dome$TaxGDPShareLabel <- NA
  GlobalOutcome_dome$TaxGDPShareLabel[GlobalOutcome_dome$Vari %in% "TaxLevel"] <- 
    GlobalOutcome_dome$value[GlobalOutcome_dome$Vari %in% "TaxLevel"]*CO2_Response_Recy[7,1,1]/sum(Tax_revenue[,8,1])*100

  GlobalOutcome_GloRec$TaxGDPShareLabel <- NA
  GlobalOutcome_GloRec$TaxGDPShareLabel[GlobalOutcome_GloRec$Vari %in% "TaxLevel"] <- 
    GlobalOutcome_GloRec$value[GlobalOutcome_GloRec$Vari %in% "TaxLevel"]*CO2_Response_Recy[7,1,1]/sum(Tax_revenue[,8,1])*100
  
  Tradeoff$ChgrateIPL <- Tradeoff$PPop_ipl_tax/Tradeoff$PPop_ipl_ori-1
  
  save(DEC_name,DEC_num,
       Transfer_Str_Decile,VA_Tot_Chg,
       GlobalOutcome_dome,#Global poverty, ineq outcome under domestic policy mix, by climate goal (1.5 or 2.0)
       GlobalOutcome_GloRec,#Global poverty, ineq outcome under global policy mix, by climate goal (1.5 or 2.0)
       Data_Unevenburden, Data_Unevenburden_agg,#Uneven burden across decile at region/country level, for 60$/ton
       CBDR,RevenueAnalysis,Luxury,
       OUT_TAXSP,OUT_TAXSP_GF,#raw full results by country, scenarios, tax level
       CO2_Response_Recy,CO2_Response_GloRecy,#CO2 response by tax level
       SP_Scenarios,SPCover_Reg,#Scoial protection
       Reg_Fund,#Global climate fund
       Nation_BestRecy,#Best recycling policy by country
       OUT_Tax_20,OUT_TaxSA_20,OUT_TaxSAGF_20,OUT_Tax_15,OUT_TaxSA_15,OUT_TaxSAGF_15,#national outcome under best policy mix
       Reg_Outcome_20_Figdata,Reg_Outcome_15_Figdata,#regional outcome under best policy mix
       Data_AllTaxLevel,#Global outcome under all tax level
       Reg,Gnam,Reg_Inclu,secnam,Tax_level_nam,Tax_Level,
       Tradeoff,
       Tax_reg_C,Tax_reg_P,
       GloNPLheadcount,GloIPLheadcount,
       file = str_c(pathout5,"/Results summary",Recynam[z],".Rdata"))
}

rm(list = ls()[-which(ls() %in% c("path","pathout","pathout2","pathout3","pathout4","pathout5",
                                  "pathdata3","pathdata4","pathcode"))])
gc()

