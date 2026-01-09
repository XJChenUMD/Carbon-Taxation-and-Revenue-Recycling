#Module 4: global climate finance scenarios
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu


# Gather $100 billion from developed countries and then allocate to developing countries. 
# How to differentiate the responsibility of developed countries:
# 1.	Historical emissions 
# 2.	Current emissions (also determines how much tax the government can collect)
# How to allocate the fund between developing countries:
# 1.	Based on poverty headcount.
# 2.	Based on population (because the redistribution within countries may not target at the poor).

load(str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))

ClimateFund <- array(0,dim = c(length(Reg_Inclu),6),
                     dimnames = list(Reg_Inclu,
                                     c("His.PoverPop","His.Pop","His.PoverGap",
                                       "Curren.PoverPop","Curren.Pop","Curren.PoverGap")))

#------------
developed <- which(Reg_corr$Dev_Class %in% "developed")
developing <- which(Reg_corr$Dev_Class %in% "developing")

Share_His <- Reg_corr$AccumCO2[developed]/sum(Reg_corr$AccumCO2[developed])
Share_Curren <- Reg_corr$WB_CO2_2017[developed]/sum(Reg_corr$WB_CO2_2017[developed])

Poverty.Headcount <- Reg_corr$Pop_2017_WB*Reg_corr$WB_2.15/100
Share_Head <- Poverty.Headcount[developing]/sum(Poverty.Headcount[developing])

Share_Pop <- Reg_corr$Pop_2017_WB[developing]/sum(Reg_corr$Pop_2017_WB[developing])
#------------


#Match extreme poverty lines
#-----------------
PR_2017 <- Reg_corr$WB_2.15/100#"WB_2.15" 
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

c <- pracma::repmat(NPL_2017,201,1)
d <- Expenditure_perCap < c
d[is.na(d)] <- FALSE

#Poverty gap: money needed for eradicating poverty
Gap_poverty <- array(0,dim = dim(Expenditure_perCap))
dimnames(Gap_poverty) <- dimnames(Expenditure_perCap)
Gap_poverty[d] <- (c[d]- Expenditure_perCap[d])*(Population[d])/10^6#M$
Poverty.Gap <- colSums(Gap_poverty,na.rm = T)
Share_Gap <- Poverty.Gap[developing]/sum(Poverty.Gap[developing])
#-----------------

#Scenarios
#------------
#"His.PovertyHead"
redis <- rep(0, length(Reg_Inclu))
redis[developed] <- -Share_His*10^5; redis[developing] <- Share_Head*10^5#M$
ClimateFund[,1] <- redis

# "His.Pop"
redis <- rep(0, length(Reg_Inclu))
redis[developed] <- -Share_His*10^5; redis[developing] <- Share_Pop*10^5#M$
ClimateFund[,2] <- redis

# "His.PovertyGap"
redis <- rep(0, length(Reg_Inclu))
redis[developed] <- -Share_His*10^5; redis[developing] <- Share_Gap*10^5#M$
ClimateFund[,3] <- redis

# "Curren.PovertyHead"
redis <- rep(0, length(Reg_Inclu))
redis[developed] <- -Share_Curren*10^5; redis[developing] <- Share_Head*10^5#M$
ClimateFund[,4] <- redis

# "Curren.Pop"
redis <- rep(0, length(Reg_Inclu))
redis[developed] <- -Share_Curren*10^5; redis[developing] <- Share_Pop*10^5#M$
ClimateFund[,5] <- redis

# "Curren.PovertyGap"
redis <- rep(0, length(Reg_Inclu))
redis[developed] <- -Share_Curren*10^5; redis[developing] <- Share_Gap*10^5#M$
ClimateFund[,6] <- redis
#------------

save(ClimateFund,Poverty.Gap,Poverty.Headcount,PR_2017,NPL_2017,developed,developing,
     file = str_c(pathout,"/Climate finance scenarios.Rdata"))

rm(list = ls()[-which(ls() %in% c("path","pathout","pathout2","pathout3","pathout4","pathout5",
                                  "pathdata3","pathdata4","pathcode"))])
gc()
