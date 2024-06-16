#Module 4: social assistance scenarios
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu

# 1.	Without recycling 
# 2.	lump-sum
# 3.	Current social assistance programs
# 4.	Social assistance during covid-19 period
# 5.	PMT

load(str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))

SP_Scenarios <- array(0,dim = c(201,length(Reg_Inclu),6),
                      dimnames = list(str_c("Bin",0:200), Reg_Inclu,
                                      c("Null","Universal","Cash",
                                        "SP_current","SP_covid","PMT")))

# 1.	Without recycling 
#--------------------
SP_Scenarios[,,1] <- 0
#--------------------

# 2.	lump-sum 
#--------------------
SP_Scenarios[,,2] <- 1
#--------------------

# 3.	Cash
# 4.	Current social assistance programs
# 5.	Social assistance during covid-19 period
# 6.	PMT
#--------------------
for (k in 1:4) {
  NewSA <- readxl::read_xlsx(str_c(pathdata4,"/SA final/SA_all_20230213.xlsx"),
                             sheet = k)
  SA_data <- apply(NewSA[,-c(1:2)], 2, as.numeric)/100
  
  for (r in 1:length(Reg_corr$WBGDPreg)) {
    print (str_c(r,"---", Reg_corr$WBGDPreg[r]))
    SA_percent <- SA_data[which(NewSA$country %in% Reg_corr$WBGDPreg[r]),]
    
    if (sum(Population[,r]) < 10^7) {
      popcum <- cumsum(Population[,r])
      SAfull <- rep(SA_percent, each = ceiling(sum(Population[,r])/100))
    } else {
      popcum <- cumsum(Population[,r])/100
      SAfull <- rep(SA_percent, each = ceiling(sum(Population[,r])/100/100))
    }
    
    for (q in 1:201) {
      if(q == 1) {  
        if (popcum[q] != 0) {
          SP_Scenarios[r,q,2+k] <- mean(SAfull[1:popcum[q]])
        }
      } else {
        if((popcum[q] - popcum[q-1]) != 0){
          SP_Scenarios[q,r,2+k] <- mean(SAfull[popcum[q-1]:popcum[q]])
        }
      }
    }
  }
}
SP_Scenarios[is.nan(SP_Scenarios)] <- 0
#--------------------


save(SP_Scenarios,
     file = str_c(pathout,"/Social assistance scenarios.Rdata"))

rm(list = ls()[-which(ls() %in% c("path","pathout","pathout2","pathout3","pathout4","pathout5",
                                  "pathdata3","pathdata4","pathcode"))])
gc()

