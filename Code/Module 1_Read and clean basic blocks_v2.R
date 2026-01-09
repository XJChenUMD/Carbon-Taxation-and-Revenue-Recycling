




t0 <- Sys.time()

#Read GAMS output data-----
QT0 <- readxl::read_xlsx(str_c(pathdata,"/QT0.xlsx"))
X0 <- readxl::read_xlsx(str_c(pathdata,"/X0.xlsx"))
Tz0 <- readxl::read_xlsx(str_c(pathdata,"/Tz0.xlsx"))
Tm0 <- readxl::read_xlsx(str_c(pathdata,"/Tm0.xlsx"))
F0 <- readxl::read_xlsx(str_c(pathdata,"/F0.xlsx"))
Tf0 <- readxl::read_xlsx(str_c(pathdata,"/Tf0.xlsx"))
#FF and F0 are identical


Xp0 <- readxl::read_xlsx(str_c(pathdata,"/Xp0.xlsx"))
Xg0 <- readxl::read_xlsx(str_c(pathdata,"/Xg0.xlsx"))
Xv0 <- readxl::read_xlsx(str_c(pathdata,"/Xv0.xlsx"))
FF <- readxl::read_xlsx(str_c(pathdata,"/FF.xlsx"))
Te0 <- readxl::read_xlsx(str_c(pathdata,"/Te0.xlsx"))

Td0 <- readxl::read_xlsx(str_c(pathdata,"/Td0.xlsx"))
Sp0 <- readxl::read_xlsx(str_c(pathdata,"/Sp0.xlsx"))
Sg0 <- readxl::read_xlsx(str_c(pathdata,"/Sg0.xlsx"))
Sf <- readxl::read_xlsx(str_c(pathdata,"/Sf.xlsx"))
Ts0 <- readxl::read_xlsx(str_c(pathdata,"/Ts0.xlsx"))
VST <- readxl::read_xlsx(str_c(pathdata,"/VST.xlsx"))
SVC <- readxl::read_xlsx(str_c(pathdata,"/SVC.xlsx"))


#Read raw trade data from GTAP----
#IO and trade share data
VMSB <- readxl::read_xlsx(str_c(pathdata2,"/VMSB.xlsx"))#Import by source

VDFB <- readxl::read_xlsx(str_c(pathdata2,"/VDFB.xlsx"))#Domestic IO
VMFB <- readxl::read_xlsx(str_c(pathdata2,"/VMFB.xlsx"))#Import IO

VDGB <- readxl::read_xlsx(str_c(pathdata2,"/VDGB.xlsx"))
VMGB <- readxl::read_xlsx(str_c(pathdata2,"/VMGB.xlsx"))

VDPB <- readxl::read_xlsx(str_c(pathdata2,"/VDPB.xlsx"))
VMPB <- readxl::read_xlsx(str_c(pathdata2,"/VMPB.xlsx"))

VDIB <- readxl::read_xlsx(str_c(pathdata2,"/VDIB.xlsx"))
VMIB <- readxl::read_xlsx(str_c(pathdata2,"/VMIB.xlsx"))

#define the names for sectors, regions, and factors, institutions----
secnam <- unique(QT0$...1)
trdnam <- secnam[52:54]
regnam <- colnames(QT0)[-c(1:2)]
facnam <- unique(F0$...1)
instnam <- c("Hou","Inv","Gov")
factors <- unique(F0$...1)
N <- length(secnam);G <- length(regnam)
GN <- G*N
regsecnam <- str_c(rep(regnam,each = N),rep(secnam, G),sep = "_")

#Clean data one by one-----
#row denote payment, column denotes receive money
#no treatment, Sg,Sf,Sp,Td, 1*G
Sg_new <- as.numeric(Sg0)
Sf_new <- as.numeric(Sf)
Sp_new <- as.numeric(Sp0)
Td_new <- as.numeric(Td0)
rm(Sg0,Sf,Sp0,Td0);gc()

#SVC and VST, 3*G
SVC_new <- array(NA,dim = c(length(trdnam),G),dimnames = list(trdnam,regnam))
VST_new <- SVC_new

SVC_new[,] <- apply(SVC[,-1], 2, as.numeric)
VST_new[,] <- apply(VST[,-1], 2, as.numeric)
rm(SVC,VST);gc()

#Tz0, Xp0, Xg0, Xv0, N*G
Tz_new <- array(NA,dim = c(N,G),dimnames = list(secnam,regnam))
Xp_new <- Tz_new;Xg_new <- Tz_new;Xv_new <- Tz_new
VDPB_new <- Tz_new;VMPB_new <- Tz_new
VDGB_new <- Tz_new;VMGB_new <- Tz_new
VDIB_new <- Tz_new;VMIB_new <- Tz_new

Tz_new[,] <- apply(Tz0[,-1], 2, as.numeric)
Xp_new[,] <- apply(Xp0[,-1], 2, as.numeric)
VDPB_new[,] <- apply(VDPB[,-1], 2, as.numeric)
VMPB_new[,] <- apply(VMPB[,-1], 2, as.numeric)

Xg_new[match(Xg0$...1,secnam),] <- apply(Xg0[,-1], 2, as.numeric)
Xv_new[match(Xv0$...1,secnam),] <- apply(Xv0[,-1], 2, as.numeric)
VDGB_new[match(VDGB$...1,secnam),] <- apply(VDGB[,-1], 2, as.numeric)
VMGB_new[match(VMGB$...1,secnam),] <- apply(VMGB[,-1], 2, as.numeric)
VDIB_new[match(VDIB$...1,secnam),] <- apply(VDIB[,-1], 2, as.numeric)
VMIB_new[match(VMIB$...1,secnam),] <- apply(VMIB[,-1], 2, as.numeric)

Xg_new[is.na(Xg_new)] <- 0
Xv_new[is.na(Xv_new)] <- 0
VDGB_new[is.na(VDGB_new)] <- 0
VMGB_new[is.na(VMGB_new)] <- 0
VDIB_new[is.na(VDIB_new)] <- 0
VMIB_new[is.na(VMIB_new)] <- 0
rm(Tz0, Xp0, Xg0, Xv0,VMGB,VDGB,VMPB,VDPB,VDIB,VMIB);gc()

#F0, FF, Tf0, 4*GN
F_new <- array(NA,dim = c(length(factors),GN),dimnames = list(factors,regsecnam))
FF_new <- F_new;Tf_new <- F_new;

FF %>% pivot_longer(c(-1,-2),names_to = "country", values_to = "value") %>%
  pivot_wider(names_from = c("country","...2")) -> FF_int
FF_new[,match(colnames(FF_int)[-1],colnames(FF_new))] <- apply(FF_int[,-1], 2, as.numeric)

F0 %>% pivot_longer(c(-1,-2),names_to = "country", values_to = "value") %>%
  pivot_wider(names_from = c("country","...2")) -> F0_int
F_new[,match(colnames(F0_int)[-1],colnames(F_new))] <- apply(F0_int[,-1], 2, as.numeric)

Tf0 %>% pivot_longer(c(-1,-2),names_to = "country", values_to = "value") %>%
  pivot_wider(names_from = c("country","...2")) -> Tf0_int
Tf_new[,match(colnames(Tf0_int)[-1],colnames(Tf_new))] <- apply(Tf0_int[,-1], 2, as.numeric)

F_new[is.na(F_new)] <- 0
FF_new[is.na(FF_new)] <- 0
Tf_new[is.na(Tf_new)] <- 0

rm(F0,FF,Tf0,FF_int,F0_int,Tf0_int);gc()
rm(FF_new);gc()#FF and F0 are identical

#X0,VDFB,VMFB ,N*GN
X_new <- array(NA,dim = c(N,GN),dimnames = list(secnam,regsecnam))
VDFB_new <- X_new;VMFB_new <- X_new

X0 %>% pivot_longer(c(-1,-2),names_to = "country", values_to = "value") %>%
  pivot_wider(names_from = c("country","...2")) -> X0_int
X_new[,match(colnames(X0_int)[-1],colnames(X_new))] <- apply(X0_int[,-1], 2, as.numeric)

VDFB %>% pivot_longer(c(-1,-2),names_to = "country", values_to = "value") %>%
  pivot_wider(names_from = c("country","...2")) -> VDFB_int
VDFB_new[,match(colnames(VDFB_int)[-1],colnames(VDFB_new))] <- apply(VDFB_int[,-1], 2, as.numeric)

VMFB %>% pivot_longer(c(-1,-2),names_to = "country", values_to = "value") %>%
  pivot_wider(names_from = c("country","...2")) -> VMFB_int
VMFB_new[,match(colnames(VMFB_int)[-1],colnames(VMFB_new))] <- apply(VMFB_int[,-1], 2, as.numeric)

X_new[is.na(X_new)] <- 0
rm(X0,VDFB,VMFB,X0_int,VDFB_int,VMFB_int);gc()

#QT0,Te0,Tm0, GN*G
QT0$matchindex <- str_c(QT0$...2,QT0$...1,sep = "_")
Te0$matchindex <- str_c(Te0$...2,Te0$...1,sep = "_")
Tm0$matchindex <- str_c(Tm0$...2,Tm0$...1,sep = "_")
QT_new <- array(NA,dim = c(GN,G),dimnames = list(regsecnam,regnam))
Te_new <- QT_new;Tm_new <- QT_new

for (j in 1:G){
  a <- which(colnames(QT0) %in% regnam[j])
  if (sum(a)!=0){
    QT_new[match(QT0$matchindex,regsecnam),j] <- apply(QT0[,a],1,as.numeric)
  }
  a <- which(colnames(Te0) %in% regnam[j])
  if (sum(a)!=0){
    Te_new[match(Te0$matchindex,regsecnam),j] <- apply(Te0[,a],1,as.numeric)
  }
  a <- which(colnames(Tm0) %in% regnam[j])
  if (sum(a)!=0){
    Tm_new[match(Tm0$matchindex,regsecnam),j] <- apply(Tm0[,a],1,as.numeric)
  }
}

QT_new[is.na(QT_new)] <- 0
Te_new[is.na(Te_new)] <- 0
Tm_new[is.na(Tm_new)] <- 0
rm(QT0,Te0,Tm0);gc()

#VMSB, G*GN
VMSB_new <- array(NA,dim = c(G,GN),dimnames = list(regnam,regsecnam))
VMSB %>% pivot_longer(c(-1,-2),names_to = "country", values_to = "value") %>%
  pivot_wider(names_from = c("country","...1")) -> VMSB_int
VMSB_new[,match(colnames(VMSB_int)[-1],colnames(VMSB_new))] <- apply(VMSB_int[,-1], 2, as.numeric)
VMSB_new[is.na(VMSB_new)] <- 0
rm(VMSB);gc()

#Ts0, G*N*N,G
Ts_new <- array(NA,dim = c(G*3,GN),
                dimnames = list(str_c(rep(regnam,each = 3),rep(trdnam,G), sep = "_"),regsecnam))
Ts0 %>% pivot_longer(c(-1,-2,-3),names_to = "country", values_to = "value") %>%
  pivot_wider(names_from = c("country","...2")) -> Ts_int
Ts_new[match(str_c(Ts_int$...3,Ts_int$...1,sep = "_"),rownames(Ts_new)),
       match(colnames(Ts_int)[-c(1,2)],colnames(Ts_new))] <- apply(Ts_int[,-c(1,2)], 2, as.numeric)
Ts_new[is.na(Ts_new)] <- 0
rm(Ts0,Ts_int);gc()


#Calculate Import share, Intermediate and final demand matrix--------
Import_share <- t(t(VMSB_new)/colSums(VMSB_new))

#Intermediate trade
Inter_Trade <- array(0,dim = c(GN,GN),dimnames = list(regsecnam,regsecnam))

for (k in 1:N) {
  print(str_c("begin",k,secnam[k],sep = " "))
  for (i in 1:GN) {
    tar_reg <- substr(regsecnam[i],1,3)
    tar_sec <- secnam[k]
    tar <- str_c(tar_reg,tar_sec,sep = "_")
    a <- which(colnames(Import_share) %in% tar)
    b <- which(substr(rownames(Inter_Trade),5,7) %in% secnam[k])
    Inter_Trade[b,i] <- Import_share[,a]*VMFB_new[k,i]
  }
}

for (i in 1:G) {
  m <- (i-1)*N+1;n <- i*N
  Inter_Trade[m:n,m:n] <- VDFB_new[,m:n]+Inter_Trade[m:n,m:n]#diagonal block
}

#Final goods trade
Final_Trade <- array(0,dim = c(GN,G*3),
                     dimnames = list(regsecnam,str_c(rep(regnam,each= 3),rep(instnam,G),sep = "_")))

for (k in 1:N) {
  print(str_c("begin",k,secnam[k],sep = " "))
  for (i in 1:G) {
    tar_reg <- regnam[i]
    tar_sec <- secnam[k]
    tar <- str_c(tar_reg,tar_sec,sep = "_")
    a <- which(colnames(Import_share) %in% tar)
    b <- which(substr(rownames(Final_Trade),5,7) %in% secnam[k])
    
    p <- (i-1)*3+1;q <- i*3
    
    Final_Trade[b,p:q] <- cbind(Import_share[,a]*VMPB_new[k,i],
                                Import_share[,a]*VMIB_new[k,i],
                                Import_share[,a]*VMGB_new[k,i])
  }
}

for (i in 1:G) {
  m <- (i-1)*N+1;n <- i*N
  p <- (i-1)*3+1;q <- i*3
  Final_Trade[m:n,p:q] <- cbind(VDPB_new[,i],VDIB_new[,i],VDGB_new[,i])+
    Final_Trade[m:n,p:q]#diagonal block
}
dimnames(Final_Trade) <- list(regsecnam,str_c(rep(regnam,each= 3),
                                              rep(instnam,G),sep = "_"))

#Convert VST to 1*GN
VST_vec <- rep(0,GN)
for (i in 1:G) {
  m <- (i-1)*N+52;n <- (i-1)*N+54
  VST_vec[m:n] <- VST_new[,i]
}
rm(VST_new);gc()

rm(X_new,Xg_new,Xp_new,Xv_new);gc()

TotOutput <- rowSums(Inter_Trade)+rowSums(Final_Trade) + VST_vec

#Elements in columns:   Tf, Tm, Te, F,
dim(Tz_new) <- c(GN,1)
ProdTax <- as.vector(Tz_new)
TrdMargin <- colSums(Ts_new)
LabCompen <- colSums(F_new[2:3,])
LandRent <- F_new[1,]
CapReven <- F_new[4,]
FactorTax <- colSums(Tf_new)

TrdTaxSub <-  TotOutput - colSums(Inter_Trade) - 
  (ProdTax + TrdMargin + colSums(F_new) + FactorTax)
#sum(Tm_new)+sum(Te_new)  # equal to TrdTaxSub

TotVA <- TotOutput - colSums(Inter_Trade)

rm(VDPB_new,VMPB_new,VDIB_new,VMIB_new,VDFB_new,VMFB_new,VDGB_new,VMGB_new,
   VMSB_int,VMSB_new);gc()


#Calculate the MRIO elements and also the CO2------
A <- t(t(Inter_Trade)/TotOutput)#Direct requirement coefficient
A[is.na(A)] <- 0
B <- solve(diag(GN)-A)#Leontief inverse matrix

MDF <- read.csv(str_c(pathdata3,"/MDF_2017.csv"))#MT CO2
MDF <- apply(MDF[-66,-c(1,162,163)], 2, as.numeric)
MMF <- read.csv(str_c(pathdata3,"/MMF_2017.csv"))#MT CO2
MMF <- apply(MMF[-66,-c(1,162,163)], 2, as.numeric)

MDP <- read.csv(str_c(pathdata3,"/MDP_2017.csv"))#MT CO2
hhco2Sec <- substr(MDP$MDP[1:5],3,5)
MDP <- apply(MDP[-6,-c(1,162,163)], 2, as.numeric)
MMP <- read.csv(str_c(pathdata3,"/MMP_2017.csv"))#MT CO2
MMP <- apply(MMP[-6,-c(1,162,163)], 2, as.numeric)

FCO2 <- MDF+MMF
dim(FCO2) <- c(GN,1)
FCO2 <- as.vector(FCO2)

PCO2 <- MDP+MMP
rownames(PCO2) <- hhco2Sec;colnames(PCO2) <- regnam
rm(MDF,MMF,MDP,MMP)


print(str_c("Time requirement:",
            round(Sys.time()-t0,2),sep = " "))

#Save as .RData--------

rm(a,b,i,j,k,m,n,p,q,r,tar,tar_reg,tar_sec,t0)

save(list = ls(),
     file = str_c(pathout,"/GTAP_11b_2017_Elements for SAM.Rdata"))

save(Final_Trade,Inter_Trade,A,B,TotVA,TotOutput,
     VST_vec, ProdTax,TrdMargin,LabCompen,LandRent,CapReven,FactorTax,TrdTaxSub,
     PCO2,FCO2,
     regnam,secnam,regsecnam,hhco2Sec,instnam,trdnam,
     file = str_c(pathout,"/GTAP_11b_2017_Elements for MRIO.Rdata"))

