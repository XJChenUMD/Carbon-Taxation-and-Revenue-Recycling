#Module 9: Visualization
#2022-12-25
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu
 
library(RColorBrewer)
library(ggrepel)
library(pheatmap)
library(ggplotify)
library(cowplot)
library(ggpubr)

load(str_c(pathout5,"/Results summary.Rdata"))

#Fig1: Tax scenarios setting.
#===================
#a. Tax level under CBDR_P and CBDR_P#-----------
label <- Reg
# label[2] <- "SSA";label[5] <- "LAC";label[7] <- "APD"; label[10] <- "EA & DP"
label[2] <- "Sub-Saharan\nAfrica";label[5] <- "Latin America\nand Caribbean";
label[7] <- "Asia-Pacific\nDeveloped"; label[10] <- "East Asia and\nDeveloping Pacific"
CBDR <- as.data.frame(CBDR)
CBDR$Label <- label
CBDR %>% pivot_longer(-c(Region,Label),
                      names_to = "Type",
                      values_to = "TaxLevel") -> X

order <- CBDR$Label[order(as.numeric(CBDR$Tax_P))]

Fig.1a <- X %>% ggplot()+
  geom_col(aes(Label,round(as.numeric(TaxLevel)),fill = Type), position = "dodge")+
  labs(x = "", y = str_c("Tax level (US$/tCO2)"))+
  scale_x_discrete(limit = order)+
  geom_hline(yintercept = 50, color = "green")+
  scale_fill_manual(name =  'Principles',
                    limits = c("Tax_P","Tax_C"),
                    labels = c("Nation-based production tax",
                               "Nation-based consumption tax"),
                    values = brewer.pal(3,"Dark2")[2:3])+
  coord_flip()+
  theme_bw(base_size = 16)+
  theme(legend.text = element_text(size = 12),legend.title = element_text(size = 12, face = "bold"),
        legend.position = c(0.7,0.12),
        # legend.key.height = unit(0.7, 'cm'),
        legend.background = element_blank())
#-----------

#b. Tax level by sectors under luxury.#-----------
Luxury <- as.data.frame(Luxury)
Sec_label <- rep(NA,length(Luxury$Secnam))
Sec_label[match(c("oil","coa","wht","gro","ocr","c_b","pdr",
                  "ros","afs","otn","ins","mvh","atp"),
                Luxury$Secnam)] <- c("Oil","Coal","Wheat","Cereal grains nec","Crops nec","Sugar cane, sugar beet","Paddy rice",
                                     "Recreational and other services","Accommodation, Food and service activities","Transport equipment nec","Insurance",
                                     "Motor vehicles and parts","Air transport")
Fig.1b <- Luxury %>% ggplot()+
  geom_point(aes(as.numeric(Elas),as.numeric(TaxRate)),
             size = 4, alpha = 0.8, color = brewer.pal(3,"Dark2")[3])+
  geom_text_repel(aes(as.numeric(Elas),as.numeric(TaxRate),label = Sec_label),
                   size = 4, fontface= "italic",
                  arrow = arrow(length=unit(0.01, "npc")),force = 6, max.iter = 3e3,
                  segment.colour = "grey50",nudge_x = 0.25)+
  geom_vline(xintercept = 1,color = "green")+
  annotate("text", x = 0.5, y = 54, label = "above average tax",size = 5,fontface = "bold")+
  annotate("text", x = 0.5, y = 46, label = "below average tax",size = 5,fontface = "bold")+
  geom_hline(yintercept = 50,color = "green")+
  labs(x = "Income elasticity", y = str_c("Tax level (US$/tCO2)"))+
  theme_bw(base_size = 16)
#-----------

#c. Tax revenue#-----------
Revenue <- as.data.frame(Revenue[,1:6])
Revenue$Label <- label
Revenue %>% pivot_longer(-Label,
                      names_to = "Type",
                      values_to = "Revenue") -> X

RevenuetoGDP <- as.data.frame(RevenuetoGDP[,1:6])
RevenuetoGDP$Label <- label
RevenuetoGDP %>% pivot_longer(-Label,
                         names_to = "Type",
                         values_to = "RevenueShare") -> Y

X$RevenueShare <- Y$RevenueShare

order.tax <- c("Universal_P","CBDR_P","Universal_C","CBDR_C","Luxury","Luxury&CBDR")
order.taxname <- c("Global uniform\nproduction tax","Nation-based\nproduction tax",
                "Global uniform\nconsumption tax","Nation-based\nconsumption tax",
                "Luxury\nconsumption tax","Luxury & nation-based\nconsumption tax")
  
Fig.1c <- X %>% ggplot(aes(Type,Label,fill=Revenue/10^3))+
  geom_tile(color="gray",size=0.1)+
  geom_label(aes(Type,Label,
                 label= str_c(round(Revenue/10^3),", ",round(RevenueShare*100),"%")),
             size = 3, fontface= "bold")+
  scale_y_discrete(limit = order)+
  scale_x_discrete(limit = order.tax,labels = order.taxname)+
  labs(x = "Tax scenarios", y = "")+
  scale_fill_gradient(name="Revenue (Billion$)", 
                       low=brewer.pal(3,"Dark2")[3],high=brewer.pal(3,"Dark2")[2])+
  theme_bw(base_size = 16)+
  theme(legend.position = "bottom",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1))
#-----------

#d. tax by decile#-----------
# LABEL <- c("Sub-Saharan Africa" = "SSA","North Africa" = "North Africa","China" ="China",
#   "Eurasia"="Eurasia","Europe" ="Europe" ,"India" ="India" , "Latin America and Caribbean"= "LAC",
#   "Middle East" = "Middle East", "East Asia and Developing Pacific"="EA & DP",
#   "Southern Asia" ="Southern Asia","Asia-Pacific Developed" = "APD","United States" ="United States")

LABEL <- c("Sub-Saharan Africa" = "Sub-Saharan\nAfrica","North Africa" = "North Africa","China" ="China",
           "Eurasia"="Eurasia","Europe" ="Europe" ,"India" ="India" , "Latin America and Caribbean"= "Latin America\nand Caribbean",
           "Middle East" = "Middle East", "East Asia and Developing Pacific"="East Asia and\nDeveloping Pacific",
           "Southern Asia" ="Southern Asia","Asia-Pacific Developed" = "Asia-Pacific\nDeveloped","United States" ="United States")

Name_Tax <- dimnames(Cost_perEXP_decile_agg)[[3]]

Fig.1d <- Cost_perEXP_All %>% 
  mutate(Region = factor(Region, levels = rev(CBDR$Region[order(as.numeric(CBDR$Tax_P))]))) %>%
  ggplot()+
  geom_point(aes(Group, Universal_P, color = brewer.pal(5,"Dark2")[1]), stat = "identity", alpha=0.7, size=1.7)+
  geom_point(aes(Group, Luxury, color = brewer.pal(5,"Dark2")[2]), stat = "identity", alpha=0.7, size=1.7)+
  geom_point(aes(Group, CBDR_P, color = brewer.pal(5,"Dark2")[3]), stat = "identity", alpha=0.7, size=1.7)+
  geom_point(aes(Group, CBDR_C, color = brewer.pal(5,"Dark2")[4]), stat = "identity", alpha=0.7, size=1.7)+
  geom_point(aes(Group, `Luxury&CBDR_C`, color = brewer.pal(8,"Dark2")[8]), stat = "identity", alpha=0.7, size=1.7)+
  facet_wrap(~Region, ncol = 3, labeller = as_labeller(LABEL))+
  labs(x = "Income decile", y = "Tax/Expenditure")+
  scale_x_discrete(limits = factor(str_c("G", seq(10,100,10))),
                   breaks = str_c("G", seq(10,100,10)),
                   labels = str_c(seq(1,10,1)))+
  scale_y_continuous(limits = c(0,.09),breaks = seq(0,.09,.02),labels = str_c(seq(0,9,2),"%"))+
  scale_color_manual(name = "",
                     limits=c(brewer.pal(5,"Dark2")[1],brewer.pal(5,"Dark2")[3],
                              brewer.pal(5,"Dark2")[4],brewer.pal(5,"Dark2")[2],
                              brewer.pal(8,"Dark2")[8]),
                     values = c(brewer.pal(5,"Dark2")[1],brewer.pal(5,"Dark2")[3],
                                brewer.pal(5,"Dark2")[4],brewer.pal(5,"Dark2")[2],
                                brewer.pal(8,"Dark2")[8]),
                     labels = c("Global uniform production/consumption tax",
                                "Nation-based production tax","Nation-based consumption tax",
                                "Luxury consumption tax","Luxury & nation-based consumption tax"))+
  theme_bw(base_size = 16)+
  guides(color = guide_legend(nrow = 3,byrow = F))+
  theme(legend.position = "bottom",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 10),legend.background = element_blank(),
        legend.spacing.y = unit(10,"pt"))
#-----------

#Save fig#-----------
ggdraw()+ 
  draw_plot(Fig.1a, x=0, y=0.65, width=0.49, height = 0.35)+
  draw_plot(Fig.1b, x=0.51, y=0.65, width = 0.48, height = 0.35)+ 
  draw_plot(Fig.1c, x=0, y=0, width = 0.49, height = 0.63)+ 
  draw_plot(Fig.1d, x=0.51, y=0, width = 0.48, height = 0.63)+ 
  draw_plot_label(label = c("a)", "b)", "c)", "d)"), size = 15, 
                  x=c(0, 0.51, 0, 0.51), y=c(1, 1, 0.63, 0.63))
ggsave(str_c(pathout5,"/", "Fig1_Tax scenarios.jpg"),
       width = 13,height = 15,dpi = 800)
#-----------
#===================


#Fig2: Recycling scenarios setting.
#===================
#Coverage of social assistance #-----------
Name_SP <- dimnames(Cover_decile_agg)[[3]]

Fig.2a <- Cover_All %>% 
  mutate(Region = factor(Region, levels = rev(CBDR$Region[order(as.numeric(CBDR$Tax_P))]))) %>% 
  ggplot()+
  geom_point(aes(Group, Universal, color = brewer.pal(4,"Dark2")[1]), stat = "identity", alpha=0.7, size=2)+
  geom_point(aes(Group, SP_current, color = brewer.pal(4,"Dark2")[2]), stat = "identity", alpha=0.7, size=2)+
  geom_point(aes(Group, SP_covid, color = brewer.pal(4,"Dark2")[3]), stat = "identity", alpha=0.7, size=2)+
  geom_point(aes(Group, PMT, color = brewer.pal(4,"Dark2")[4]), stat = "identity", alpha=0.7, size=2)+
  facet_wrap(~Region, ncol = 4, labeller = as_labeller(LABEL))+
  labs(x = "Income decile", y = "Coverage of social protection")+
  scale_x_discrete(limits = factor(str_c("G", seq(10,100,10))),
                   breaks = str_c("G", seq(10,100,10)),
                   labels = str_c(seq(1,10,1)))+
  scale_y_continuous(limits = c(0,105),breaks = seq(0,100,20),labels = str_c(seq(0,100,20),"%"))+
  scale_color_manual(name = "",
                     breaks = brewer.pal(4,"Dark2"),values = brewer.pal(4,"Dark2"),
                     labels = c("Universal","Current social assistance",
                                "Social assistance during COVID","Proxy Means Test (PMT)"))+
  theme_bw(base_size = 16)+
  theme(legend.position = "bottom",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank())+
  guides(color = guide_legend(nrow = 1,byrow = T))
#-----------

#Climate fund #-----------
Reg_Fund.Share <- Reg_Fund/10^5
Reg_Fund <- as.data.frame(Reg_Fund)
Reg_Fund$Label <- label
Reg_Fund %>% pivot_longer(-Label,
                         names_to = "Scenarios",
                         values_to = "Fund") -> X

order.gf <- c("His.PoverPop","His.Pop","His.PoverGap","Curren.PoverPop","Curren.Pop","Curren.PoverGap")
order.gfname <- c("Historical emissions\n& Poverty headcount",
                  "Historical emissions\n& Population",
                  "Historical emissions\n& Poverty gap",
                  "Current emissions\n& Poverty headcount",
                  "Current emissions\n& Population",
                  "Current emissions\n& Poverty gap")

Fig.2b <- X %>% ggplot(aes(Label,Scenarios,fill=Fund/10^3))+
  geom_tile(color="gray",size=0.1)+
  geom_label(aes(Label,Scenarios,
                 label= round(Fund/10^3,2)),
             size = 3, fontface= "bold")+
  labs(y = "Principles of global\nclimate fund allocation",x = "")+
  scale_x_discrete(limit = order)+
  scale_y_discrete(limit = order.gf,labels = order.gfname)+
  scale_fill_gradient2(name="Fund (Billion$)", 
                       high=brewer.pal(3,"Dark2")[1], low=brewer.pal(4,"Dark2")[4],
                       mid = "white",midpoint = 0)+
  theme_bw(base_size = 16)+
  theme(legend.position = "bottom",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1))
#-----------

#Save fig#-----------
ggdraw()+ 
  draw_plot(Fig.2a, x=0, y=0.47, width=1, height = 0.53)+
  draw_plot(Fig.2b, x=0, y=0, width = 1, height = 0.45)+
  draw_plot_label(label = c("a)", "b)"), size = 15, 
                  x=c(0, 0), y=c(1, 0.45))
ggsave(str_c(pathout5,"/", "Fig2_Recycling scenarios.jpg"),
       width = 10,height = 12.5,dpi = 800)
#-----------
#===================


#Fig3: TAX+SA scenarios outcome.
#===================
#add the 30% results
Global_PovInq_SATAX_30 %>% filter(SA == "Universal") -> a
a$SA <- "Universal (30%)"
Global_PovInq_SATAX <- rbind(a,Global_PovInq_SATAX)

#SA + TAX outcome#-----------
order.tax <- c("Universal_P","CBDR_P","Universal_C","CBDR_C","Luxury","Luxury&CBDR_C")
order.sp <- c("Null","Universal (30%)","Universal","SP_current","SP_covid","PMT")
order.spname <- c("No social\nassistance(0%)","Universal(30%)","Universal(100%)",
                  "Current social\nassistance(100%)",
                  "Social assistance\nduring COVID(100%)","Proxy Means\nTest (PMT)(100%)")

Fig.3a <- Global_PovInq_SATAX %>% filter(Tax %in% Name_Tax[1:6]) %>% 
  ggplot(aes(SA,Tax,fill=PPop_ipl_chg/10^6))+
  geom_tile(color="gray",size=0.1)+
  geom_label(aes(SA,Tax,
                 label= round(PPop_ipl_chg/10^6)),
             size = 4, fontface= "bold")+
  labs(y = "Tax scenarios", x = "Social assistance scenarios")+
  scale_x_discrete(limit = order.sp,labels = order.spname)+
  scale_y_discrete(limit = order.tax,labels = order.taxname)+
  scale_fill_gradient2(name="Changes in global \nextreme poverty(million)", 
                       high=brewer.pal(3,"Dark2")[1],
                       mid = "white", low=brewer.pal(4,"Dark2")[4],
                       midpoint = 0,
                       limits = c(-220,100),
                       breaks = seq(-200,100,100),labels = seq(-200,100,100))+
  theme_bw(base_size = 16)+
  theme(legend.position = "bottom",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1))
  
Fig.3b <- Global_PovInq_SATAX %>% filter(Tax %in% Name_Tax[1:6]) %>% 
  mutate(Gini_within_chg = (Gini_within_tax-Gini_within_ori)/Gini_within_ori) %>% 
  ggplot(aes(SA,Tax,fill=Gini_within_chg*100))+
  geom_tile(color="gray",size=0.1)+
  geom_label(aes(SA,Tax,
                 label= str_c(round(Gini_within_chg*100,2),"%")),
             size = 4, fontface= "bold")+
  labs(y = "Tax scenarios", x = "Social assistance scenarios")+
  scale_x_discrete(limit = order.sp,labels = order.spname)+
  scale_y_discrete(limit = order.tax,labels = order.taxname)+
  scale_fill_gradient2(name="Changes in \nlocal Gini", 
                       high=brewer.pal(3,"Dark2")[1],
                       mid = "white", low=brewer.pal(4,"Dark2")[4],
                       midpoint = 0,
                       limits = c(-10,1),
                       breaks = seq(-9,1,2),labels = str_c(seq(-9,1,2),"%"))+
  theme_bw(base_size = 16)+
  theme(legend.position = "bottom",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1))

Fig.3c <- Global_PovInq_SATAX %>% filter(Tax %in% Name_Tax[1:6]) %>% 
  mutate(Gini_btw_chg = (Gini_btw_tax-Gini_btw_ori)/Gini_btw_ori) %>% 
  ggplot(aes(SA,Tax,fill=Gini_btw_chg*100))+
  geom_tile(color="gray",size=0.1)+
  geom_label(aes(SA,Tax,
                 label= str_c(round(Gini_btw_chg*100,2),"%")),
             size = 4, fontface= "bold")+
  labs(y = "Tax scenarios", x = "Social assistance scenarios")+
  scale_x_discrete(limit = order.sp,labels = order.spname)+
  scale_y_discrete(limit = order.tax,labels = order.taxname)+
  scale_fill_gradient2(name="Changes in \ninternational Gini  ", 
                       high=brewer.pal(3,"Dark2")[1],
                       mid = "white", low=brewer.pal(4,"Dark2")[4],
                       midpoint = 0,
                       limits = c(-0.85,0.5),
                       breaks = seq(-0.75,0.5,0.5),labels = str_c(seq(-0.75,0.5,0.5),"%"))+
  theme_bw(base_size = 16)+
  theme(legend.position = "bottom",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1))

Fig.3d <- Global_PovInq_SATAX %>% filter(Tax %in% Name_Tax[1:6]) %>% 
  ggplot(aes(SA,Tax,fill=CO2_chg))+
  geom_tile(color="gray",size=0.1)+
  geom_label(aes(SA,Tax,
                 label= round(CO2_chg,0)),
             size = 4, fontface= "bold")+
  labs(y = "Tax scenarios", x = "Social assistance scenarios")+
  scale_x_discrete(limit = order.sp,labels = order.spname)+
  scale_y_discrete(limit = order.tax,labels = order.taxname)+
  scale_fill_gradient2(name="Changes in \nCO2 emissions (MT)  ", 
                       high=brewer.pal(3,"Dark2")[1],
                       mid = "white", low=brewer.pal(4,"Dark2")[4],
                       midpoint = 0,
                       limits = c(-810,100))+
  theme_bw(base_size = 16)+
  theme(legend.position = "bottom",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1))
#-----------

#Save fig#-----------
# ggdraw()+ 
#   draw_plot(Fig.3a, x=0, y=0, width=0.33, height = 1)+
#   draw_plot(Fig.3b, x=0.33, y=0, width = 0.33, height = 1)+
#   draw_plot(Fig.3c, x=0.66, y=0, width = 0.33, height = 1)+
#   draw_plot_label(label = c("a)", "b)", "c)"), size = 15, 
#                   x=c(0, 0.33, 0.66), y=c(1, 1, 1))
# ggsave(str_c(pathout5,"/", "Fig3_SA+TAX outcome.jpg"),
#        width = 18,height = 6,dpi = 800)

ggdraw()+ 
  draw_plot(Fig.3a, x=0, y=0.5, width=0.48, height = .48)+
  draw_plot(Fig.3d, x=0.5, y=0.5, width = 0.48, height = .48)+
  draw_plot(Fig.3b, x=0, y=0, width = 0.48, height = .48)+
  draw_plot(Fig.3c, x=0.5, y=0, width = 0.48, height = .48)+
  draw_plot_label(label = c("a)", "b)", "c)", "d)"), size = 15, 
                  x=c(0, 0.5, 0, 0.5), y=c(1, 1,0.5, 0.5))
ggsave(str_c(pathout5,"/", "Fig3_SA+TAX outcome_V2.jpg"),
       width = 14,height = 12,dpi = 800)
#-----------
#===================

#Fig4: TAX+SA+GF scenarios outcome.
#===================
#a,b,c, SA + TAX + GF outcome#-----------
order.sp <- rev(c("SP_covid","PMT"))
order.spname <- rev(c("Luxury consumption tax\n+ Social assistance\nduring COVID",
                      "Luxury consumption tax\n+ Proxy Means\nTest (PMT)"))

Fig.4a <- Global_PovInq_SATAX_GF %>% 
  ggplot(aes(GF,SA,fill=PPop_ipl_chg/10^6))+
  geom_tile(color="gray",size=0.1)+
  geom_label(aes(GF,SA,
                 label= round(PPop_ipl_chg/10^6)),
             size = 3, fontface= "bold")+
  labs(y = "Tax and social\nassistance scenarios", x = "Global climate fund scenarios")+
  scale_y_discrete(limit = order.sp,labels = order.spname)+
  scale_x_discrete(limit = order.gf,labels = order.gfname)+
  scale_fill_gradient2(name="Changes in global \nextreme poverty (million)", 
                       low=brewer.pal(3,"Dark2")[1],
                       mid = "white", high=brewer.pal(4,"Dark2")[4],
                       midpoint = 0,
                       limits = c(-370,-100),
                       breaks = seq(-350,-100,100),labels = seq(-350,-100,100))+
  theme_bw(base_size = 16)+
  theme(legend.position = "bottom",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1))

Fig.4b <- Global_PovInq_SATAX_GF %>% 
  mutate(Gini_within_chg = (Gini_within_tax-Gini_within_ori)/Gini_within_ori) %>% 
  ggplot(aes(GF,SA,fill=Gini_within_chg*100))+
  geom_tile(color="gray",size=0.1)+
  geom_label(aes(GF,SA,
                 label= str_c(round(Gini_within_chg*100,2),"%")),
             size = 3, fontface= "bold")+
  labs(y = "Tax and social\nassistance scenarios", x = "Global climate fund scenarios")+
  scale_y_discrete(limit = order.sp,labels = order.spname)+
  scale_x_discrete(limit = order.gf,labels = order.gfname)+
  scale_fill_gradient2(name="Changes in \nlocal Gini", 
                       low=brewer.pal(3,"Dark2")[1],
                       mid = "white", high=brewer.pal(4,"Dark2")[4],
                       midpoint = 0,
                       limits = c(-11,1),
                       breaks = seq(-10,0,2),labels = str_c(seq(-10,0,2),"%"))+
  theme_bw(base_size = 16)+
  theme(legend.position = "bottom",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1))

Fig.4c <- Global_PovInq_SATAX_GF %>% 
  mutate(Gini_btw_chg = (Gini_btw_tax-Gini_btw_ori)/Gini_btw_ori) %>% 
  ggplot(aes(GF,SA,fill=Gini_btw_chg*100))+
  geom_tile(color="gray",size=0.1)+
  geom_label(aes(GF,SA,
                 label= str_c(round(Gini_btw_chg*100,2),"%")),
             size = 3, fontface= "bold")+
  labs(y = "Tax and social\nassistance scenarios", x = "Global climate fund scenarios")+
  scale_y_discrete(limit = order.sp,labels = order.spname)+
  scale_x_discrete(limit = order.gf,labels = order.gfname)+
  scale_fill_gradient2(name="Changes in \ninternational Gini  ", 
                       low=brewer.pal(3,"Dark2")[1],
                       mid = "white", high=brewer.pal(4,"Dark2")[4],
                       midpoint = 0,
                       limits = c(-0.9,-0.2),
                       breaks = seq(-0.8,-0.2,0.2),labels = str_c(seq(-0.8,-0.2,0.2),"%"))+
  theme_bw(base_size = 16)+
  theme(legend.position = "bottom",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1))
#-----------

#d.e,z,f regional poverty and ineq outcome#-----------
National_Poverty <- as.data.frame(cbind(Reg_Outcome[,1,1],Reg_Outcome[,1,2],Reg_Outcome[,1,3],Reg_Outcome[,1,4]))
colnames(National_Poverty) <- dimnames(Reg_Outcome)[[3]]
National_Poverty$Label <- label
order <- rev(National_Poverty$Label[order(as.numeric(National_Poverty$Null))])

order.spname <- rev(c("Luxury consumption tax\n+ Social assistance\nduring COVID",
                      "Luxury consumption tax\n+ Proxy Means\nTest (PMT)"))

Fig.4d <- National_Poverty %>% ggplot()+
  geom_bar(aes(Label,Null/10^6),stat = "identity",alpha = 0.3)+
  geom_point(aes(Label,Tax/10^6,color = brewer.pal(4,"Dark2")[1]),alpha = 0.8, size = 12, shape = 95)+
  geom_point(aes(Label,TaxSA/10^6,color = brewer.pal(4,"Dark2")[2]),alpha = 0.8, size = 12, shape = 95)+
  geom_point(aes(Label,TaxSAGF/10^6,color = brewer.pal(4,"Dark2")[3]),alpha = 0.8, size = 12, shape = 95)+
  scale_x_discrete(limits = order, breaks = order, labels = order)+
  labs(x = "", y = str_c("National poverty headcount (million)"))+
  scale_y_continuous(breaks = seq(0,450,50))+
  theme_test(base_size = 16)+
  scale_color_manual(name = "",
                     breaks = c("black",brewer.pal(3,"Dark2")),
                     values = c("black",brewer.pal(3,"Dark2")),
                     labels = c("Baseline","Luxury consumption tax without recycling",
                                "Luxury consumption tax+Social assistance during COVID",
                                "Luxury consumption tax+Social assistance during COVID+Current emissions & Poverty headcount"))+
  theme(legend.text = element_text(size = 12),legend.title = element_text(size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 1,hjust=1))+
  guides(color = guide_legend(nrow = 3,byrow = T))


Inter_Poverty <- as.data.frame(cbind(Reg_Outcome[,3,1],Reg_Outcome[,3,2],Reg_Outcome[,3,3],Reg_Outcome[,3,4]))
colnames(Inter_Poverty) <- dimnames(Reg_Outcome)[[3]]
Inter_Poverty$Label <- label

Fig.4e <-Inter_Poverty %>% ggplot()+
  geom_bar(aes(Label,Null/10^6),stat = "identity",alpha = 0.3)+
  geom_point(aes(Label,Tax/10^6,color = brewer.pal(4,"Dark2")[1]),alpha = 0.8, size = 12, shape = 95)+
  geom_point(aes(Label,TaxSA/10^6,color = brewer.pal(4,"Dark2")[2]),alpha = 0.8, size = 12, shape = 95)+
  geom_point(aes(Label,TaxSAGF/10^6,color = brewer.pal(4,"Dark2")[3]),alpha = 0.8, size = 12, shape = 95)+
  scale_x_discrete(limits = order, breaks = order, labels = order)+
  labs(x = "", y = str_c("Extreme poverty headcount (million)"))+
  scale_y_continuous(breaks = seq(0,450,50))+
  theme_test(base_size = 16)+
  scale_color_manual(name = "",
                     breaks = c("black",brewer.pal(3,"Dark2")),
                     values = c("black",brewer.pal(3,"Dark2")),
                     labels = c("Baseline","Luxury consumption tax without recycling",
                                "Luxury consumption tax+Social assistance during COVID",
                                "Luxury consumption tax+Social assistance during COVID+Current emissions & Poverty headcount"))+
  theme(legend.text = element_text(size = 12),legend.title = element_text(size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 1,hjust=1))+
  guides(color = guide_legend(nrow = 3,byrow = T))



#combine npl and ipl
National_Poverty$Line <- "National poverty lines"
Inter_Poverty$Line <- "Extreme (international) poverty line"
Poverty <- rbind(National_Poverty,Inter_Poverty)
Poverty$Line <- factor(Poverty$Line, levels = c("National poverty lines","Extreme (international) poverty line")) 

Fig.4z <- Poverty %>% ggplot()+
  geom_bar(aes(Label,Null/10^6),stat = "identity",alpha = 0.1, fill = "yellow", color = "gray")+
  geom_point(aes(Label,Tax/10^6,color = brewer.pal(4,"Dark2")[1]),alpha = 0.8, size = 12, shape = 95)+
  geom_point(aes(Label,TaxSA/10^6,color = brewer.pal(4,"Dark2")[2]),alpha = 0.8, size = 12, shape = 95)+
  geom_point(aes(Label,TaxSAGF/10^6,color = brewer.pal(4,"Dark2")[3]),alpha = 0.8, size = 12, shape = 95)+
  scale_x_discrete(limits = order, breaks = order, labels = order)+
  labs(x = "", y = str_c("Poverty headcount (million)"))+
  facet_wrap(~Line,ncol = 1)+
  scale_y_continuous(breaks = seq(0,450,50))+
  theme_test(base_size = 16)+
  scale_color_manual(name = "",
                     breaks = c("black",brewer.pal(3,"Dark2")),
                     values = c("black",brewer.pal(3,"Dark2")),
                     labels = c("Baseline","Luxury consumption tax without recycling",
                                "Luxury consumption tax+Social assistance during COVID",
                                "Luxury consumption tax+Social assistance during COVID+Current emissions & Poverty headcount"))+
  theme(legend.text = element_text(size = 12),legend.title = element_text(size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = .5,hjust=1))+
  guides(color = guide_legend(nrow = 3,byrow = T))



National_Gini <- as.data.frame(cbind(Reg_Outcome[,5,1],Reg_Outcome[,5,2],Reg_Outcome[,5,3],Reg_Outcome[,5,4]))
colnames(National_Gini) <- dimnames(Reg_Outcome)[[3]]
National_Gini$Label <- label

Fig.4f <-National_Gini %>% ggplot()+
  geom_bar(aes(Label,Null),stat = "identity",alpha = 0.1, fill = "yellow", color = "gray")+
  geom_point(aes(Label,Tax,color = brewer.pal(4,"Dark2")[1]),alpha = 0.8, size = 12, shape = 95)+
  geom_point(aes(Label,TaxSA,color = brewer.pal(4,"Dark2")[2]),alpha = 0.8, size = 12, shape = 95)+
  geom_point(aes(Label,TaxSAGF,color = brewer.pal(4,"Dark2")[3]),alpha = 0.8, size = 12, shape = 95)+
  scale_x_discrete(limits = order, breaks = order, labels = order)+
  labs(x = "", y = str_c("Local Gini coefficent"))+
  theme_test(base_size = 16)+
  scale_color_manual(name = "",
                     breaks = c("black",brewer.pal(3,"Dark2")),
                     values = c("black",brewer.pal(3,"Dark2")),
                     labels = c("Baseline","Luxury consumption tax without recycling",
                                "Luxury consumption tax+Social assistance during COVID",
                                "Luxury consumption tax+Social assistance during COVID+Current emissions & Poverty headcount"))+
  theme(legend.text = element_text(size = 12),legend.title = element_text(size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = .5,hjust=1))+
  guides(color = guide_legend(nrow = 3,byrow = T))
#-----------

#g,h various tax and fund level#-----------
Poverty_taxlevel <- as.data.frame(Poverty_taxlevel)
Poverty_taxlevel$Ipl_chg <- (Poverty_taxlevel$PPop_ipl_tax-Poverty_taxlevel$PPop_ipl_ori)/
  Poverty_taxlevel$PPop_ipl_ori

Fig.4g <-Poverty_taxlevel %>% ggplot()+
  geom_smooth(aes(TaxLevel,Ipl_chg))+
  labs(x = "Tax level (US$/tCO2)", y = str_c("Proportional changes in extreme poverty"))+
  scale_y_continuous(limits = c(-0.6,0),breaks = seq(-0.6,0,0.1),labels = str_c(seq(-60,0,10),"%"))+
  theme_test(base_size = 16)

Poverty_GF <- as.data.frame(Poverty_GF)
Poverty_GF$Ipl_chg <- (Poverty_GF$PPop_ipl_tax-Poverty_GF$PPop_ipl_ori)/
  Poverty_GF$PPop_ipl_ori

Fig.4h <- Poverty_GF %>% ggplot()+
  geom_smooth(aes(TaxLevel,Ipl_chg), color = "black",size=1)+
  geom_vline(xintercept = 100, color = "green",linetype = "dashed",size=1)+
  geom_hline(yintercept = -0.56, color = "green",linetype = "dashed",size=1)+
  labs(x = "Fund size (billion$)", y = str_c("Proportional changes \n in global extreme poverty"))+
  scale_y_continuous(limits = c(-0.58,0),breaks = c(-0.6,-0.56,seq(-0.5,0,0.1)),
                     labels = str_c(c(-60,-56,seq(-50,0,10)),"%"))+
  scale_x_continuous(limits = c(0,1000),breaks = c(100,seq(0,1000,200)))+
  annotate("text",x = 180, y = -0.32, label = "marginal poverty reduction > 0", fontface = "bold", angle = -55)+
  annotate("text",x = 750, y = -0.5, label = "marginal poverty reduction = 0", fontface = "bold")+
  theme_test(base_size = 16)
#-----------

#Save fig#-----------
Fig.4y <- ggarrange(Fig.4z,Fig.4f, common.legend = T,legend = "bottom")
Fig.4y
ggsave(str_c(pathout5,"/", "Fig4_regional outcome under best policy.jpg"),
       width = 11,height = 8,dpi = 800)

Fig.4h
ggsave(str_c(pathout5,"/", "Fig5_scale international fund.jpg"),
       width = 6,height = 5,dpi = 800)


write.csv(Global_PovInq_SATAX_GF,file = str_c(pathout5,"/", "Table3_effect of international fund.csv"))

ggdraw()+ 
  draw_plot(Fig.4a, x=0, y=0.64, width=0.33, height = 0.36)+
  draw_plot(Fig.4b, x=0.33, y=0.64, width = 0.33, height = 0.36)+
  draw_plot(Fig.4c, x=0.66, y=0.64, width = 0.33, height = 0.36)+
  draw_plot(Fig.4y, x=0, y=0, width=0.66, height = 0.63)+
  draw_plot(Fig.4h, x=0.66, y=0.15, width = 0.33, height = 0.48)+
  draw_plot_label(label = c("a)", "b)", "c)","d)", "e)"), size = 15, 
                  x=c(0, 0.33, 0.66,0, 0.66), 
                  y=c(1, 1, 1,0.63, 0.63))
ggsave(str_c(pathout5,"/", "Fig4b_SA+TAX+GF outcome.jpg"),
       width = 18,height = 14,dpi = 800)
#-----------
#===================
