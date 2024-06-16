#Module 9: Results summary and visualization
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu



Recy <- c(0.3,0.8,1)
Recynam <- c("30percent","80percent","100percent")
z=2#for 80percent

load(str_c(pathout5,"/Results summary",Recynam[z],".Rdata"))

order.tax <- c("Universal_P","CBDR_P","Universal_C","CBDR_C","Luxury","Luxury&CBDR_C")
order.taxname <- c("(T1) Global uniform\nproduction tax","(T2) Nation-differentiated\nproduction tax",
                   "(T3) Global uniform\nconsumption tax","(T4) Nation-differentiated\nconsumption tax",
                   "(T5) Luxury\nconsumption tax","(T6) Luxury & nation-\ndifferentiated consumption tax")
order.sp <- c("Null","Cash","SP_current","SP_covid","Universal","PMT")
order.spname <- c("No revenue\nrecycling",
                  "(S1) Cash-based programs", "(S2) Current social\nassistance",
                  "(S3) Social assistance\nCOVID-19 expansion","(S4) Universal dividend"
                  ,"(S5) Proxy Means\nTest (PMT)")


#Figure: Uneven burden by deciles under different tax scenarios.
#===========================
#--------------
LABEL <- c("Sub-Saharan Africa" = "Sub-Saharan\nAfrica","North Africa" = "North Africa","China" ="China",
           "Eurasia"="Eurasia","Europe" ="Europe" ,"India" ="India" , "Latin America and Caribbean"= "Latin America\nand Caribbean",
           "Middle East" = "Middle East", "East Asia and Developing Pacific"="East Asia and\nDeveloping Pacific",
           "Southern Asia" ="Southern Asia","Asia-Pacific Developed" = "Asia-Pacific\nDeveloped","United States" ="United States")

Reg_dc <- c("Europe","Eurasia","Middle East", "Asia-Pacific Developed","East Asia and Developing Pacific","United States")

Reg_ldc <- c("Sub-Saharan Africa","North Africa","China","India", "Latin America and Caribbean","Southern Asia")
#--------------
#================


#Figure: Tax level required for climate goals; Poverty, ineq outcome under domestic policy mix
#================
# 1.5 degree------------
Fig.1a  <- GlobalOutcome_dome  %>% filter(Goal %in% "1.5degree") %>% 
  filter(Vari %in% c("NPLchg","IPLchg")) %>% 
  mutate(Vari = factor(Vari, levels = c("IPLchg","NPLchg"))) %>% 
  ggplot()+
  facet_wrap(.~Vari,nrow = 1, 
             labeller = as_labeller(c(IPLchg = "Extreme (international) poverty line\n(baseline poverty headcount: 750 million)",
                                      NPLchg = "National poverty lines\n(baseline poverty headcount: 1445 million)")))+
  geom_tile(aes(SP, Tax, fill = PovertyRateLabel))+
  geom_label(aes(SP, Tax, 
                 label= str_c(format(round(PovertyRateLabel,2),nsmall =2),"%")),
             size = 2.7, fontface= "bold")+
  labs(y = "Tax scenarios", x = "Domestic revenue recycling scenarios")+
  scale_x_discrete(limit = order.sp,labels = order.spname)+
  scale_y_discrete(limit = rev(order.tax),labels = rev(order.taxname))+
  scale_fill_gradient2(name="Changes in poverty headcount under a carbon tax with\n80% revenue recycled aligned with the 1.5 °C goal(%)", 
                       high=brewer.pal(3,"Dark2")[2],
                       mid = "gray88", low=brewer.pal(4,"Dark2")[3],
                       midpoint = 0,
                       limits = c(-36,18),
                       breaks = seq(-30,15,15),
                       labels = str_c(seq(-30,15,15),"%"))+
  theme_test(base_line_size = 1,base_size = 12)+
  theme(legend.position = "top",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = -325, vjust = 1,hjust=1),
        axis.title = element_text(size = 14,face = "bold"))

Fig.1b  <- GlobalOutcome_dome  %>% filter(Goal %in% "1.5degree") %>% 
  filter(Vari %in% c("LocalGiniChg","InterGiniChg")) %>% 
  mutate(Vari = factor(Vari, levels = c("LocalGiniChg","InterGiniChg"))) %>% 
  ggplot()+
  facet_wrap(.~Vari,nrow = 1, 
             labeller = as_labeller(c(LocalGiniChg = "Inequality within countries", 
                                      InterGiniChg = "Inequality between countries")))+
  geom_tile(aes(SP, Tax, fill = value*100))+
  geom_label(aes(SP, Tax, 
                 label= str_c(format(round(value*100,2), nsmall= 2),"%")),
             size = 2.7, fontface= "bold")+
  labs(y = "Tax scenarios", x = "Domestic revenue recycling scenarios")+
  scale_x_discrete(limit = order.sp,labels = order.spname)+
  scale_y_discrete(limit = rev(order.tax),labels = rev(order.taxname))+
  scale_fill_gradient2(name="Changes in Gini coefficient under a carbon tax with\n80% revenue recycled aligned with the 1.5 °C goal(%)", 
                       high=brewer.pal(3,"Dark2")[2],
                       mid = "gray88", low=brewer.pal(4,"Dark2")[3],
                       midpoint = 0,
                       limits = c(-13,2.1),
                       breaks = seq(-12,2,4),labels = str_c(seq(-12,2,4),"%"))+
  theme_test(base_line_size = 1,base_size = 12)+
  theme(legend.position = "top",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = -325, vjust = 1,hjust=1),
        axis.title = element_text(size = 14,face = "bold"))

ggdraw()+ 
  draw_plot(Fig.1a, x=0, y=0.5, height = .49)+
  draw_plot(Fig.1b, x=0, y=0, height = .49)+
  draw_plot_label(label = c("a)", "b)"), size = 15, 
                  x=c(0, 0), y=c(1, 0.5))
ggsave(str_c(pathout5,"/", "SA+TAX outcome_15degree",Recynam[z],".jpg"),
       width = 8.5,height = 8.5,dpi = 500)

write.csv(GlobalOutcome_dome  %>% filter(Goal %in% "1.5degree") %>% 
            filter(Vari %in% c("NPLchg","IPLchg","LocalGiniChg","InterGiniChg")) ,
          str_c(pathout5,"/", "SA+TAX outcome_15degree",Recynam[z],".csv"))

# required tax level------------
Fig.T1 <- GlobalOutcome_dome  %>% 
  filter(Vari %in% "TaxLevel") %>% 
  ggplot()+
  geom_tile(aes(SP, Tax),fill = "gray88")+
  geom_label(aes(SP, Tax, 
                 label= str_c(value,",",format(round(TaxGDPShareLabel,1),nsmall = 1),"%")),
             size = 2.6, fontface= "bold",
             label.size = 0.1,label.padding = unit(0.1, "lines"))+
  facet_wrap(.~Goal,nrow = 1,
             labeller = as_labeller(c(`1.5degree` = "Global average tax level required for a 14.7%\nannual CO2 decline ($/tCO2,revenue/GDP)", 
                                      `2.0degree` = "Global average tax level required for a 2.7%\nannual CO2 decline ($/tCO2,revenue/GDP)")))+
  labs(y = "Tax scenarios", x = "Social assistance scenarios")+
  scale_x_discrete(limit = order.sp,labels = order.spname)+
  scale_y_discrete(limit = rev(order.tax),labels = rev(order.taxname))+
  theme_minimal(base_line_size = 1,base_size = 12)+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1),
        strip.text = element_text(face = "bold"),
        axis.title = element_text(size = 14,face = "bold"))


# By tax level global outcome ---------
order.taxname2 <- c("(T1) Global uniform\nproduction tax","(T2) Nation-differentiated\nproduction tax",
                    "(T3) Global uniform\nconsumption tax","(T4) Nation-differentiated\nconsumption tax",
                    "(T5) Luxury consumption tax","(T6) Luxury & nation-\ndifferentiated consumption tax")
order.sp2 <- c("Null","Cash","SP_current","SP_covid")
order.spname2 <- c("No revenue recycling",
                   "(S1) Cash-based programs", "(S2) Current social assistance",
                   "(S3) Social assistance\nCOVID-19 expansion")

Fig.T2 <-Data_AllTaxLevel %>% filter(TaxLevel %in% seq(0,180,15)) %>% 
  filter(SP != "PMT") %>% filter(SP != "Universal") %>%
  filter(Tax != "Null") %>% pivot_wider(id_cols = c(Tax,TaxLevel,SP),names_from = Vari) %>% 
  ggplot()+
  geom_vline(xintercept = CO2_Response_Recy[7,1,1],color = "black",linetype = "dashed",linewidth = 1,alpha = 0.6)+
  geom_vline(xintercept = CO2_Response_Recy[7,1,1]*0.973,color = "black",linetype = "dashed",linewidth = 1,alpha = 0.6)+
  geom_vline(xintercept = CO2_Response_Recy[7,1,1]*0.853,color = "black",linetype = "dashed",linewidth = 1,alpha = 0.6)+
  geom_hline(yintercept = sum(OUT_TAXSP[,4,7,2,1])/10^6,color = "purple",linetype = "dotdash",linewidth = 1,alpha = 0.6)+
  annotate("text", x = 22500, y = 770, label = "Baseline", color = "purple")+
  geom_point(aes(CO2_Effect,IPL_Effect,color = SP,shape = Tax,size = TaxLevel),alpha = 0.8,stroke = 1)+
  scale_shape_manual(name = "Tax scenarios", values = c(0,1,2,4,5,6),
                     breaks = order.tax,labels = order.taxname2)+
  scale_color_manual(name = "Domestic revenue\nrecycling scenarios", values = wes_palette("Rushmore1", n = 5)[2:5],
                     breaks = order.sp2,labels = order.spname2)+
  scale_size_continuous(name = "Tax level ($/tCO2)",range = c(0.5,3.5))+
  scale_x_continuous(breaks = c(23000,25000,CO2_Response_Recy[7,1,1]*0.853,27000,29000,CO2_Response_Recy[7,1,1]*0.973,
                                CO2_Response_Recy[7,1,1]),
                     labels = c(23,25,"26.20\n(-14.7%)",27,29,"29.88\n(-2.7%)","30.71\n(0%)"))+
  scale_y_continuous(limits = c(600,1150),
                     breaks = c(seq(400,1200,50)))+
  labs(x = "Carbon emissions (GT)", y = "Extreme poverty headcount (million)")+
  theme_test(base_line_size = 1,base_size = 11)+
  theme(legend.position = "right",
        legend.text = element_text(size = 10),legend.background = element_blank(),
        legend.key.height = unit(22,"pt"),
        legend.key.width = unit(5,"pt"),
        legend.title = element_text(face = "bold"),
        axis.text = element_text(size = 12,face = "bold"),
        axis.title = element_text(size = 14,face = "bold"))+
  guides(shape = guide_legend(order = 1),
         size = guide_legend(nrow = 1,byrow = T, order = 2),
         color = guide_legend(order = 3))

ggdraw()+ 
  draw_plot(Fig.T2, x=0, y=0.4, height = .59)+
  draw_plot(Fig.T1, x=0, y=0, height = .39)+
  draw_plot_label(label = c("a)", "b)"), size = 15, 
                  x=c(0, 0), y=c(1, 0.4))
ggsave(str_c(pathout5,"/", "Global CO2 and IPL outcome_all results and required tax",Recynam[z],".jpg"),
       width = 8.5,height = 9,dpi = 500)


LABEL1 <- c("CO2_Effect" = "CO2 emissions (MT)",
            "NPL_Effect" = "Poverty headcount under\nnational poverty lines (million)", 
            "IPL_Effect" = "Poverty headcount under\nextreme poverty line (million)")

LABEL2 <- c("Universal_P" = "(T1) Global uniform\nproduction tax",
            "CBDR_P" = "(T2) Nation-differen\ntiated production tax",
            "Universal_C" = "(T3) Global uniform\nconsumption tax",
            "CBDR_C" = "(T4) Nation-differen\ntiated consumption tax",
            "Luxury" = "(T5) Luxury\nconsumption tax",
            "Luxury&CBDR_C" = "(T6) Luxury & nation\n-differentiated\n consumption tax")

Data_AllTaxLevel %>% 
  filter(Tax != "Null") %>% 
  mutate(Tax = factor(Tax, levels = order.tax)) %>% 
  ggplot()+
  geom_line(aes(TaxLevel,value,group = SP, color = SP),alpha = 0.7, linewidth  = 0.9)+
  facet_grid2(Tax~Vari,axes = "all",scales = "free_y",remove_labels = "x",independent = "y",
              labeller = labeller(.cols = as_labeller(LABEL1), .rows = as_labeller(LABEL2)))+
  labs(x = "Global average tax level ($/tCO2)", y = NULL)+
  scale_x_continuous(limits = c(0,180),breaks = c(seq(0,180,20)))+
  scale_color_manual(name = "", values = wes_palette("AsteroidCity2", n = 6),
                     breaks = order.sp,labels = order.spname)+
  theme_test(base_line_size = 1,base_size = 11)+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(30,"pt"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "lavender", colour = "black", linewidth = 1))

ggsave(str_c(pathout5,"/", "Global CO2 and poverty outcome by tax level",Recynam[z],".jpg"),
       width = 8.5,height = 10,dpi = 500)
#================


#Figure: International policy mix
#================
order.sp3 <- c("SP_covid","Universal","PMT")
order.spname3 <- c("(T5) Luxury consumption tax +\n(S3) Social assistance\nCOVID-19 expansion",
                   "(T5) Luxury consumption tax +\n(S4) Universal dividend"
                   ,"(T5) Luxury consumption tax +\n(S5) Proxy Means Test (PMT)")

order.gf <- c("His.PoverPop","His.Pop","His.PoverGap",
              "Curren.PoverPop","Curren.Pop","Curren.PoverGap")
order.gfname <- c("(G1) Historical emissions\n& poverty headcount",
                  "(G2) Historical emissions\n& population",
                  "(G3) Historical emissions\n& poverty gap",
                  "(G4) Current emissions\n& poverty headcount",
                  "(G5) Current emissions\n& population",
                  "(G6) Current emissions\n& poverty gap")

#---------2 degree----------
Fig.1a <- GlobalOutcome_GloRec %>% filter(Goal %in% "2.0degree") %>% 
  filter(Vari %in% c("NPLchg","IPLchg")) %>% 
  mutate(Vari = factor(Vari, levels = c("IPLchg","NPLchg"))) %>% 
  ggplot()+
  facet_wrap(.~Vari,nrow = 1, 
             labeller = as_labeller(c(IPLchg = "Extreme (international) poverty line\n(baseline poverty headcount: 750 million)",
                                      NPLchg = "National poverty lines\n(baseline poverty headcount: 1445 million)")))+
  geom_tile(aes(SP, GF, fill = PovertyRateLabel))+
  geom_label(aes(SP, GF, 
                 label= str_c(format(round(PovertyRateLabel,2),nsmall =2),"%")),
             size = 3, fontface= "bold")+
  labs(y = "International revenue\nrecycling scenarios", x = "Tax and social assistance scenarios")+
  scale_x_discrete(limit = order.sp3,labels = order.spname3)+
  scale_y_discrete(limits = rev(order.gf), breaks = order.gf,labels = order.gfname)+
  scale_fill_gradient2(name="Changes in poverty headcount under the carbon tax\nrequired for a 2.7% annual CO2 decline (%)", 
                       high=brewer.pal(3,"Dark2")[2],
                       mid = "white", low=brewer.pal(4,"Dark2")[3],
                       midpoint = 0,
                       limits = c(-42,2),
                       breaks = seq(-40,0,10),
                       labels = str_c(seq(-40,0,10),"%"))+
  theme_test(base_line_size = 1,base_size = 12)+
  theme(legend.position = "top",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(30,"pt"),
        axis.text.x = element_text(angle = -325, vjust = 1,hjust=1),
        axis.title = element_text(size = 14,face = "bold"))

Fig.1b <- GlobalOutcome_GloRec %>% filter(Goal %in% "2.0degree") %>% 
  filter(Vari %in% c("LocalGiniChg","InterGiniChg")) %>% 
  mutate(Vari = factor(Vari, levels = c("LocalGiniChg","InterGiniChg"))) %>% 
  ggplot()+
  facet_wrap(.~Vari,nrow = 1, 
             labeller = as_labeller(c(LocalGiniChg = "Inequality within countries", 
                                      InterGiniChg = "Inequality between countries")))+
  geom_tile(aes(SP, GF, fill = value*100))+
  geom_label(aes(SP, GF, 
                 label= str_c(format(round(value*100,2),nsmall = 2),"%")),
             size = 3, fontface= "bold")+
  labs(y = "International revenue\nrecycling scenarios", x = "Tax and social assistance scenarios")+
  scale_x_discrete(limit = order.sp3,labels = order.spname3)+
  scale_y_discrete(limits = rev(order.gf), breaks = order.gf,labels = order.gfname)+
  scale_fill_gradient2(name="Changes in Gini coefficient under the carbon tax\nrequired for a 2.7% annual CO2 decline (%)", 
                       high=brewer.pal(3,"Dark2")[2],
                       mid = "white", low=brewer.pal(4,"Dark2")[3],
                       midpoint = 0,
                       limits = c(-2.2,0),
                       breaks = seq(-2,0,1),labels = str_c(seq(-2,0,1),"%"))+
  theme_test(base_line_size = 1,base_size = 12)+
  theme(legend.position = "top",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(30,"pt"),
        axis.text.x = element_text(angle = -325, vjust = 1,hjust=1),
        axis.title = element_text(size = 14,face = "bold"))

ggdraw()+ 
  draw_plot(Fig.1a, x=0, y=0.5, height = .49)+
  draw_plot(Fig.1b, x=0, y=0, height = .49)+
  draw_plot_label(label = c("a)", "b)"), size = 15, 
                  x=c(0, 0), y=c(1, 0.5))
ggsave(str_c(pathout5,"/", "Luxury_SA+GF outcome_2degree",Recynam[z],".jpg"),
       width = 8.5,height = 9,dpi = 500)
#------------

#---------1.5 degree----------
Fig.1a <- GlobalOutcome_GloRec %>% filter(Goal %in% "1.5degree") %>% 
  filter(Vari %in% c("NPLchg","IPLchg")) %>% 
  mutate(Vari = factor(Vari, levels = c("IPLchg","NPLchg"))) %>% 
  ggplot()+
  facet_wrap(.~Vari,nrow = 1, 
             labeller = as_labeller(c(IPLchg = "Extreme (international) poverty line\n(baseline poverty headcount: 750 million)",
                                      NPLchg = "National poverty lines\n(baseline poverty headcount: 1445 million)")))+
  geom_tile(aes(SP, GF, fill = PovertyRateLabel))+
  geom_label(aes(SP, GF, 
                 label= str_c(format(round(PovertyRateLabel,2),nsmall =2),"%")),
             size = 3, fontface= "bold")+
  labs(y = "International revenue\nrecycling scenarios", x = "Tax and social assistance scenarios")+
  scale_x_discrete(limit = order.sp3,labels = order.spname3)+
  scale_y_discrete(limits = rev(order.gf), breaks = order.gf,labels = order.gfname)+
  scale_fill_gradient2(name="Changes in Gini coefficient under the carbon tax\nrequired for a 14.7% annual CO2 decline (%)", 
                       high=brewer.pal(3,"Dark2")[2],
                       mid = "white", low=brewer.pal(4,"Dark2")[3],
                       midpoint = 0,
                       limits = c(-46,-8),
                       breaks = seq(-45,-10,10),
                       labels = str_c(seq(-45,-10,10),"%")
  )+
  theme_test(base_line_size = 1,base_size = 12)+
  theme(legend.position = "top",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(30,"pt"),
        axis.text.x = element_text(angle = -325, vjust = 1,hjust=1),
        axis.title = element_text(size = 14,face = "bold"))

Fig.1b <- GlobalOutcome_GloRec %>% filter(Goal %in% "1.5degree") %>% 
  filter(Vari %in% c("LocalGiniChg","InterGiniChg")) %>% 
  mutate(Vari = factor(Vari, levels = c("LocalGiniChg","InterGiniChg"))) %>% 
  ggplot()+
  facet_wrap(.~Vari,nrow = 1, 
             labeller = as_labeller(c(LocalGiniChg = "Inequality within countries", 
                                      InterGiniChg = "Inequality between countries")))+
  geom_tile(aes(SP, GF, fill = value*100))+
  geom_label(aes(SP, GF, 
                 label= str_c(format(round(value*100,2),nsmall = 2),"%")),
             size = 3, fontface= "bold")+
  labs(y = "International revenue\nrecycling scenarios", x = "Tax and social assistance scenarios")+
  scale_x_discrete(limit = order.sp3,labels = order.spname3)+
  scale_y_discrete(limits = rev(order.gf), breaks = order.gf,labels = order.gfname)+
  scale_fill_gradient2(name="Changes in Gini coefficient under the carbon tax\nrequired for a 14.7% annual CO2 decline (%)", 
                       high=brewer.pal(3,"Dark2")[2],
                       mid = "white", low=brewer.pal(4,"Dark2")[3],
                       midpoint = 0,
                       limits = c(-7.2,1.2),
                       breaks = seq(-7,1,2),labels = str_c(seq(-7,1,2),"%"))+
  theme_test(base_line_size = 1,base_size = 12)+
  theme(legend.position = "top",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(30,"pt"),
        axis.text.x = element_text(angle = -325, vjust = 1,hjust=1),
        axis.title = element_text(size = 14,face = "bold"))

ggdraw()+ 
  draw_plot(Fig.1a, x=0, y=0.5, height = .49)+
  draw_plot(Fig.1b, x=0, y=0, height = .49)+
  draw_plot_label(label = c("a)", "b)"), size = 15, 
                  x=c(0, 0), y=c(1, 0.5))
ggsave(str_c(pathout5,"/", "Luxury_SA+GF outcome_15degree",Recynam[z],".jpg"),
       width = 8.5,height = 9,dpi = 500)
#------------

# required tax level------------
GlobalOutcome_GloRec  %>% 
  filter(Vari %in% "TaxLevel") %>% 
  ggplot()+
  geom_tile(aes(SP, GF),fill = "gray88")+
  geom_label(aes(SP, GF, 
                 label= str_c(value,",",format(round(TaxGDPShareLabel,1),nsmall = 1),"%")),
             size = 3, fontface= "bold",
             label.size = 0.1,label.padding = unit(0.1, "lines"))+
  facet_wrap(.~Goal,nrow = 1,
             labeller = as_labeller(c(`1.5degree` = "Global average tax level required for a 14.7%\nannual CO2 decline ($/tCO2,revenue/GDP)", 
                                      `2.0degree` = "Global average tax level required for a 2.7%\nannual CO2 decline ($/tCO2,revenue/GDP)")))+
  labs(y = "International revenue\nrecycling scenarios", x = "Tax and social assistance scenarios")+
  scale_x_discrete(limits = order.sp3,labels = order.spname3)+
  scale_y_discrete(limits = rev(order.gf), breaks = order.gf,labels = order.gfname)+
  theme_minimal(base_line_size = 1,base_size = 12)+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1),
        strip.text = element_text(face = "bold"),
        axis.title = element_text(size = 14,face = "bold"))
ggsave(str_c(pathout5,"/", "Luxury_SA+GF required tax level",Recynam[z],".jpg"),
       width = 8.5,height = 5,dpi = 500)
#================


#Figure: National best domestic policy mix
#================
write.csv(Nation_BestRecy,str_c(pathout5,"/","National best policy mix",Recynam[z],".csv"),row.names = F)
#================


#Figure: Regional performance under the best International policy mix
#================
regorder <- rev(unique(Reg_Outcome_20_Figdata$Reg)[order(Reg_Outcome_20_Figdata$value[which(Reg_Outcome_20_Figdata$Scenario %in% "Null" & 
                                                                                              Reg_Outcome_20_Figdata$Vari %in% "PPop_ipl_tax")])])
reglabel <- regorder
reglabel[which(regorder %in% "Sub-Saharan Africa")] <- "Sub-Saharan\nAfrica"
reglabel[which(regorder %in% "Latin America and Caribbean")] <- "Latin America\nand Caribbean"
reglabel[which(regorder %in% "Asia-Pacific Developed")] <- "Asia-Pacific\nDeveloped"
reglabel[which(regorder %in% "East Asia and Developing Pacific")] <- "East Asia and\nDeveloping Pacific"


#---------2 degree----------
Fig.1a <- Reg_Outcome_20_Figdata %>% 
  filter(Scenario != "Null") %>% 
  filter(Vari %in% c("PPop_ipl_chgrate","PPop_npl_chgrate")) %>% 
  ggplot()+
  geom_point(aes(Reg, value,group = Scenario,color = Scenario,shape = Scenario),size = 4,alpha = 0.8)+
  geom_hline(yintercept = 0,linetype = "dashed",color = "red")+
  facet_wrap(~Vari,ncol = 1, 
             labeller = as_labeller(c(PPop_ipl_chgrate = "Extreme (international) poverty line",
                                      PPop_npl_chgrate = "National poverty lines")))+
  scale_x_discrete(limits = regorder, breaks = regorder, labels = reglabel)+
  labs(x = "", y = str_c("Changes in poverty headcount (%)"))+
  scale_y_continuous(breaks = seq(-0.5,0.1,0.1),labels = str_c(seq(-0.5,0.1,0.1)*100,"%"))+
  theme_test(base_size = 12)+
  scale_color_manual(name = "",
                     breaks = unique(Reg_Outcome_20_Figdata$Scenario)[-1],
                     values = brewer.pal(3,"Dark2"),
                     labels = c("(T5) Luxury consumption tax without recycling",
                                "(T5+S4) Luxury consumption tax+\nSocial assistance COVID-19 expansion",
                                "(T5+S4+G4) Luxury consumption tax+\nSocial assistance COVID-19 expansion+\nGlobal fund: current emissions & Poverty headcount"))+
  scale_shape_manual(name = "",
                     breaks = unique(Reg_Outcome_20_Figdata$Scenario)[-1],
                     values = c(15:17),
                     labels = c("(T5) Luxury consumption tax without recycling",
                                "(T5+S4) Luxury consumption tax+\nSocial assistance COVID-19 expansion",
                                "(T5+S4+G4) Luxury consumption tax+\nSocial assistance COVID-19 expansion+\nGlobal fund: current emissions & Poverty headcount"))+
  theme(legend.text = element_text(size = 8),legend.key.height = unit(10,"pt"),
        axis.text.x = element_text(angle = 90, vjust = .5,hjust=1),
        axis.title = element_text(size = 14,face = "bold"))+
  guides(color = guide_legend(nrow = 1,byrow = T))

Fig.1b  <- Reg_Outcome_20_Figdata %>% 
  filter(Scenario != "Null") %>% 
  filter(Vari %in% "PPop_Gini_chgrate") %>% 
  ggplot()+
  geom_point(aes(Reg, value,group = Scenario,color = Scenario,shape = Scenario),size = 4,alpha = 0.8)+
  geom_hline(yintercept = 0,linetype = "dashed",color = "red")+
  facet_wrap(~Vari,ncol = 1, 
             labeller = as_labeller(c(PPop_Gini_chgrate = "Inequality within countries")))+
  scale_x_discrete(limits = regorder, breaks = regorder, labels = reglabel)+
  labs(x = "", y = str_c("Changes in local Gini coefficent (%)"))+
  scale_y_continuous(breaks = seq(-0.04,0.01,.01),labels = str_c(seq(-0.04,0.01,.01)*100,"%"))+
  theme_test(base_size = 12)+
  scale_color_manual(name = "",
                     breaks = unique(Reg_Outcome_20_Figdata$Scenario)[-1],
                     values = c(brewer.pal(3,"Dark2")),
                     labels = c("(T5) Luxury consumption tax without recycling",
                                "(T5+S4) Luxury consumption tax+\nSocial assistance COVID-19 expansion",
                                "(T5+S4+G4) Luxury consumption tax+\nSocial assistance COVID-19 expansion+\nGlobal fund: current emissions & Poverty headcount"))+
  scale_shape_manual(name = "",
                     breaks = unique(Reg_Outcome_20_Figdata$Scenario)[-1],
                     values = c(15:17),
                     labels = c("(T5) Luxury consumption tax without recycling",
                                "(T5+S4) Luxury consumption tax+\nSocial assistance COVID-19 expansion",
                                "(T5+S4+G4) Luxury consumption tax+\nSocial assistance COVID-19 expansion+\nGlobal fund: current emissions & Poverty headcount"))+
  theme(legend.text = element_text(size = 8),legend.key.height = unit(10,"pt"),
        axis.text.x = element_text(angle = 90, vjust = .5,hjust=1),
        axis.title = element_text(size = 14,face = "bold"))+
  guides(color = guide_legend(nrow = 1,byrow = T))

ggarrange(Fig.1a,Fig.1b, common.legend = T,legend = "bottom")
ggsave(str_c(pathout5,"/", "Regional outcome under best global policy mix_2degree",Recynam[z],".jpg"),
       width = 8.5,height = 5.5,dpi = 500)
#-------------


#---------1.5 degree----------
Fig.1a <- Reg_Outcome_15_Figdata %>% 
  filter(Scenario != "Null") %>% 
  filter(Vari %in% c("PPop_ipl_chgrate","PPop_npl_chgrate")) %>% 
  ggplot()+
  geom_point(aes(Reg, value,group = Scenario,color = Scenario,shape = Scenario),size = 4,alpha = 0.8)+
  geom_hline(yintercept = 0,linetype = "dashed",color = "red")+
  facet_wrap(~Vari,ncol = 1, 
             labeller = as_labeller(c(PPop_ipl_chgrate = "Extreme (international) poverty line",
                                      PPop_npl_chgrate = "National poverty lines")))+
  scale_x_discrete(limits = regorder, breaks = regorder, labels = reglabel)+
  labs(x = "", y = str_c("Changes in poverty headcount (%)"))+
  scale_y_continuous(breaks = seq(-1,0.4,0.2),labels = str_c(seq(-1,0.4,0.2)*100,"%"))+
  theme_test(base_size = 12)+
  scale_color_manual(name = "",
                     breaks = unique(Reg_Outcome_15_Figdata$Scenario)[-1],
                     values = brewer.pal(3,"Dark2"),
                     labels = c("(T5) Luxury consumption tax without recycling",
                                "(T5+S4) Luxury consumption tax+\nSocial assistance COVID-19 expansion",
                                "(T5+S4+G4) Luxury consumption tax+\nSocial assistance COVID-19 expansion+\nGlobal fund: current emissions & Poverty headcount"))+
  scale_shape_manual(name = "",
                     breaks = unique(Reg_Outcome_15_Figdata$Scenario)[-1],
                     values = c(15:17),
                     labels = c("(T5) Luxury consumption tax without recycling",
                                "(T5+S4) Luxury consumption tax+\nSocial assistance COVID-19 expansion",
                                "(T5+S4+G4) Luxury consumption tax+\nSocial assistance COVID-19 expansion+\nGlobal fund: current emissions & Poverty headcount"))+
  theme(legend.text = element_text(size = 8),legend.key.height = unit(10,"pt"),
        axis.text.x = element_text(angle = 90, vjust = .5,hjust=1),
        axis.title = element_text(size = 14,face = "bold"))+
  guides(color = guide_legend(nrow = 1,byrow = T))

Fig.1b  <- Reg_Outcome_15_Figdata %>% 
  filter(Scenario != "Null") %>% 
  filter(Vari %in% "PPop_Gini_chgrate") %>% 
  ggplot()+
  geom_point(aes(Reg, value,group = Scenario,color = Scenario,shape = Scenario),size = 4,alpha = 0.8)+
  geom_hline(yintercept = 0,linetype = "dashed",color = "red")+
  facet_wrap(~Vari,ncol = 1, 
             labeller = as_labeller(c(PPop_Gini_chgrate = "Inequality within countries")))+
  scale_x_discrete(limits = regorder, breaks = regorder, labels = reglabel)+
  labs(x = "", y = str_c("Changes in local Gini coefficent (%)"))+
  scale_y_continuous(breaks = seq(-0.25,0.05,.05),labels = str_c(seq(-0.25,0.05,.05)*100,"%"))+
  theme_test(base_size = 12)+
  scale_color_manual(name = "",
                     breaks = unique(Reg_Outcome_15_Figdata$Scenario)[-1],
                     values = c(brewer.pal(3,"Dark2")),
                     labels = c("(T5) Luxury consumption tax without recycling",
                                "(T5+S4) Luxury consumption tax+\nSocial assistance COVID-19 expansion",
                                "(T5+S4+G4) Luxury consumption tax+\nSocial assistance COVID-19 expansion+\nGlobal fund: current emissions & Poverty headcount"))+
  scale_shape_manual(name = "",
                     breaks = unique(Reg_Outcome_15_Figdata$Scenario)[-1],
                     values = c(15:17),
                     labels = c("(T5) Luxury consumption tax without recycling",
                                "(T5+S4) Luxury consumption tax+\nSocial assistance COVID-19 expansion",
                                "(T5+S4+G4) Luxury consumption tax+\nSocial assistance COVID-19 expansion+\nGlobal fund: current emissions & Poverty headcount"))+
  theme(legend.text = element_text(size = 8),legend.key.height = unit(10,"pt"),
        axis.text.x = element_text(angle = 90, vjust = .5,hjust=1),
        axis.title = element_text(size = 14,face = "bold"))+
  guides(color = guide_legend(nrow = 1,byrow = T))

ggarrange(Fig.1a,Fig.1b, common.legend = T,legend = "bottom")
ggsave(str_c(pathout5,"/", "Regional outcome under best global policy mix_15degree",Recynam[z],".jpg"),
       width = 8.5,height = 5.5,dpi = 500)
#-------------
#================

