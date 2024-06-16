#Module 9: Results summary and visualization
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu



Recy <- c(0.3,0.8,1)
Recynam <- c("30percent","80percent","100percent")
z=3#for 100percent

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


#Figure: Uneven burden by deciles under different tax scenarios.#===========================
LABEL <- c("Sub-Saharan Africa" = "Sub-Saharan\nAfrica","North Africa" = "North Africa","China" ="China",
           "Eurasia"="Eurasia","Europe" ="Europe" ,"India" ="India" , "Latin America and Caribbean"= "Latin America\nand Caribbean",
           "Middle East" = "Middle East", "East Asia and Developing Pacific"="East Asia and\nDeveloping Pacific",
           "Southern Asia" ="Southern Asia","Asia-Pacific Developed" = "Asia-Pacific\nDeveloped","United States" ="United States")

Reg_dc <- c("Europe","Eurasia","Middle East", "Asia-Pacific Developed","East Asia and Developing Pacific","United States")

Reg_ldc <- c("Sub-Saharan Africa","North Africa","China","India", "Latin America and Caribbean","Southern Asia")

LABEL2 <- c("Total_Effect" = "Total effect", "Price_Effect" = "Price effect",  "Income_Effect" = "Income effect")

Name_Tax <- unique(Data_Unevenburden_agg$TaxScenario)
Data_Unevenburden_agg$Group <- as.numeric(substr(Data_Unevenburden_agg$Group,2,nchar(Data_Unevenburden_agg$Group)))/10

Fig.a <- Data_Unevenburden_agg %>% filter(TaxScenario != "Null",
                                          TaxScenario != "Universal_C") %>% 
  filter(Reg %in% Reg_dc) %>% 
  ggplot()+
  geom_line(aes(Group, value, group = TaxScenario, color = TaxScenario),linewidth = 0.3)+
  geom_point(aes(Group, value, color = TaxScenario), alpha=.7, size=0.8)+
  facet_grid(Vari~Reg, labeller = labeller(.cols = as_labeller(LABEL) , .rows = as_labeller(LABEL2)),
             scales = "free_y")+
  scale_x_continuous(breaks = 1:10)+
  scale_y_continuous(limits = c(0,.14),breaks = seq(0,.14,0.03),labels = str_c(seq(0,14,3),"%"))+
  scale_color_manual(name = "", values = wes_palette("Cavalcanti1", n = 5),
                     breaks = order.tax[-3],
                     labels = c("(T1/T3) Global uniform\nproduction/consumption tax",
                                "(T2) Nation-differentiated\nproduction tax","(T4) Nation-differentiated\nconsumption tax",
                                "(T5) Luxury consumption tax","(T6) Luxury & nation-differentiated\nconsumption tax"))+
  labs(x = "Expenditure decile", y = "Effect, measured as percentage of expenditure")+
  theme_test(base_size = 11)+
  theme(strip.text = element_text(face = "bold"),legend.spacing.x = unit(1,"cm"),
        strip.background = element_rect(fill = "lavender", colour = "black", size = 1),
        axis.title = element_text(size = 12,face = "bold"))+
  guides(color = guide_legend(nrow = 2,byrow = T))

Fig.b <- Data_Unevenburden_agg %>% filter(TaxScenario != "Null",
                                          TaxScenario != "Universal_C") %>% 
  filter(Reg %in% Reg_ldc) %>% 
  ggplot()+
  geom_line(aes(Group, value, group = TaxScenario, color = TaxScenario),linewidth = 0.3)+
  geom_point(aes(Group, value, color = TaxScenario), alpha=.7, size=0.8)+
  facet_grid(Vari~Reg, labeller = labeller(.cols = as_labeller(LABEL) , .rows = as_labeller(LABEL2)),
             scales = "free_y")+
  scale_x_continuous(breaks = 1:10)+
  scale_y_continuous(limits = c(0,.14),breaks = seq(0,.14,0.03),labels = str_c(seq(0,14,3),"%"))+
  scale_color_manual(name = "", values = wes_palette("Cavalcanti1", n = 5),
                     breaks = order.tax[-3],
                     labels = c("(T1/T3) Global uniform\nproduction/consumption tax",
                                "(T2) Nation-differentiated\nproduction tax","(T4) Nation-differentiated\nconsumption tax",
                                "(T5) Luxury consumption tax","(T6) Luxury & nation-differentiated\nconsumption tax"))+
  labs(x = "Expenditure decile", y = "Effect, measured as percentage of expenditure")+
  theme_test(base_size = 11)+
  theme(strip.text = element_text(face = "bold"),legend.spacing.x = unit(1,"cm"),
        strip.background = element_rect(fill = "lavender", colour = "black", size = 1),
        axis.title = element_text(size = 12,face = "bold"))+
  guides(color = guide_legend(nrow = 2,byrow = T))

ggarrange(Fig.a,Fig.b,common.legend = T,legend = "bottom", nrow = 2)
ggsave(str_c(pathout5,"/", "Uneven burden by tax scenarios_60.jpg"),
       width = 8.5,height = 9,dpi = 500)

write.csv(Data_Unevenburden_agg %>% filter(TaxScenario != "Null",
                                          TaxScenario != "Universal_C"),
          str_c(pathout5,"/", "Uneven burden by tax scenarios_Region level_60.csv"))


#Figure: Tax level required for climate goals; Poverty, ineq outcome under domestic policy mix#================
# 2.0 degree------
Fig.1a <- GlobalOutcome_dome  %>% filter(Goal %in% "2.0degree") %>% 
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
  scale_fill_gradient2(name="Changes in poverty headcount under a revenue-\nneutral carbon tax aligned with the 2.0 °C goal(%)", 
                       high=brewer.pal(3,"Dark2")[2],
                       mid = "gray88", low=brewer.pal(4,"Dark2")[3],
                       midpoint = 0,
                       limits = c(-16,2),
                       breaks = seq(-15,1,4),
                       labels = str_c(seq(-15,1,4),"%"))+
  theme_bw(base_line_size = 1,base_size = 12)+
  theme(legend.position = "top",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(30,"pt"),
        axis.text.x = element_text(angle = -325, vjust = 1,hjust=1),
        axis.title = element_text(size = 14,face = "bold"))

Fig.1b <- GlobalOutcome_dome  %>% filter(Goal %in% "2.0degree") %>% 
  filter(Vari %in% c("LocalGiniChg","InterGiniChg")) %>% 
  mutate(Vari = factor(Vari, levels = c("LocalGiniChg","InterGiniChg"))) %>% 
  ggplot()+
  facet_wrap(.~Vari,nrow = 1, 
             labeller = as_labeller(c(LocalGiniChg = "Inequality within countries", 
                                      InterGiniChg = "Inequality between countries")))+
  geom_tile(aes(SP, Tax, fill = value*100))+
  geom_label(aes(SP, Tax, 
                 label= str_c(format(round(value*100,2),nsmall =2),"%")),
             size = 2.7, fontface= "bold")+
  labs(y = "Tax scenarios", x = "Domestic revenue recycling scenarios")+
  scale_x_discrete(limit = order.sp,labels = order.spname)+
  scale_y_discrete(limit = rev(order.tax),labels = rev(order.taxname))+
  scale_fill_gradient2(name="Changes in Gini coefficient under a revenue-\nneutral carbon tax aligned with the 2.0 °C goal(%)", 
                       high=brewer.pal(3,"Dark2")[2],
                       mid = "gray88", low=brewer.pal(4,"Dark2")[3],
                       midpoint = 0,
                       limits = c(-2.1,1.1),
                       breaks = seq(-2,1,1),labels = str_c(seq(-2,1,1),"%"))+
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
ggsave(str_c(pathout5,"/", "SA+TAX outcome_2degree",Recynam[z],".jpg"),
       width = 8.5,height = 8.5,dpi = 500)

write.csv(GlobalOutcome_dome  %>% filter(Goal %in% "2.0degree") %>% 
            filter(Vari %in% c("NPLchg","IPLchg","LocalGiniChg","InterGiniChg")) ,
                   str_c(pathout5,"/", "SA+TAX outcome_2degree",Recynam[z],".csv"))

# 1.5 degree------------
Fig.1a <- GlobalOutcome_dome  %>% filter(Goal %in% "1.5degree") %>% 
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
  scale_fill_gradient2(name="Changes in poverty headcount under a revenue-\nneutral carbon tax aligned with the 1.5 °C goal(%)", 
                       high=brewer.pal(3,"Dark2")[2],
                       mid = "gray88", low=brewer.pal(4,"Dark2")[3],
                       midpoint = 0,
                       limits = c(-47,20),
                       breaks = seq(-45,15,15),
                       labels = str_c(seq(-45,15,15),"%"))+
  theme_test(base_line_size = 1,base_size = 12)+
  theme(legend.position = "top",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(30,"pt"),
        axis.text.x = element_text(angle = -325, vjust = 1,hjust=1),
        axis.title = element_text(size = 14,face = "bold"))

Fig.1b <- GlobalOutcome_dome  %>% filter(Goal %in% "1.5degree") %>% 
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
  scale_fill_gradient2(name="Changes in Gini coefficient under a revenue-\nneutral carbon tax aligned with the 1.5 °C goal(%)", 
                       high=brewer.pal(3,"Dark2")[2],
                       mid = "gray88", low=brewer.pal(4,"Dark2")[3],
                       midpoint = 0,
                       limits = c(-17,4),
                       breaks = seq(-16,4,4),labels = str_c(seq(-16,4,4),"%"))+
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
ggsave(str_c(pathout5,"/", "SA+TAX outcome_15degree",Recynam[z],".jpg"),
       width = 8.5,height = 8.5,dpi = 500)

write.csv(GlobalOutcome_dome  %>% filter(Goal %in% "1.5degree") %>% 
            filter(Vari %in% c("NPLchg","IPLchg","LocalGiniChg","InterGiniChg")) ,
          str_c(pathout5,"/", "SA+TAX outcome_1.5degree",Recynam[z],".csv"))

# required tax level------------
Fig.T1 <- GlobalOutcome_dome  %>% 
  filter(Vari %in% "TaxLevel") %>% 
  ggplot()+
  # geom_tile(aes(SP, Tax),fill = "gray88")+
  geom_tile(aes(SP, Tax,fill = value))+
  geom_label(aes(SP, Tax, 
                 label= str_c(value,", ",format(round(TaxGDPShareLabel,1),nsmall = 1),"%")),
             size = 2.6, fontface= "bold",
             label.size = 0.1,label.padding = unit(0.1, "lines"))+
  facet_wrap(.~Goal,nrow = 1,
             labeller = as_labeller(c(`1.5degree` = "Global average tax level aligned with\nthe 1.5 °C goal ($/tCO2,revenue/GDP)", 
                                      `2.0degree` = "Global average tax level aligned with\nthe 2.0 °C goal ($/tCO2,revenue/GDP)")))+
  labs(y = "Tax scenarios", x = "Domestic revenue recycling scenarios")+
  scale_x_discrete(limit = order.sp,labels = order.spname)+
  scale_y_discrete(limit = rev(order.tax),labels = rev(order.taxname))+
  scale_fill_gradient(low = "orange3",high = "red4",guide = NULL)+
  theme_minimal(base_line_size = 1,base_size = 12)+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1),
        strip.text = element_text(face = "bold",size = 12),
        axis.title = element_text(size = 14,face = "bold"))

write.csv(GlobalOutcome_dome  %>% filter(Vari %in% "TaxLevel") ,
          str_c(pathout5,"/", "SA+TAX required tax level",Recynam[z],".csv"))

# By tax level global outcome ---------
order.taxname2 <- c("(T1) Global uniform\nproduction tax","(T2) Nation-differentiated\nproduction tax",
                    "(T3) Global uniform\nconsumption tax","(T4) Nation-differentiated\nconsumption tax",
                    "(T5) Luxury consumption tax","(T6) Luxury & nation-\ndifferentiated consumption tax")
order.sp2 <- c("Null","Cash","SP_current","SP_covid","Universal","PMT")
order.spname2 <- c("No revenue recycling",
                   "(S1) Cash-based programs", "(S2) Current social assistance",
                   "(S3) Social assistance\nCOVID-19 expansion","(S4) Universal dividend"
                   ,"(S5) Proxy Means Test (PMT)")

Fig.T2 <-Data_AllTaxLevel %>% filter(TaxLevel %in% seq(0,180,15)) %>% 
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
  scale_color_manual(name = "Domestic revenue\nrecycling scenarios", values = c("black",wes_palette("Cavalcanti1",n =5)),
                     breaks = order.sp2,labels = order.spname2)+
  scale_size_continuous(name = "Tax level ($/tCO2)",range = c(0.5,3.5))+
  scale_x_continuous(breaks = c(23000,25000,CO2_Response_Recy[7,1,1]*0.853,27000,29000,CO2_Response_Recy[7,1,1]*0.973,
                                CO2_Response_Recy[7,1,1]),
                     labels = c(23,25,"26.20\n(-14.7%)",27,29,"29.88\n(-2.7%)","30.71\n(0%)"))+
  scale_y_continuous(limits = c(380,1150),
                     breaks = c(seq(400,1200,100)))+
  labs(x = "Carbon emissions (GT)", y = "Extreme poverty headcount (million)")+
  theme_test(base_line_size = 1,base_size = 11)+
  theme(legend.position = "right",
        legend.text = element_text(size = 9),legend.background = element_blank(),
        legend.key.height = unit(20.5,"pt"),
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
       width = 9,height = 9.5,dpi = 500)

write.csv(Data_AllTaxLevel %>% filter(TaxLevel %in% seq(0,180,15)) %>% 
            filter(Tax != "Null"),
          str_c(pathout5,"/", "Global CO2 and IPL outcome_all results",Recynam[z],".csv"))



#Figure: International policy mix#================
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
  labs(y = "International revenue\nrecycling scenarios", x = "Tax and Domestic revenue recycling scenarios")+
  scale_x_discrete(limit = order.sp3,labels = order.spname3)+
  scale_y_discrete(limits = rev(order.gf), breaks = order.gf,labels = order.gfname)+
  scale_fill_gradient2(name="Changes in poverty headcount under a revenue-\nneutral carbon tax aligned with the 2.0 °C goal(%)(%)", 
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
  labs(y = "International revenue\nrecycling scenarios", x = "Tax and Domestic revenue recycling scenarios")+
  scale_x_discrete(limit = order.sp3,labels = order.spname3)+
  scale_y_discrete(limits = rev(order.gf), breaks = order.gf,labels = order.gfname)+
  scale_fill_gradient2(name="Changes in Gini coefficient under a revenue-\nneutral carbon tax aligned with the 2.0 °C goal(%)", 
                       high=brewer.pal(3,"Dark2")[2],
                       mid = "white", low=brewer.pal(4,"Dark2")[3],
                       midpoint = 0,
                       limits = c(-4.2,0),
                       breaks = seq(-4,0,1),labels = str_c(seq(-4,0,1),"%"))+
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

write.csv(GlobalOutcome_GloRec  %>% filter(Goal %in% "2.0degree") %>% 
            filter(Vari %in% c("NPLchg","IPLchg","LocalGiniChg","InterGiniChg")) ,
          str_c(pathout5,"/", "Luxury_SA+GF outcome_2degree",Recynam[z],".csv"))


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
  labs(y = "International revenue\nrecycling scenarios", x = "Tax and Domestic revenue recycling scenarios")+
  scale_x_discrete(limit = order.sp3,labels = order.spname3)+
  scale_y_discrete(limits = rev(order.gf), breaks = order.gf,labels = order.gfname)+
  scale_fill_gradient2(name="Changes in poverty headcount under a revenue-\nneutral carbon tax aligned with the 1.5 °C goal(%)", 
                       high=brewer.pal(3,"Dark2")[2],
                       mid = "white", low=brewer.pal(4,"Dark2")[3],
                       midpoint = 0,
                       limits = c(-58,-38),
                       breaks = seq(-55,-40,5),
                       labels = str_c(seq(-55,-40,5),"%")
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
  labs(y = "International revenue\nrecycling scenarios", x = "Tax and Domestic revenue recycling scenarios")+
  scale_x_discrete(limit = order.sp3,labels = order.spname3)+
  scale_y_discrete(limits = rev(order.gf), breaks = order.gf,labels = order.gfname)+
  scale_fill_gradient2(name="Changes in Gini coefficient under a revenue-\nneutral carbon tax aligned with the 1.5 °C goal(%)", 
                       high=brewer.pal(3,"Dark2")[2],
                       mid = "white", low=brewer.pal(4,"Dark2")[3],
                       midpoint = 0,
                       limits = c(-18.2,0),
                       breaks = seq(-18,0,3),labels = str_c(seq(-18,0,3),"%"))+
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
write.csv(GlobalOutcome_GloRec  %>% filter(Goal %in% "1.5degree") %>% 
            filter(Vari %in% c("NPLchg","IPLchg","LocalGiniChg","InterGiniChg")) ,
          str_c(pathout5,"/", "Luxury_SA+GF outcome_15degree",Recynam[z],".csv"))


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
             labeller = as_labeller(c(`1.5degree` = "Global average tax level aligned with\nthe 1.5 °C goal ($/tCO2,revenue/GDP)", 
                                      `2.0degree` = "Global average tax level aligned with\nthe 2.0 °C goal ($/tCO2,revenue/GDP)")))+
  labs(y = "International revenue\nrecycling scenarios", x = "Tax and domestic revenue recycling scenarios")+
  scale_x_discrete(limits = order.sp3,labels = order.spname3)+
  scale_y_discrete(limits = rev(order.gf), breaks = order.gf,labels = order.gfname)+
  theme_minimal(base_line_size = 1,base_size = 12)+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1),
        strip.text = element_text(face = "bold"),
        axis.title = element_text(size = 14,face = "bold"))
ggsave(str_c(pathout5,"/", "Luxury_SA+GF required tax level",Recynam[z],".jpg"),
       width = 8.5,height = 5,dpi = 500)

write.csv(GlobalOutcome_GloRec  %>% filter(Vari %in% "TaxLevel") ,
          str_c(pathout5,"/", "Luxury_SA+TAX required tax level",Recynam[z],".csv"))


#Figure: National best domestic policy mix#================
write.csv(Nation_BestRecy,str_c(pathout5,"/","National best policy mix",Recynam[z],".csv"),row.names = F)



#Figure: Regional performance under the best International policy mix#================
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
  labs(x = "", y = str_c("Changes in inequality within countries (%)"))+
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

write.csv(Reg_Outcome_20_Figdata %>% 
            filter(Scenario != "Null") %>% 
            filter(Vari %in% c("PPop_ipl_chgrate","PPop_npl_chgrate","PPop_Gini_chgrate")),
          str_c(pathout5,"/", "Regional outcome under best global policy mix_2degree",Recynam[z],".csv"))



#---------1.5 degree----------
write.csv(Reg_Outcome_15_Figdata,str_c(pathout5,"/", "Regional outcome under best global policy mix_15degree",Recynam[z],".csv"))

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
write.csv(Reg_Outcome_15_Figdata %>% 
            filter(Scenario != "Null") %>% 
            filter(Vari %in% c("PPop_ipl_chgrate","PPop_npl_chgrate","PPop_Gini_chgrate")),
          str_c(pathout5,"/", "Regional outcome under best global policy mix_15degree",Recynam[z],".csv"))


# Figure: Tax scenarios design ============
#a. Tax level under CBDR_P and CBDR_P#-----------
regorder2 <- unique(CBDR$Region)[order(CBDR$Tax_C)]
reglabel2 <- regorder2
reglabel2[which(regorder2 %in% "Sub-Saharan Africa")] <- "Sub-Saharan\nAfrica"
reglabel2[which(regorder2 %in% "Latin America and Caribbean")] <- "Latin America\nand Caribbean"
reglabel2[which(regorder2 %in% "Asia-Pacific Developed")] <- "Asia-Pacific\nDeveloped"
reglabel2[which(regorder2 %in% "East Asia and Developing Pacific")] <- "East Asia and\nDeveloping Pacific"

Fig.1a <- CBDR %>% pivot_longer(-Region, names_to = "Vari") %>%
  ggplot()+
  geom_col(aes(Region,value,fill = Vari), position = "dodge")+
  labs(x = "", y = str_c("Tax level (US$/tCO2)"))+
  geom_hline(yintercept = 60, color = "green",linetype = "dashed")+
  scale_x_discrete(limit = regorder2, labels = reglabel2)+
  scale_fill_manual(name =  'Principles',
                    limits = c("Tax_P","Tax_C"),
                    labels = c("(T2) Nation-differentiated\nproduction tax",
                               "(T4) Nation-differentiated\nconsumption tax"),
                    values = brewer.pal(3,"Dark2")[2:3])+
  coord_flip()+
  theme_bw(base_size = 12)+
  theme(legend.text = element_text(size = 8),legend.title = element_text(size = 10, face = "bold"),
        legend.position = c(0.7,0.1),
        legend.background = element_blank(),
        axis.title = element_text(size = 14,face = "bold"))

write.csv(CBDR,str_c(pathout5,"/", "CBDR_taxlevel_60.csv"))

#b. Tax level by sectors under luxury.#-----------
Luxury <- as.data.frame(Luxury)
Sec_label <- rep(NA,length(Luxury$Secnam))
Sec_label[match(c("oil","coa","wht","gro","ocr","c_b","pdr",
                  "ros","afs","otn","ins","mvh","atp"),
                Luxury$Secnam)] <- c("Oil","Coal","Wheat","Cereal grains nec","Crops nec","Sugar cane, sugar beet","Paddy rice",
                                     "Recreational and other services","Accommodation, Food and service activities","Transport equipment nec","Insurance",
                                     "Motor vehicles and parts","Air transport")

Fig.1b <- Luxury %>% ggplot()+
  geom_point(aes(Elas,TaxRate),
             size = 4, alpha = 0.8, color = brewer.pal(3,"Dark2")[3])+
  geom_text_repel(aes(Elas,TaxRate,label = Sec_label),
                  size = 2.5, fontface= "italic",
                  arrow = arrow(length=unit(0.01, "npc")),force = 6, max.iter = 3e3,
                  segment.colour = "grey50",nudge_x = 0.25)+
  geom_vline(xintercept = 1,color = "red",linetype = "dashed")+
  annotate("text", x = 0.7, y = 64, label = "above average tax",size = 3,fontface = "bold")+
  annotate("text", x = 0.7, y = 56, label = "below average tax",size = 3,fontface = "bold")+
  geom_hline(yintercept = 60,color = "green",linetype = "dashed")+
  labs(x = "Expenditure elasticity", y = str_c("Tax level (US$/tCO2)"))+
  theme_bw(base_size = 12)+
  theme(axis.title = element_text(size = 14,face = "bold"))

write.csv(Luxury,str_c(pathout5,"/", "Luxury_taxlevel_60.csv"))

#c. Tax revenue as share of GDP-------
order.tax2 <- c("Universal_P","CBDR_P","Universal_C","CBDR_C","Luxury","Luxury&CBDR")
order.taxname2 <- c("(T1) Global uniform\nproduction tax","(T2) Nation-differentiated\nproduction tax",
                   "(T3) Global uniform\nconsumption tax","(T4) Nation-differentiated\nconsumption tax",
                   "(T5) Luxury\nconsumption tax","(T6) Luxury & nation-\ndifferentiated consumption tax")

Fig.1c <- RevenueAnalysis %>% filter(Vari %in% "RevenueShare") %>% 
  ggplot()+
  geom_tile(aes(Reg,Type,fill=value),color="gray",size=0.1)+
  geom_label(aes(Reg,Type,
                 label= str_c(format(round(value*100,2),nsmall = 2),"%")),
             size = 3, fontface= "bold")+
  labs(y = "Tax scenarios",x = "")+
  scale_y_discrete(limit = rev(order.tax2), breaks = order.tax2,labels = order.taxname2)+
  scale_x_discrete(limit = rev(regorder2), labels = rev(reglabel2))+
  scale_fill_gradient2(name="Tax revenue as share of GDP (%)", 
                       high=brewer.pal(3,"Dark2")[2], low=brewer.pal(4,"Dark2")[3],
                       mid = "white",midpoint = 0,
                       breaks = seq(0,0.055,0.01),labels = str_c(seq(0,0.055,0.01)*100,"%"))+
  theme_test(base_size = 11)+
  theme(legend.position = "bottom",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(45,"pt"),
        axis.title = element_text(size = 14,face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1))

write.csv(RevenueAnalysis %>% filter(Vari %in% "RevenueShare"),str_c(pathout5,"/", "RevenueAnalysis_60.csv"))

ggdraw()+ 
  draw_plot(Fig.1a, x=0, y=0.5,width = 0.49, height = 0.49)+
  draw_plot(Fig.1b, x=0.5, y=0.5,width = 0.49, height = 0.49)+
  draw_plot(Fig.1c, x=0, y=0,width = 1, height = 0.49)+
  draw_plot_label(label = c("a)", "b)", "c)"), size = 15, 
                  x=c(0, 0.5,0), y=c(1, 1,0.5))
ggsave(str_c(pathout5,"/", "Tax design_60 example.jpg"),
       width = 8.5,height = 8.5,dpi = 500)



# Figure: Revenue recycling scenarios design ============
#Coverage of social assistance #-----------
SPCover_Reg$Group <- as.numeric(substr(SPCover_Reg$Group,2,nchar(SPCover_Reg$Group)))/10

order.sp4 <- c("Cash","SP_current","SP_covid","Universal","PMT")
order.spname4 <- c("(S1) Cash-based programs", "(S2) Current social assistance",
                   "(S3) Social assistance COVID-19 expansion","(S4) Universal dividend",
                   "(S5) Proxy Means Test (PMT)")

Fig1 <- SPCover_Reg %>% 
  filter(SPScenario != "Null") %>% 
  ggplot()+
  geom_line(aes(Group, value, group = SPScenario, color = SPScenario),linewidth = 0.3)+
  geom_point(aes(Group, value, color = SPScenario), alpha=.7, size=0.8)+
  facet_wrap(.~Reg, nrow = 2,
             labeller = as_labeller(LABEL))+
  scale_x_continuous(breaks = 1:10)+
  scale_y_continuous(limits = c(0,100),breaks = seq(0,100,20),labels = str_c(seq(0,100,20),"%"))+
  scale_color_manual(name = "", values = wes_palette("Cavalcanti1", n = 5),
                     breaks = order.sp4,
                     labels = order.spname4)+
  labs(x = "Expenditure decile", y = "Coverage of social assistance")+
  theme_test(base_size = 11)+
  theme(strip.text = element_text(face = "bold"),legend.spacing.x = unit(0.4,"cm"),
        strip.background = element_rect(fill = "lavender", colour = "black", size = 1),
        legend.position = "bottom",legend.key.height = unit(22,"pt"),
        legend.key.width = unit(2,"pt"),
        axis.title = element_text(size = 14,face = "bold"))+
  guides(color = guide_legend(nrow = 2,byrow = T))

write.csv(SPCover_Reg %>% 
            filter(SPScenario != "Null") ,str_c(pathout5,"/", "SPCover_Reg.csv"))


#Climate fund #-----------
Reg_Fund.Share <- Reg_Fund/10^5
Reg_Fund <- as.data.frame(Reg_Fund)
Reg_Fund$Reg <- rownames(Reg_Fund)

Fig2 <- Reg_Fund %>% pivot_longer(-Reg,
                                  names_to = "Scenarios",
                                  values_to = "Fund") %>% 
  ggplot(aes(Reg,Scenarios,fill=Fund/10^3))+
  geom_tile(color="gray",size=0.1)+
  geom_label(aes(Reg,Scenarios,
                 label= format(round(Fund/10^3,2), nsmall = 2)),
             size = 3, fontface= "bold")+
  labs(y = "Principles of global\nclimate fund allocation",x = "")+
  scale_x_discrete(breaks = names(LABEL), labels = LABEL)+
  scale_y_discrete(limits = rev(order.gf), breaks = order.gf,labels = order.gfname)+
  scale_fill_gradient2(name="Fund recieved(+) or contributed(-) for\na $100 billion climate fund (billion$)", 
                       high=brewer.pal(3,"Dark2")[2], low=brewer.pal(4,"Dark2")[3],
                       mid = "white",midpoint = 0)+
  theme_test(base_size = 11)+
  theme(legend.position = "bottom",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(45,"pt"),
        axis.title = element_text(size = 14,face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1))

write.csv(Reg_Fund ,str_c(pathout5,"/", "Reg_Fund_100billion.csv"))

ggdraw()+ 
  draw_plot(Fig1, x=0, y=0.5, height = .49)+
  draw_plot(Fig2, x=0, y=0, height = .49)+
  draw_plot_label(label = c("a)", "b)"), size = 15, 
                  x=c(0, 0), y=c(1, 0.5))
ggsave(str_c(pathout5,"/", "Revenue recycling scenarios.jpg"),
       width = 8.5,height = 9,dpi = 500)


# Figure: Poverty and climate fund trade off--------
Tradeoff %>% 
  ggplot()+
  geom_line(aes(Size,ChgrateIPL),color = "black")+
  geom_segment(aes(x = -50, y = -0.4231, xend = 0, yend = -0.4231),color = "orange",linetype = "dotted",size=0.8)+
  geom_segment(aes(x = 0, y = -0.4231, xend = 0, yend = -0.61),color = "orange",linetype = "dotted",size=0.8)+
  
  geom_segment(aes(x = -50, y = -0.4904, xend = 50, yend = -0.4904),color = "orange",linetype = "dotted",size=0.8)+
  geom_segment(aes(x = 50, y = -0.4904, xend = 50, yend = -0.61),color = "orange",linetype = "dotted",size=0.8)+
  
  geom_segment(aes(x = -50, y = -0.5224, xend = 100, yend = -0.5224),color = "orange",linetype = "dotted",size=0.8)+
  geom_segment(aes(x = 100, y = -0.5224, xend = 100, yend = -0.61),color = "orange",linetype = "dotted",size=0.8)+
  
  geom_segment(aes(x = -50, y = -0.5952, xend = 600, yend = -0.5952),color = "red",linetype = "dotted",size=0.8)+
  geom_segment(aes(x = 600, y = -0.5952, xend = 600, yend = -0.61),color = "red",linetype = "dotted",size=0.8)+
  
  labs(x = "Global climate fund size (billion$)", y = str_c("Changes in global extreme poverty (%)"))+
  scale_y_continuous(limits = c(-0.61,-0.4),breaks = c(-0.5952,-0.55,-0.5224,-0.5,-0.4904,-0.4231,-0.4),
                     labels = str_c(c(-0.5952,-0.55,-0.5224,-0.5,-0.4904,-0.4231,-0.4)*100,"%"),expand = c(0,0))+
  scale_x_continuous(limits = c(-50,1050),breaks = c(50,100,seq(0,1000,200)),expand = c(0,0))+
  annotate("text",x = 118, y = -0.48, label = "marginal poverty reduction > 0", fontface = "bold", angle = -72.5,color = "orange")+
  annotate("text",x = 750, y = -0.585, label = "marginal poverty reduction = 0", fontface = "bold",color = "red")+
  theme_test(base_size = 12)+
  theme(axis.title = element_text(size = 13,face = "bold"))

write.csv(Tradeoff ,str_c(pathout5,"/", "Tradeoff_climate fund and poverty.csv"))

ggsave(str_c(pathout5,"/", "Scale international fund.jpg"),
       width = 5.5,height = 5,dpi = 500)


#National outcome under the best policy mix=======
OUT_TaxSAGF_20$IPL_Change_rate <- (OUT_TaxSAGF_20$PPop_ipl_tax - OUT_TaxSAGF_20$PPop_ipl_ori)/
  OUT_TaxSAGF_20$PPop_ipl_ori
OUT_TaxSAGF_20$NPL_Change_rate <- (OUT_TaxSAGF_20$PPop_npl_tax - OUT_TaxSAGF_20$PPop_npl_ori)/
  OUT_TaxSAGF_20$PPop_npl_ori
write.csv(OUT_TaxSAGF_20 ,str_c(pathout5,"/", "OUT_TaxSAGF_2degree.csv"))

OUT_TaxSAGF_15$IPL_Change_rate <- (OUT_TaxSAGF_15$PPop_ipl_tax - OUT_TaxSAGF_15$PPop_ipl_ori)/
  OUT_TaxSAGF_15$PPop_ipl_ori
OUT_TaxSAGF_15$NPL_Change_rate <- (OUT_TaxSAGF_15$PPop_npl_tax - OUT_TaxSAGF_15$PPop_npl_ori)/
  OUT_TaxSAGF_15$PPop_npl_ori
write.csv(OUT_TaxSAGF_15 ,str_c(pathout5,"/", "OUT_TaxSAGF_15degree.csv"))



#Some additional data need to export========
Nation_diff_c <- Tax_reg_C[,which(Tax_Level %in% 60)]
Nation_diff_p <- Tax_reg_P[,which(Tax_Level %in% 60)]
write.csv(Nation_diff_c ,str_c(pathout5,"/", "Nation_diff_taxlevel_consum_60.csv"))
write.csv(Nation_diff_p ,str_c(pathout5,"/", "Nation_diff_taxlevel_produc_60.csv"))
