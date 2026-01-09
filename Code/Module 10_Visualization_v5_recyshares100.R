#Module 9: Results summary and visualization
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu



Recy <- c(0.3,0.8,1)
Recynam <- c("30percent","80percent","100percent")
z=3#for 100percent

load(str_c(pathout5,"/Results summary",Recynam[z],".Rdata"))


#VA change by institution
apply(VA_Tot_Chg, c(1,3), sum)
t(apply(VA_Tot_Chg, c(1,3), sum))/colSums(apply(VA_Tot_Chg, c(1,3), sum))




order.tax <- c("Universal_P","CBDR_P","Universal_C","CBDR_C","Luxury","Luxury&CBDR_C")
order.taxname <- c("(T1) Global uniform\nproduction tax","(T2) Nation-differentiated\nproduction tax",
                   "(T3) Global uniform\nconsumption tax","(T4) Nation-differentiated\nconsumption tax",
                   "(T5) Luxury\nconsumption tax","(T6) Luxury & nation-\ndifferentiated consumption tax")
order.sp <- c("Null","Cash","SP_current","SP_covid","Universal","PerfectTargeted")
order.spname <- c("No revenue\nrecycling",
                  "(S1) Cash-based programs", "(S2) Current social\nassistance",
                  "(S3) Social assistance\nCOVID-19 expansion","(S4) Universal dividend"
                  ,"(S5) Poverty-focused\nrecycling")


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



nature_theme <- theme_minimal(base_size=9,base_family="Helvetica")+
  theme(panel.grid.major=element_line(size=.25,colour="grey85"),panel.grid.minor=element_blank(),
        strip.background=element_blank(),strip.text=element_text(face="bold",size=9),
        axis.title=element_text(face="bold",size=10),axis.text=element_text(size=8),
        legend.position="bottom",legend.title=element_blank(),legend.text=element_text(size=8),
        legend.key.width=unit(1.2,"cm"),legend.key.height=unit(0.35,"cm"),
        legend.box.margin=margin(t=0,r=5,b=2,l=5),plot.margin=margin(t=5.5,r=10,b=18,l=5.5))

tax_colors <- c("#0072B2","#D55E00","#009E73","#CC79A7","#999999")
tax_labels <- c("(T1/T3) Global uniform\nproduction/consumption tax","(T2) Nation-differentiated\nproduction tax","(T4) Nation-differentiated\nconsumption tax","(T5) Luxury consumption tax","(T6) Luxury & nation-differentiated\nconsumption tax")

Fig.a <- Data_Unevenburden_agg %>% filter(TaxScenario!="Null",TaxScenario!="Universal_C",Reg%in%Reg_dc) %>%
  ggplot()+geom_line(aes(Group,value,color=TaxScenario,group=TaxScenario),linewidth=.4)+
  geom_point(aes(Group,value,color=TaxScenario),size=.7,alpha=.9)+
  facet_grid(Vari~Reg,labeller=labeller(.cols=as_labeller(LABEL),.rows=as_labeller(LABEL2)),scales="free_y")+
  scale_x_continuous(breaks=1:10)+scale_y_continuous(limits=c(0,.12),breaks=seq(0,.14,0.03),labels=str_c(seq(0,14,3),"%"))+
  scale_color_manual("",values=tax_colors,breaks=order.tax[-3],labels=tax_labels)+
  labs(x="Expenditure decile",y="Effect, measured as % of expenditure")+nature_theme+
  guides(color=guide_legend(nrow=2,byrow=T))

Fig.b <- Data_Unevenburden_agg %>% filter(TaxScenario!="Null",TaxScenario!="Universal_C",Reg%in%Reg_ldc) %>%
  ggplot()+geom_line(aes(Group,value,color=TaxScenario,group=TaxScenario),linewidth=.4)+
  geom_point(aes(Group,value,color=TaxScenario),size=.7,alpha=.9)+
  facet_grid(Vari~Reg,labeller=labeller(.cols=as_labeller(LABEL),.rows=as_labeller(LABEL2)),scales="free_y")+
  scale_x_continuous(breaks=1:10)+scale_y_continuous(limits=c(0,.12),breaks=seq(0,.14,0.03),labels=str_c(seq(0,14,3),"%"))+
  scale_color_manual("",values=tax_colors,breaks=order.tax[-3],labels=tax_labels)+
  labs(x="Expenditure decile",y="Effect, measured as % of expenditure")+nature_theme+
  guides(color=guide_legend(nrow=2,byrow=T))

ggarrange(Fig.a,Fig.b,nrow=2,common.legend=T,legend="bottom")
ggsave(str_c(pathout5,"/Uneven burden by tax scenarios_50.jpg"),width=8.5,height=9.5,dpi=500)

write.csv(Data_Unevenburden_agg %>% filter(TaxScenario != "Null",TaxScenario != "Universal_C"),
          str_c(pathout5,"/", "Uneven burden by tax scenarios_Region level_50.csv"))


#Figure: Tax level required for climate goals; Poverty, ineq outcome under domestic policy mix#================
# 2.0 degree------
nature_theme_heat <- theme_minimal(base_size=10,base_family="Helvetica")+
  theme(panel.grid=element_blank(),
        strip.background=element_blank(),
        strip.text=element_text(face="bold",size=9,lineheight=1.05),
        strip.clip="off",                        
        axis.title=element_text(face="bold",size=12),
        axis.text=element_text(size=9),
        legend.position="top",
        legend.title=element_text(size=11,face="bold"),
        legend.text=element_text(size=9),
        legend.background=element_blank(),
        legend.key.width=unit(30,"pt"),
        legend.key.height=unit(8,"pt"),
        panel.spacing.y=unit(1,"lines"),
        plot.margin=margin(t=5.5,r=5.5,b=5.5,l=30))

Fig.1a <- GlobalOutcome_dome %>% filter(Goal=="2.0degree",Vari%in%c("NPLchg","IPLchg")) %>%
  mutate(Vari=factor(Vari,levels=c("IPLchg","NPLchg"))) %>%
  ggplot()+
  facet_wrap(.~Vari,nrow=1,
             labeller=as_labeller(c(IPLchg="Extreme (international) poverty\n(baseline poverty headcount: 718 million)",
                                    NPLchg="National poverty\n(baseline poverty headcount: 1442 million)")))+
  geom_tile(aes(SP,Tax,fill=PovertyRateLabel))+
  geom_label(aes(SP,Tax,label=str_c(format(round(PovertyRateLabel,2),nsmall=2),"%")),
             size=2.5,fontface="plain")+         
  labs(y="Tax scenarios",x="Domestic revenue recycling scenarios")+
  scale_x_discrete(limits=order.sp,labels=order.spname)+
  scale_y_discrete(limits=rev(order.tax),labels=rev(order.taxname))+
  scale_fill_gradient2(name="Changes in poverty headcount(%),\nlow-ambition revenue-neutral carbon tax",
                       high="#CC79A7",mid="white",low="#15616d",
                       midpoint=0,limits=c(-36,8),
                       breaks=seq(-35,7,7),labels=str_c(seq(-35,7,7),"%"))+
  nature_theme_heat+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1))
  
Fig.1b <- GlobalOutcome_dome %>% filter(Goal=="2.0degree",Vari%in%c("LocalGiniChg","InterGiniChg")) %>%
  mutate(Vari=factor(Vari,levels=c("LocalGiniChg","InterGiniChg"))) %>%
  ggplot()+
  facet_wrap(.~Vari,nrow=1,
             labeller=as_labeller(c(LocalGiniChg="Inequality within countries",
                                    InterGiniChg="Inequality between countries")))+
  geom_tile(aes(SP,Tax,fill=value*100))+
  geom_label(aes(SP,Tax,label=str_c(format(round(value*100,2),nsmall=2),"%")),
             size=2.5,fontface="plain")+       
  labs(y="Tax scenarios",x="Domestic revenue recycling scenarios")+
  scale_x_discrete(limits=order.sp,labels=order.spname)+
  scale_y_discrete(limits=rev(order.tax),labels=rev(order.taxname))+
  scale_fill_gradient2(name="Changes in Gini coefficient(%),\nlow-ambition revenue-neutral carbon tax",
                       high="#CC79A7",mid="white",low="#15616d",
                       midpoint=0,limits=c(-7.2,1.1),
                       breaks=seq(-7,1,1),labels=str_c(seq(-7,1,1),"%"))+
  nature_theme_heat+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1))

ggdraw()+ draw_plot(Fig.1a, x=0, y=0.5, height = .49)+
  draw_plot(Fig.1b, x=0, y=0, height = .49)+
  draw_plot_label(label = c("a)", "b)"), size = 15, 
                  x=c(0, 0), y=c(1, 0.5))
ggsave(str_c(pathout5,"/", "SA+TAX outcome_2degree",Recynam[z],".jpg"),width = 8.5,height = 8.5,dpi = 500)

write.csv(GlobalOutcome_dome  %>% filter(Goal %in% "2.0degree") %>% 
            filter(Vari %in% c("NPLchg","IPLchg","LocalGiniChg","InterGiniChg")) ,
                   str_c(pathout5,"/", "SA+TAX outcome_2degree",Recynam[z],".csv"))

# 1.5 degree------------
Fig.1a <- GlobalOutcome_dome %>% filter(Goal=="1.5degree",Vari%in%c("NPLchg","IPLchg")) %>%
  mutate(Vari=factor(Vari,levels=c("IPLchg","NPLchg"))) %>%
  ggplot()+
  facet_wrap(.~Vari,nrow=1,
             labeller=as_labeller(
               c(IPLchg="Extreme (international) poverty\n(baseline poverty headcount: 718 million)",
                 NPLchg="National poverty\n(baseline poverty headcount: 1442 million)")
             ))+
  geom_tile(aes(SP,Tax,fill=PovertyRateLabel))+
  geom_label(aes(SP,Tax,label=str_c(format(round(PovertyRateLabel,2),nsmall=2),"%")),
             size=2.5,fontface="plain")+
  labs(y="Tax scenarios",x="Domestic revenue recycling scenarios")+
  scale_x_discrete(limits=order.sp,labels=order.spname)+
  scale_y_discrete(limits=rev(order.tax),labels=rev(order.taxname))+
  scale_fill_gradient2(name="Changes in poverty headcount(%),\nhigh-ambition revenue-neutral carbon tax",
                       high="#CC79A7",mid="white",low="#15616d",
                       midpoint=0,limits=c(-45,11),
                       breaks=seq(-50,10,10),labels=str_c(seq(-50,10,10),"%"))+
  nature_theme_heat+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1))

Fig.1b <- GlobalOutcome_dome %>% filter(Goal=="1.5degree",Vari%in%c("LocalGiniChg","InterGiniChg")) %>%
  mutate(Vari=factor(Vari,levels=c("LocalGiniChg","InterGiniChg"))) %>%
  ggplot()+
  facet_wrap(.~Vari,nrow=1,
             labeller=as_labeller(
               c(LocalGiniChg="Inequality within countries",
                 InterGiniChg="Inequality between countries")
             ))+
  geom_tile(aes(SP,Tax,fill=value*100))+
  geom_label(aes(SP,Tax,label=str_c(format(round(value*100,2),nsmall=2),"%")),
             size=2.5,fontface="plain")+
  labs(y="Tax scenarios",x="Domestic revenue recycling scenarios")+
  scale_x_discrete(limits=order.sp,labels=order.spname)+
  scale_y_discrete(limits=rev(order.tax),labels=rev(order.taxname))+
  scale_fill_gradient2(name="Changes in Gini coefficient(%),\nhigh-ambition revenue-neutral carbon tax",
                       high="#CC79A7",mid="white",low="#15616d",
                       midpoint=0,limits=c(-14,3),
                       breaks=seq(-12,3,3),labels=str_c(seq(-12,3,3),"%"))+
  nature_theme_heat+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1))

ggdraw()+ draw_plot(Fig.1a, x=0, y=0.5, height = .49)+
  draw_plot(Fig.1b, x=0, y=0, height = .49)+
  draw_plot_label(label = c("a)", "b)"), size = 15, 
                  x=c(0, 0), y=c(1, 0.5))
ggsave(str_c(pathout5,"/", "SA+TAX outcome_15degree",Recynam[z],".jpg"),width = 8.5,height = 8.5,dpi = 500)

write.csv(GlobalOutcome_dome  %>% filter(Goal %in% "1.5degree") %>% 
            filter(Vari %in% c("NPLchg","IPLchg","LocalGiniChg","InterGiniChg")) ,
          str_c(pathout5,"/", "SA+TAX outcome_1.5degree",Recynam[z],".csv"))



# required tax level------------
Fig.T1 <- GlobalOutcome_dome %>% filter(Vari=="TaxLevel") %>% ggplot()+
  geom_tile(aes(SP,Tax,fill=value))+
  geom_label(aes(SP,Tax,label=str_c(value,", ",format(round(TaxGDPShareLabel,1),nsmall=1),"%")),
             size=2.5,fontface="plain",label.size=0.1,label.padding=unit(0.1,"lines"))+
  facet_wrap(.~Goal,nrow=1,
             labeller=as_labeller(c(`1.5degree`="Global average tax level aligned with\nhigh-ambition mitigation ($/tCO\u2082, revenue/GDP)",
                                    `2.0degree`="Global average tax level aligned with\nlow-ambition mitigation ($/tCO\u2082, revenue/GDP)")))+
  labs(y="Tax scenarios",x="Domestic revenue recycling scenarios")+
  scale_x_discrete(limits=order.sp,labels=order.spname)+
  scale_y_discrete(limits=rev(order.tax),labels=rev(order.taxname))+
  scale_fill_gradient(low="orange3",high="red4",guide=NULL)+
  nature_theme_heat+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1))

write.csv(GlobalOutcome_dome  %>% filter(Vari %in% "TaxLevel") ,
          str_c(pathout5,"/", "SA+TAX required tax level",Recynam[z],".csv"))

# By tax level global outcome ---------
order.taxname2 <- c("(T1) Global uniform\nproduction tax","(T2) Nation-differentiated\nproduction tax",
                    "(T3) Global uniform\nconsumption tax","(T4) Nation-differentiated\nconsumption tax",
                    "(T5) Luxury consumption tax","(T6) Luxury & nation-\ndifferentiated consumption tax")
order.sp2 <- c("Null","Cash","SP_current","SP_covid","Universal","PerfectTargeted")
order.spname2 <- c("No revenue recycling",
                   "(S1) Cash-based programs", "(S2) Current social assistance",
                   "(S3) Social assistance\nCOVID-19 expansion","(S4) Universal dividend"
                   ,"(S5) Poverty-focused\nrecycling")

nature_theme_scatter <- theme_minimal(base_size=10,base_family="Helvetica")+
  theme(panel.grid.major=element_line(size=.25,colour="grey85"),
        panel.grid.minor=element_blank(),
        axis.title=element_text(face="bold",size=12),
        axis.text=element_text(size=10),
        legend.title=element_text(face="bold",size=10),
        legend.text=element_text(size=9),
        legend.background=element_blank(),
        plot.margin=margin(5.5,5.5,5.5,5.5))

sp_cols <- c("black","#0072B2","#D55E00","#009E73","#CC79A7","#999999")  # No recycling + S1–S5

Fig.T2 <- Data_AllTaxLevel %>% filter(TaxLevel%in%seq(0,180,10),Tax!="Null") %>%
  pivot_wider(id_cols=c(Tax,TaxLevel,SP),names_from=Vari) %>%
  ggplot()+
  geom_vline(xintercept=CO2_Response_Recy[7,1,1],colour="black",linetype="dashed",linewidth=.6,alpha=.7)+
  geom_vline(xintercept=CO2_Response_Recy[7,1,1]*0.96,colour="black",linetype="dashed",linewidth=.6,alpha=.7)+
  geom_vline(xintercept=CO2_Response_Recy[7,1,1]*0.92,colour="black",linetype="dashed",linewidth=.6,alpha=.7)+
  geom_hline(yintercept=sum(OUT_TAXSP[,4,7,2,1])/1e6,colour="purple",linetype="dotdash",linewidth=.6,alpha=.7)+
  annotate("text",x=25100,y=730,label="Baseline",colour="purple",size=3)+
  geom_point(aes(CO2_Effect,IPL_Effect,colour=SP,shape=Tax,size=TaxLevel),
             alpha=.85,stroke=.4)+
  scale_shape_manual("Tax scenarios",values=c(0,1,2,4,5,6),
                     breaks=order.tax,labels=order.taxname2)+
  scale_color_manual("Domestic revenue\nrecycling scenarios",values=sp_cols,
                     breaks=order.sp2,labels=order.spname2)+
  scale_size_continuous("Tax level (US$/tCO\u2082)",breaks=seq(0,100,25),range=c(.8,4.5))+
  scale_x_continuous(breaks=c(25000,27000,CO2_Response_Recy[7,1,1]*0.92,29000,
                              CO2_Response_Recy[7,1,1]*0.96,CO2_Response_Recy[7,1,1]),
                     labels=c(25,27,"28.25(-8%)\nhigh ambition",29,"29.48(-4%)\nlow ambition","30.71(0%)"))+
  scale_y_continuous(limits=c(330,890),breaks=seq(300,1200,50))+
  labs(x="Carbon emissions (GT)",y="Extreme poverty headcount (million)")+
  nature_theme_scatter+
  theme(legend.position="right",legend.key.height=unit(16,"pt"),legend.key.width=unit(6,"pt"))+
  guides(shape=guide_legend(order=1),size =guide_legend(order=2,nrow=1,byrow=TRUE),
         color=guide_legend(order=3))

ggdraw()+ 
  draw_plot(Fig.T2, x=0, y=0.4, height = .59)+draw_plot(Fig.T1, x=0, y=0, height = .39)+
  draw_plot_label(label = c("a)", "b)"), size = 15, x=c(0, 0), y=c(1, 0.4))
ggsave(str_c(pathout5,"/", "Global CO2 and IPL outcome_all results and required tax",Recynam[z],".jpg"),
       width = 9,height = 9.5,dpi = 500)

write.csv(Data_AllTaxLevel %>% filter(TaxLevel%in%seq(0,180,10),Tax!="Null"),
          str_c(pathout5,"/", "Global CO2 and IPL outcome_all results",Recynam[z],".csv"))



#Figure: International policy mix#================
order.sp3 <- c("SP_covid","Universal","PerfectTargeted")
order.spname3 <- c("(T5) Luxury consumption tax +\n(S3) Social assistance\nCOVID-19 expansion",
                   "(T5) Luxury consumption tax +\n(S4) Universal dividend"
                   ,"(T5) Luxury consumption tax +\n(S5) Poverty-focused recycling")

order.gf <- c("His.PoverPop","His.Pop","His.PoverGap",
              "Curren.PoverPop","Curren.Pop","Curren.PoverGap")
order.gfname <- c("(G1) Historical emissions\n& poverty headcount",
                  "(G2) Historical emissions\n& population",
                  "(G3) Historical emissions\n& poverty gap",
                  "(G4) Current emissions\n& poverty headcount",
                  "(G5) Current emissions\n& population",
                  "(G6) Current emissions\n& poverty gap")

#---------2 degree----------
Fig.1a <- GlobalOutcome_GloRec %>% filter(Goal=="2.0degree",Vari%in%c("NPLchg","IPLchg")) %>%
  mutate(Vari=factor(Vari,levels=c("IPLchg","NPLchg"))) %>% ggplot()+
  facet_wrap(~Vari,nrow=1,labeller=as_labeller(c(IPLchg="Extreme (international) poverty\n(baseline poverty headcount: 718 million)",
                                                 NPLchg="National poverty\n(baseline poverty headcount: 1442 million)")))+
  geom_tile(aes(SP,GF,fill=PovertyRateLabel))+
  geom_label(aes(SP,GF,label=str_c(round(PovertyRateLabel,2),"%")),size=2.6,fontface="plain",label.size=0)+
  scale_x_discrete(limits=order.sp3,labels=order.spname3)+
  scale_y_discrete(limits = rev(order.gf), breaks = rev(order.gf),labels = rev(order.gfname))+
  scale_fill_gradient2(name="Changes in poverty headcount(%),\nlow-ambition revenue-neutral carbon tax",
                       high="#CC79A7",mid="white",low="#15616d",midpoint=0,
                       limits=c(-72,2),breaks=seq(-70,0,20),labels=str_c(seq(-70,0,20),"%"))+
  labs(x="Carbon tax +Domestic revenue recycling",y="International revenue recycling")+
  theme(legend.position="top")+
  nature_theme_heat+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1))


Fig.1b <- GlobalOutcome_GloRec %>% filter(Goal=="2.0degree",Vari%in%c("LocalGiniChg","InterGiniChg")) %>%
  mutate(Vari=factor(Vari,levels=c("LocalGiniChg","InterGiniChg"))) %>% ggplot()+
  facet_wrap(~Vari,nrow=1,labeller=as_labeller(c(LocalGiniChg="Inequality within countries",InterGiniChg="Inequality between countries")))+
  geom_tile(aes(SP,GF,fill=value*100))+
  geom_label(aes(SP,GF,label=str_c(round(value*100,2),"%")),size=2.6,fontface="plain",label.size=0)+
  scale_x_discrete(limits=order.sp3,labels=rev(order.spname3))+
  scale_y_discrete(limits = rev(order.gf), breaks = rev(order.gf),labels = rev(order.gfname))+
  scale_fill_gradient2(name="Changes in Gini coefficient(%),\nlow-ambition revenue-neutral carbon tax",
                       high="#CC79A7",mid="white",low="#15616d",midpoint=0,
                       limits=c(-10.2,0),breaks=seq(-10,0,2),labels=str_c(seq(-10,0,2),"%"))+
  labs(x="Carbon tax +Domestic revenue recycling",y="International revenue recycling")+
  theme(legend.position="top")+
  nature_theme_heat+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1))

ggdraw()+ 
  draw_plot(Fig.1a, x=0, y=0.5, height = .49)+
  draw_plot(Fig.1b, x=0, y=0, height = .49)+
  draw_plot_label(label = c("a)", "b)"), size = 15, x=c(0, 0), y=c(1, 0.5))
ggsave(str_c(pathout5,"/", "Luxury_SA+GF outcome_2degree",Recynam[z],".jpg"),
       width = 8.5,height = 9,dpi = 500)

write.csv(GlobalOutcome_GloRec  %>% filter(Goal %in% "2.0degree") %>% 
            filter(Vari %in% c("NPLchg","IPLchg","LocalGiniChg","InterGiniChg")) ,
          str_c(pathout5,"/", "Luxury_SA+GF outcome_2degree",Recynam[z],".csv"))


#---------1.5 degree----------
Fig.1a <- GlobalOutcome_GloRec %>% filter(Goal=="1.5degree",Vari%in%c("NPLchg","IPLchg")) %>% 
  mutate(Vari=factor(Vari,levels=c("IPLchg","NPLchg"))) %>% 
  ggplot()+facet_wrap(~Vari,nrow=1,
                      labeller=as_labeller(c(IPLchg="Extreme (international) poverty\n(baseline poverty headcount: 718 million)",
                                             NPLchg="National poverty\n(baseline poverty headcount: 1442 million)"))) +
  geom_tile(aes(SP,GF,fill=PovertyRateLabel))+
  geom_label(aes(SP,GF,label=str_c(round(PovertyRateLabel,2),"%")),size=2.6,label.size=0)+
  scale_x_discrete(limits=order.sp3,labels=order.spname3)+
  scale_y_discrete(limits = rev(order.gf), breaks = rev(order.gf),labels = rev(order.gfname))+
  scale_fill_gradient2(name="Changes in poverty headcount(%),\nhigh-ambition revenue-neutral carbon tax", 
                       high="#CC79A7",mid="white",low="#15616d",midpoint=0,
                       limits = c(-76,-20),
                       breaks = seq(-75,-25,10),
                       labels = str_c(seq(-75,-25,10),"%"))+
  labs(x="Carbon tax +Domestic revenue recycling",y="International revenue recycling")+
  theme(legend.position="top")+
  nature_theme_heat+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1))

Fig.1b <- GlobalOutcome_GloRec %>% filter(Goal=="1.5degree",Vari%in%c("LocalGiniChg","InterGiniChg")) %>% 
  mutate(Vari=factor(Vari,levels=c("LocalGiniChg","InterGiniChg"))) %>% 
  ggplot()+facet_wrap(~Vari,nrow=1,
                      labeller=as_labeller(c(LocalGiniChg="Inequality within countries",InterGiniChg="Inequality between countries"))) +
  geom_tile(aes(SP,GF,fill=value*100))+
  geom_label(aes(SP,GF,label=str_c(round(value*100,2),"%")),size=2.6,label.size=0)+
  scale_x_discrete(limits=order.sp3,labels=order.spname3)+
  scale_y_discrete(limits = rev(order.gf), breaks = rev(order.gf),labels = rev(order.gfname))+
  scale_fill_gradient2(name="Changes in Gini coefficient(%),\nhigh-ambition revenue-neutral carbon tax", 
                       high="#CC79A7",mid="white",low="#15616d",midpoint=0,
                       limits = c(-21,1),
                       breaks = seq(-20,0,5),labels = str_c(seq(-20,0,5),"%"))+
  labs(x="Carbon tax +Domestic revenue recycling",y="International revenue recycling")+
  theme(legend.position="top")+
  nature_theme_heat+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1))

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
             labeller = as_labeller(c(`1.5degree`="Global average tax level aligned with\nhigh-ambition mitigation ($/tCO\u2082, revenue/GDP)",
                                      `2.0degree`="Global average tax level aligned with\nlow-ambition mitigation ($/tCO\u2082, revenue/GDP)")))+
  labs(y = "International revenue recycling", x = "Carbon tax + Domestic revenue recycling")+
  scale_x_discrete(limits = order.sp3,labels = order.spname3)+
  scale_y_discrete(limits = rev(order.gf), breaks = order.gf,labels = order.gfname)+
  theme_minimal(base_line_size = 1,base_size = 12)+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1),
        strip.text = element_text(face = "bold"),
        axis.title = element_text(size = 14,face = "bold"))+
  nature_theme_heat+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1))
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
theme_nat <- theme_classic(base_size=12)+
  theme(panel.grid.major.y=element_line(colour="grey90",linewidth=.3),
        axis.text.x=element_text(angle=90,vjust=.5,hjust=1),
        legend.position="bottom",
        legend.text=element_text(size=8),
        legend.key.height=unit(9,"pt"),
        legend.key.width=unit(12,"pt"),
        axis.title=element_text(size=13,face="bold"))

full_labs <- c("(T5) Luxury consumption tax without recycling",
               "(T5+S3) Luxury consumption tax+\nSocial assistance COVID-19 expansion",
               "(T5+S3+G1) Luxury consumption tax+\nSocial assistance COVID-19 expansion+\nGlobal fund: historical emissions & poverty headcount")

Fig.1a <- Reg_Outcome_20_Figdata %>% 
  filter(Scenario!="Null",Vari %in% c("PPop_ipl_chgrate","PPop_npl_chgrate")) %>%
  ggplot(aes(Reg,value,color=Scenario,shape=Scenario))+
  geom_hline(yintercept=0,linetype="dashed",colour="grey60",linewidth=.5)+
  geom_point(size=3,alpha=.75,stroke=.3)+
  facet_wrap(~Vari,ncol=1,labeller=as_labeller(c(PPop_ipl_chgrate="Extreme (international) poverty",
                                                 PPop_npl_chgrate="National poverty")))+
  scale_x_discrete(limits=regorder,labels=reglabel)+
  scale_y_continuous(breaks=seq(-.8,.1,.2),labels=scales::percent)+
  scale_color_manual(values=c("#D55E00","#009E73","#CC79A7"),name=NULL,labels=full_labs)+
  scale_shape_manual(values=c(15,16,17),name=NULL,labels=full_labs)+
  labs(x=NULL,y="Changes in poverty headcount (%)")+
  theme_nat+
  guides(color = guide_legend(nrow = 1,byrow = T))

Fig.1b <- Reg_Outcome_20_Figdata %>%
  filter(Scenario!="Null",Vari=="PPop_Gini_chgrate") %>%
  ggplot(aes(Reg,value,color=Scenario,shape=Scenario))+
  geom_hline(yintercept=0,linetype="dashed",colour="grey60",linewidth=.5)+
  geom_point(size=3,alpha=.75,stroke=.3)+
  facet_wrap(~Vari,labeller=as_labeller(c(PPop_Gini_chgrate="Inequality within countries")))+
  scale_x_discrete(limits=regorder,labels=reglabel)+
  scale_y_continuous(breaks=seq(-.04,.01,.01),labels=scales::percent)+
  scale_color_manual(values=c("#D55E00","#009E73","#CC79A7"),name=NULL,labels=full_labs)+
  scale_shape_manual(values=c(15,16,17),name=NULL,labels=full_labs)+
  labs(x=NULL,y="Changes in local Gini coefficient (%)")+
  theme_nat+
  guides(color = guide_legend(nrow = 1,byrow = T))

ggarrange(Fig.1a,Fig.1b,common.legend=T,legend="bottom")
ggsave(str_c(pathout5,"/", "Regional outcome under best global policy mix_2degree",Recynam[z],".jpg"),
       width = 8.5,height = 5.5,dpi = 500)

write.csv(Reg_Outcome_20_Figdata %>% 
            filter(Scenario != "Null") %>% 
            filter(Vari %in% c("PPop_ipl_chgrate","PPop_npl_chgrate","PPop_Gini_chgrate")),
          str_c(pathout5,"/", "Regional outcome under best global policy mix_2degree",Recynam[z],".csv"))

#---------1.5 degree----------
Fig.1a <- Reg_Outcome_15_Figdata %>%
  filter(Scenario != "Null", Vari %in% c("PPop_ipl_chgrate","PPop_npl_chgrate")) %>%
  ggplot(aes(Reg, value, color = Scenario, shape = Scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = .5) +
  geom_point(size = 3, alpha = .75, stroke = .3) +
  facet_wrap(~Vari, ncol = 1,
             labeller = as_labeller(c(PPop_ipl_chgrate = "Extreme (international) poverty",
                                      PPop_npl_chgrate = "National poverty"))) +
  scale_x_discrete(limits = regorder, labels = reglabel) +
  scale_y_continuous(breaks = seq(-1, 0.4, 0.2), labels = scales::percent) +
  scale_color_manual(values=c("#D55E00","#009E73","#CC79A7"),name=NULL,labels=full_labs)+
  scale_shape_manual(values = c(15,16,17), name = NULL, labels = full_labs) +
  labs(x = NULL, y = "Changes in poverty headcount (%)") +
  theme_nat +
  guides(color = guide_legend(nrow = 1,byrow = T))

Fig.1b <- Reg_Outcome_15_Figdata %>%
  filter(Scenario != "Null", Vari == "PPop_Gini_chgrate") %>%
  ggplot(aes(Reg, value, color = Scenario, shape = Scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = .5) +
  geom_point(size = 3, alpha = .75, stroke = .3) +
  facet_wrap(~Vari,
             labeller = as_labeller(c(PPop_Gini_chgrate = "Inequality within countries"))) +
  scale_x_discrete(limits = regorder, labels = reglabel) +
  scale_y_continuous(breaks = seq(-0.25, 0.05, 0.05), labels = scales::percent) +
  scale_color_manual(values=c("#D55E00","#009E73","#CC79A7"),name=NULL,labels=full_labs)+
  scale_shape_manual(values = c(15,16,17), name = NULL, labels = full_labs) +
  labs(x = NULL, y = "Changes in local Gini coefficient (%)") +
  theme_nat +
  guides(color = guide_legend(nrow = 1,byrow = T))

ggarrange(Fig.1a, Fig.1b, common.legend = TRUE, legend = "bottom")
ggsave(str_c(pathout5,"/", "Regional outcome under best global policy mix_15degree",Recynam[z],".jpg"),
       width = 8.5,height = 5.5,dpi = 500)
write.csv(Reg_Outcome_15_Figdata %>% 
            filter(Scenario != "Null") %>% 
            filter(Vari %in% c("PPop_ipl_chgrate","PPop_npl_chgrate","PPop_Gini_chgrate")),
          str_c(pathout5,"/", "Regional outcome under best global policy mix_15degree",Recynam[z],".csv"))


# Figure: Tax scenarios design ============
regorder2 <- unique(CBDR$Region)[order(CBDR$Tax_C)]
reglabel2 <- regorder2
reglabel2[which(regorder2 %in% "Sub-Saharan Africa")] <- "Sub-Saharan\nAfrica"
reglabel2[which(regorder2 %in% "Latin America and Caribbean")] <- "Latin America\nand Caribbean"
reglabel2[which(regorder2 %in% "Asia-Pacific Developed")] <- "Asia-Pacific\nDeveloped"
reglabel2[which(regorder2 %in% "East Asia and Developing Pacific")] <- "East Asia and\nDeveloping Pacific"

#a. Tax level under CBDR_P and CBDR_P#-----------
Fig.1a <- CBDR %>% pivot_longer(-Region) %>% 
  ggplot()+ geom_col(aes(Region,value,fill=name),position="dodge",width=.65)+
  geom_hline(yintercept=50,linetype="dashed",color="gray40",linewidth=.6)+
  coord_flip()+ labs(x="",y="Tax level (US$/tCO\u2082)")+
  scale_x_discrete(limit=regorder2,labels=reglabel2)+
  scale_y_continuous(breaks = seq(0,100,25))+
  scale_fill_manual("Principles",
                    limits=c("Tax_P","Tax_C"),
                    labels=c("(T2) Nation-differentiated\nproduction tax","(T4) Nation-differentiated\nconsumption tax"),
                    values=c("#264653","#8ecae6"))+
  theme_classic(base_size=10)+
  theme(legend.position=c(.72,.1),legend.title=element_text(size=9,face="bold"),
        axis.title=element_text(size=11,face="bold"),
        axis.text=element_text(size=9),
        legend.text=element_text(size=8),
        legend.background=element_blank())

write.csv(CBDR,str_c(pathout5,"/", "CBDR_taxlevel_50.csv"))

#b. Tax level by sectors under luxury.#-----------
Luxury <- as.data.frame(Luxury)
Sec_label <- rep(NA,length(Luxury$Secnam))
Sec_label[match(c("oil","coa","wht","gro","ocr","c_b","pdr",
                  "ros","afs","otn","ins","mvh","atp"),
                Luxury$Secnam)] <- c("Oil","Coal","Wheat","Cereal grains nec","Crops nec","Sugar cane, sugar beet","Paddy rice",
                                     "Recreational and other services","Accommodation, Food and service activities","Transport equipment nec","Insurance",
                                     "Motor vehicles and parts","Air transport")

Fig.1b <- Luxury %>% ggplot()+
  geom_point(aes(Elas,TaxRate),size=3,alpha=.75,color="#8ecae6")+
  geom_text_repel(aes(Elas,TaxRate,label=Sec_label),
                  size=2.1,segment.colour="grey60",max.iter=3e3,force=5)+
  geom_vline(xintercept=1,linetype="dashed",color="gray40",linewidth=.6)+
  geom_hline(yintercept=50,linetype="dashed",color="gray40",linewidth=.6)+
  annotate("text", x = 0.7, y = 54, label = "above average tax",size = 3,fontface = "bold")+
  annotate("text", x = 0.7, y = 46, label = "below average tax",size = 3,fontface = "bold")+
  labs(x="Expenditure elasticity",y="Tax level (US$/tCO\u2082)")+
  theme_classic(base_size=10)+
  theme(axis.title=element_text(size=11,face="bold"),
        axis.text=element_text(size=9))

write.csv(Luxury,str_c(pathout5,"/", "Luxury_taxlevel_50.csv"))

#c. Tax revenue as share of GDP-------
order.tax2 <- c("Universal_P","CBDR_P","Universal_C","CBDR_C","Luxury","Luxury&CBDR")
order.taxname2 <- c("(T1) Global uniform\nproduction tax","(T2) Nation-differentiated\nproduction tax",
                    "(T3) Global uniform\nconsumption tax","(T4) Nation-differentiated\nconsumption tax",
                    "(T5) Luxury\nconsumption tax","(T6) Luxury & nation-\ndifferentiated consumption tax")

Fig.1c <- RevenueAnalysis %>% filter(Vari=="RevenueShare") %>% 
  ggplot()+ geom_tile(aes(Reg,Type,fill=value),color="white",linewidth=.4)+
  geom_text(aes(Reg,Type,label=str_c(format(round(value*100,2),nsmall=2),"%")),
            size=2.4)+ labs(y="Tax scenarios",x="")+
  scale_y_discrete(limit=rev(order.tax2),labels=rev(order.taxname2))+
  scale_x_discrete(limit=rev(regorder2),labels=rev(reglabel2))+
  scale_fill_gradient2("Tax revenue as share of GDP (%)",
                       mid="#fef7ec",low="white",high="#15616d",midpoint=.025,
                       labels=str_c(seq(0,5,1),"%"),breaks=seq(0,.05,.01))+
  theme_classic(base_size=10)+
  theme(legend.position="bottom",
        legend.title=element_text(size=10,face="bold"),
        legend.text=element_text(size=9),
        axis.title=element_text(size=11,face="bold"),
        axis.text.x=element_text(angle=90,vjust=.5,hjust=1))+
  theme(legend.key.height = unit(10,"pt"),legend.key.width = unit(45,"pt"))+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1))

write.csv(RevenueAnalysis %>% filter(Vari %in% "RevenueShare"),str_c(pathout5,"/", "RevenueAnalysis_50.csv"))

ggdraw()+ 
  draw_plot(Fig.1a, x=0, y=0.5,width = 0.49, height = 0.49)+
  draw_plot(Fig.1b, x=0.5, y=0.5,width = 0.49, height = 0.49)+
  draw_plot(Fig.1c, x=0, y=0,width = 1, height = 0.49)+
  draw_plot_label(label = c("a)", "b)", "c)"), size = 13,x=c(0, 0.5,0), y=c(1, 1,0.5))
ggsave(str_c(pathout5,"/", "Tax design_50 example.jpg"),
       width = 8,height = 8,dpi = 500)


# Figure: Revenue recycling scenarios design ============
#Coverage of social assistance #-----------
SPCover_Reg$Group <- as.numeric(substr(SPCover_Reg$Group,2,nchar(SPCover_Reg$Group)))/10

order.sp4 <- c("Cash","SP_current","SP_covid","Universal","PerfectTargeted","PerfectTargeted20","PerfectTargeted50")
order.spname4 <- c("(S1) Cash-based programs", "(S2) Current social assistance",
                   "(S3) Social assistance COVID-19 expansion","(S4) Universal dividend",
                   "(S5) Poverty-focused\nrecycling (US$10/tCO2)",
                   "(S5) Poverty-focused\nrecycling (US$20/tCO2)",
                   "(S5) Poverty-focused\nrecycling (US$50/tCO2)")

Fig1 <- SPCover_Reg %>% 
  filter(SPScenario != "Null") %>% 
  ggplot()+
  geom_line(aes(Group, value, group = SPScenario, color = SPScenario),linewidth = 0.3)+
  geom_point(aes(Group, value, color = SPScenario), alpha=.8, size=0.5)+
  facet_wrap(.~Reg, nrow = 2,
             labeller = as_labeller(LABEL))+
  scale_x_continuous(breaks = 1:10)+
  scale_y_continuous(limits = c(0,100),breaks = seq(0,100,20),labels = str_c(seq(0,100,20),"%"))+
  scale_color_manual(name = "", values = wes_palette("Zissou1", n = 7, type = "continuous"),
                     breaks = order.sp4,
                     labels = order.spname4)+
  labs(x = "Expenditure decile", y = "Coverage of social assistance")+
  theme_test(base_size = 11)+
  guides(color = guide_legend(nrow = 2,byrow = T))+
  nature_theme_heat+
  theme(strip.text = element_text(face = "bold"),legend.spacing.x = unit(0.4,"cm"),
        legend.position = "bottom",legend.key.height = unit(22,"pt"),
        legend.key.width = unit(2,"pt"),
        axis.title = element_text(size = 14,face = "bold"))

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
                 label= format(round(Fund/10^3,2), nsmall = 2)),size = 3)+
  labs(y = "Principles of global\ncarbon revenue fund allocation",x = "")+
  scale_x_discrete(breaks = names(LABEL), labels = LABEL)+
  scale_y_discrete(limits = rev(order.gf), breaks = order.gf,labels = order.gfname)+
  scale_fill_gradient2(name="Fund recieved(+) or contributed(-) for\na $100 billion carbon revenue fund (billion$)", 
                       high="#D55E00",mid="white",low="#0072B2",midpoint = 0)+
  theme_test(base_size = 11)+
  theme(legend.position = "bottom",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(45,"pt"),
        axis.title = element_text(size = 14,face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1))+
  nature_theme_heat+
  theme(axis.text.x = element_text(angle = -325, vjust = 1,hjust=1))

write.csv(Reg_Fund ,str_c(pathout5,"/", "Reg_Fund_100billion.csv"))

ggdraw()+ 
  draw_plot(Fig1, x=0, y=0.5, height = .49)+
  draw_plot(Fig2, x=0, y=0, height = .49)+
  draw_plot_label(label = c("a)", "b)"), size = 15, 
                  x=c(0, 0), y=c(1, 0.5))
ggsave(str_c(pathout5,"/", "Revenue recycling scenarios.jpg"),
       width = 9,height = 9,dpi = 500)


# Figure: Poverty and climate fund trade off--------

Tradeoff %>% filter(Size %in% c(0,50,100,300,500,700,800,1000))

Tradeoff %>%
  ggplot(aes(Size, ChgrateIPL)) +
  geom_line(color = "black", linewidth = 0.6) +
  # geom_smooth(se=FALSE, span=0.25, color="black", linewidth=0.7, method="loess") +
  geom_segment(aes(x = -50, y = -0.2208, xend = 0, yend = -0.2208),color = "orange",linetype = "dotted",size=0.5)+
  geom_segment(aes(x = 0, y = -0.2208, xend = 0, yend = -0.6),color = "orange",linetype = "dotted",size=0.5)+
  
  geom_segment(aes(x = -50, y = -0.3052, xend = 50, yend = -0.3052),color = "orange",linetype = "dotted",size=0.5)+
  geom_segment(aes(x = 50, y = -0.3052, xend = 50, yend = -0.6),color = "orange",linetype = "dotted",size=0.5)+
  
  geom_segment(aes(x = -50, y = -0.3672, xend = 100, yend = -0.3672),color = "orange",linetype = "dotted",size=0.5)+
  geom_segment(aes(x = 100, y = -0.3672, xend = 100, yend = -0.6),color = "orange",linetype = "dotted",size=0.5)+
  
  geom_segment(aes(x = -50, y = -0.5010, xend = 300, yend = -0.5010),color = "orange",linetype = "dotted",size=0.5)+
  geom_segment(aes(x = 300, y = -0.5010, xend = 300, yend = -0.6),color = "orange",linetype = "dotted",size=0.5)+
  
  # geom_segment(aes(x = -50, y = -0.5325, xend = 500, yend = -0.5325),color = "red",linetype = "dotted",size=0.5)+
  # geom_segment(aes(x = 500, y = -0.5325, xend = 500, yend = -0.6),color = "red",linetype = "dotted",size=0.5)+
  
  geom_segment(aes(x = -50, y = -0.5617, xend = 700, yend = -0.5617),color = "red",linetype = "dotted",size=0.5)+
  geom_segment(aes(x = 700, y = -0.5617, xend = 700, yend = -0.6),color = "red",linetype = "dotted",size=0.5)+
  labs(x = "Global carbon revenue fund size (billion$)",
       y = "Changes in global extreme poverty (%)") +
  scale_y_continuous( limits = c(-0.6, -0.25),
    breaks = c(-0.5617,-0.5010,-0.3672,-0.3052,-0.2208),
    labels = str_c(c(-0.5617,-0.5010,-0.3672,
                     -0.3052,-0.2208)*100,"%"),expand = c(0,0)) +
  scale_x_continuous(limits = c(-50,1020),
    breaks = c(50,100,300,500,seq(0,1000,100)), expand = c(0,0) ) +
  annotate("text", x = 200, y = -0.38, label = "marginal poverty reduction > 0",
           fontface = "bold", angle = -65, color = "#f4a259", size = 3) +
  annotate("text", x = 700, y = -0.545,label = "marginal poverty reduction = 0",
           fontface = "bold", color = "#d62828", size = 3) +
  theme_classic(base_size = 11) +
  theme(panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    axis.title = element_text(size = 13, face = "bold"),axis.text  = element_text(size = 10)  )

ggsave(str_c(pathout5,"/", "Scale international fund.jpg"),
       width = 5.5,height = 5,dpi = 500)
write.csv(Tradeoff ,str_c(pathout5,"/", "Tradeoff_climate fund and poverty.csv"))


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
Nation_diff_c <- Tax_reg_C[,which(Tax_Level %in% 50)]
Nation_diff_p <- Tax_reg_P[,which(Tax_Level %in% 50)]
write.csv(Nation_diff_c ,str_c(pathout5,"/", "Nation_diff_taxlevel_consum_50.csv"))
write.csv(Nation_diff_p ,str_c(pathout5,"/", "Nation_diff_taxlevel_produc_50.csv"))


#Regressivity/progressivity
Data_Unevenburden %>% filter(Vari %in% "Total_Effect", Group %in% c("G10","G100")) %>% 
  pivot_wider(id_cols = c(Country,TaxScenario),names_from = Group,values_from = value) -> A
A$Regressivity <- NA
A$Regressivity[A$G100 > A$G10] <- "Progressive"
A$Regressivity[A$G100 < A$G10] <- "Regressive"


write.csv(A %>% pivot_wider(id_cols = c(Country),names_from = TaxScenario,values_from = Regressivity) ,
          str_c(pathout5,"/", "Progressive or Regressive.csv"))

