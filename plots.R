setwd("C:/github/ERIA_services")
library(tidyverse)
library(readxl)
library(scales)
library(ARDL)
library(modelsummary)
library(fixest)
library(WDI)

## ICIO and aggg
idn<-read_excel("data/icio/manuficio.xlsx")
idn$country<-"IDN"
sgp<-read_excel("data/icio/SGP.xlsx")
sgp$country<-"SGP"
vnm<-read_excel("data/icio/VNM.xlsx")
vnm$country<-"VNM"
mys<-read_excel("data/icio/MYS.xlsx")
mys$country<-"MYS"
tha<-read_excel("data/icio/THA.xlsx")
tha$country<-"THA"
icio<-rbind(idn,sgp,vnm,tha,mys)
icio$psds<-icio$sds*100
icio$psfs<-icio$sfs*100
icio$psfg<-icio$sfg*100
icio$psdg<-icio$sdg*100
icio<-icio|>
  select(year,industry,country,VA,OUT,ds,fs,dg,fg,psfs,psds,psfg,psdg)

icio1<-icio|>
  select(`value added`=VA,`output`=OUT,
         `domestic services`=ds,`foreign services`=fs,
         `domestic goods`=dg,`foreign goods`=fg,
         `for. services share`=psfs,
         `dom. services share`=psds,
         `for. goods share`=psfg,
         `dom. goods share`=psdg,country)
icio1$what<-"all"
icio2<-icio1|>filter(country=="IDN")
icio2$what<-'IDN'
icio3<-rbind(icio1,icio2)

icio$s<-icio$fs+icio$ds
agg<-icio|>select(!industry)|>group_by(year,country)|>summarise(across(everything(),sum))

indi<-c(
  "MVA"="NV.IND.MANF.CD",
  "MVAf"="NV.IND.MANF.ZS"
)

dun<-tibble(WDI(
  country=c("IDN","SGP","VNM","THA","MYS"),
  indicator=indi,start=2002,end=2020
))

dun<-dun|>select(iso3c,year,MVA,MVAf)|>rename(country=iso3c)
dun<-tibble(dun)
agg$year<-as.integer(agg$year)

aggg<-inner_join(dun,agg)

aggg$lmva<-log(aggg$MVA)
aggg$lds<-log(aggg$ds)
aggg$ls<-log(aggg$s)
aggg$lfs<-log(aggg$fs)
aggg$ldg<-log(aggg$dg)
aggg$lfg<-log(aggg$fg)
aggg$psfs<-aggg$fs/aggg$OUT*100
aggg$pfs<-aggg$fs/(aggg$OUT-aggg$VA)*100

## graphs

read_excel("data/services.xlsx") |>
  pivot_longer(!year,names_to="flow",values_to="value") |>
  ggplot(aes(x=year,y=value,color=flow))+geom_line(linewidth=1.1)+
  scale_y_continuous(labels = scales::comma)+
  scale_color_discrete(labels=c('Import', 'Export'))+     
  labs(x="",y="Million current USD",caption="source: SEKI")+theme_classic()
ggsave("plot/plots/flow.png",width=6,height=4,dpi=300)

wew<-tibble(
  year=c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023),
  goods=c(31003,33825,8680,5833,6983,14049,15318,18814,-228,3508,28301,43806,62672,46453),
  services=c(-9791,-9803,-10564,-12070,-10010,-8697,-7084,-7379,-6485,-7641,-9755,-14599,-19957,-18089)) |>
  pivot_longer(!year,names_to = "net trade",values_to = "value")

wew|>ggplot(aes(x=year,y=value,color=`net trade`))+
  geom_line(linewidth=1.1)+geom_hline(yintercept=0)+
  scale_y_continuous(expand = c(0,0),limits = c(-22000,65000),labels = scales::comma)+
  scale_x_continuous(breaks = c(2010,2012,2014,2016,2018,2020,2022))+
  labs(x="",y="Million USD",caption = "source: SEKI")+
  theme_classic()
ggsave("plot/plots/net_trade.png",width=6,height=4,dpi=300)

indi<-c(            # membuat dictionary
  "goods"="TG.VAL.TOTL.GD.ZS",
  "services"="BG.GSR.NFSV.GD.ZS"
)

dat<-WDI(           # Menarik data World Bank
  country=c("IDN","WLD"),
  indicator=indi,start=1981
  
) 

dat|>
  ggplot(aes(x=year,y=goods,color=iso3c))+geom_line(linewidth=1.1)+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",y="",caption="source: WDI")+theme_classic()
ggsave("plot/plots/WDI1.png",width=6,height=4,dpi=300)

dat|>
  ggplot(aes(x=year,y=services,color=iso3c,lty=iso3c))+
  geom_line(linewidth=1.1)+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",y="")+theme_classic()

ggsave("plot/plots/WDI2.png",width=6,height=4,dpi=300)

aggg|>ggplot(aes(x=year,y=ds,color=country))+
  geom_line(linewidth=1.1)+
  scale_y_continuous(expand = c(0,0),labels = scales::comma)+
  labs(x="",y="")+
  theme_classic()
ggsave("plot/plots/DS.png",width=6,height=4,dpi=300)

aggg|>ggplot(aes(x=year,y=fs,color=country))+
  geom_line(linewidth=1.1)+
  scale_y_continuous(expand = c(0,0),labels = scales::comma)+
  labs(x="",y="")+
  theme_classic()
ggsave("plot/plots/FS.png",width=6,height=4,dpi=300)