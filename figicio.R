library('readxl')
library('tidyverse')
library('ARDL')
library('fixest')
library("modelsummary")
setwd("C:/github/ERIA_services")
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

icio|>
  select(VA,OUT,ds,fs,dg,fg,psfs,psds,psfg,psdg) |>
  datasummary_skim()

icio$lv<-log(icio$VA)
icio$lo<-log(icio$OUT)
icio$lfs<-log(icio$fs)
icio$lds<-log(icio$ds)


icio|>select(lv,lo,lfs,lds)|>datasummary_skim()

pois<-fepois(VA~lfs+lds | industry+country,icio)
ols<-feols(lv~lfs+lds | industry+country,icio)

reg<-list(
  "ols"=ols,
  "pois"=pois
)

modelsummary(reg,stars = T)

idn2<-icio|>filter(country=="IDN")
sgp2<-icio|>filter(country=="SGP")
vnm2<-icio|>filter(country=="VNM")
tha2<-icio|>filter(country=="THA")
mys2<-icio|>filter(country=="MYS")

all3<-feols(lv~lfs+lds | industry+country,icio)
idn3<-feols(lv~lfs+lds | industry,idn2)
sgp3<-feols(lv~lfs+lds | industry,sgp2)
vnm3<-feols(lv~lfs+lds | industry,vnm2)
tha3<-feols(lv~lfs+lds | industry,tha2)
mys3<-feols(lv~lfs+lds | industry,mys2)

idn4<-fepois(VA~lfs+lds | industry+country,idn2)
sgp4<-fepois(VA~lfs+lds | industry+country,sgp2)
vnm4<-fepois(VA~lfs+lds | industry+country,vnm2)
tha4<-fepois(VA~lfs+lds | industry+country,tha2)
mys4<-fepois(VA~lfs+lds | industry+country,mys2)

regols<-list(
  "IDN"=idn3,
  "SGP"=sgp3,
  "VNM"=vnm3,
  "THA"=tha3,
  "MYS"=mys3
)

regpois<-list(
  "IDN"=idn4,
  "SGP"=sgp4,
  "VNM"=vnm4,
  "THA"=tha4,
  "MYS"=mys4
)

idn5<-feols(lo~lfs+lds | industry+country,idn2)
sgp5<-feols(lo~lfs+lds | industry+country,sgp2)
vnm5<-feols(lo~lfs+lds | industry+country,vnm2)
tha5<-feols(lo~lfs+lds | industry+country,tha2)
mys5<-feols(lo~lfs+lds | industry+country,mys2)

idn6<-fepois(OUT~lfs+lds | industry+country,idn2)
sgp6<-fepois(OUT~lfs+lds | industry+country,sgp2)
vnm6<-fepois(OUT~lfs+lds | industry+country,vnm2)
tha6<-fepois(OUT~lfs+lds | industry+country,tha2)
mys6<-fepois(OUT~lfs+lds | industry+country,mys2)

oregols<-list(
  "IDN"=idn5,
  "SGP"=sgp5,
  "VNM"=vnm5,
  "THA"=tha5,
  "MYS"=mys5
)

oregpois<-list(
  "IDN"=idn6,
  "SGP"=sgp6,
  "VNM"=vnm6,
  "THA"=tha6,
  "MYS"=mys6
)

regols|>modelsummary(coef_rename=c("lfs"="Foreign service","lds"="Domestic service"),
                     stars=T,output = "reg/regols.docx")
oregols|>modelsummary(coef_rename=c("lfs"="Foreign service","lds"="Domestic service"),
                      stars = T,output="reg/oregols.docx")
