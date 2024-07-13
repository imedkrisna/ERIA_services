setwd("E:/Data ajar pribadi Dhany/teliti/project imed")
library('readxl')
library('tidyverse')
library('ARDL')

# Baca data
dat<-read_excel('tes.xlsx')
gambar<-read_excel('plot.xlsx')

# regresi
auto_ardl(exM~imM+imSev,data=dat,max_order = 5)
auto_ardl(pdb~imM+imSev,data=dat,max_order = 5)
ekspor1<-ardl(exM~imM+imSev,data=dat,order = c(1,0,0))
ekspor2<-ardl(exM~imM+imSev,data=dat,order = c(1,1,1))
pdb1<-ardl(pdb~imM+imSev,data=dat,order = c(1,0,0))
pdb2<-ardl(pdb~imM+imSev,data=dat,order = c(1,1,1))
summary(ekspor1)
summary(pdb1)

coba<-pivot_longer(gambar, !Year)
datlog<-pivot_longer(dat, !Year)
#plot1 x=year, y=value, color=export manufaktur, impor manufaktur, impor servis
coba %>%
  filter(name!="Sexport") %>%
  filter(name!="PDB") %>%
  ggplot(aes(Year, value, color=name))+
  geom_point()+
  geom_line()
#plot2 x=year, y=value, color=PDB manufaktur, impor manufaktur, impor servis  
coba %>%
  filter(name!="Sexport") %>%
  filter(name!="Mexport") %>%
  ggplot(aes(Year, value, color=name))+
  geom_point()+
  geom_line()

#plot3log x=year, y=value, color=export manufaktur, impor manufaktur, impor servis
datlog %>%
  filter(name!="exM") %>%
  filter(name!="pdb") %>%
  ggplot(aes(Year, value, color=name))+
  geom_point()+
  geom_line()

#plot4log x=year, y=value, color=PDB manufaktur, impor manufaktur, impor servis  
datlog %>%
  filter(name!="exM") %>%
  filter(name!="exSev") %>%
  ggplot(aes(Year, value, color=name))+
  geom_point()+
  geom_line()




