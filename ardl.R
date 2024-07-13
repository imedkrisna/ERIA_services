library('readxl')
library('tidyverse')
library('ARDL')
library('modelsummary')

# Baca data
dat<-read_excel('data/tes.xlsx')
gambar<-read_excel('data/plot.xlsx')

# regresi
ekspor1<-ardl(exM~imM+imSev,data=dat,order = c(1,0,0))
ekspor2<-ardl(exM~imM+imSev,data=dat,order = c(1,1,1))
pdb1<-ardl(pdb~imM+imSev,data=dat,order = c(1,0,0))
pdb2<-ardl(pdb~imM+imSev,data=dat,order = c(1,1,1))

models <- list(
  "Export 1" = ekspor1,
  "Export 2" = ekspor2,
  "GDP 1"=pdb1,
  "GDP 2"=pdb2
  
)

modelsummary(models,stars = T,output="reg/reg.docx")

coba<-pivot_longer(gambar, !Year)

#plot1 x=year, y=value, color=export manufaktur, impor manufaktur, impor servis
coba %>%
  filter(name!="Sexport") %>%
  filter(name!="PDB") %>%
  ggplot(aes(Year, value, color=name))+
  geom_point()+
  geom_line()+
  scale_color_discrete(labels=c('Manufactures export', 'Manufactures import','Services import'))+
  labs(x="",y="Mill. current USD",caption="source: SEKI")+
  theme_classic()
ggsave("fig/fig.png")

