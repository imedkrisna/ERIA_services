setwd("E:/Data ajar pribadi Dhany/teliti/project imed")
library('readxl')
library('writexl')
library('tidyverse')
library('dplyr')
library('viridisLite')
library('viridis')
library('hrbrthemes')

serv<-read_excel('batis.xlsx')

allsectex <- serv %>%
  filter(counterpart !="World",code !="SPX1", code !="SOX1", code !="SOX", code !="SPX4", flow == "X", time =="2021") %>%
  group_by(code) %>%
  top_n(6)

allsectex %>% 
  ggplot(aes(x=code, y=value, fill=counterpart)) + 
  geom_bar(position = "stack",stat = "identity")+
  labs(title="Indonesian Service Export 2021")+
  theme_classic()+
  ylab("in Million USD")+
  xlab("services code")
ggsave("plot/Indonesian Service export.png", width=8,height=5,units= "in")
    
allsectim <- serv %>%
  filter(counterpart !="World",code !="SPX1", code !="SOX1", code !="SOX", code !="SPX4", flow == "M", time =="2021") %>%
  group_by(code) %>%
  top_n(6)

allsectim %>% 
  ggplot(aes(x=code, y=value, fill=counterpart)) + 
  geom_bar(position = "stack",stat = "identity")+
  labs(title="Indonesian Service Import 2021")+
  theme_classic()+
  ylab("in Million USD")+
  xlab("services code")
ggsave("plot/Indonesian Service import.png", width=8,height=5,units= "in")


#allsect %>% 
#  ggplot(aes(x=time, y=value, fill=service)) + 
#  geom_area(alpha=0.6 , size=.5, colour="white")+
#  scale_fill_viridis(discrete = T) +
#  theme_ipsum()+
#  labs(title="Indonesia Service export")+
#  ylab("Million USD")+
#  xlab("Year") 
  #filter(time == "2021", counterpart !="World", flow == "M", value==max(value))  
  #group_by(flow, code) %>%
  #summarise (value = max(value, na.rm=TRUE))

#allsect %>%
#  ggplot(aes (x=code, y=value))+
#  geom_bar(stat="identity")+
  

#Transport export
SCEX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SC") %>% 
  group_by(time) %>% 
  top_n(6)
SCEX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Transport Export")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SCEX, "sheet/SCEX transport.xlsx")
ggsave("plot/SCEX.png")

#Transport Import
SCIM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SC") %>% 
  group_by(time) %>% 
  top_n(6)
SCIM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Transport Import")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SCIM, "sheet/SCIM transport.xlsx")
ggsave("plot/SCIM.png")

#Manufacturing Services export
SAEX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SA") %>% 
  group_by(time) %>% 
  top_n(6)
SAEX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Manufacturing Services Export")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SAEX, "sheet/SAEX Manufacturing Services.xlsx")
ggsave("plot/SAEX.png")

#Manufacturing Services Import
SAIM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SA") %>% 
  group_by(time) %>% 
  top_n(6)
SAIM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Manufacturing Services Import")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SAIM, "sheet/SAIM transport.xlsx")
ggsave("plot/SAIM.png")

#Maintenance and repair export
SBEX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SB") %>% 
  group_by(time) %>% 
  top_n(6)
SBEX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Maintenance and repair Export")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SBEX, "sheet/SBEX Maintenance and repair.xlsx")
ggsave("plot/SBEX.png")

#Maintenance and repair Import
SBIM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SB") %>% 
  group_by(time) %>% 
  top_n(6)
SBIM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Maintenance and repair Import")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SBIM, "sheet/SBIM Maintenance and repair.xlsx")
ggsave("plot/SBIM.png")

#Travel export
SDEX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SD") %>% 
  group_by(time) %>% 
  top_n(6)
SDEX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Travel Export")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SDEX, "sheet/SDEX Travel.xlsx")
ggsave("plot/SDEX.png")

#Travel Import
SDIM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SD") %>% 
  group_by(time) %>% 
  top_n(6)
SDIM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Travel Import")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SDIM, "sheet/SDIM Travel.xlsx")
ggsave("plot/SDIM.png")

#Construction export
SEEX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SE") %>% 
  group_by(time) %>% 
  top_n(6)
SEEX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Construction Export")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SEEX, "sheet/SEEX Construction.xlsx")
ggsave("plot/SEEX.png")

#Construction import
SEIM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SE") %>% 
  group_by(time) %>% 
  top_n(6)
SEIM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Construction Import")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SEIM, "sheet/SEIM Construction.xlsx")
ggsave("plot/SEIM.png")

#Insurance and pension export
SFEX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SF") %>% 
  group_by(time) %>% 
  top_n(6)
SFEX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Insurance and pension Export")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SFEX, "sheet/SFEX Insurance and pension.xlsx")
ggsave("plot/SFEX.png")

#Insurance and pension Import
SFIM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SF") %>% 
  group_by(time) %>% 
  top_n(6)
SFIM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Insurance and pension Import")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SFIM, "sheet/SFIM Insurance and pension.xlsx")
ggsave("plot/SFIM.png")

#Financial Services export
SGEX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SG") %>% 
  group_by(time) %>% 
  top_n(6)
SGEX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Financial Services")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SGEX, "sheet/SGEX Financial Services.xlsx")
ggsave("plot/SGEX.png")

#Financial Services Import
SGIM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SG") %>% 
  group_by(time) %>% 
  top_n(6)
SGIM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Financial Services Import")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SGIM, "sheet/SGIM Financial Services.xlsx")
ggsave("plot/SGIM.png")

#Intelectual Property export
SHEX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SH") %>% 
  group_by(time) %>% 
  top_n(6)
SHEX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Intelectual Property Export")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SHEX, "sheet/SHEX transport.xlsx")
ggsave("plot/SHEX.png")

#Intelectual Property Import
SHIM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SH") %>% 
  group_by(time) %>% 
  top_n(6)
SHIM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Intelectual Property Import")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SHIM, "sheet/SHIM Intelectual Property.xlsx")
ggsave("plot/SHIM.png")

# Telecomunications, computer, and information export
SIEX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SI") %>% 
  group_by(time) %>% 
  top_n(6)
SIEX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Telecomunications, computer, and information Export")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SIEX, "sheet/SIEX Telecomunications, computer, and information.xlsx")
ggsave("plot/SIEX.png")

#Telecomunications, computer, and information Import
SIIM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SI") %>% 
  group_by(time) %>% 
  top_n(6)
SIIM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Telecomunications, computer, and information Import")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SIIM, "sheet/SIIM Telecomunications, computer, and information.xlsx")
ggsave("plot/SIIM.png")

#Other Business export
SJEX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SJ") %>% 
  group_by(time) %>% 
  top_n(6)
SJEX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Other Business")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SJEX, "sheet/SJEX Other Business.xlsx")
ggsave("plot/SJEX.png")

#Other Business Import
SJIM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SJ") %>% 
  group_by(time) %>% 
  top_n(6)
SJIM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Other Business")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SJIM, "sheet/SJIM Other Business.xlsx")
ggsave("plot/SJIM.png")

#Personal cultural recreational export
SKEX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SK") %>% 
  group_by(time) %>% 
  top_n(6)
SKEX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Personal cultural recreational Export")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SKEX, "sheet/SKEX Personal cultural recreational.xlsx")
ggsave("plot/SKEX.png")

#Personal cultural recreational Import
SKIM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SK") %>% 
  group_by(time) %>% 
  top_n(6)
SKIM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Personal cultural recreational Import")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SKIM, "sheet/SKIM Personal cultural recreational.xlsx")
ggsave("plot/SKIM.png")

#Government goods and services export
SLEX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SL") %>% 
  group_by(time) %>% 
  top_n(6)
SLEX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Government goods and services Export")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SLEX, "sheet/SLEX Government goods and services.xlsx")
ggsave("plot/SLEX.png")

#Government goods and services Import
SLIM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SL") %>% 
  group_by(time) %>% 
  top_n(6)
SLIM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Government goods and services Import")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SLIM, "sheet/SLIM Government goods and services.xlsx")
ggsave("plot/SLIM.png")

#Memo Grouping export
SOXEX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SOX") %>% 
  group_by(time) %>% 
  top_n(6)
SOXEX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Memo Grouping Export")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SOXEX, "sheet/SOXEX Memo Grouping.xlsx")
ggsave("plot/SOXEX.png")

#Memo Grouping Import
SOXIM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SOX") %>% 
  group_by(time) %>% 
  top_n(6)
SOXIM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Memo Grouping Import")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SOXIM, "sheet/SOXIM Memo Grouping.xlsx")
ggsave("plot/SOXIM.png")

#Other Commercial Services export
SOX1EX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SOX1") %>% 
  group_by(time) %>% 
  top_n(6)
SOX1EX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Other Commercial Services Export")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SOX1EX, "sheet/SOX1EX Other Commercial Services.xlsx")
ggsave("plot/SOX1EX.png")

#Other Commercial Services Import
SOX1IM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SOX1") %>% 
  group_by(time) %>% 
  top_n(6)
SOX1IM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Other Commercial Services Import")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SOX1IM, "sheet/SOX1IM Other Commercial Services.xlsx")
ggsave("plot/SOX1IM.png")

#Other Services export
SPX1EX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SPX1") %>% 
  group_by(time) %>% 
  top_n(6)
SPX1EX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Other Services Export")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SPX1EX, "sheet/SPX1EX Other Services.xlsx")
ggsave("plot/SPX1EX.png")

#Other Services Import
SPX1IM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SPX1") %>% 
  group_by(time) %>% 
  top_n(6)
SPX1IM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Other Services Import")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SPX1IM, "sheet/SPX1IM Other Services.xlsx")
ggsave("plot/SPX1IM.png")

#Goods related services export
SPX4EX <- serv %>%
  filter(counterpart !="World", flow == "X", code =="SPX4") %>% 
  group_by(time) %>% 
  top_n(6)
SPX1EX %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Goods related services Export")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SPX4EX, "sheet/SPX4EX Goods related services.xlsx")
ggsave("plot/SPX4EX.png")

#Goods related services Import
SPX4IM <- serv %>%
  filter(counterpart !="World", flow == "M", code =="SPX4") %>% 
  group_by(time) %>% 
  top_n(6)
SPX4IM %>% 
  ggplot(aes(x=time, y=value, fill=counterpart)) + 
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_classic()+
  labs(title="Goods related services Import")+
  ylab("in Million USD")+
  xlab("Year")
write_xlsx(SPX4IM, "sheet/SPX4IM Goods related services.xlsx")
ggsave("plot/SPX4IM.png")
