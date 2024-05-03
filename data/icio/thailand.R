library(tidyverse)
library(lubridate)
library(writexl)

setwd('C:/github/ERIA_services/data/icio')

#### FOOD MANUFACTURING

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2002"="THA_C10T12")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2003"="THA_C10T12")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2004"="THA_C10T12")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2005"="THA_C10T12")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2006"="THA_C10T12")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2007"="THA_C10T12")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2008"="THA_C10T12")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2009"="THA_C10T12")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2010"="THA_C10T12")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2011"="THA_C10T12")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2012"="THA_C10T12")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2013"="THA_C10T12")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2014"="THA_C10T12")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2015"="THA_C10T12")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2016"="THA_C10T12")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2017"="THA_C10T12")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2018"="THA_C10T12")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2019"="THA_C10T12")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C10T12) |> arrange(desc(THA_C10T12)) |>
  rename("2020"="THA_C10T12")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
                              ## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))
    
gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
            )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )



gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

food<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
food$industry<-"C10T12"

#### TEXTILES

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2002"="THA_C13T15")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2003"="THA_C13T15")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2004"="THA_C13T15")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2005"="THA_C13T15")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2006"="THA_C13T15")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2007"="THA_C13T15")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2008"="THA_C13T15")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2009"="THA_C13T15")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2010"="THA_C13T15")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2011"="THA_C13T15")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2012"="THA_C13T15")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2013"="THA_C13T15")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2014"="THA_C13T15")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2015"="THA_C13T15")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2016"="THA_C13T15")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2017"="THA_C13T15")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2018"="THA_C13T15")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2019"="THA_C13T15")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C13T15) |> arrange(desc(THA_C13T15)) |>
  rename("2020"="THA_C13T15")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )



gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

textiles<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
textiles$industry<-"C13T15"

#### WOOD

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2002"="THA_C16")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2003"="THA_C16")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2004"="THA_C16")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2005"="THA_C16")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2006"="THA_C16")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2007"="THA_C16")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2008"="THA_C16")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2009"="THA_C16")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2010"="THA_C16")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2011"="THA_C16")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2012"="THA_C16")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2013"="THA_C16")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2014"="THA_C16")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2015"="THA_C16")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2016"="THA_C16")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2017"="THA_C16")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2018"="THA_C16")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2019"="THA_C16")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C16) |> arrange(desc(THA_C16)) |>
  rename("2020"="THA_C16")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )



gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

wood<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
wood$industry<-"C16"

#### PAPER

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2002"="THA_C17_18")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2003"="THA_C17_18")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2004"="THA_C17_18")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2005"="THA_C17_18")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2006"="THA_C17_18")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2007"="THA_C17_18")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2008"="THA_C17_18")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2009"="THA_C17_18")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2010"="THA_C17_18")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2011"="THA_C17_18")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2012"="THA_C17_18")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2013"="THA_C17_18")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2014"="THA_C17_18")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2015"="THA_C17_18")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2016"="THA_C17_18")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2017"="THA_C17_18")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2018"="THA_C17_18")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2019"="THA_C17_18")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C17_18) |> arrange(desc(THA_C17_18)) |>
  rename("2020"="THA_C17_18")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )



gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

paper<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
paper$industry<-"C17_18"

#### CHEMICAL

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2002"="THA_C20")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2003"="THA_C20")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2004"="THA_C20")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2005"="THA_C20")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2006"="THA_C20")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2007"="THA_C20")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2008"="THA_C20")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2009"="THA_C20")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2010"="THA_C20")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2011"="THA_C20")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2012"="THA_C20")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2013"="THA_C20")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2014"="THA_C20")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2015"="THA_C20")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2016"="THA_C20")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2017"="THA_C20")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2018"="THA_C20")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2019"="THA_C20")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C20) |> arrange(desc(THA_C20)) |>
  rename("2020"="THA_C20")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )



gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

chemical<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
chemical$industry<-"C20"

#### PHARMA

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2002"="THA_C21")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2003"="THA_C21")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2004"="THA_C21")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2005"="THA_C21")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2006"="THA_C21")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2007"="THA_C21")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2008"="THA_C21")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2009"="THA_C21")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2010"="THA_C21")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2011"="THA_C21")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2012"="THA_C21")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2013"="THA_C21")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2014"="THA_C21")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2015"="THA_C21")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2016"="THA_C21")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2017"="THA_C21")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2018"="THA_C21")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2019"="THA_C21")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C21) |> arrange(desc(THA_C21)) |>
  rename("2020"="THA_C21")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )



gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

pharma<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
pharma$industry<-"C21"

#### RUBBER PLASTIC

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2002"="THA_C22")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2003"="THA_C22")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2004"="THA_C22")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2005"="THA_C22")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2006"="THA_C22")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2007"="THA_C22")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2008"="THA_C22")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2009"="THA_C22")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2010"="THA_C22")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2011"="THA_C22")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2012"="THA_C22")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2013"="THA_C22")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2014"="THA_C22")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2015"="THA_C22")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2016"="THA_C22")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2017"="THA_C22")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2018"="THA_C22")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2019"="THA_C22")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C22) |> arrange(desc(THA_C22)) |>
  rename("2020"="THA_C22")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )



gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

rubber<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
rubber$industry<-"C22"

#### OTHER MINERAL

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2002"="THA_C23")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2003"="THA_C23")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2004"="THA_C23")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2005"="THA_C23")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2006"="THA_C23")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2007"="THA_C23")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2008"="THA_C23")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2009"="THA_C23")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2010"="THA_C23")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2011"="THA_C23")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2012"="THA_C23")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2013"="THA_C23")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2014"="THA_C23")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2015"="THA_C23")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2016"="THA_C23")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2017"="THA_C23")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2018"="THA_C23")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2019"="THA_C23")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C23) |> arrange(desc(THA_C23)) |>
  rename("2020"="THA_C23")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )



gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

onon<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
onon$industry<-"C23"

#### BASIC METALS

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2002"="THA_C24")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2003"="THA_C24")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2004"="THA_C24")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2005"="THA_C24")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2006"="THA_C24")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2007"="THA_C24")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2008"="THA_C24")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2009"="THA_C24")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2010"="THA_C24")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2011"="THA_C24")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2012"="THA_C24")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2013"="THA_C24")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2014"="THA_C24")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2015"="THA_C24")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2016"="THA_C24")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2017"="THA_C24")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2018"="THA_C24")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2019"="THA_C24")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C24) |> arrange(desc(THA_C24)) |>
  rename("2020"="THA_C24")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )



gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

basic<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
basic$industry<-"C24"

#### FABRICATED METAL

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2002"="THA_C25")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2003"="THA_C25")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2004"="THA_C25")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2005"="THA_C25")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2006"="THA_C25")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2007"="THA_C25")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2008"="THA_C25")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2009"="THA_C25")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2010"="THA_C25")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2011"="THA_C25")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2012"="THA_C25")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2013"="THA_C25")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2014"="THA_C25")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2015"="THA_C25")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2016"="THA_C25")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2017"="THA_C25")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2018"="THA_C25")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2019"="THA_C25")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C25) |> arrange(desc(THA_C25)) |>
  rename("2020"="THA_C25")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )



gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

fab<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
fab$industry<-"C25"

#### COMPUTER

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2002"="THA_C26")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2003"="THA_C26")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2004"="THA_C26")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2005"="THA_C26")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2006"="THA_C26")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2007"="THA_C26")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2008"="THA_C26")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2009"="THA_C26")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2010"="THA_C26")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2011"="THA_C26")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2012"="THA_C26")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2013"="THA_C26")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2014"="THA_C26")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2015"="THA_C26")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2016"="THA_C26")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2017"="THA_C26")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2018"="THA_C26")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2019"="THA_C26")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C26) |> arrange(desc(THA_C26)) |>
  rename("2020"="THA_C26")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )



gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

com<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
com$industry<-"C26"

#### ELECTRICAL

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2002"="THA_C27")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2003"="THA_C27")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2004"="THA_C27")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2005"="THA_C27")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2006"="THA_C27")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2007"="THA_C27")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2008"="THA_C27")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2009"="THA_C27")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2010"="THA_C27")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2011"="THA_C27")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2012"="THA_C27")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2013"="THA_C27")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2014"="THA_C27")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2015"="THA_C27")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2016"="THA_C27")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2017"="THA_C27")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2018"="THA_C27")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2019"="THA_C27")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C27) |> arrange(desc(THA_C27)) |>
  rename("2020"="THA_C27")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )



gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

elec<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
elec$industry<-"C27"

#### MACHINERY

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2002"="THA_C28")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2003"="THA_C28")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2004"="THA_C28")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2005"="THA_C28")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2006"="THA_C28")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2007"="THA_C28")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2008"="THA_C28")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2009"="THA_C28")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2010"="THA_C28")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2011"="THA_C28")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2012"="THA_C28")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2013"="THA_C28")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2014"="THA_C28")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2015"="THA_C28")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2016"="THA_C28")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2017"="THA_C28")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2018"="THA_C28")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2019"="THA_C28")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C28) |> arrange(desc(THA_C28)) |>
  rename("2020"="THA_C28")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )



gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

machine<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
machine$industry<-"C28"

#### VEHICLES

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2002"="THA_C29")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2003"="THA_C29")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2004"="THA_C29")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2005"="THA_C29")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2006"="THA_C29")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2007"="THA_C29")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2008"="THA_C29")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2009"="THA_C29")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2010"="THA_C29")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2011"="THA_C29")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2012"="THA_C29")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2013"="THA_C29")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2014"="THA_C29")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2015"="THA_C29")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2016"="THA_C29")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2017"="THA_C29")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2018"="THA_C29")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2019"="THA_C29")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C29) |> arrange(desc(THA_C29)) |>
  rename("2020"="THA_C29")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )



gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

vehicles<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
vehicles$industry<-"C29"

#### OTHER TRANSPORT

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2002"="THA_C30")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2003"="THA_C30")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2004"="THA_C30")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2005"="THA_C30")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2006"="THA_C30")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2007"="THA_C30")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2008"="THA_C30")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2009"="THA_C30")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2010"="THA_C30")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2011"="THA_C30")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2012"="THA_C30")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2013"="THA_C30")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2014"="THA_C30")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2015"="THA_C30")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2016"="THA_C30")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2017"="THA_C30")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2018"="THA_C30")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2019"="THA_C30")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C30) |> arrange(desc(THA_C30)) |>
  rename("2020"="THA_C30")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

## Special

gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

otr<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
otr$industry<-"C30"

#### OTHER MANUF

i2002<-read_csv("2002.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2002"="THA_C31T33")
i2003<-read_csv("2003.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2003"="THA_C31T33")
i2004<-read_csv("2004.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2004"="THA_C31T33")
i2005<-read_csv("2005.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2005"="THA_C31T33")
i2006<-read_csv("2006.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2006"="THA_C31T33")
i2007<-read_csv("2007.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2007"="THA_C31T33")
i2008<-read_csv("2008.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2008"="THA_C31T33")
i2009<-read_csv("2008.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2009"="THA_C31T33")
i2010<-read_csv("2010.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2010"="THA_C31T33")
i2011<-read_csv("2011.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2011"="THA_C31T33")
i2012<-read_csv("2012.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2012"="THA_C31T33")
i2013<-read_csv("2013.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2013"="THA_C31T33")
i2014<-read_csv("2014.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2014"="THA_C31T33")
i2015<-read_csv("2015.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2015"="THA_C31T33")
i2016<-read_csv("2016.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2016"="THA_C31T33")
i2017<-read_csv("2017.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2017"="THA_C31T33")
i2018<-read_csv("2018.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2018"="THA_C31T33")
i2019<-read_csv("2019.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2019"="THA_C31T33")
i2020<-read_csv("2020.csv") |> select(`...1`,THA_C31T33) |> arrange(desc(THA_C31T33)) |>
  rename("2020"="THA_C31T33")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain THA ditulis foreign

gabung2<-gabung2 |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T"),
    1,0))

gabung2<-gabung2 |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "sds",
    group=="THA" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "THA"))
gabung2a<-gabung2a |>  replace(is.na(gabung2a),"foreign") ## sama tapi bukan persen

gabung2a<-gabung2a |>
  mutate(service=ifelse(
    str_detect(`...1`,"_D") | str_detect(`...1`,"_E") | str_detect(`...1`,"_F") |
      str_detect(`...1`,"_G") | str_detect(`...1`,"_H49") | str_detect(`...1`,"_H50") |
      str_detect(`...1`,"_H51") | str_detect(`...1`,"_H52") | str_detect(`...1`,"_H53") |
      str_detect(`...1`,"_I") | str_detect(`...1`,"_J58T60") | str_detect(`...1`,"_J61") |
      str_detect(`...1`,"_J62_63") | str_detect(`...1`,"_K") | str_detect(`...1`,"_L") |
      str_detect(`...1`,"_M") | str_detect(`...1`,"_N") | str_detect(`...1`,"_O") |
      str_detect(`...1`,"_P") | str_detect(`...1`,"_Q") | str_detect(`...1`,"_R") |
      str_detect(`...1`,"_S") | str_detect(`...1`,"_T") | str_detect(`...1`,"_B09"),
    1,0))

gabung2a<-gabung2a |>
  mutate(category=case_when(
    group=="THA" & service==1 ~ "ds",
    group=="THA" & service==0 ~ "dg",
    group=="foreign" & service==1 ~ "fs",
    TRUE~"fg"
  ))

gabung3<-gabung2|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )

gabung3a<-gabung2a|>slice(-1) |> group_by(category) |> 
  summarise(`2002`=sum(`2002`),
            `2003`=sum(`2003`),
            `2004`=sum(`2004`),
            `2005`=sum(`2005`),
            `2006`=sum(`2006`),
            `2007`=sum(`2007`),
            `2008`=sum(`2008`),
            `2009`=sum(`2009`),
            `2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
  )



gabung4<-gabung[gabung$`...1` %in% c("VA","OUT"), ] |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

other<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
other$industry<-"C31T33"

wew<-rbind(food,textiles,wood,paper,chemical,pharma,rubber,onon,basic,fab,com,elec,machine,vehicles,otr,other)
wew|>write_xlsx('manuficio.xlsx')

wew|>
  filter(industry!="C10T12") |>
  ggplot(aes(x=OUT,y=sfs+sfg,color=industry))+geom_point()+theme_classic()

wew2<-wew |> mutate(industry=NULL) |> group_by(year) |>
  summarise(across(everything(),sum))
wew2|>write_xlsx('manuficio2.xlsx')
## wew %>% mutate(industry=NULL) |> group_by(year) %>% summarize_all(sum)

## ARDL test

asuw<-wew|>filter(industry=="C20")
ardl(data=asuw,OUT~foreign,order=c(1,1))|>summary()
ardl(data=asuw,VA~foreign,order=c(1,1))|>summary()