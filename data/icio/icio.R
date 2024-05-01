library(tidyverse)
library(lubridate)

setwd('C:/github/ERIA_services/data/icio')

#### FOOD MANUFACTURING

i2002<-read_csv("2002.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2002"="IDN_C10T12")
i2003<-read_csv("2003.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2003"="IDN_C10T12")
i2004<-read_csv("2004.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2004"="IDN_C10T12")
i2005<-read_csv("2005.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2005"="IDN_C10T12")
i2006<-read_csv("2006.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2006"="IDN_C10T12")
i2007<-read_csv("2007.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2007"="IDN_C10T12")
i2008<-read_csv("2008.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2008"="IDN_C10T12")
i2009<-read_csv("2008.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2009"="IDN_C10T12")
i2010<-read_csv("2010.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2010"="IDN_C10T12")
i2011<-read_csv("2011.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2011"="IDN_C10T12")
i2012<-read_csv("2012.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2012"="IDN_C10T12")
i2013<-read_csv("2013.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2013"="IDN_C10T12")
i2014<-read_csv("2014.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2014"="IDN_C10T12")
i2015<-read_csv("2015.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2015"="IDN_C10T12")
i2016<-read_csv("2016.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2016"="IDN_C10T12")
i2017<-read_csv("2017.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2017"="IDN_C10T12")
i2018<-read_csv("2018.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2018"="IDN_C10T12")
i2019<-read_csv("2019.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2019"="IDN_C10T12")
i2020<-read_csv("2020.csv") |> select(`...1`,IDN_C10T12) |> arrange(desc(IDN_C10T12)) |>
  rename("2020"="IDN_C10T12")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
                              ## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "IDN"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain IDN ditulis foreign

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
    group=="IDN" & service==1 ~ "sds",
    group=="IDN" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))
    
gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "IDN"))
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
    group=="IDN" & service==1 ~ "ds",
    group=="IDN" & service==0 ~ "dg",
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



gabung4<-gabung|>slice(1:2) |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

food<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
food$industry<-"C10T12"

#### TEXTILES

i2002<-read_csv("2002.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2002"="IDN_C13T15")
i2003<-read_csv("2003.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2003"="IDN_C13T15")
i2004<-read_csv("2004.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2004"="IDN_C13T15")
i2005<-read_csv("2005.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2005"="IDN_C13T15")
i2006<-read_csv("2006.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2006"="IDN_C13T15")
i2007<-read_csv("2007.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2007"="IDN_C13T15")
i2008<-read_csv("2008.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2008"="IDN_C13T15")
i2009<-read_csv("2008.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2009"="IDN_C13T15")
i2010<-read_csv("2010.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2010"="IDN_C13T15")
i2011<-read_csv("2011.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2011"="IDN_C13T15")
i2012<-read_csv("2012.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2012"="IDN_C13T15")
i2013<-read_csv("2013.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2013"="IDN_C13T15")
i2014<-read_csv("2014.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2014"="IDN_C13T15")
i2015<-read_csv("2015.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2015"="IDN_C13T15")
i2016<-read_csv("2016.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2016"="IDN_C13T15")
i2017<-read_csv("2017.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2017"="IDN_C13T15")
i2018<-read_csv("2018.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2018"="IDN_C13T15")
i2019<-read_csv("2019.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2019"="IDN_C13T15")
i2020<-read_csv("2020.csv") |> select(`...1`,IDN_C13T15) |> arrange(desc(IDN_C13T15)) |>
  rename("2020"="IDN_C13T15")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "IDN"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain IDN ditulis foreign

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
    group=="IDN" & service==1 ~ "sds",
    group=="IDN" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "IDN"))
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
    group=="IDN" & service==1 ~ "ds",
    group=="IDN" & service==0 ~ "dg",
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



gabung4<-gabung|>slice(1:2) |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

textiles<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
textiles$industry<-"C13T15"

#### WOOD

i2002<-read_csv("2002.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2002"="IDN_C16")
i2003<-read_csv("2003.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2003"="IDN_C16")
i2004<-read_csv("2004.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2004"="IDN_C16")
i2005<-read_csv("2005.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2005"="IDN_C16")
i2006<-read_csv("2006.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2006"="IDN_C16")
i2007<-read_csv("2007.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2007"="IDN_C16")
i2008<-read_csv("2008.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2008"="IDN_C16")
i2009<-read_csv("2008.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2009"="IDN_C16")
i2010<-read_csv("2010.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2010"="IDN_C16")
i2011<-read_csv("2011.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2011"="IDN_C16")
i2012<-read_csv("2012.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2012"="IDN_C16")
i2013<-read_csv("2013.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2013"="IDN_C16")
i2014<-read_csv("2014.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2014"="IDN_C16")
i2015<-read_csv("2015.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2015"="IDN_C16")
i2016<-read_csv("2016.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2016"="IDN_C16")
i2017<-read_csv("2017.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2017"="IDN_C16")
i2018<-read_csv("2018.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2018"="IDN_C16")
i2019<-read_csv("2019.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2019"="IDN_C16")
i2020<-read_csv("2020.csv") |> select(`...1`,IDN_C16) |> arrange(desc(IDN_C16)) |>
  rename("2020"="IDN_C16")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "IDN"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain IDN ditulis foreign

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
    group=="IDN" & service==1 ~ "sds",
    group=="IDN" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "IDN"))
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
    group=="IDN" & service==1 ~ "ds",
    group=="IDN" & service==0 ~ "dg",
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



gabung4<-gabung|>slice(1:2) |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

wood<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
wood$industry<-"C16"

#### PAPER

i2002<-read_csv("2002.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2002"="IDN_C17_18")
i2003<-read_csv("2003.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2003"="IDN_C17_18")
i2004<-read_csv("2004.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2004"="IDN_C17_18")
i2005<-read_csv("2005.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2005"="IDN_C17_18")
i2006<-read_csv("2006.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2006"="IDN_C17_18")
i2007<-read_csv("2007.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2007"="IDN_C17_18")
i2008<-read_csv("2008.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2008"="IDN_C17_18")
i2009<-read_csv("2008.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2009"="IDN_C17_18")
i2010<-read_csv("2010.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2010"="IDN_C17_18")
i2011<-read_csv("2011.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2011"="IDN_C17_18")
i2012<-read_csv("2012.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2012"="IDN_C17_18")
i2013<-read_csv("2013.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2013"="IDN_C17_18")
i2014<-read_csv("2014.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2014"="IDN_C17_18")
i2015<-read_csv("2015.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2015"="IDN_C17_18")
i2016<-read_csv("2016.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2016"="IDN_C17_18")
i2017<-read_csv("2017.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2017"="IDN_C17_18")
i2018<-read_csv("2018.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2018"="IDN_C17_18")
i2019<-read_csv("2019.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2019"="IDN_C17_18")
i2020<-read_csv("2020.csv") |> select(`...1`,IDN_C17_18) |> arrange(desc(IDN_C17_18)) |>
  rename("2020"="IDN_C17_18")

is<-list(i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012,
         i2013,i2014,i2015,i2016,i2017,i2018,i2019,i2020)
gabung<-reduce(is,inner_join) ## Gabung semua data per tahun itu dengan cara
## melist nama datanya lalu diinner join.

gabung1<-gabung |> mutate(across(where(is.numeric), ~./.[1])) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=stringr::str_extract(`...1`, "IDN"))
gabung2<-gabung2 |>  replace(is.na(gabung2),"foreign") ## Selain IDN ditulis foreign

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
    group=="IDN" & service==1 ~ "sds",
    group=="IDN" & service==0 ~ "sdg",
    group=="foreign" & service==1 ~ "sfs",
    TRUE~"sfg"
  ))

gabung2a<-gabung |>
  mutate(group=stringr::str_extract(`...1`, "IDN"))
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
    group=="IDN" & service==1 ~ "ds",
    group=="IDN" & service==0 ~ "dg",
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



gabung4<-gabung|>slice(1:2) |> rename(category=`...1`)

gabung4<-rbind(gabung4,gabung3a)
gabung4<-rbind(gabung4,gabung3)

paper<-gabung4|>pivot_longer(!category,names_to = "year",values_to = "values")|>
  pivot_wider(names_from = "category",values_from = "values")
paper$industry<-"C17_18"

#### CHEMICAL

C20

#### PHARMA

C21

#### RUBBER PLASTIC

C22

#### OTHER MINERAL

C23

#### BASIC METALS

C24

#### FABRICATED METAL

C25

#### COMPUTER

C26

#### ELECTRICAL

C27

#### MACHINERY

C28

#### VEHICLES

C29

#### OTHER TRANSPORT

C30

#### OTHER MANUF

C31T33

wew<-rbind(food,textiles,wood,paper)