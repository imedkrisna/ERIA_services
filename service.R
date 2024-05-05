ibrary(tidyverse)
library(writexl)

setwd('C:/github/ERIA_services/data/icio')

srv<-c("IDN_D","IDN_E","IDN_F","IDN_G","IDN_H49","IDN_H50",
       "IDN_H51","IDN_H52","IDN_H53","IDN_I","IDN_J58T60",
       "IDN_J61","IDN_J62_63","IDN_K","IDN_L","IDN_M","IDN_N",
       "IDN_O","IDN_P","IDN_Q","IDN_R","IDN_S")

## Indonesian services come from?

i2002<-read_csv("2002.csv") |>filter(`...1` %in% srv ) |> 
  pivot_longer(!`...1`,names_to = "tujuan",values_to = "value") |>
  pivot_wider(names_from = `...1`,values_from = value)|> arrange(desc(IDN_D))

  mutate(group=stringr::str_extract(`tujuan`, "IDN"))
i2002<- i2002|>replace(is.na(i2002),"foreign")

write_xlsx(i2002,"servicesoutput.xlsx")

i2002<-read_csv("2002.csv") |> select(`...1`,srv) |> arrange(desc(IDN_D))
