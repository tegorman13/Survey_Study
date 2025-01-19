

library(pacman)
pacman::p_load(dplyr,purrr,tidyr,here, haven,tibble,ggplot2,ggh4x,lme4,knitr,gt,flextable,ggh4x,psych,corrplot)
options(digits=2, scipen=999, dplyr.summarise.inform=FALSE)


#/Users/thomasgorman/Library/CloudStorage/GoogleDrive-tegorman13@gmail.com/My Drive/Purdue/Survey_Study/data/NSF Instrument - 11-3-18.sav
draw <- haven::read_sav(here::here("data/NSF Instrument - 11-3-18.sav")) |> 
    mutate(id=row_number(),.before=1) |> select(-IPAddress)

dinst <- haven::read_sav(here::here("data/Instrument_Study1_1.sav")) |> 
    mutate(id=row_number() + nrow(draw),.before=1) |> select(-IPAddress)

dinst <- dinst |> rename(DEM28=Q43)




saveRDS(draw,here::here("data/draw.rds"))
write.csv(draw,here::here("data/draw.csv"), row.names = FALSE)

saveRDS(dinst,here::here("data/dinst.rds"))
write.csv(dinst,here::here("data/dinst.csv"), row.names = FALSE)





# compare column names
#colnames(draw)==colnames(dinst)
# colnames(draw)[200:208]
# "RS01"  "RS02"  "RS03"  "RS04"  "RS05"  "RS06"  "DEM28" "DEM29" "DEM30"
# colnames(dinst)[200:208]
# "RS01"  "RS02"  "RS03"  "RS04"  "RS05"  "RS06"  "Q43"   "DEM29" "DEM30"
# different col. labels at 206, but both are identical question "This past year, did you send a letter to any political official...."





#write.csv(final_data, "processed_energy_study_data.csv", row.names = FALSE)