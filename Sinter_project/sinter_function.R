library(dplyr)
library(stringr)
read_rename_sinter = function(){
  folder <- "./data/"  
  file_list = list.files(path=folder, pattern="*.csv", full.names = T)
    
  plant =list()
  for (i in 1:length(file_list)){
    plant[[i]]=assign(file_list[i], 
           read.csv(file_list[i], sep=','))
    names(plant[[i]])=str_to_title(gsub("_", " ", names(plant[[i]])))
      
  }
      
# sinter = read.csv("syntheticData_20190529.csv",
#                   stringsAsFactors = FALSE, header = TRUE)
# 
# #Remove underscores and change column names to title format for display
#     names(sinter)=str_to_title(gsub("_", " ", names(sinter)))
#     
#     sinter_new=sinter %>% 
#       plyr::rename(c("Sinter Pct" = "% Sinter",
#                      "Irf Pct" = "% Internal Return Fines",
#                      "Erf Pct" = "% External Return Fines",
#                      "Ti" = "Tumbling Index",
#                      "Si" = "Shatter Index",
#                      "Ih Temp M1" = "Internal heat temp M1",
#                      "Al2o3" = "Al2O3",
#                      "Cao" = "CaO",
#                      "Co2" = "CO2",
#                      "Machine Speed Dial" = "Machine speed reading"
#                      ))
#     
    return(plant)
}
