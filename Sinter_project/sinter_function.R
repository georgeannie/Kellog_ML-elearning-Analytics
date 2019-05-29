library(dplyr)
read_rename_sinter = function(){
    sinter = read.csv("syntheticData_20190520.csv",
                  stringsAsFactors = FALSE, header = TRUE)

#Remove underscores and change column names to title format for display
    names(sinter)=str_to_title(gsub("_", " ", names(sinter)))
    
    sinter_new=sinter %>% 
      plyr::rename(c("Sinter Pct" = "% Sinter",
                     "Irf Pct" = "% Internal Return Fines",
                     "Erf Pct" = "% External Return Fines",
                     "Ti" = "Tumbling Index",
                     "Si" = "Shatter Index",
                     "Ih Temp M1" = "Internal heat temp M1",
                     "Al2o3" = "Al2O3",
                     "Cao" = "CaO",
                     "Co2" = "CO2",
                     "Machine Speed Dial" = "Machine speed reading"
                     ))
    
    return(sinter_new)
}