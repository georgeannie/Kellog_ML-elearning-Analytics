library(dplyr)
library(stringr)
library(lubridate)
library(htmltools)
read_rename_sinter = function(){
  folder <- "./data/"  
  file_list = list.files(path=folder, pattern="*.csv", full.names = T)
    
  plant =list()
  for (i in 1:length(file_list)){
    plant[[i]]=assign(file_list[i], 
           read.csv(file_list[i], sep=','))
    names(plant[[i]])=str_to_title(gsub("_", " ", names(plant[[i]])))
    plant[[i]] = plant[[i]] %>%
      mutate(Timestamp=as.POSIXct(dmy_hm(Timestamp))) %>%
        plyr::rename(c("X.internal Return Fine" ="%Internal Return Fine",
                       "X.external Return Fine" ="%External Return Fine"))
      
  }
    return(plant)
}

read_rename_final_data = function(){
  sinter_full = read.csv("./data/syntheticData_20190531.csv",
                    stringsAsFactors = FALSE, header = TRUE)

  #Remove underscores and change column names to title format for display
      names(sinter_full)=str_to_title(gsub("_", " ", names(sinter_full)))

      return(sinter_full)
}


summary_table = function(input_table) {
  dep_no = apply(input_table, 2, function(x) (sum(!is.na(x))))
  dep_sd = apply(input_table, 2, sd, na.rm=TRUE)
  dep_mean = apply(input_table, 2, mean, na.rm=TRUE)
  dep_na = apply(input_table, 2, function(x) sum(is.na(x)))
  dep_median = apply(input_table, 2,  median, na.rm=T)
  dep_min = apply(input_table, 2, min, na.rm=TRUE)
  dep_max = apply(input_table, 2, max, na.rm=TRUE)
  
  dep_summary=data.table("Variables" = names(input_table),
                         "# of Observations" = dep_no,
                         "Standard Deviation" = round(dep_sd,2),
                         'Mean' = round(dep_mean, 2),
                         '# of Missing data' = dep_na,
                         'Median' = round(dep_median,2),
                         "Minimum Value" = dep_min,
                         "Maximum Value" = dep_max
  )
}

sinter=read_rename_final_data()

feed_data=function(){
  sinter %>%
      select("Total Bmix Flow" , "Total Clime Flow", "Total Limestone Flow", 
             "Total Solid Fuel Flow",
             "Feo"  ,    "Cao" , "Al2o3",  "Tio2" ,                
             "K2o",  "P", "Fe"  )
}

ignition_data = function(){
   sinter %>%
    select("Ih Combustion Air Temp" ,  "Ih Mixed Gas Temp" ,
           "Combustion Air Flow", "Ih Mixed Gas Flow", "Ih Temp M1" 
    )
}

sinter_bed = function(){
  sinter %>%
    select("Windbox 1 Pressure",       "Windbox 12 Pressure" ,
           "Machine Speed" , "Sinter Temp Sm Disch" ,"Sinter Bed Height" 
    )
  
}

stack= function(){
  sinter %>%
    select("Co", "Co2", "O2"                      
    )
}

esp=function(){
  sinter %>%
    select("Waste Water Flow" , "Temp Before Esp" , "Waste Gas Fan Rpm"                           
    )
  
}

cooler=function(){
  sinter %>%
    select("Cooler Pressure",  "Cooler Speed",  "Cooler Dischrg Temp"                             
    )
  
}