Transplant_centers_active_buffer <- list()
Transplant_centers_HIV_buffer <- list()

for (organ_loop in organ_list) {
  for (distance_loop in c("60min")) {
    for (year_loop in year_list)
    {
      
      active_name<-paste0("cache/Transplant_centers_active_",distance_loop,"_buffer_",organ_loop,"_",year_loop, ".RDS")
      HIV_name<-paste0("cache/Transplant_centers_HIV_",distance_loop,"_buffer_",organ_loop,"_",year_loop, ".RDS")
      
      Transplant_centers_active_buffer[[organ_loop]][[distance_loop]][[year_loop]]<-readRDS(active_name)
      Transplant_centers_HIV_buffer[[organ_loop]][[distance_loop]][[year_loop]]<-readRDS(HIV_name)
      
        
    }
  }
}

