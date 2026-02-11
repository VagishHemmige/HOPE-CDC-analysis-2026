# Create buffer objects for transplant centers based on distance

Transplant_center_all_buffer<-list()
Transplant_centers_active_buffer<-list()
Transplant_centers_HIV_buffer<-list()
Transplant_centers_HOPE_buffer<-list()


for (organ_loop in organ_list){
  for (distance_loop in distance_list){
    
    if (str_detect(distance_loop, "mile"))
    {
      
      distance<-case_when(
        distance_loop=="50mile" ~ 80467.2,
        distance_loop=="100mile" ~ 160934.4,
        distance_loop=="200mile" ~ 321868
      )
      
      Transplant_center_all_buffer[[organ_loop]][[distance_loop]]<-st_buffer(Transplant_centers_all_sf [[organ_loop]],dist = distance)
      
      for (year_loop in year_list)
      {
        
        Transplant_centers_active_buffer[[organ_loop]][[distance_loop]][[year_loop]]<-
          st_buffer(Transplant_centers_active_SF[[organ_loop]][[year_loop]], 
                    dist = distance)
        
        Transplant_centers_HIV_buffer[[organ_loop]][[distance_loop]][[year_loop]]<-
          st_buffer(Transplant_centers_HIV_sf[[organ_loop]][[year_loop]], 
                    dist = distance)
        
        Transplant_centers_HOPE_buffer[[organ_loop]][[distance_loop]][[year_loop]]<-
          st_buffer(Transplant_centers_HOPE_sf[[organ_loop]][[year_loop]], 
                    dist = distance)
        
      }
      
      
    }
    
  }
}


