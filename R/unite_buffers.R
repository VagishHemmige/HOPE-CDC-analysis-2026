#Unite for total calculations

Transplant_center_all_buffer_united<-list()
Transplant_centers_active_buffer_united<-list()
Transplant_centers_HIV_buffer_united<-list()
Transplant_centers_HOPE_buffer_united<-list()

for (organ_loop in organ_list){
  for (distance_loop in distance_list){
    
    Transplant_center_all_buffer_united[[organ_loop]][[distance_loop]]<-
      Transplant_center_all_buffer[[organ_loop]][[distance_loop]]%>%
      reframe(
        catchment_area = "All Centers", 
        geometry = st_union(geometry)
      )
    
    for (year_loop in year_list){
      
      
      Transplant_centers_active_buffer_united[[organ_loop]][[distance_loop]][[year_loop]]<-
        Transplant_centers_active_buffer[[organ_loop]][[distance_loop]][[year_loop]]%>%
        reframe(
          catchment_area = "All Centers", 
          geometry = st_union(geometry)
        )
      
      Transplant_centers_HIV_buffer_united[[organ_loop]][[distance_loop]][[year_loop]]<-
        Transplant_centers_HIV_buffer[[organ_loop]][[distance_loop]][[year_loop]]%>%
        reframe(
          catchment_area = "All Centers", 
          geometry = st_union(geometry)
        )
      
      
      
      Transplant_centers_HOPE_buffer_united[[organ_loop]][[distance_loop]][[year_loop]]<-
        Transplant_centers_HOPE_buffer[[organ_loop]][[distance_loop]][[year_loop]]%>%
        reframe(
          catchment_area = "All Centers", 
          geometry = st_union(geometry)
        )
      
      
      
    }
    
  }
}





