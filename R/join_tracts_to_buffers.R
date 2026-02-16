#ST join to individual tracts and transplant centers


tracts_in_buffers_active<-list()
tracts_in_buffers_HIV<-list()
tracts_in_buffers_HOPE<-list()




for (organ_loop in organ_list){
  for (distance_loop in distance_list){
    for (year_loop in year_list){

      message(paste("Currently on:", organ_loop, "", distance_loop, "for year:", year_loop))
      message("Calculating tracts for active...")
      
      #This leads to duplication of tracts if they fall in more than one transplant center's catchment area)      
      tracts_in_buffers_active[[organ_loop]][[distance_loop]][[year_loop]] <- 
        st_join(Merged_tracts[[year_loop]], 
                Transplant_centers_active_buffer[[organ_loop]][[distance_loop]][[year_loop]], 
                join = st_intersects)
      
      
      message("Calculating tracts for HIV...")
      
      tracts_in_buffers_HIV[[organ_loop]][[distance_loop]][[year_loop]] <- 
        st_join(Merged_tracts[[year_loop]], 
                Transplant_centers_HIV_buffer[[organ_loop]][[distance_loop]][[year_loop]], 
                join = st_intersects)
      
      message("Calculating tracts for HOPE...")
      
      tracts_in_buffers_HOPE[[organ_loop]][[distance_loop]][[year_loop]] <- 
        st_join(Merged_tracts[[year_loop]], 
                Transplant_centers_HOPE_buffer[[organ_loop]][[distance_loop]][[year_loop]], 
                join = st_intersects)


      
            
    }
  }
}


