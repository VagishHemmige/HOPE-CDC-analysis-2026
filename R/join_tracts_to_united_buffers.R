#Join tracts to united buffers


for (organ_loop in organ_list){
  for (distance_loop in distance_list){
    for (year_loop in year_list){
      
      message(paste("Currently on:", organ_loop, "", distance_loop, "for year:", year_loop))
      message("Calculating tracts for active...")
      
      #ST join to individual tracts and the united transplant center geography to avoid duplication
      tracts_in_buffers_active_united[[organ_loop]][[distance_loop]][[year_loop]] <- 
        st_join(Merged_tracts[[year_loop]], 
                Transplant_centers_active_buffer_united[[organ_loop]][[distance_loop]][[year_loop]], 
                join = st_intersects)
      
      message("Calculating tracts for HIV...")
      tracts_in_buffers_HIV_united[[organ_loop]][[distance_loop]][[year_loop]] <- 
        st_join(Merged_tracts[[year_loop]], 
                Transplant_centers_HIV_buffer_united[[organ_loop]][[distance_loop]][[year_loop]], 
                join = st_intersects)
      
      message("Calculating tracts for HOPE...")
      tracts_in_buffers_HOPE_united[[organ_loop]][[distance_loop]][[year_loop]] <- 
        st_join(Merged_tracts[[year_loop]], 
                Transplant_centers_active_buffer_united[[organ_loop]][[distance_loop]][[year_loop]], 
                join = st_intersects)
    }}}


=