#ST join to individual tracts and transplant centers


tracts_in__buffers_active<-list()
tracts_in__buffers_HIV<-list()
tracts_in__buffers_HOPE<-list()

tracts_in__buffers_active_united<-list()
tracts_in__buffers_HIV_united<-list()
tracts_in__buffers_HOPE_united<-list()


for (organ_loop in organ_list){
  for (distance_loop in distance_list){
    for (year_loop in year_list){

      
      #This leads to duplication of tracts if they fall in more than one transplant center's catchment area)      
      tracts_in__buffers_active[[organ_loop]][[distance_loop]][[year_loop]] <- 
        st_join(Merged_tracts[[year_loop]], 
                Transplant_centers_active_buffer[[organ_loop]][[distance_loop]][[year_loop]], 
                join = st_intersects)
      
      tracts_in__buffers_HIV[[organ_loop]][[distance_loop]][[year_loop]] <- 
        st_join(Merged_tracts[[year_loop]], 
                Transplant_centers_HIV_buffer[[organ_loop]][[distance_loop]][[year_loop]], 
                join = st_intersects)
      
      tracts_in__buffers_HOPE[[organ_loop]][[distance_loop]][[year_loop]] <- 
        st_join(Merged_tracts[[year_loop]], 
                Transplant_centers_HOPE_buffer[[organ_loop]][[distance_loop]][[year_loop]], 
                join = st_intersects)


      
            
    }
  }
}


for (organ_loop in organ_list){
  for (distance_loop in distance_list){
    for (year_loop in year_list){
      
#ST join to individual tracts and the united transplant center geography to avoid duplication
tracts_in__buffers_active_united[[organ_loop]][[distance_loop]][[year_loop]] <- 
  st_join(Merged_tracts[[year_loop]], 
          Transplant_centers_active_buffer_united[[organ_loop]][[distance_loop]][[year_loop]], 
          join = st_intersects)
tracts_in__buffers_HIV_united[[organ_loop]][[distance_loop]][[year_loop]] <- 
  st_join(Merged_tracts[[year_loop]], 
          Transplant_centers_HIV_buffer_united[[organ_loop]][[distance_loop]][[year_loop]], 
          join = st_intersects)
tracts_in__buffers_HOPE_united[[organ_loop]][[distance_loop]][[year_loop]] <- 
  st_join(Merged_tracts[[year_loop]], 
          Transplant_centers_active_buffer_united[[organ_loop]][[distance_loop]][[year_loop]], 
          join = st_intersects)
}}}