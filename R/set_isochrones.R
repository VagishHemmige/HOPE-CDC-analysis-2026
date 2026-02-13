#set isochrones


#Convert 7 AM local time to UTC time for every time zone in the US, using the with_tz function

converted_times<-list()

for (year_loop in year_list){
  
  
  #Create list for each year that converts the time zones
  converted_times[[year_loop]] <- tibble(
    timezone = timezones
  ) %>%
    mutate(
      local_time = map2(
        reference_date[[year_loop]], timezone,
        ~ ymd_hms(paste(.x, "07:00:00"), tz = .y)
      ),
      utc_time = map(local_time, ~ with_tz(.x, "UTC"))
    ) %>%
    unnest(c(local_time, utc_time))
}
  
  #Create isochrone objects for transplant centersusing the mapboxapi package if results not already cached
  for (organ_loop in organ_list){

    
    all_center_path<-paste0("cache/Transplant_center_all_60min_buffer_",organ_loop, ".RDS")
    if (file.exists(all_center_path)) {
      Transplant_center_all_buffer[[organ_loop]][["60min"]] <- 
        readRDS(all_center_path)
    }else {
      
      Transplant_center_all_buffer[[organ_loop]][["60min"]]<-
        set_60_min_isochrone_at_7AM(Transplant_centers_all_sf[[organ_loop]], "2017")
      saveRDS(Transplant_center_all_buffer[[organ_loop]][["60min"]],
              all_center_path)
    } 
    
    for (year_loop in year_list){
    
       
    
    active_center_path<-paste0("cache/Transplant_centers_active_60min_buffer_",organ_loop, "_", year_loop,".RDS")
    if (file.exists(active_center_path)) {
      Transplant_centers_active_buffer[[organ_loop]][["60min"]][[year_loop]] <- 
        readRDS(active_center_path)
    }else {
      
    Transplant_centers_active_buffer[[organ_loop]][["60min"]][[year_loop]]<-
      set_60_min_isochrone_at_7AM(Transplant_centers_active_SF[[organ_loop]][[year_loop]], year_loop)
    saveRDS(Transplant_centers_active_buffer[[organ_loop]][["60min"]][[year_loop]],
            active_center_path)
    }
    
    HIV_center_path<-paste0("cache/Transplant_centers_HIV_60min_buffer_",organ_loop, "_", year_loop,".RDS")
    if (file.exists(HIV_center_path)) {
      Transplant_centers_HIV_buffer[[organ_loop]][["60min"]][[year_loop]]<-
        readRDS(HIV_center_path)
    } else {
      Transplant_centers_HIV_buffer[[organ_loop]][["60min"]][[year_loop]]<-
        set_60_min_isochrone_at_7AM(Transplant_centers_HIV_sf[[organ_loop]][[year_loop]], year_loop)
      saveRDS(Transplant_centers_HIV_buffer[[organ_loop]][["60min"]][[year_loop]],
              HIV_center_path)
    }
      

    HOPE_center_path<-paste0("cache/Transplant_centers_HOPE_60min_buffer_",organ_loop, "_", year_loop,".RDS")
    if (file.exists(HOPE_center_path)) {
      Transplant_centers_HOPE_buffer[[organ_loop]][["60min"]][[year_loop]]<-
        readRDS(HOPE_center_path)
    } else {
      Transplant_centers_HOPE_buffer[[organ_loop]][["60min"]][[year_loop]]<-
        set_60_min_isochrone_at_7AM(Transplant_centers_HOPE_sf[[organ_loop]][[year_loop]], year_loop)
      saveRDS(Transplant_centers_HOPE_buffer[[organ_loop]][["60min"]][[year_loop]],
              HOPE_center_path)
    }
  }
}

