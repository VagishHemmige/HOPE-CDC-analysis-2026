


Transplant_centers_all_sf<-list()
Transplant_centers_active_SF<-list()
Transplant_centers_HIV_sf<-list()
Transplant_centers_HOPE_sf<-list()

for (organ_loop in organ_list)
  
{

  #Convert transplant center data to geographical data
  Transplant_centers_all_sf[[organ_loop]]<-Transplant_centers_all[[organ_loop]]%>%
    st_transform(5070)  
  
  
  
  for (year_loop in year_list) {
    

    Transplant_centers_active_SF[[organ_loop]][[year_loop]]<-Transplant_centers_active[[organ_loop]][[year_loop]]%>%
      st_transform(5070)
    

    #Geographic file of centers that performed an HIV HIV R+ transplant at the designated time point
    Transplant_centers_HIV_sf[[organ_loop]][[year_loop]] <- Transplant_centers_all_sf[[organ_loop]]%>%
      filter(OTCCode %in% HIV_center_volumes[[organ_loop]][[year_loop]]$REC_CTR_CD)
    
    #Geographic file of centers that performed an HIV D+/R+ transplant at the designated time point
    Transplant_centers_HOPE_sf[[organ_loop]][[year_loop]] <- Transplant_centers_all_sf[[organ_loop]]%>%
      filter(OTCCode %in% HOPE_center_volumes[[organ_loop]][[year_loop]]$REC_CTR_CD)
    
    
    
    
  }
  
  
}



Merged_Counties<-list()
Merged_Counties_nonSF<-list()
Merged_tracts<-list()

for (year_loop in year_list) {

  #Join county geographical data with CDC data.
  Merged_Counties[[year_loop]]<-left_join(us_counties_population[[year_loop]],
                                          AtlasPlusTableData_county_totals[[year_loop]], 
                                          by=join_by(GEOID==geo_id))
  
  Merged_Counties_nonSF[[year_loop]]<-Merged_Counties[[year_loop]]%>%
    st_drop_geometry()
  

  
  #Merge county data with tracts
  Merged_tracts[[year_loop]]<-left_join(us_tracts_population[[year_loop]], 
                                Merged_Counties_nonSF[[year_loop]], 
                                by=c("county_fips"="GEOID"))%>%
    select(-NAME.x, -NAME.y)%>%
    #Estimate census tract HIV population
    mutate(tract_cases=county_cases*tract_population/county_population)%>%
    #Estimate HIV-negative population in census tracts
    mutate(tract_noncases=tract_population-tract_cases)%>%
    st_transform(5070)
  
  
}



