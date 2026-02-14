# Figures

county_dots<-list()
county_dots_transformed<-list()

for (year_loop in year_list)
  
{

county_dots[[year_loop]]<-as_dot_density(Merged_Counties[[year_loop]], 
                                         "county_cases", 
                                         values_per_dot =100)

county_dots_transformed[[year_loop]]<-county_dots[[year_loop]]%>%
  st_transform(5070)

}

#Transform us state map
us_state_transformed<-us_states%>%
  st_transform(5070)


Activemap<-list()
HIV_map<-list()

for (organ_loop in organ_list) {
  for (distance_loop in distance_list){
    for (year_loop in year_list) {
      
      #Map all transplant centers and a 50-mile catchment area upon the dot map of HIV
      Activemap[[organ_loop]][[year_loop]]<-ggplot() +
        geom_sf(data = county_dots_transformed[[year_loop]], size = 0.3, color = "navy", alpha = 0.5) +
        theme_minimal() +
        labs(title = paste("Radius around kidney transplant centers,", year_loop),
             subtitle = "(1 dot = 100 HIV cases)")+
        geom_sf(data=Transplant_centers_active[[organ_loop]][[year_loop]], color="red", fill=NA)+
        geom_sf(data=us_state_transformed, fill=NA)
      ggsave(paste0("figures/",organ_loop,"/Active ", organ_loop," ",distance_loop," map", year_loop,".svg"))
     
      
      
      #Map transplant centers with HIV R+ and a 60 minute catchment area upon the dot map of HIV, 2013-2017
      HIV_map[[organ_loop]][[year_loop]]<-ggplot() +
        geom_sf(data = county_dots_transformed[[year_loop]], size = 0.3, color = "navy", alpha = 0.5) +
        theme_minimal() +
        labs(title = paste0(as.numeric(year_loop)-4, " - ", year_loop))+
        geom_sf(data=Transplant_centers_HIV_buffer[[organ_loop]][[distance_loop]][[year_loop]], color="red", fill=NA)+
        geom_sf(data=us_state_transformed, fill=NA)
      ggsave(paste0("figures/",organ_loop,"/HIV ", organ_loop," ",distance_loop," map", year_loop,".svg"))
      

    }
  }
}

