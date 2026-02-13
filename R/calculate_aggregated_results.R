#Calculate results for geographic analyses


Results_HIV_df<-list()
Results_nonHIV_df<-list()
Tract_total_HIV_cases<-list()
Tract_total_nonHIV_cases<-list()
                      
for (organ_loop in organ_list)
  
{
  
  #Initialize data frames
  Results_HIV_df[[organ_loop]] <- tibble(
    Characteristic = character(),
    Numerator = numeric(),
    Denominator = numeric(),
    Percentage = numeric(),
    Year= numeric()
  )
  
  Results_nonHIV_df[[organ_loop]] <- tibble(
    Characteristic = character(),
    Numerator = numeric(),
    Denominator = numeric(),
    Percentage = numeric(),
    Year= numeric()
  )
  
  
  #Calculate the total number of HIV cases assigned to a tract
  Tract_total_HIV_cases[[year_loop]]<-Merged_tracts[[year_loop]]%>%
    st_drop_geometry()%>%
    summarize(total_cases=sum(tract_cases, na.rm=TRUE))%>%
    pull(total_cases)%>%
    first()
  
  #Calculate the total number of nonHIV cases (i.e. people without HIV) assigned to a tract
  Tract_total_nonHIV_cases[[year_loop]]<-Merged_tracts[[year_loop]]%>%
    st_drop_geometry()%>%
    summarize(total_cases=sum(tract_noncases, na.rm=TRUE))%>%
    pull(total_cases)%>%
    first()
}




#Calculate table for PLWH


for (organ_loop in organ_list)
{
  for (distance_loop in distance_list){
    for (year_loop in year_list){
      for (type in c("active", "HIV", "HOPE")) {
        
        name_of_df<-paste0("tracts_in_buffers_", type, "_united")
        print(name_of_df)
        
        temp_Num<-get(name_of_df)[[organ_loop]][[distance_loop]][[year_loop]]%>%
          st_drop_geometry()%>%
          group_by(catchment_area)%>%
          summarize(total_cases=sum(tract_cases, na.rm=TRUE))%>%
          pull(total_cases)%>%
          first()
        
        temp_Denom<-Tract_total_HIV_cases[[year_loop]]
        
        
        Results_HIV_df[[organ_loop]]<-Results_HIV_df[[organ_loop]]%>%
          add_row(
            Characteristic = paste(type, distance_loop),
            Numerator = round(temp_Num, 0),
            Denominator = temp_Denom,
            Percentage = round(100*Numerator/Denominator,1),
            Year= year_loop
          ) 
        
      }
    }
  }
}


#Calculate tract-level results for people without HIV
for (organ_loop in organ_list)
{
  for (distance_loop in distance_list){
    for (year_loop in year_list){
      for (type in c("active")) {
        
        name_of_df<-paste0("tracts_in_buffers_", type, "_united")
        print(name_of_df)
        
        temp_Num<-get(name_of_df)[[organ_loop]][[distance_loop]][[year_loop]]%>%
          st_drop_geometry()%>%
          group_by(catchment_area)%>%
          summarize(total_noncases=sum(tract_noncases, na.rm=TRUE))%>%
          pull(total_noncases)%>%
          first()
        
        temp_Denom<-Tract_total_nonHIV_cases[[year_loop]]
        
        Results_nonHIV_df[[organ_loop]]<-Results_nonHIV_df[[organ_loop]]%>%
          add_row(
            Characteristic = paste(type, distance_loop),
            Numerator = round(temp_Num, 0),
            Denominator = temp_Denom,
            Percentage = round(100*Numerator/Denominator,1),
            Year= year_loop)
        
        
        
      }
    }
  }
  
}


