

#Define a function that takes an SF data object and returns 60 minute isochrones
set_60_min_isochrone_at_7AM<-function(SFObject, year) {
  
  if (nrow(SFObject) == 0) {
    return(
      SFObject %>%
        st_transform(5070)
    )
  }else{
  tempDF<-SFObject%>%
    mutate(TimeZone=tz_lookup(., method="accurate"))%>%
    st_transform(4326)
  
  FinalDF <- vector("list", length(timezones))
  
  
  for(i in 1:length(timezones))
  {
    TZ<-converted_times[[year]][[i,1]]
    
    UTCTime<-format(converted_times[[year]][[i,3]], "%Y-%m-%dT%H:%M:%SZ")
    
    if (nrow(filter(tempDF,TimeZone==TZ))>0)
    {
      temp<-tempDF%>%
        filter(TimeZone==TZ)%>%
        mb_isochrone(time = 60, 
                     profile = "driving-traffic",
                     depart_at = UTCTime,
                     id_column = "OTCCode")%>%
        rename(OTCName=id)
      FinalDF[[i]]<-temp
    }
  }
  
  FinalDF%>%
    bind_rows()%>%
    st_transform(5070)%>%
    return()
  }
}

#Function that converts results df into a gt format table
