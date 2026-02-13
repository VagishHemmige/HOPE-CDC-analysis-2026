#Unite for total calculations

Transplant_center_all_50mile_buffer_united<-Transplant_center_all_50mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_active_2017_50mile_buffer_united<-Transplant_centers_active_2017_50mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_active_2022_50mile_buffer_united<-Transplant_centers_active_2022_50mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HIV_2017_50mile_buffer_united<-Transplant_centers_kidney_HIV_2017_50mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HIV_2022_50mile_buffer_united<-Transplant_centers_kidney_HIV_2022_50mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HOPE_2017_50mile_buffer_united<-Transplant_centers_kidney_HOPE_2017_50mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HOPE_2022_50mile_buffer_united<-Transplant_centers_kidney_HOPE_2022_50mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )


#100 mile
Transplant_center_all_100mile_buffer_united<-Transplant_center_all_100mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_active_2017_100mile_buffer_united<-Transplant_centers_active_2017_100mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_active_2022_100mile_buffer_united<-Transplant_centers_active_2022_100mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HIV_2017_100mile_buffer_united<-Transplant_centers_kidney_HIV_2017_100mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HIV_2022_100mile_buffer_united<-Transplant_centers_kidney_HIV_2022_100mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HOPE_2017_100mile_buffer_united<-Transplant_centers_kidney_HOPE_2017_100mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HOPE_2022_100mile_buffer_united<-Transplant_centers_kidney_HOPE_2022_100mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )



#200 miles
Transplant_center_all_200mile_buffer_united<-Transplant_center_all_200mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_active_2017_200mile_buffer_united<-Transplant_centers_active_2017_200mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_active_2022_200mile_buffer_united<-Transplant_centers_active_2022_200mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HIV_2017_200mile_buffer_united<-Transplant_centers_kidney_HIV_2017_200mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HIV_2022_200mile_buffer_united<-Transplant_centers_kidney_HIV_2022_200mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HOPE_2017_200mile_buffer_united<-Transplant_centers_kidney_HOPE_2017_200mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HOPE_2022_200mile_buffer_united<-Transplant_centers_kidney_HOPE_2022_200mile_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )



#60 min buffer
Transplant_centers_active_2017_60min_buffer_united<-Transplant_centers_active_2017_60min_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_active_2022_60min_buffer_united<-Transplant_centers_active_2022_60min_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HIV_2017_60min_buffer_united<-Transplant_centers_kidney_HIV_2017_60min_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HIV_2022_60min_buffer_united<-Transplant_centers_kidney_HIV_2022_60min_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HOPE_2017_60min_buffer_united<-Transplant_centers_kidney_HOPE_2017_60min_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )

Transplant_centers_kidney_HOPE_2022_60min_buffer_united<-Transplant_centers_kidney_HOPE_2022_60min_buffer%>%
  summarize(
    catchment_area = "All Centers", 
    geometry = st_union(geometry)
  )
