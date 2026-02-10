#Loads data

# ----Load US state maps from census----
states_sf <- tigris::states(cb = FALSE, year = 2022)%>%
  select(NAME, STUSPS, geometry) %>%
  filter(!(NAME %in% c("Alaska", 
                       "Hawaii", 
                       "Puerto Rico", 
                       "United States Virgin Islands",
                       "Commonwealth of the Northern Mariana Islands"))) %>%
  rename(State=NAME)


#----Import transplant data from UNOS SRTR----

tx<-list()

tx[["kidney"]] <- load_srtr_file("tx_ki")
tx[["liver"]] <- load_srtr_file("tx_li")
tx[["heart"]] <- load_srtr_file("tx_hr")
tx[["lung"]] <- load_srtr_file("tx_lu")
tx[["pancreas"]] <- load_srtr_file("tx_kp")



donor_deceased <- load_srtr_file("donor_deceased")%>%
  select(DONOR_ID, DON_ANTI_HIV, DON_HIV_NAT)


#----Load transplant center data from SRTR package----
Transplant_centers_all<-list()

for (organ_loop in organ_list)
{
  

  Transplant_centers_all[[organ_loop]]<-sRtr::get_hrsa_transplant_centers()%>%
    filter(OPO_STATE_ABBR != "PR" & OPO_STATE_ABBR!= "HI")%>%
    select(OTC_NM, OTC_CD, Service_Lst, X, Y) %>%
    filter(str_detect(Service_Lst, str_to_sentence(organ_loop)))%>%
    select(-Service_Lst)%>%
    distinct()
  
  
  
}
