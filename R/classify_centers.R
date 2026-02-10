#Identify active adult centers in years of interest


# Initalize lists

Active_tx_centers<-list()
HIV_center_volumes<-list()
HOPE_center_volumes<-list()
State_level_transplant_volumes<-list()

for (organ_loop in organ_list)
{
  
  for (year_loop in year_list)
    
    
  {
    
    Active_tx_centers[[organ_loop]][[year_loop]]<-tx[[organ_loop]]%>%
      filter(REC_AGE_IN_MONTHS_AT_TX>=216)%>% #Adult recipient
      mutate(year_transplant=year(REC_TX_DT))%>%
      filter(year_transplant==year_loop)%>%
      group_by(REC_CTR_CD)%>%
      count()%>%
      pull(REC_CTR_CD)
    
    #Calculate center level volumes for pts with HIV in five-year period ending in year for active centers
    HIV_center_volumes[[organ_loop]][[year_loop]]<-tx[[organ_loop]]%>%
      filter(REC_HIV_STAT=="P")%>%
      filter(REC_AGE_IN_MONTHS_AT_TX>=216)%>% #Adult recipient
      mutate(year_transplant=year(REC_TX_DT))%>%
      filter(year_transplant>=(year_loop-4) & year_transplant<=year_loop)%>%
      group_by(REC_CTR_CD)%>%
      count()%>%
      filter(REC_CTR_CD %in% Active_tx_centers[[organ_loop]][[year_loop]])
    
    
    #Identify HOPE centers in years of interest
    HOPE_center_volumes[[organ_loop]][[year_loop]]<-tx[[organ_loop]]%>%
      filter(REC_HIV_STAT=="P")%>%
      filter(REC_AGE_IN_MONTHS_AT_TX>=216)%>%
      mutate(year_transplant=year(REC_TX_DT	))%>%
      filter(year_transplant>=(year_loop-4) & year_transplant<=year_loop)%>%
      left_join(donor_deceased)%>%
      filter(DON_HIV_NAT=="P"| DON_ANTI_HIV=="P")%>%
      group_by(REC_CTR_CD	)%>%
      count()
    
    #Convert to state transplant volumes for recipients with HIV
    State_level_transplant_volumes[[organ_loop]][[year_loop]]<-HIV_center_volumes[[organ_loop]][[year_loop]]%>%
      mutate(state_code = substr(REC_CTR_CD, 1, 2))%>%
      group_by(state_code)%>%
      summarize(total_HIV_transplants=sum(n, na.rm = TRUE))
    
  }
}