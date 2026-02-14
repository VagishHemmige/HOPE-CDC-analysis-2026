#Tables



#Define table with labels based on values in distance_list and types of outcomes

table_factor_values<-expand.grid(
  Type = c("active", "HIV", "HOPE"),
  Distance = distance_list,
  stringsAsFactors = FALSE)%>%
  mutate(value=paste(Type, Distance))%>%
  mutate(
    label = paste(
      recode(Type,
             active = paste("Active", organ_loop, "center"),
             HIV    = paste("Active HIV R+", organ_loop, "center"),
             HOPE   = paste("Active HOPE D+", organ_loop, "center")
      ),
      case_when(
        stringr::str_detect(Distance, "mile$") ~ 
          stringr::str_replace(Distance, "mile$", " mile radius"),
        stringr::str_detect(Distance, "min$")  ~ 
          stringr::str_replace(Distance, "min$", " min radius"),
        TRUE ~ Distance
      )
    )
  )

#Create tables as docx

for (organ_loop in organ_list)
{
  
  
  
  Results_HIV_df[[organ_loop]]%>%
    convert_resultsdf_to_table(year_list_fn=year_list,
                               row_values = table_factor_values$value,
                               row_labels = table_factor_values$label,
                               table_title= "Access to transplant in PLWH")%>%
    gtsave(paste0("tables/", organ_loop,"/",organ_loop, " analysis results Table 1.docx"))
  
  Results_nonHIV_df%>%
    convert_resultsdf_to_table(year_list_fn=year_list,
                               row_values = table_factor_values$value,
                               row_labels = table_factor_values$label,
                               table_title= "Access to transplant in people without HIV")%>%
    gtsave(paste0("tables/", organ_loop,"/",organ_loop, " analysis results Table 2.docx"))
  
  
}