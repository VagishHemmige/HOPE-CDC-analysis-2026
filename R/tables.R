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


for (organ_loop in organ_list)
{
  
  
  
  Results_HIV_df[[organ_loop]]%>%
    
    #Reshape data set
    pivot_wider(
      names_from = Year,
      values_from = c(Numerator, Denominator, Percentage),
      names_vary = "slowest"
      )%>%
    
    #Use the table defined above to turn Characteristic into a factor variable
    mutate(Characteristic=factor(Characteristic,
                                 levels=table_factor_values$value,
                                 labels = table_factor_values$label))%>%
    arrange(Characteristic)%>%
    
    #Convert to a gt format table
    gt()%>%
    
    #Groups columns by year with a spanner label
    {
      purrr::reduce(
        year_list,
        ~ .x %>%
          tab_spanner(
            label = .y,
            columns = matches(paste0("_", .y, "$"))
          ),
        .init = .
      )
    } %>%
    
    #Sets coluns to round to 0 decimal places for numbers and 1 decimal place for percentages
    fmt_number(
      columns = matches(paste0("Numerator_|Denominator_")),
      decimals = 0,
      use_seps = TRUE
    ) %>%
    fmt_number(
      columns = matches("Percentage_"),
      decimals = 1
    )%>%tab_header(
      title = "Access of PLWH to transplant centers"
    )%>%
    
    #Removes "_year" from columns
    cols_label_with(~ "Numerator",   columns = matches("^Numerator_")) %>%
    cols_label_with(~ "Denominator", columns = matches("^Denominator_")) %>%
    cols_label_with(~ "Percentage",  columns = matches("^Percentage_"))%>%
    
    #Shades columns by year for easier reading
    
    {
      purrr::reduce(
        seq_along(year_list),
        ~ .x %>%
          tab_style(
            style = cell_fill(
              color = colorRampPalette(c("lightgray","lightblue"))(length(year_list))[.y]
            ),
            locations = cells_body(
              columns = matches(paste0("_", year_list[.y], "$"))
            )
          ),
        .init = .
      )
    }%>%
      
      #Add separators between years
      {
        purrr::reduce(
          seq_len(length(year_list) - 1),
          ~ .x %>%
            tab_style(
              style = cell_borders(
                sides = "right",
                weight = px(2),
                color = "black"
              ),
              locations = cells_body(
                columns = matches(paste0("Percentage_", year_list[.y], "$"))
              )
            ) %>%
            tab_style(
              style = cell_borders(
                sides = "left",
                weight = px(2),
                color = "black"
              ),
              locations = cells_body(
                columns = matches(paste0("Numerator_", year_list[.y + 1], "$"))
              )
            ),
          .init = .
        )
      }
  
  %>%
    gtsave(paste0("tables/", organ_loop,"/",organ_loop, " analysis results Table 1.docx"))
  
  Results_nonHIV_df%>%
    pivot_wider(
      names_from = Year,
      values_from = c(Numerator, Denominator, Percentage))%>%
    select(Characteristic, 
           Numerator_2017, Denominator_2017, Percentage_2017, 
           Numerator_2022, Denominator_2022, Percentage_2022)%>%
    mutate(Characteristic=factor(Characteristic,
                                 levels=c("active 50mile","active 100mile", "active 200mile", "active 60min",
                                          "HIV 50mile","HIV 100mile", "HIV 200mile", "HIV 60min",
                                          "HOPE 50mile","HOPE 100mile", "HOPE 200mile", "HOPE 60min"),
                                 labels = c("Active kidney center 50 mile radius",
                                            "Active kidney center 100 mile radius", 
                                            "Active kidney center 200 mile radius", 
                                            "Active kidney center 60 min radius",
                                            "Active HIV R+ kidney center 50 mile radius",
                                            "Active HIV R+ kidney center 100 mile radius", 
                                            "Active HIV R+ kidney center 200 mile radius", 
                                            "Active HIV R+ kidney center 60 min radius",
                                            "Active HOPE D+ kidney center 50 mile radius",
                                            "Active HOPE D+ kidney center 100 mile radius",
                                            "Active HOPE D+ kidney center 200 mile radius",
                                            "Active HOPE D+ kidney center 60 min radius"
                                            
                                            
                                 )))%>%
    arrange(Characteristic)%>%
    gt() %>%
    tab_spanner(
      label = "2017", 
      columns = vars(Numerator_2017, Denominator_2017, Percentage_2017)
    ) %>%
    tab_spanner(
      label = "2022", 
      columns = vars(Numerator_2022, Denominator_2022, Percentage_2022)
    )%>%
    fmt_number(
      columns = vars(Numerator_2017, Denominator_2017, 
                     Numerator_2022, Denominator_2022),
      decimals = 0,  
      use_seps = TRUE
    )%>%
    tab_header(
      title = "Access of people without HIV to transplant centers in 2017 and 2022"  # Title of your table
    )%>%
    cols_label(
      Characteristic = "Characteristic",
      Numerator_2017 = "Numerator",
      Denominator_2017 = "Denominator",
      Percentage_2017 = "Percentage",
      Numerator_2022 = "Numerator",
      Denominator_2022 = "Denominator",
      Percentage_2022 = "Percentage"
    )%>%
    # Style for 2017 columns (data cells)
    tab_style(
      style = cell_fill(color = "lightgray"),
      locations = cells_body(columns = vars(Numerator_2017, Denominator_2017, Percentage_2017))
    ) %>%
    # Style for 2022 columns (data cells)
    tab_style(
      style = cell_fill(color = "lightblue"),
      locations = cells_body(columns = vars(Numerator_2022, Denominator_2022, Percentage_2022))
    ) %>%
    # Add borders to separate the two groups (2017 and 2022)
    tab_style(
      style = cell_borders(sides = c("right"), weight = px(2), color = "black"),
      locations = cells_body(columns = vars(Numerator_2017, Denominator_2017, Percentage_2017))
    ) %>%
    tab_style(
      style = cell_borders(sides = c("left"), weight = px(2), color = "black"),
      locations = cells_body(columns = vars(Numerator_2022, Denominator_2022, Percentage_2022))
    )%>%
    gtsave("Kidney analysis results/Table 2.docx")
  
  
}