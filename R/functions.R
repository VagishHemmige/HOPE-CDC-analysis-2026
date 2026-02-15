

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

convert_resultsdf_to_table<-function(resultsdf, 
                                     year_list_fn=year_list,
                                     row_values,
                                     row_labels,
                                     table_title){
  
  
  resultsdf%>%
    
    #Reshape data set
    pivot_wider(
      names_from = Year,
      values_from = c(Numerator, Denominator, Percentage),
      names_vary = "slowest"
    )%>%
    
    #Use the table defined above to turn Characteristic into a factor variable
    mutate(Characteristic=factor(Characteristic,
                                 levels = row_values,
                                 labels = row_labels))%>%
    arrange(Characteristic)%>%
    
    #Convert to a gt format table
    gt()%>%
    
    #Groups columns by year with a spanner label
    {
      purrr::reduce(
        year_list_fn,
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
      title = table_title
    )%>%
    
    #Removes "_year" from columns
    cols_label_with(~ "Numerator",   columns = matches("^Numerator_")) %>%
    cols_label_with(~ "Denominator", columns = matches("^Denominator_")) %>%
    cols_label_with(~ "Percentage",  columns = matches("^Percentage_"))%>%
    
    #Shades columns by year for easier reading
    
    {
      purrr::reduce(
        seq_along(year_list_fn),
        ~ .x %>%
          tab_style(
            style = cell_fill(
              color = colorRampPalette(c("lightgray","lightblue"))(
                length(year_list_fn)
              )[.y]            ),
            locations = cells_body(
              columns = matches(paste0("_", year_list_fn[.y], "$"))
            )
          ),
        .init = .
      )
    }%>%
    
    #Add separators between years
    {
      purrr::reduce(
        seq_len(length(year_list_fn) - 1),
        ~ .x %>%
          tab_style(
            style = cell_borders(
              sides = "right",
              weight = px(2),
              color = "black"
            ),
            locations = cells_body(
              columns = matches(paste0("Percentage_", year_list_fn[.y], "$"))
            )
          ) %>%
          tab_style(
            style = cell_borders(
              sides = "left",
              weight = px(2),
              color = "black"
            ),
            locations = cells_body(
              columns = matches(paste0("Numerator_", year_list_fn[.y + 1], "$"))
            )
          ),
        .init = .
      )
    }
  
  
}



#Write a function to parameterize the above so that it's easier to create the needed plots
make_paired_plot<-function(organ,
                           distance,
                           outcome,
                           buffer_list,
                           year1="2017",
                           year2="2022",
                           plottitle,
                           include_nonHIV=FALSE)
{
  
  # ----Error checking
  
  # Validate include_nonHIV
  if (!is.logical(include_nonHIV) || length(include_nonHIV) != 1 || is.na(include_nonHIV)) {
    stop("include_nonHIV must be a single TRUE or FALSE value.")
  }
  
  
  #Access name of object passed to `buffer_list`
  buffer_name <- deparse(substitute(buffer_list))
  
  #Make sure name of outcome matches buffer_list
  if (!stringr::str_detect(
    stringr::str_to_lower(buffer_name),
    stringr::str_to_lower(outcome)
  )) {
    warning(glue::glue(
      "Buffer object ('{buffer_name}') may not match outcome '{outcome}'."
    ))
  }
  
  if (!year1 %in% year_list) {
    stop(glue::glue("year1 ({year1}) is not in year_list."))
  }
  
  if (!year2 %in% year_list) {
    stop(glue::glue("year2 ({year2}) is not in year_list."))
  }
  
  if (!outcome %in% c("active", "HIV", "HOPE")) {
    stop(glue::glue("outcome ({outcome}) is not in c('active', 'HIV', 'HOPE')."))
  }
  
  #Keep only elements of Results_HIV_df necessary for plot
  summary_df_HIV<-Results_HIV_df[[organ]]%>%
    filter(str_detect(Characteristic, outcome))%>%
    filter(str_detect(Characteristic, distance))
  
  #Keep only elements of Results_nonHIV_df necessary for plot, if even needed in the first place
  if (include_nonHIV)
  {
    summary_df_nonHIV<-Results_nonHIV_df[[organ]]%>%
      filter(str_detect(Characteristic, outcome))%>%
      filter(str_detect(Characteristic, distance))
  }
  
  #Creates text for nonHIV (is NULL if )
  title_text_nonHIV<-list()
  
  if (include_nonHIV==TRUE) {
  title_text_nonHIV[[year1]]<-
    glue("<br>",
         "<span style='font-size:30pt; font-weight:normal;'>HIV negative:</span> ",
         "<span style='font-size:30pt; font-weight:bold;'>",
         "{summary_df_nonHIV$Percentage[summary_df_nonHIV$Year == as.numeric(year1)]}%",
         "</span>",
         "<br>",
         "{comma(summary_df_nonHIV$Numerator[summary_df_nonHIV$Year == as.numeric(year1)])}",
         " / ",
         "{comma(summary_df_nonHIV$Denominator[summary_df_nonHIV$Year == as.numeric(year1)])}")
  
  title_text_nonHIV[[year2]]<-
    glue("<br>",
         "<span style='font-size:30pt; font-weight:normal;'>HIV negative:</span> ",
         "<span style='font-size:30pt; font-weight:bold;'>",
         "{summary_df_nonHIV$Percentage[summary_df_nonHIV$Year == as.numeric(year2)]}%",
         "</span>",
         "<br>",
         "{comma(summary_df_nonHIV$Numerator[summary_df_nonHIV$Year == as.numeric(year2)])}",
         " / ",
         "{comma(summary_df_nonHIV$Denominator[summary_df_nonHIV$Year == as.numeric(year2)])}")
  }
  else {
    title_text_nonHIV[[year1]]<-NULL
    title_text_nonHIV[[year2]]<-NULL
  }
  

  
  #Left graph
  Plot1<-ggplot() +
    geom_sf(data = county_dots_transformed[[year1]], size = 0.3, color = "navy", alpha = 0.5) +
    theme_void() +
    theme(
      axis.title.y = element_text(size = 36, angle = 90, vjust = 0.5),
      plot.title = element_textbox(size = 16, hjust = 0.5,
                                   halign = 0.5)
    )+
    labs(y=year1,
         title = glue("<span style='font-size:30pt; font-weight:normal;'>PLWH:</span> ",
                      "<span style='font-size:30pt; font-weight:bold;'>",
                      "{summary_df_HIV$Percentage[summary_df_HIV$Year == as.numeric(year1)]}%",
                      "</span>",
                      "<br>",
                      "{comma(summary_df_HIV$Numerator[summary_df_HIV$Year == as.numeric(year1)])}",
                      " / ",
                      "{comma(summary_df_HIV$Denominator[summary_df_HIV$Year == as.numeric(year1)])}",
                      title_text_nonHIV[[year1]]
         ))+
    geom_sf(data=buffer_list[[organ]][[distance]][[year1]], color="red", fill=NA)+
    geom_sf(data=states_sf_transformed, fill=NA)
  
  
  #Right plot
  Plot2<-ggplot() +
    geom_sf(data = county_dots_transformed[[year2]], size = 0.3, color = "navy", alpha = 0.5) +
    theme_void() +
    theme(
      axis.title.y = element_text(size = 36, angle = 90, vjust = 0.5),
      plot.title = element_textbox(size = 16, hjust = 0.5,
                                   halign = 0.5)
    )+
    labs(y=year2,
         title = glue("<span style='font-size:30pt; font-weight:normal;'>PLWH:</span> ",
                      "<span style='font-size:30pt; font-weight:bold;'>",
                      "{summary_df_HIV$Percentage[summary_df_HIV$Year == as.numeric(year2)]}%",
                      "</span>",
                      "<br>",
                      "{comma(summary_df_HIV$Numerator[summary_df_HIV$Year == as.numeric(year2)])}",
                      " / ",
                      "{comma(summary_df_HIV$Denominator[summary_df_HIV$Year == as.numeric(year2)])}",
                      title_text_nonHIV[[year2]]
         ))+
    geom_sf(data=buffer_list[[organ]][[distance]][[year2]], color="red", fill=NA)+
    geom_sf(data=states_sf_transformed, fill=NA)
  
  #Combine plots
  combined<-cowplot::plot_grid(Plot1, Plot2,
                               ncol = 2, align = "hv")
  
  final_plot <- ggdraw() +
    draw_plot(combined) +
    draw_label(
      plottitle,
      x = 0.5, y = 0.9,           # top center
      hjust = 0.5, vjust = 1,   # anchor to the top edge
      fontface = 'bold',
      size = 25
    )+
    # 
    draw_label(
      "Each blue dot represents 100 people with HIV\nRed outlines represent the catchment area for each transplant center",
      x = 0.02, y = 0.1,          # left margin position
      hjust = 0, vjust = 0,        # align text to the left edge
      size = 14,
      fontface = "italic"
    )
  
  return(final_plot)
  
}

