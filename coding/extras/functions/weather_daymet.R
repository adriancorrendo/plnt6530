weather.daymet <- function(input, dpp = 0){ 
  # Downloads the daily weather data from the DAYMET database and process it
  # Args:
  #  input = input file containing the locations and the start & end dates for the time series
  #  dpp = days prior to the Start
  # Returns:
  #  a tibble of DAYMET weather variables for the requested time period
  # STEP 1. Make use of metadata (locations and dates)
  input %>%
    dplyr::mutate(
      Weather = purrr::pmap(list(ID = ID,
                                 # Rename vars to avoid conflicts
                                 lat = latitude,
                                 lon = longitude,
                                 sta = Start - dpp,
                                 end = End),
                            
                            # STEP 2. Retrieving daymet data:
                            function(ID, lat, lon, sta, end) {
                              daymetr::download_daymet(site = ID,
                                                       lat = lat, 
                                                       lon = lon,
                                                       # Extracting year from date:
                                                       start = as.numeric(substr(sta, 1, 4)),
                                                       end = as.numeric(substr(end, 1, 4)),
                                                       internal = TRUE, 
                                                       simplify = TRUE)})) %>% 
    
    # STEP 3. Organizing dataframe (Re-arranging rows and columns)
    dplyr::mutate(Weather = Weather %>% 
                    # i. Adjusting dates format with lubridate and map()
                    purrr::map(~ 
                                 dplyr::mutate(., 
                                               Date = as.Date(as.numeric(yday) - 1, # Day of the year
                                                              origin = paste0(year, '-01-01')),
                                               Year = year(Date),
                                               Month = month(Date),
                                               Day = mday(Date))) %>%
                    # ii. Select columns of interest
                    purrr::map(~ 
                                 dplyr::select(., yday, Year, Month, Day,
                                               Date, measurement, value)) %>%
                    # iii. Re-arrange columns wider
                    purrr::map(~ 
                                 tidyr::pivot_wider(.,
                                                    names_from = measurement, values_from = value)) %>%
                    # iv. Renaming variables with rename_with()
                    purrr::map(~ rename_with(., ~c(
                      "DOY",   # Date as Day of the year
                      "Year",  # Year
                      "Month", # Month 
                      "Day",   # Day of the month
                      "Date",  # Date as normal format
                      "DL",    # Day length (sec)
                      "PP",    # Precipitation (mm)
                      "Rad",   # Radiation (W/m2)
                      "SWE",   # Snow water (kg/m2)
                      "Tmax",  # Max. temp. (degC)
                      "Tmin",  # Min. temp. (degC)
                      "VP")))) %>%   # Vap Pres (Pa)
    # STEP 4. Processing data given start and ending dates:
    dplyr::mutate(Weather = purrr::pmap(list(sta = Start - dpp, 
                                             end = End, 
                                             data = Weather), # Requested period
                                        function(sta, end, data) {
                                          dplyr::filter(data, Date >= sta & Date <= end) 
                                        })) %>%
    # STEP 5. Unnest
    tidyr::unnest(cols = c(Weather)) %>% 
    
    # STEP 6. Converting units and adding extra variables:
    dplyr::mutate(Rad = Rad*0.000001*DL, # Radiation (W/m2 to MJ/m2)
                  Tmean = (Tmax+Tmin)/2, # Mean temperature (degC),
                  VP = VP / 1000, # VP (Pa to kPa),
                  # Creating variables for ET0 estimation:
                  lat_rad = latitude*0.0174533,
                  dr = 1 + 0.033*cos((2*pi/365)*DOY),
                  Sd = 0.409*sin((2*pi/365)*DOY - 1.39),
                  ws = acos(-tan(lat_rad)*tan(Sd)),
                  Ra = (24*60)/(pi) * Gsc * dr * (ws*sin(lat_rad)*sin(Sd) + cos(lat_rad)*sin(ws)),
                  ET0_HS = 0.0135 * kRs * (Ra / 2.45) * (sqrt(Tmax-Tmin)) * (Tmean + 17.8),
                  # Extreme PP events
                  EPE_i = case_when((PP > 25) ~ 1, TRUE ~ 0),
                  # Extreme Temp events
                  ETE_i = case_when((Tmax >= 30) ~ 1, TRUE ~ 0),
                  # Day length (hours)
                  DL = (DL/60)/60 
    ) %>% 
    dplyr::select(-lat_rad, -dr, -Sd, -ws, -Ra)
  
}