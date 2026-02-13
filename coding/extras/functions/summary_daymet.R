# Defining the function to summarize DAYMET and/or NASA-POWER
summary.daymet <- function(input, intervals) {
  # Creates summaries of the DAYMET daily data over the requested period
  # Args:
  #  input = a weather data object such as df.weather.daymet with the daily weather data
  #  intervals = a tibble with the start and end date for the summary period
  # STEP 1. 
  intervals %>%
    # NOTE: mergeing on ID only as the key, so remove Site:
    dplyr::select(-Site) %>%
    # Merging weather data:
    dplyr::left_join(input %>%
                       # Nesting weather data back for each site-ID:
                       dplyr::select_if(names(.) %in% 
                                          c("ID", "Crop", "Site",
                                            "Date","DL", "PP", 
                                            "Rad", "Tmax", "Tmin",
                                            "Tmean", "VP", "ET0_HS")) %>%
                       dplyr::group_by(ID, Crop, Site) %>% 
                       tidyr::nest(Weather = -c(ID, Crop, Site)) %>%
                       dplyr::ungroup(), 
                     by = c("ID")) %>% 
    # STEP 2. Create Weather column filtering for desired period only.
    dplyr::mutate(Weather = purrr::pmap(
      .l = list(x = Start.in,
                y = End.in, 
                data = Weather),
      # Filter function
      .f = function(x, y, data) {
        dplyr::filter(data, Date >= x & Date < y)} ) ) %>% 
    
    # STEP 3. Calculation of variables (daily) that will be useful to summarize the intervals 
    dplyr::mutate(Weather = Weather %>% 
                    # User must adapt depending on the crop (these ay be corn-specific)
                    purrr::map(~ mutate(.,
                                        # Extreme Precip. event:
                                        EPEi = case_when(PP > 25 ~1, TRUE ~ 0),
                                        # Extreme Temp. event:
                                        ETEi = case_when(Tmax >= 30 ~1, TRUE ~ 0), 
                                        # Tmax factor,  crop heat units (CHU):
                                        Ymax = case_when(Tmax < 10 ~ 0,
                                                         Tmax >= 30 ~ 0,
                                                         TRUE ~ 3.33*(Tmax-10) - 0.084*(Tmax-10)^2),
                                        # Tmin factor, Crop heat units (CHU):
                                        Ymin = case_when(Tmin < 4.44 ~ 0, 
                                                         TRUE ~ 1.8*(Tmin-4.44)), 
                                        # Daily CHU:
                                        Yavg = (Ymax + Ymin)/2,
                                        # Estimate CHU calculation
                                        # Tmax factor,  crop heat units (CHU):
                                        tmax_c = case_when(Tmax < 10 ~ 0,
                                                           Tmax >= 30 ~ 0,
                                                         TRUE ~ 3.33*(Tmax-10) - 0.084*(Tmax-10)^2),
                                        # Tmin factor, Crop heat units (CHU):
                                        tmin_c = case_when(Tmin <= 4.44 ~ 0, 
                                                         TRUE ~ 1.8*(Tmin-4.44)), 
                                        # Daily CHU:
                                        chu = (tmax_c + tmin_c) / 2,
                                        # For WHEAT (diff. base temp and winter negatives)
                                        # # Tmin threshold Growing Degrees:
                                        # Gmin = case_when(Tmin >= 0 ~ Tmin, TRUE ~ 0),
                                        # # Tmax threshold Growing Degrees:
                                        # Gmax = case_when(Tmax > 30 ~ 30,
                                        #                  Tmin < 0 ~ 0, 
                                        #                  between(Tmax, 0, 30) ~ Tmax),
                                        # # Daily Growing Degree Units:
                                        # GDU = ((Gmin + Gmax)/2) - 0,
                                        # GDD_c = cumsum(GDU) 
                                        # For CORN, SOYBEAN (Base temp = 10)
                                        # Tmin threshold Growing Degrees:
                                        Gmin = case_when(Tmin >= 10 ~ Tmin, TRUE ~ 10),
                                        # Tmax threshold Growing Degrees:
                                        Gmax = case_when(Tmax > 30 ~ 30,
                                                         between(Tmax, 10, 30) ~ Tmax,
                                                         Tmax < 10 ~ 10),
                                        # Daily Growing Degree Units:
                                        GDU = ((Gmin + Gmax)/2) - 10) ) ) %>% 
    
    # STEP 4. Summary for each variable over the period of interest:
    dplyr::mutate(
      # Duration of interval (days):
      Dur = Weather %>% purrr::map(~nrow(.)),
      # Accumulated PP (mm):
      PP = Weather %>% purrr::map(~sum(.$PP)),
      # Mean Temp (C):
      Tmean = Weather %>% purrr::map(~mean(.$Tmean)),
      # Accumulated Rad (MJ/m2):
      Rad = Weather %>% purrr::map(~sum(.$Rad)),
      # Accumulated VP (kPa):
      VP = Weather %>% purrr::map(~sum(.$VP)),
      # Accumulated ET0 (mm):
      ET0_HS = Weather %>% purrr::map(~sum(.$ET0_HS)),
      # Number of ETE (#):
      ETE = Weather %>% purrr::map(~sum(.$ETEi)),
      # Number of EPE (#):
      EPE = Weather %>% purrr::map(~sum(.$EPEi)),
      # Accumulated Crop Heat Units (CHU):
      CHU = Weather %>% purrr::map(~sum(.$Yavg)),
      # Shannon Diversity Index for PP:
      SDI = Weather %>% purrr::map(~ vegan::diversity(.$PP, index = "shannon")/log(length(.$PP))),
      # Accumulated Growing Degree Days (GDD):
      GDD =  Weather %>% purrr::map(~sum(.$GDU))) %>% 
    
    # Additional indices and final units:
    dplyr::select(-Weather) %>% 
    # DS: `cols` is now required when using unnest()
    tidyr::unnest(cols = c(Dur, PP, Tmean, Rad, VP, ET0_HS, ETE, EPE, CHU, SDI, GDD)) %>% 
    dplyr::mutate(
      # Photo-thermal quotient (Q):
      Q_chu = Rad/CHU,
      Q_gdd = Rad/GDD,
      # Abundant and Well Distributed Water:
      AWDR = PP*SDI) 
}