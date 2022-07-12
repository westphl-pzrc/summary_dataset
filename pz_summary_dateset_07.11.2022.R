#title: promise zone summary dataset
#name: nissim lebovits
#last edit: 7/12/2022

#####Intro

#Summary: the goal of this script is to provide Promise Zone partners and stakeholders
#with a simple dataset summarizing key Promise Zone metrics relating to demographics
#and the five policy focus areas (education, health, housing, public safety, and workforce
#and economic opportunity).

#notes: Because of issues with the 2020 US Census, it is not yet possible to use 
#population-based weights to estimate spatially-intensive variables. As a result,
#this pull is provisional. Ideally, this script should be updated with population
#weights when the summary file for the 2020 US census becomes available.

#Additionally, these data are all estimates. I have written rounding into the script
#in order to make this fact as clear as possible, but it should still be emphasized
#whenever these data are shared.

#datasources:
  #Americn Community Survey
  #Philadelphia Department of Public Health
  #Philadelphia Police Department
  #Childcare Map (the Reinvestment Fund)

#####Steps

#1: setup
#2: import pz boundaries
#3: import crimes; sort into violent and property; aggregate to pz
#4: import PDPH mortality data; interpolate to pz
#5: import childcare map data; interpolate to pz
#6: import desired acs variables; group into spatially intensive vs. extensive; interpolate
#7: combine into final dataframe
#8: repeat for whole city to provide point of comparison
#9: write .csv file

##############################################################################

#####
#Setup

library(tigris, quietly = T)
library(tidyverse, quietly = T)
library(sf, quietly = T)
library(mapview, quietly = T)
library(acs, quietly = T)
library(tidycensus, quietly = T)
library(gtools, quietly = T) #for smartbind
library(janitor) #to clean dirty excel files
library(stringr)

options(tigris_use_cache = TRUE)

#####
#Import PZ boundaries
#https://www.opendataphilly.org/dataset/west-philadelphia-promise-zone

pz = read_sf("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Transition Documents/Data/R Scripts and Datasets/General Boundaries/Shapefiles/PZ_Shapefile",
             "PZ_Boundaries", stringsAsFactors = FALSE) |>
  st_transform(crs = st_crs("EPSG:4326"))

vars20 = load_variables(2020, 
                        "acs5")

######################################CRIME############################################

#####
#Import crimes
#https://www.opendataphilly.org/dataset/crime-incidents
crimes_import_twentyone = read.csv("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Transition Documents/Data/R Scripts and Datasets/Public Safety/Crime/PhilaCrime2021.csv") |>
                            filter(!duplicated(dc_key)) |>
                              drop_na(lat) |>
                              drop_na(lng) |>
                              filter(lat > 38 &
                                       lng < 78) |>
                              st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4326")

#categorize
homicides = crimes_import_twentyone |>
              filter(grepl("homicide", crimes_import_twentyone$text_general_code, ignore.case = T))

assaults = crimes_import_twentyone |>
            filter(grepl("assault", crimes_import_twentyone$text_general_code, ignore.case = T))

robbery_or_burglary = crimes_import_twentyone |>
                         filter(grepl("robbery|burglary", crimes_import_twentyone$text_general_code, ignore.case = T))

#clean for phl totals
phl_homicides = homicides |>
  as.data.frame() |>
  select(-geometry) |>
  tally() |>
  rename(phila = n)

row.names(phl_homicides) = "homicides"

phl_assaults = assaults |>
  as.data.frame() |>
  select(-geometry) |>
  tally() |>
  rename(phila = n)

row.names(phl_assaults) = "assaults"

phl_robbery_or_burglary = robbery_or_burglary |>
  as.data.frame() |>
  select(-geometry) |>
  tally() |>
  rename(phila = n)

row.names(phl_robbery_or_burglary) = "robbery_or_burglary"

phl_crime = rbind(phl_homicides, phl_assaults, phl_robbery_or_burglary)


#clean for pz totals
pz_homicides = homicides[pz, ] |>
                as.data.frame() |>
                select(-geometry) |>
                tally() |>
                rename(promise_zone = n)

row.names(pz_homicides) = "homicides"

pz_assaults = assaults[pz, ] |>
                  as.data.frame() |>
                  select(-geometry) |>
                  tally() |>
                  rename(promise_zone = n)

row.names(pz_assaults) = "assaults"

pz_robbery_or_burglary = robbery_or_burglary[pz, ] |>
                          as.data.frame() |>
                          select(-geometry) |>
                          tally() |>
                          rename(promise_zone = n)

row.names(pz_robbery_or_burglary) = "robbery_or_burglary"

pz_crime = rbind(pz_homicides, pz_assaults, pz_robbery_or_burglary)

#total crime
total_crime = cbind(phl_crime, pz_crime)


######################################MORTALITY############################################

mortality = st_read("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Transition Documents/Data/R Scripts and Datasets/Vital_Mortality_PD.shp")|>
  clean_names() |>
  st_transform(crs = st_crs("EPSG:4326")) |>
  filter(year == "2019",
         metric_nam == "age_adjusted_mortality_rate_per_100k",
         leading_ca == "All causes",
         sex == "All sexes",
         race_ethni == "All races/ethnicities",
         metric_val > -1)

mapview(mortality, zcol = "metric_val")

mortality_for_interp = mortality |>
                        dplyr::select(metric_val) 

pz_mortality = st_interpolate_aw(mortality_for_interp, pz, ext = FALSE) |>
                  as.data.frame() |>
                  select(-geometry)

row.names(pz_mortality) = "mean_mortality_per_100k"
colnames(pz_mortality) = "promise_zone"

phl_mortality = as.data.frame(sum((mortality$metric_val * mortality$shape_are) / sum(mortality$shape_are)))

row.names(phl_mortality) = "mean_mortality_per_100k"
colnames(phl_mortality) = "phila"

total_mortality = cbind(phl_mortality, pz_mortality)

##################################CHILDCARE#####################################
#data from: https://www.childcaremap.org/tool.html
phl = block_groups(state = "PA", county = "Philadelphia") |>
        rename(block_group = GEOID) |>
        st_transform(crs = st_crs("EPSG:4326"))

children_under_5 = read.csv("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Transition Documents/Data/R Scripts and Datasets/phl_children_under_5_07.11.2022.csv") |>
                clean_names() |>
                dplyr::select(-c(state, fips_code, formatted_fips)) |>
                rename(under_5 = number_of_children_under_five_years_old_block_group_as_of_2021)
  
total_supply = read.csv("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Transition Documents/Data/R Scripts and Datasets/phl_total_childcare_supply_07.11.2022.csv")|>
                   clean_names() |>
                   dplyr::select(-c(state, fips_code, formatted_fips)) |>
                   rename(supply = estimated_total_supply_block_group_as_of_2021)

  
high_quality_supply = read.csv("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Transition Documents/Data/R Scripts and Datasets/phl_high_quality_childcare_supply_07.11.2022.csv") |>
                          clean_names() |>
                          dplyr::select(-c(state, fips_code, formatted_fips))|>
                          rename(hq_supply = estimated_supply_of_high_quality_seats_block_group_as_of_2021)

phl_child_care = left_join(phl, children_under_5)
phl_child_care = left_join(phl_child_care, total_supply)
phl_child_care = left_join(phl_child_care, high_quality_supply)

mapview(phl_child_care, zcol = "under_5")

#calculate phl numbers
phl_childcare_combined = as.data.frame(
                                      c(
                                       sum(na.omit(phl_child_care$under_5)), 
                                       sum(na.omit(phl_child_care$supply)),
                                       sum(na.omit(phl_child_care$hq_supply))
                                       )
                                      )

row.names(phl_childcare_combined) = c("children_under_5",
                "childcare_seats",
                "high_quality_childcare_seats")

colnames(phl_childcare_combined) = "phila"


#calculate pz numbers
pz_children_under_5 = phl_child_care |>
                    dplyr::select(under_5) |>
                    filter(!is.na(under_5)) |>
                    st_interpolate_aw(pz, ext = T) |>
                      as.data.frame() |>
                      select(-geometry)

row.names(pz_children_under_5) = "children_under_5"
colnames(pz_children_under_5) = "promise_zone"

pz_supply = phl_child_care |>
                    dplyr::select(supply) |>
                    filter(!is.na(supply)) |>
                    st_interpolate_aw(pz, ext = T) |>
                    as.data.frame() |>
                    select(-geometry)

row.names(pz_supply) = "childcare_seats"
colnames(pz_supply) = "promise_zone"

pz_hq_supply = phl_child_care |>
                    dplyr::select(hq_supply) |>
                    filter(!is.na(hq_supply)) |>
                    st_interpolate_aw(pz, ext = T) |>
                    as.data.frame() |>
                    select(-geometry)

row.names(pz_hq_supply) = "high_quality_childcare_seats"
colnames(pz_hq_supply) = "promise_zone"

pz_childcare_combined = rbind(pz_children_under_5, pz_supply, pz_hq_supply)

total_childcare_combined = cbind(phl_childcare_combined, pz_childcare_combined)

######################PULL DEMOS####################################

phl_demos <- get_acs(geography = "tract", # What is the lowest level of geography are we interested in?
                     year = 2020, # What year do we want - this can also be used for 2000 census data
                     variables = c(
                       #Population
                       "B01003_001E", #Total population
                       "B11001_001E", #Total number of households
                       "B09001_001E", #Total population under 18
                       "B09021_022", #Estimate!!Total:!!65 years and over:
                       "B01002_001", #Estimate!!Median age --!!Total:
                       
                       
                       #Race
                       "B02001_002E", #Total white population
                       "B02001_003E", #Total Black population
                       "B02001_004E", #American Indian and Alaska Native alone
                       "B02001_005E", #Total Asian population
                       "B02001_006E", #Native Hawaiian and Other Pacific Islander alone
                       "B02001_007E", #Some other race alone
                       "B02001_008E", #Two or more races
                       
                       #Ethnicitiy
                       "B01001I_001E", #Total: Hispanic or Latino (distinct from race)
                       
                       #Health
                        #no health care
                       "B27010_017E", #Estimate!!Total:!!Under 19 years:!!No health insurance coverage
                       "B27010_033E", #Estimate!!Total:!!19 to 34 years:!!No health insurance coverage
                       "B27010_050E", #Estimate!!Total:!!35 to 64 years:!!No health insurance coverage
                       "B27010_066E", #Estimate!!Total:!!65 years and over:!!No health insurance coverage
                       
                        #public health care
                       "B18135_006E", #Estimate!!Total:!!Under 19 years:!!With a disability:!!With health insurance coverage:!!With public health coverage
                       "B18135_011E", #Estimate!!Total:!!Under 19 years:!!No disability:!!With health insurance coverage:!!With public health coverage
                       "B18135_017E", #Estimate!!Total:!!19 to 64 years:!!With a disability:!!With health insurance coverage:!!With public health coverage
                       "B18135_022E", #Estimate!!Total:!!19 to 64 years:!!No disability:!!With health insurance coverage:!!With public health coverage
                       "B18135_028E", #Estimate!!Total:!!65 years and over:!!With a disability:!!With health insurance coverage:!!With public health coverage
                       "B18135_033E", #Estimate!!Total:!!65 years and over:!!No disability:!!With health insurance coverage:!!With public health coverage
                       
                        #snap
                       "B99221_001E", #Estimate!!Total: SNAP recipients
                       
                       #Housing
                       "B25064_001E", #Median gross rent
                       "B25070_007E", #Rent 30.0 to 34.9 percent
                       "B25070_008E", #Rent 35.0 to 39.9 percent
                       "B25070_009E", #Rent 40.0 to 49.9 percent
                       "B25070_010E", #Rent 50.0 percent or more
                       "B25003_002E", #Owner occupied
                       "B25003_003E", #Renter occupied
                       "B25001_001E", #Number of residential units
                       "B25002_002E", #Occupied
                       "B25002_003E", #Vacant
                       "B25077_001E", #Median house value (dollars)
                       
                       #Income & Work
                       "B19013_001E", #Median household income
                       "B17001_002E", #Income in the past 12 months below poverty level:
                       "B23025_003E", #In labor force:!!Civilian labor force:
                       "B23025_004E", #In labor force:!!Civilian labor force:!!Employed
                       "B23025_005E", #In labor force:!!Civilian labor force:!!Unemployed
                       
                       #Education
                       "B15003_001E", #Total Pop 25+
                       "B15003_017E", #Regular high school diploma
                       "B15003_018E", #GED or alternative credential
                       "B15003_019E", #Some college, less than 1 year
                       "B15003_020E", #Some college, 1 or more years, no degree
                       "B15003_021E", #Associate's degree
                       "B15003_022E", #Bachelor's degree
                       "B15003_023E", #Master's degree
                       "B15003_024E", #Professional school degree
                       "B15003_025E"),  #Doctorate degree 
                     geometry = T, # Do we want this as a shapefile? No, not now.
                     state = "PA", # What state?
                     county = "Philadelphia", # What County?
                     output = "wide") |>
  rename(#Population
    tot_pop = B01003_001E,
    tot_hh = B11001_001E,
    tot_under_eighteen = B09001_001E,
    tot_65_and_up = B09021_022E,
    med_age = B01002_001E, 
    
    #Race
    tot_wht_pop = B02001_002E,
    tot_blk_pop = B02001_003E,
    tot_native_pop = B02001_004E,
    tot_other_race = B02001_007E,
    tot_two_plus_race = B02001_008E,
    
    #Ethnicitiy
    tot_hisp_pop = B01001I_001E,
    
     #snap
    snap_enrollment = B99221_001E,
    
    #Housing
    med_gross_rent = B25064_001E, #Median gross rent
    owner_occ = B25003_002E, #Owner occupied
    renter_occ = B25003_003E, #Renter occupied
    num_resid_units = B25001_001E, #Number of residential units
    occ_units = B25002_002E, #Occupied
    vac_units = B25002_003E, #Vacant
    med_house_value = B25077_001E, #Median house value (dollars)
    
    #Income & Work
    med_hh_inc = B19013_001E, #Median household income
    inc_below_pov = B17001_002E, #Income in the past 12 months below poverty level:
    tot_lab_force = B23025_003E, #In labor force:!!Civilian labor force:
    tot_unempl = B23025_005E, #In labor force:!!Civilian labor force:!!Unemployed
    
    #Education
    tot_pop_25plus = B15003_001E) |> #Total Pop 25+
  mutate(non_wht_pop = tot_pop - tot_wht_pop,
         tot_aapi_pop = (B02001_005E + #Tot Asian pop
                           B02001_006E), #Tot Hawaiian and Pacific Islander pop
         tot_rent_burden = (B25070_007E + #Rent 30.0 to 34.9 percent
                              B25070_008E + #Rent 35.0 to 39.9 percent
                              B25070_009E + #Rent 40.0 to 49.9 percent
                              B25070_010E), #Rent 50.0 percent or more
         tot_hs_dip_or_alt = (B15003_017E+ #Regular high school diploma
                                B15003_018E), #GED or alternative credential
         tot_some_college = (B15003_019E + #Some college, less than one year
                               B15003_020E), #Some college, 1 or more years, no degree
         tot_bach_plus = (B15003_021E +  #Associate's degree
                            B15003_022E + #Bachelor's degree
                            B15003_023E + #Master's degree
                            B15003_024E + #Professional school degree
                            B15003_025E),  #Doctorate degree 
          tot_no_health_care = (B27010_017E + #under 18
                                B27010_033E + #19 to 34
                                B27010_050E + #35 to 64
                                B27010_066E), #65 and up
          tot_public_health_care = (B18135_006E + #Estimate!!Total:!!Under 19 years:!!With a disability:!!With health insurance coverage:!!With public health coverage
                                    B18135_011E + #Estimate!!Total:!!Under 19 years:!!No disability:!!With health insurance coverage:!!With public health coverage
                                    B18135_017E + #Estimate!!Total:!!19 to 64 years:!!With a disability:!!With health insurance coverage:!!With public health coverage
                                    B18135_022E + #Estimate!!Total:!!19 to 64 years:!!No disability:!!With health insurance coverage:!!With public health coverage
                                    B18135_028E + #Estimate!!Total:!!65 years and over:!!With a disability:!!With health insurance coverage:!!With public health coverage
                                    B18135_033E)) #Estimate!!Total:!!65 years and over:!!No disability:!!With health insurance coverage:!!With public health coverage)

phl_demos = st_transform(phl_demos, crs = st_crs("EPSG:4326"))

#########################SEPARATE VARIABLES BY TYPE#######################

#Extensive variables
phl_demos_ext = phl_demos |>
  dplyr::select(tot_pop,
                tot_hh,
                tot_under_eighteen,
                tot_65_and_up,
                tot_wht_pop,
                tot_blk_pop,
                tot_native_pop,
                tot_other_race,
                tot_two_plus_race,
                tot_hisp_pop,
                owner_occ,
                renter_occ,
                num_resid_units,
                occ_units,
                vac_units,
                inc_below_pov,
                tot_lab_force,
                tot_unempl,
                tot_pop_25plus, 
                non_wht_pop,
                tot_aapi_pop,
                tot_rent_burden,
                tot_hs_dip_or_alt,
                tot_some_college,
                tot_bach_plus,
                snap_enrollment,
                tot_no_health_care,
                tot_public_health_care)

#Intensive variables
phl_demos_int = phl_demos |>
  dplyr::select(med_gross_rent,
                med_house_value,
                med_hh_inc,
                med_age)

######################RUN SPATIAL INTERPOLATION#########################

pz_demos_ext = st_interpolate_aw(phl_demos_ext[, 1:28], pz, ext = TRUE)

pz_demos_int = st_interpolate_aw(na.omit(phl_demos_int[, 1:4]), pz, ext = FALSE)

pz_demos_tot = st_join(pz_demos_ext, pz_demos_int)

pz_demos_tot = pz_demos_tot |>
  mutate(pct_und_18 = (tot_under_eighteen / tot_pop),
         pct_non_wht_pop = (non_wht_pop / tot_pop),
         pct_blk = (tot_blk_pop / tot_pop),
         pct_aapi = (tot_aapi_pop / tot_pop),
         pct_hisp = (tot_hisp_pop / tot_pop),
         pct_own_occ = (owner_occ / num_resid_units),
         pct_rent_occ = (renter_occ / num_resid_units),
         resid_vac_rate = (vac_units / num_resid_units),
         pov_rt = (inc_below_pov / tot_pop),
         unempl_rt = (tot_unempl / tot_lab_force),
         pct_hs_or_equiv = (tot_hs_dip_or_alt / tot_pop_25plus),
         pct_some_college = (tot_some_college / tot_pop_25plus),
         pct_bach_plus = (tot_bach_plus / tot_pop_25plus)
  )

pz_demos_tot = pz_demos_tot |>
                as.data.frame() |>
                select(-geometry,
                       tot_pop,
                       tot_hh,
                       tot_under_eighteen,
                       tot_65_and_up,
                       tot_wht_pop,
                       tot_blk_pop,
                       tot_native_pop,
                       tot_other_race,
                       tot_two_plus_race,
                       tot_hisp_pop,
                       owner_occ,
                       renter_occ,
                       num_resid_units,
                       occ_units,
                       vac_units,
                       inc_below_pov,
                       tot_lab_force,
                       tot_unempl,
                       tot_pop_25plus, 
                       non_wht_pop,
                       tot_aapi_pop,
                       tot_rent_burden,
                       tot_hs_dip_or_alt,
                       tot_some_college,
                       snap_enrollment,
                       tot_no_health_care,
                       tot_public_health_care,
                       med_gross_rent,
                       med_house_value,
                       med_hh_inc,
                       med_age,
                       pct_und_18,
                       pct_non_wht_pop,
                       pct_blk,
                       pct_aapi,
                       pct_hisp,
                       pct_own_occ,
                       pct_rent_occ,
                       resid_vac_rate,
                       pov_rt,
                       unempl_rt,
                       pct_hs_or_equiv,
                       pct_some_college,
                       pct_bach_plus)

pz_demos_tot = t(pz_demos_tot)

colnames(pz_demos_tot) = "promise_zone"

################REPEAT FOR PHL

phl_county_demos <- get_acs(geography = "county", # What is the lowest level of geography are we interested in?
                            year = 2020, # What year do we want - this can also be used for 2000 census data
                            variables = c(
                              #Population
                              "B01003_001E", #Total population
                              "B11001_001E", #Total number of households
                              "B09001_001E", #Total population under 18
                              "B09021_022", #Estimate!!Total:!!65 years and over:
                              "B01002_001", #Estimate!!Median age --!!Total:
                              
                              
                              #Race
                              "B02001_002E", #Total white population
                              "B02001_003E", #Total Black population
                              "B02001_004E", #American Indian and Alaska Native alone
                              "B02001_005E", #Total Asian population
                              "B02001_006E", #Native Hawaiian and Other Pacific Islander alone
                              "B02001_007E", #Some other race alone
                              "B02001_008E", #Two or more races
                              
                              #Ethnicitiy
                              "B01001I_001E", #Total: Hispanic or Latino (distinct from race)
                              
                              #Health
                              #no health care
                              "B27010_017E", #Estimate!!Total:!!Under 19 years:!!No health insurance coverage
                              "B27010_033E", #Estimate!!Total:!!19 to 34 years:!!No health insurance coverage
                              "B27010_050E", #Estimate!!Total:!!35 to 64 years:!!No health insurance coverage
                              "B27010_066E", #Estimate!!Total:!!65 years and over:!!No health insurance coverage
                              
                              #public health care
                              "B18135_006E", #Estimate!!Total:!!Under 19 years:!!With a disability:!!With health insurance coverage:!!With public health coverage
                              "B18135_011E", #Estimate!!Total:!!Under 19 years:!!No disability:!!With health insurance coverage:!!With public health coverage
                              "B18135_017E", #Estimate!!Total:!!19 to 64 years:!!With a disability:!!With health insurance coverage:!!With public health coverage
                              "B18135_022E", #Estimate!!Total:!!19 to 64 years:!!No disability:!!With health insurance coverage:!!With public health coverage
                              "B18135_028E", #Estimate!!Total:!!65 years and over:!!With a disability:!!With health insurance coverage:!!With public health coverage
                              "B18135_033E", #Estimate!!Total:!!65 years and over:!!No disability:!!With health insurance coverage:!!With public health coverage
                              
                              #snap
                              "B99221_001E", #Estimate!!Total: SNAP recipients
                              
                              #Housing
                              "B25064_001E", #Median gross rent
                              "B25070_007E", #Rent 30.0 to 34.9 percent
                              "B25070_008E", #Rent 35.0 to 39.9 percent
                              "B25070_009E", #Rent 40.0 to 49.9 percent
                              "B25070_010E", #Rent 50.0 percent or more
                              "B25003_002E", #Owner occupied
                              "B25003_003E", #Renter occupied
                              "B25001_001E", #Number of residential units
                              "B25002_002E", #Occupied
                              "B25002_003E", #Vacant
                              "B25077_001E", #Median house value (dollars)
                              
                              #Income & Work
                              "B19013_001E", #Median household income
                              "B17001_002E", #Income in the past 12 months below poverty level:
                              "B23025_003E", #In labor force:!!Civilian labor force:
                              "B23025_004E", #In labor force:!!Civilian labor force:!!Employed
                              "B23025_005E", #In labor force:!!Civilian labor force:!!Unemployed
                              
                              #Education
                              "B15003_001E", #Total Pop 25+
                              "B15003_017E", #Regular high school diploma
                              "B15003_018E", #GED or alternative credential
                              "B15003_019E", #Some college, less than 1 year
                              "B15003_020E", #Some college, 1 or more years, no degree
                              "B15003_021E", #Associate's degree
                              "B15003_022E", #Bachelor's degree
                              "B15003_023E", #Master's degree
                              "B15003_024E", #Professional school degree
                              "B15003_025E"),  #Doctorate degree 
                            geometry = F, 
                            state = "PA", # What state?
                            county = "Philadelphia", # What County?
                            output = "wide") |>
  rename(#Population
    tot_pop = B01003_001E,
    tot_hh = B11001_001E,
    tot_under_eighteen = B09001_001E,
    tot_65_and_up = B09021_022E,
    med_age = B01002_001E, 
    
    #Race
    tot_wht_pop = B02001_002E,
    tot_blk_pop = B02001_003E,
    tot_native_pop = B02001_004E,
    tot_other_race = B02001_007E,
    tot_two_plus_race = B02001_008E,
    
    #Ethnicitiy
    tot_hisp_pop = B01001I_001E,
    
    #snap
    snap_enrollment = B99221_001E,
    
    #Housing
    med_gross_rent = B25064_001E, #Median gross rent
    owner_occ = B25003_002E, #Owner occupied
    renter_occ = B25003_003E, #Renter occupied
    num_resid_units = B25001_001E, #Number of residential units
    occ_units = B25002_002E, #Occupied
    vac_units = B25002_003E, #Vacant
    med_house_value = B25077_001E, #Median house value (dollars)
    
    #Income & Work
    med_hh_inc = B19013_001E, #Median household income
    inc_below_pov = B17001_002E, #Income in the past 12 months below poverty level:
    tot_lab_force = B23025_003E, #In labor force:!!Civilian labor force:
    tot_unempl = B23025_005E, #In labor force:!!Civilian labor force:!!Unemployed
    
    #Education
    tot_pop_25plus = B15003_001E) |> #Total Pop 25+
   mutate(non_wht_pop = tot_pop - tot_wht_pop,
         tot_aapi_pop = (B02001_005E + #Tot Asian pop
                           B02001_006E), #Tot Hawaiian and Pacific Islander pop
         tot_rent_burden = (B25070_007E + #Rent 30.0 to 34.9 percent
                              B25070_008E + #Rent 35.0 to 39.9 percent
                              B25070_009E + #Rent 40.0 to 49.9 percent
                              B25070_010E), #Rent 50.0 percent or more
         tot_hs_dip_or_alt = (B15003_017E+ #Regular high school diploma
                                B15003_018E), #GED or alternative credential
         tot_some_college = (B15003_019E + #Some college, less than one year
                               B15003_020E), #Some college, 1 or more years, no degree
         tot_bach_plus = (B15003_021E +  #Associate's degree
                            B15003_022E + #Bachelor's degree
                            B15003_023E + #Master's degree
                            B15003_024E + #Professional school degree
                            B15003_025E),  #Doctorate degree 
         tot_no_health_care = (B27010_017E + #under 18
                                 B27010_033E + #19 to 34
                                 B27010_050E + #35 to 64
                                 B27010_066E), #65 and up
         tot_public_health_care = (B18135_006E + #Estimate!!Total:!!Under 19 years:!!With a disability:!!With health insurance coverage:!!With public health coverage
                                     B18135_011E + #Estimate!!Total:!!Under 19 years:!!No disability:!!With health insurance coverage:!!With public health coverage
                                     B18135_017E + #Estimate!!Total:!!19 to 64 years:!!With a disability:!!With health insurance coverage:!!With public health coverage
                                     B18135_022E + #Estimate!!Total:!!19 to 64 years:!!No disability:!!With health insurance coverage:!!With public health coverage
                                     B18135_028E + #Estimate!!Total:!!65 years and over:!!With a disability:!!With health insurance coverage:!!With public health coverage
                                     B18135_033E), #Estimate!!Total:!!65 years and over:!!No disability:!!With health insurance coverage:!!With public health coverage)
         pct_und_18 = (tot_under_eighteen / tot_pop),
         pct_non_wht_pop = (non_wht_pop / tot_pop),
         pct_blk = (tot_blk_pop / tot_pop),
         pct_aapi = (tot_aapi_pop / tot_pop),
         pct_hisp = (tot_hisp_pop / tot_pop),
         pct_own_occ = (owner_occ / num_resid_units),
         pct_rent_occ = (renter_occ / num_resid_units),
         resid_vac_rate = (vac_units / num_resid_units),
         pov_rt = (inc_below_pov / tot_pop),
         unempl_rt = (tot_unempl / tot_lab_force),
         pct_hs_or_equiv = (tot_hs_dip_or_alt / tot_pop_25plus),
         pct_some_college = (tot_some_college / tot_pop_25plus),
         pct_bach_plus = (tot_bach_plus / tot_pop_25plus))


phl_county_demos = phl_county_demos |>
                      select(tot_pop,
                          tot_hh,
                          tot_under_eighteen,
                          tot_65_and_up,
                          tot_wht_pop,
                          tot_blk_pop,
                          tot_native_pop,
                          tot_other_race,
                          tot_two_plus_race,
                          tot_hisp_pop,
                          owner_occ,
                          renter_occ,
                          num_resid_units,
                          occ_units,
                          vac_units,
                          inc_below_pov,
                          tot_lab_force,
                          tot_unempl,
                          tot_pop_25plus, 
                          non_wht_pop,
                          tot_aapi_pop,
                          tot_rent_burden,
                          tot_hs_dip_or_alt,
                          tot_some_college,
                          tot_bach_plus,
                          snap_enrollment,
                          tot_no_health_care,
                          tot_public_health_care,
                          med_gross_rent,
                          med_house_value,
                          med_hh_inc,
                          med_age,
                          pct_und_18,
                          pct_non_wht_pop,
                          pct_blk,
                          pct_aapi,
                          pct_hisp,
                          pct_own_occ,
                          pct_rent_occ,
                          resid_vac_rate,
                          pov_rt,
                          unempl_rt,
                          pct_hs_or_equiv,
                          pct_some_college,
                          pct_bach_plus) |>
                    t()

colnames(phl_county_demos) = "phila"


#combine phl and phila
total_demos = cbind(phl_county_demos, pz_demos_tot)

#####################################COMBINE EVERYTHING#####################################

pz_summary_dataset = as.data.frame(t(format(as.data.frame(rbind(total_crime, total_mortality, total_childcare_combined, total_demos)), scientific = F))) |>
                        lapply(as.numeric) |>
                        as.data.frame()

#estimate w/3 sig figs
pz_summary_dataset$children_under_5 = signif(pz_summary_dataset$children_under_5, digits = 3)
pz_summary_dataset$childcare_seats = signif(pz_summary_dataset$childcare_seats, digits = 3)
pz_summary_dataset$high_quality_childcare_seats = signif(pz_summary_dataset$high_quality_childcare_seats, digits = 3)
pz_summary_dataset$tot_pop = signif(pz_summary_dataset$tot_pop, digits = 3)
pz_summary_dataset$tot_hh = signif(pz_summary_dataset$tot_hh, digits = 3)
pz_summary_dataset$tot_under_eighteen = signif(pz_summary_dataset$tot_under_eighteen, digits = 3)
pz_summary_dataset$tot_65_and_up = signif(pz_summary_dataset$tot_65_and_up, digits = 3)
pz_summary_dataset$tot_wht_pop = signif(pz_summary_dataset$tot_wht_pop, digits = 3)
pz_summary_dataset$tot_blk_pop = signif(pz_summary_dataset$tot_blk_pop, digits = 3)
pz_summary_dataset$tot_two_plus_race = signif(pz_summary_dataset$tot_two_plus_race, digits = 3)
pz_summary_dataset$tot_hisp_pop = signif(pz_summary_dataset$tot_hisp_pop, digits = 3)
pz_summary_dataset$owner_occ = signif(pz_summary_dataset$owner_occ, digits = 3)
pz_summary_dataset$renter_occ = signif(pz_summary_dataset$renter_occ, digits = 3)
pz_summary_dataset$num_resid_units = signif(pz_summary_dataset$num_resid_units, digits = 3)
pz_summary_dataset$vac_units = signif(pz_summary_dataset$vac_units, digits = 3)
pz_summary_dataset$occ_units = signif(pz_summary_dataset$occ_units, digits = 3)
pz_summary_dataset$inc_below_pov = signif(pz_summary_dataset$inc_below_pov, digits = 3)
pz_summary_dataset$tot_lab_force = signif(pz_summary_dataset$tot_lab_force, digits = 3)
pz_summary_dataset$tot_unempl = signif(pz_summary_dataset$tot_unempl, digits = 3)
pz_summary_dataset$tot_pop_25plus = signif(pz_summary_dataset$tot_pop_25plus, digits = 3)
pz_summary_dataset$non_wht_pop = signif(pz_summary_dataset$non_wht_pop, digits = 3)
pz_summary_dataset$tot_aapi_pop = signif(pz_summary_dataset$tot_aapi_pop, digits = 3)
pz_summary_dataset$tot_rent_burden = signif(pz_summary_dataset$tot_rent_burden, digits = 3)
pz_summary_dataset$tot_hs_dip_or_alt = signif(pz_summary_dataset$tot_hs_dip_or_alt, digits = 3)
pz_summary_dataset$tot_some_college = signif(pz_summary_dataset$tot_some_college, digits = 3)
pz_summary_dataset$tot_bach_plus = signif(pz_summary_dataset$tot_bach_plus, digits = 3)
pz_summary_dataset$snap_enrollment = signif(pz_summary_dataset$snap_enrollment, digits = 3)
pz_summary_dataset$tot_no_health_care = signif(pz_summary_dataset$tot_no_health_care, digits = 3)
pz_summary_dataset$tot_public_health_care = signif(pz_summary_dataset$tot_public_health_care, digits = 3)
pz_summary_dataset$med_house_value = signif(pz_summary_dataset$med_house_value, digits = 3)
pz_summary_dataset$med_hh_inc = signif(pz_summary_dataset$med_hh_inc, digits = 3)

#estimate w/1 decimal place
pz_summary_dataset$med_age = round(pz_summary_dataset$med_age, digits = 1)

#estimate w/2 sig figs
pz_summary_dataset$mean_mortality_per_100k = signif(pz_summary_dataset$mean_mortality_per_100k, digits = 2)
pz_summary_dataset$tot_native_pop = signif(pz_summary_dataset$tot_native_pop, digits = 2)
pz_summary_dataset$tot_other_race = signif(pz_summary_dataset$tot_other_race, digits = 2)
pz_summary_dataset$med_gross_rent = signif(pz_summary_dataset$med_gross_rent, digits = 2)
pz_summary_dataset$pct_und_18 = signif(pz_summary_dataset$pct_und_18, digits = 2)
pz_summary_dataset$pct_non_wht_pop = signif(pz_summary_dataset$pct_non_wht_pop, digits = 2)
pz_summary_dataset$pct_blk = signif(pz_summary_dataset$pct_blk, digits = 2)
pz_summary_dataset$pct_aapi = signif(pz_summary_dataset$pct_aapi, digits = 2)
pz_summary_dataset$pct_hisp = signif(pz_summary_dataset$pct_hisp, digits = 2)
pz_summary_dataset$pct_own_occ = signif(pz_summary_dataset$pct_own_occ, digits = 2)
pz_summary_dataset$pct_rent_occ = signif(pz_summary_dataset$pct_rent_occ, digits = 2)
pz_summary_dataset$resid_vac_rate = signif(pz_summary_dataset$resid_vac_rate, digits = 2)
pz_summary_dataset$pov_rt = signif(pz_summary_dataset$pov_rt, digits = 2)
pz_summary_dataset$unempl_rt = signif(pz_summary_dataset$unempl_rt, digits = 2)
pz_summary_dataset$pct_hs_or_equiv = signif(pz_summary_dataset$pct_hs_or_equiv, digits = 2)
pz_summary_dataset$pct_some_college = signif(pz_summary_dataset$pct_some_college, digits = 2)
pz_summary_dataset$pct_bach_plus = signif(pz_summary_dataset$pct_bach_plus, digits = 2)

row.names(pz_summary_dataset) = c("Philadelphia", "Promise Zone")

write.csv(pz_summary_dataset, "pz_summary_dataset_07.12.2022.csv")
