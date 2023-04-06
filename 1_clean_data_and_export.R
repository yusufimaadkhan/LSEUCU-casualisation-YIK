# All this file does is replicates Lieutaud's data wrangling/cleaning in an rscript and then exports it as a csv

# All credit to Lieutaud. I just copy and pasted from the rmd file because I just wanted to work in a script and then mess around with the csv. 

# Note - filepaths should be relativised now that its a project? 

# Get libraries
library(tidyverse)

# Basically all of the code below is from line 134 - 290 of Lieutaud's rmd file called LSEUCU_reportcasualisation_forpublication_27-03-2023.Rmd

# All of the analyses run here are based on publicly-available data published by the Higher Education Statistics Agency (HESA).
# This data can be directly downloaded on the HESA website: \href{https://www.hesa.ac.uk/data-and-analysis}{https://www.hesa.ac.uk/data-and-analysis}.

# Data import and binding
# I downloaded the HESA data then manually removed the data description from the csv files and added the relevant academic year to each file name. 

# Start with HESA data on fixed term and permanent staff
# available at https://www.hesa.ac.uk/data-and-analysis/staff/employment-conditions

data_files <- list.files("HESA data/Permanent and fixed term staff by HE provider/") # path to data folder
HESA <- grep("HESA-dt025-table-7", data_files, value=TRUE) # HESA tables on fixed-term / permanent academic staff

#import all data files on fixed-term / permanent contracts
for(i in 1:length(HESA)) {                                                  
  assign(paste0("hesa_wave", (i+2013),"-",(i+2014)),                        # Read and store data frames into files called "hesa-wave[academic year]"
         read.csv(paste0("HESA data/Permanent and fixed term staff by HE provider/",
                         HESA[i])) %>%
           mutate(date = paste0((i+2013),"-",(i+2014))))                    # Add a date variable to each data frame to identify academic years.
}


list_hesa <- list(`hesa_wave2014-2015`,
                  `hesa_wave2015-2016`,
                  `hesa_wave2016-2017`,
                  `hesa_wave2017-2018`,
                  `hesa_wave2018-2019`,
                  `hesa_wave2019-2020`,
                  `hesa_wave2020-2021`,
                  `hesa_wave2021-2022`)

hs <- bind_rows(list_hesa)

rm(`hesa_wave2014-2015`,
   `hesa_wave2015-2016`,
   `hesa_wave2016-2017`,
   `hesa_wave2017-2018`,
   `hesa_wave2018-2019`,
   `hesa_wave2019-2020`,
   `hesa_wave2020-2021`,
   `hesa_wave2021-2022`,
   list_hesa)


# same procedure with data on student enrollment
data_files <- list.files("HESA data/Total students number by HE provider/") # path to data folder
HESA_students <- grep("dt051-table-1", data_files, value=TRUE) # HESA tables on students enrollment by HE provider

#import all data files on students enrollment overall
for(i in 1:length(HESA_students)) {                                                  
  assign(paste0("hesa_wave", (i+2013),"-",(i+2014)),                        # Read and store data frames into files called "hesa-wave[academic year]"
         read.csv(paste0("HESA data/Total students number by HE provider/",
                         HESA_students[i])) %>%
           mutate(across(!HE.provider,~str_remove(.x, ","))) %>%
           mutate(across(!HE.provider, ~as.numeric(.x))) %>%
           mutate(date = paste0((i+2013),"-",(i+2014))))                    # Add a date variable to each data frame to identify academic years.
}


list_hesa <- list(`hesa_wave2014-2015`,
                  `hesa_wave2015-2016`,
                  `hesa_wave2016-2017`,
                  `hesa_wave2017-2018`,
                  `hesa_wave2018-2019`,
                  `hesa_wave2019-2020`,
                  `hesa_wave2020-2021`,
                  `hesa_wave2021-2022`)

he <- bind_rows(list_hesa) %>%
  select(c(HE.provider, Total, date)) # drop breakdown by gender of students. Keep only totals

rm(`hesa_wave2014-2015`,
   `hesa_wave2015-2016`,
   `hesa_wave2016-2017`,
   `hesa_wave2017-2018`,
   `hesa_wave2018-2019`,
   `hesa_wave2019-2020`,
   `hesa_wave2020-2021`,
   `hesa_wave2021-2022`,
   list_hesa)


# Same procedure for data on international and EU students
data_files <- list.files("HESA data/Non-UK and EU student number/") # path to data folder
HESA_international_students <- grep("dt051-table-28", data_files, value=TRUE) # HESA tables on EU and international students enrollment by HE provider

for(i in 1:length(HESA_international_students)) {                                                  
  assign(paste0("hesa_wave", (i+2013),"-",(i+2014)),                        
         read.csv(paste0("HESA data/Non-UK and EU student number/",
                         HESA_international_students[i])) %>%
           rename(HE.provider = HE.Provider,
                  Total_nonuk = Total) %>%
           mutate(across(!HE.provider,~str_remove(.x, ","))) %>%
           mutate(across(!HE.provider, ~as.numeric(.x))) %>%
           mutate(date = paste0((i+2013),"-",(i+2014))))                    
}

list_hesa <- list(`hesa_wave2014-2015`,
                  `hesa_wave2015-2016`,
                  `hesa_wave2016-2017`,
                  `hesa_wave2017-2018`,
                  `hesa_wave2018-2019`,
                  `hesa_wave2019-2020`,
                  `hesa_wave2020-2021`,
                  `hesa_wave2021-2022`)

hi <- bind_rows(list_hesa) %>%
  select(HE.provider, Total.European.Union, Total_nonuk, date)

rm(`hesa_wave2014-2015`,
   `hesa_wave2015-2016`,
   `hesa_wave2016-2017`,
   `hesa_wave2017-2018`,
   `hesa_wave2018-2019`,
   `hesa_wave2019-2020`,
   `hesa_wave2020-2021`,
   `hesa_wave2021-2022`,
   list_hesa)


h <- hs %>%
  left_join(he, by = c("HE.provider", "date")) %>%
  left_join(hi, by = c("HE.provider", "date")) %>%
  relocate(date, .after = HE.provider)

# h = full dataframe combining staff and student data


# data recoding and calculated variables
h <- h %>%
  mutate(across(4:9,~str_remove(.x, ","))) %>% # remove comas
  mutate(across(4:9, ~as.numeric(.x))) # treat all number-based variables as numeric

h <- h %>%
  mutate(ratio_permanent = Open.ended.Permanent/Fixed.term,
         ratio_casual = Fixed.term/Open.ended.Permanent,
         prop_permanent = (Open.ended.Permanent/Total.academic.staff)*100,
         prop_casual = (Fixed.term/Total.academic.staff)*100,
         staffstudent_ratio = Total/Open.ended.Permanent,
         prop_non_uk = round((Total_nonuk/Total)*100, digits=0),
         prop_eu = round((Total.European.Union/Total)*100, digits=0))


hs_2022 <- h %>% # roundabout & ugly way to get latest measures of casualisation
  filter(date == "2021-2022") %>%
  rename(latest_prop_casual_2022 = prop_casual) %>%
  select(HE.provider, latest_prop_casual_2022) 

h <- h %>%
  left_join(hs_2022, by = "HE.provider") 

rm(hs_2022) # tidy it out

# Export the cleaned csv
write.csv(h,'HESA data/staff_and_student_df.csv', row.names = FALSE)



