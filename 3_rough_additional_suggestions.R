# A script for some additional suggestions

library(tidyverse)
library(janitor) # yes use janitor this time Yusuf. Don't be silly. 

staff_and_student_df <- read_csv("HESA data/staff_and_student_df.csv") %>% clean_names()

# @psychicyogamat

# Would be interesting to correlate to overall staffing levels and incoming student numbers. 
# I suspect it's linked to the lifting of caps and the corresponding decline in students entering non-RG universities, 
# putting them in crisis (also why pay negotiations are stalling)

# Okay so staffing levels and incoming student numbers

staff_student_num <- staff_and_student_df %>% 
  select(ukprn, 
         he_provider, 
         date, 
         open_ended_permanent, 
         fixed_term, 
         total_academic_staff, 
         total) %>%  # this is total students
  
  # Russell Group Flag
  
  mutate(rg_flag = if_else(he_provider %in% c(
    "The University of Birmingham",
    "The University of Bristol", 
    "The University of Cambridge",
    "Cardiff University",
    "University of Durham", 
    "The University of Edinburgh",
    "The University of Exeter",
    "The University of Glasgow",
    "Imperial College of Science, Technology and Medicine",
    "King's College London",
    "The University of Leeds",
    "The University of Liverpool",
    "London School of Economics and Political Science",
    "The University of Manchester",
    "Newcastle University",
    "University of Nottingham", 
    "The University of Oxford", 	
    "Queen Mary University of London", 
    "Queen's University Belfast",
    "The University of Sheffield", 
    "The University of Southampton", 
    "University College London", 	
    "The University of Warwick", 	
    "The University of York"), 1, 0)) #%>% 
    #filter(rg_flag==1) %>% select(he_provider) %>% distinct() # should have 24 Russell Group unis. Check here.
    
    # Test whether aggregate figures add up    

  #total_test_1 <- staff_student_num %>% 
  #filter(he_provider=="Total") # test if totals add up to same value
  
  # total_test_2 <- staff_student_num %>% 
  # filter(!he_provider=="Total") %>% 
  #   group_by(date) %>% 
  #   summarise_at(c(4,5,6,7), sum, na.rm = TRUE)

  # Ok so some values are slightly off and I do NOT trust the aggregate student figures. But hopefully serves as rough indication. Will caveat

  # Do for total

  total_staff_student_num <- staff_student_num %>% 
    filter(!he_provider=="Total") %>% 
    rename(student_numbers = total) %>% 
    group_by(date) %>% 
    summarise(across(3:7, ~ sum(.x,na.rm = TRUE))) %>% select(-rg_flag) %>% 
    pivot_longer(2:5,names_to = "var", values_to = "values")
    
  
  # And then just Russell/Hustle group
  
  rg_staff_student_num <- staff_student_num %>% 
    filter(rg_flag==1) %>% 
    rename(student_numbers = total) %>% 
    group_by(date) %>% 
    summarise(across(3:7, ~ sum(.x,na.rm = TRUE))) %>% select(-rg_flag) %>% 
    pivot_longer(2:5,names_to = "var", values_to = "values")
  
  # plot it all - total and based
  
  # Totals - number
  
  p <- total_staff_student_num %>% 
    ggplot(aes(date,values,group=var,color=var)) +
    geom_line(linewidth=1.5) +
    theme_minimal()
  
  p
  
  # RG - number
  
  # Totals - based
  
  # Totals - number
