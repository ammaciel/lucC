library(tidyverse)

test_tb <- tibble(longitude = double(), 
                  latitude = double(), 
                  start_date = character(), 
                  end_date = character(), 
                  label = character())
test_tb

test_tb <- add_row(test_tb, longitude = -55.43242, latitude = -11.54353, start_date = "2001-09-25", end_date = "2002-09-25", label = "Cana") 
test_tb <- add_row(test_tb, longitude = -55.43242, latitude = -11.54353, start_date = "2002-09-25", end_date = "2003-09-25", label = "Cana") 
test_tb <- add_row(test_tb, longitude = -55.43242, latitude = -11.54353, start_date = "2003-09-25", end_date = "2004-09-25", label = "Cana") 
test_tb <- add_row(test_tb, longitude = -55.43242, latitude = -11.54353, start_date = "2004-09-25", end_date = "2005-09-25", label = "Cana") 
test_tb <- add_row(test_tb, longitude = -55.43242, latitude = -11.54353, start_date = "2005-09-25", end_date = "2006-09-25", label = "Cana") 
test_tb <- add_row(test_tb, longitude = -55.43242, latitude = -11.54353, start_date = "2006-09-25", end_date = "2007-09-25", label = "Cana") 
test_tb <- add_row(test_tb, longitude = -55.43242, latitude = -11.54353, start_date = "2007-09-25", end_date = "2008-09-25", label = "Cana") 


test_tb





