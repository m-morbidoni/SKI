install.packages(c("tidyverse", "lubridate", "data.table", "installr"))
library(tidyverse)
library(data.table)
library(lubridate)
library(installr)

installr()

batch_list <- function() {
  rm(list=ls())
  setwd(choose.dir())
  list <- list()  
  files <- list.files(getwd(), 
                      pattern = "*.csv")
  batch_data <- function(x) {
    as_tibble(fread(x, sep = ";",  
                    colClasses = "character", 
                    na.strings=c("","NA")))
  }
  list <- lapply(files, batch_data)
  names(list) <- str_sub(files, 1, 12)
  return(list)
}
try <- batch_list()
single_df <- bind_rows(try, .id = "Batch")
test_file_tidy <- single_df %>%
  #the Time_IN column is read as a chr class, but can be converted into dttm
  mutate(TimeStamp = mdy_hms(Time_IN)) %>%
  mutate(TrackCode = as.character(TrackCode)) %>%
  mutate(Res_AOI_1 = as_factor(Res_AOI_1), 
         Res_Hipot_Test = as_factor(Res_Hipot_Test), 
         Res_Hot_Test = as_factor(Res_Hot_Test), 
         Res_Cold_Test = as_factor(Res_Cold_Test), 
         Res_Laser = as_factor(Res_Laser), 
         Res_AOI_2 = as_factor(Res_AOI_2)) %>%
  mutate(Code = str_extract(LaserMarkCode_read, 
                            pattern = "\\d\\d\\d+")) %>% 
  mutate(Code = na_if(Code, "")) %>% 
  group_by(Batch) %>%
  fill(Code, .direction = "downup") %>%
  ungroup() %>%
  mutate(Batch = as_factor(Batch), 
         Code = as_factor(Code), 
         AOI_1 = as_factor(Failcode_AOI_1), 
         Hipot_test = as_factor(Failcode_Hipot_Test), 
         Hot_test = as_factor(Failcode_Hot_Test), 
         Cold_test = as_factor(Failcode_Cold_Test), 
         Laser = as_factor(Failcode_Laser), 
         AOI_2 = as_factor(Failcode_Aoi_2), 
         Bin_Output = as_factor(Bin_Output)) %>%
  select(-c(Time_IN, 
            TrackCode_read,
            Failcode_AOI_1, 
            Failcode_Hipot_Test, 
            Failcode_Hot_Test, 
            Failcode_Cold_Test, 
            Failcode_Aoi_2,
            Failcode_Laser,
            Time_OUT)) %>%
  mutate(Weight = as.numeric(Weight), 
         Convexity = as.integer(Convexity), 
         StepHeight_1 = as.integer(StepHeight_1), 
         StepHeight_2 = as.integer(StepHeight_2), 
         StepHeight_1_P2 = as.integer(StepHeight_1_P2), 
         StepHeight_2_P2 = as.integer(StepHeight_2_P2)) %>%
  mutate(Week = isoweek(TimeStamp), 
         Year = year(TimeStamp), 
         DateCode = as_factor(format(TimeStamp, "%Y%W")))

scrap_dataset <- test_file_tidy %>% 
  group_by(Batch, Code, TimeStamp = date(TimeStamp)) %>% 
  summarise(AOI1_scrPC = 100*sum(Res_AOI_1 == "FAIL")/n(), 
            Hipot_scrPC = 100*sum(Res_Hipot_Test == "FAIL")/n(), 
            Hot_scrPC = 100*sum(Res_Hot_Test == "FAIL")/n(), 
            Cold_scrPC = 100*sum(Res_Cold_Test == "FAIL")/n(), 
            Laser_scrPC = 100*sum(Res_Laser == "FAIL")/n(), 
            AOI2_scrPC = 100*sum(Res_AOI_2 == "FAIL")/n(), nParts =  n())

scrap_analysis <- scrap_dataset %>% 
  group_by(TimeStamp) %>% 
  summarise(AOI1 = mean(AOI1_scrPC, na.rm = TRUE), 
            Hipot = mean(Hipot_scrPC, na.rm = TRUE), 
            Hot = mean(Hot_scrPC, na.rm = TRUE), 
            Cold = mean(Cold_scrPC, na.rm = TRUE), 
            Laser = mean(Laser_scrPC, na.rm = TRUE), 
            AOI2 = mean(AOI2_scrPC, na.rm = TRUE), 
            tot = sum(AOI1, Hipot, Hot, Cold, Laser, AOI2), 
            nParts = sum(nParts)) %>%  
  pivot_longer(c(AOI1, Hipot, Hot, Cold, Laser, AOI2), 
               names_to = "Test_type", 
               values_to = "ScrapPC") %>% 
  ggplot(aes(x = TimeStamp, 
             y = ScrapPC)) + 
  geom_point(size = 3, na.rm = TRUE)  + 
  scale_x_date() +
  facet_wrap(~Test_type)

weights_by_code <- test_file_tidy %>% 
  group_by(Code, TimeStamp = date(TimeStamp)) %>% 
  summarise(min = min(Weight, 
                      na.rm = TRUE), 
            first_quartile = quantile(Weight, 0.25, 
                                      na.rm = TRUE), 
            median =  median(Weight, 
                             na.rm = TRUE), 
            third_quartile = quantile(Weight, 0.75, 
                                      na.rm = TRUE), 
            max = max(Weight, 
                      na.rm = TRUE), 
            mean = mean(Weight, 
                        na.rm = TRUE), 
            sd = sd(Weight, 
                    na.rm = TRUE), 
            variance = var(Weight, 
                           na.rm = TRUE), 
            n = n())

test_file_long <- test_file_tidy %>% 
  pivot_longer(c(Hipot_test, 
                 Hot_test, 
                 Cold_test, 
                 AOI_1, 
                 AOI_2, 
                 Laser), 
               names_to = "Remove", 
               values_to = "Failcode") %>% 
  select(-Remove) %>%
  pivot_longer(c(Res_AOI_1,
                 Res_Hipot_Test, 
                 Res_Hot_Test, 
                 Res_Cold_Test, 
                 Res_Laser, 
                 Res_AOI_2), 
               names_to = "Test_type", 
               values_to = "Test_result", 
               values_drop_na = TRUE, 
               names_transform = list(Test_results = as_factor)) %>%
  distinct()
