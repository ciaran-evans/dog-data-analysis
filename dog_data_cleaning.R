library(tidyverse)

# Import the raw data
# A description of each variable in dog_data_raw.csv can be
# found in the data dictionary (data_dictionary.pdf)

dog_data <- read_csv("dog_data_raw.csv")

# Section 3.4 (data description)
# dimensions of the raw data
# 284 rows, 210 columns
dim(dog_data)


# Section 4.1: Processing the data

# there are a few values in PN, L2, and SC which are outside the allowed range
# replace these values with NAs 
# Also reverse the scores for selected items (Loneliness, SCS, and Integration)

dog_data <- dog_data %>%
  mutate(across(starts_with("PN"), ~ replace(.x, .x > 5, NA)),
         across(starts_with("L2"), ~ replace(.x, .x > 4, NA)),
         across(starts_with("SC"), ~ replace(.x, .x > 6, NA)),
         across(paste("L1_", c(1, 5, 6, 9, 10, 15, 16, 19, 20), sep=""),
                function(x){5-x}),
         across(paste("L2_", c(1, 5, 6, 9, 10, 15, 16, 19, 20), sep=""),
                function(x){5-x}),
         across(paste("SC1_", c(3, 6, 7, 9, 11, 13, 15, 17, 18, 20), sep=""),
                function(x){7-x}),
         across(paste("SC2_", c(3, 6, 7, 9, 11, 13, 15, 17, 18, 20), sep=""),
                function(x){7-x}),
         Engagement1 = 6 - HO1_2,
         Engagement2 = 6 - HO2_2)


# Section 4.1: calculating scores
# The original data contains a column for each item on 
# the survey. To get scores like the positive affect score, we need to 
# average all items for that score. Here we average the scores, then
# remove the original columns for the individual items.
# The resulting data still contains 284 rows, but now only has 24 columns

dog_data <- dog_data %>%
  mutate(PANAS_PA1 = rowMeans(select(dog_data,
                                   num_range("PN1_", range = c(3,5,7,8,10))),
                            na.rm = TRUE),
         PANAS_PA2 = rowMeans(select(dog_data,
                                   num_range("PN2_", range = c(3,5,7,8,10))),
                            na.rm = TRUE), 
         SHS1 = rowMeans(select(dog_data, num_range("HA1_", range = c(1,2,3))), na.rm = TRUE),
         SHS2 = rowMeans(select(dog_data, num_range("HA2_", range = c(1,2,3))), na.rm = TRUE),
         SCS1 = rowMeans(select(dog_data, starts_with("SC1_")), na.rm = TRUE),
         SCS2 = rowMeans(select(dog_data, starts_with("SC2_")), na.rm = TRUE),
         FS1 = rowMeans(select(dog_data, starts_with("F1_")), na.rm = TRUE),
         FS2 = rowMeans(select(dog_data, starts_with("F2_")), na.rm = TRUE),
         Stress1 = S1_1,
         Stress2 = S2_1,
         Homesick1 = HO1_1,
         Homesick2 = HO2_1,
         Lonely1 = rowMeans(select(dog_data, starts_with("L1_")), na.rm = T),
         Lonely2 = rowMeans(select(dog_data, starts_with("L2_")), na.rm = T),
         PANAS_NA1 = rowMeans(select(dog_data,
                                   num_range("PN1_", range = c(1,2,4,6,9))),
                            na.rm = TRUE),
         PANAS_NA2 = rowMeans(select(dog_data,
                                   num_range("PN2_", range = c(1,2,4,6,9))),
                            na.rm = TRUE)) %>%
  dplyr::select(RID, GroupAssignment, Age_Yrs, Year_of_Study, 
                Live_Pets, Consumer_BARK,
                PANAS_PA1, PANAS_PA2, SHS1, SHS2, 
                SCS1, SCS2, Engagement1, Engagement2,
                FS1, FS2, Stress1, Stress2, 
                Homesick1, Homesick2, Lonely1, 
                Lonely2, PANAS_NA1, PANAS_NA2)

dim(dog_data)


# finally, let's make the data long not wide, by making a new column 
# for stage (pre/post)

dog_data <- dog_data %>% 
  pivot_longer( 
    cols = -c(RID, GroupAssignment, Age_Yrs, Year_of_Study,
              Live_Pets, Consumer_BARK),
    names_to = c(".value", "Stage"),
    names_pattern = "([\\D]+)(\\d+)") %>%
  mutate(Stage = ifelse(Stage == 1, "pre", "post"))


# export cleaned data

write_csv(dog_data, "dog_data_cleaned.csv")


# long data for app usage
dog_data_long <- dog_data %>%
  mutate(GroupAssignment = factor(GroupAssignment, 
                                  levels = c("Control", "Indirect", "Direct")))%>%
  pivot_wider(values_from = c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                              "Homesick", "Lonely", "PANAS_NA"), names_from = Stage) 
write.csv(x = dog_data_long, file = "dog_data_long.csv")