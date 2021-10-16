# Alzheimers Projeect
# load in necessary libraries
library(tabulizer)
library(dplyr)
library(rJava)
library(pdftools)
library(tidyverse)

# I want to start by explaining why i didn't create a list of the urls, 
# to loop through instead of repeating code 5 times. 
# This is because the State ID needs to be included in the final dataset
# otherwise we can't see cases by geographical location

# I also want to say that I first tried this on a much much bigger pdf
# and was unable to get this code to work, because of the table formats
# in it, but upon selecting a smaller pdf I was able to extract
# the tables much more easily.

# First lets get a table of the state populations so that we can compare
# percentage of alzheimers by state / region. 
population <- read.csv("csvData.csv")
summary(population)

# For the Alzheimers cases dataset;
# First we get the table from the pdf, we start by reading the lines of the pdf

tn_alz_pdf <- pdf_text(
  "https://www.alz.org/getmedia/1734b340-3943-4035-8682-7de647199a6f/tennessee-alzheimers-facts-figures") %>% 
readr::read_lines()

# Next we take the lines that have our table values that we want
# and we squish it to make everything have the same 'separator'
# and we split it up by the 'separator', which is a space.

tn_alz_squish <- tn_alz_pdf[4:5] %>%
  str_squish() %>%
  strsplit(split = " ")

# Next we use the plyr::ldply to turn the rows into a table.

tn_alz <- plyr::ldply(tn_alz_squish)

# We have the column names from the line in the pdf that we 
# read in earlier, so we take them and create a list of them.
# Note that every state will have the same column names, so we
# will reuse this list. 

all_colnames <- c("Year", "65-74", "75-84", "85+" ,"TOTAL")

# we give our dataset the column names that we extracted from the pdf

colnames(tn_alz) <- all_colnames

# did it work?????

tn_alz

# we repeat the same process for Utah, California, Florida, and New York

ut_alz_pdf <- pdf_text(
  "https://www.alz.org/getmedia/fea37c93-542e-4f54-8220-8770c9589f81/utah-alzheimers-facts-figures") %>% 
  readr::read_lines()

ut_alz_squish <- ut_alz_pdf[4:5] %>%
  str_squish() %>%
  strsplit(split = " ")

ut_alz <- plyr::ldply(ut_alz_squish)

colnames(ut_alz) <- all_colnames

ut_alz


ca_alz_pdf <- pdf_text(
  "https://www.alz.org/getmedia/e2141cc7-e1b5-4369-b509-f4da3d5b2484/california-alzheimers-facts-figures") %>% 
  readr::read_lines()

ca_alz_squish <- ca_alz_pdf[4:5] %>%
  str_squish() %>%
  strsplit(split = " ")

ca_alz <- plyr::ldply(ca_alz_squish)

colnames(ca_alz) <- all_colnames

ca_alz


fl_alz_pdf <- pdf_text(
  "https://www.alz.org/getmedia/66c8d005-238e-4e47-b536-dc6734a188c9/florida-alzheimers-facts-figures") %>% 
  readr::read_lines()

fl_alz_squish <- fl_alz_pdf[4:5] %>%
  str_squish() %>%
  strsplit(split = " ")

fl_alz <- plyr::ldply(fl_alz_squish)

colnames(fl_alz) <- all_colnames

fl_alz


ny_alz_pdf <- pdf_text(
  "https://www.alz.org/getmedia/0138f037-56fb-4b65-9eb2-598a05237f28/newyork-alzheimers-facts-figures") %>% 
  readr::read_lines()

ny_alz_squish <- ny_alz_pdf[4:5] %>%
  str_squish() %>%
  strsplit(split = " ")

ny_alz <- plyr::ldply(ny_alz_squish)

colnames(ny_alz) <- all_colnames

ny_alz


# Now that we have the data for all the 5 states we want to look at
# we can see if the number of cases is different for different areas
# of the country.

# We want all 5 states in the same dataframe, because the data all has
# the same column names, we can append the different tables to each other
# but we want to see the state for each one, so we add a state ID column
# to each frame. 

tn_alz$State_ID <- "Tennessee"
ut_alz$State_ID <- "Utah"
fl_alz$State_ID <- "Florida"
ca_alz$State_ID <- "California"
ny_alz$State_ID <- "New York"

# Now we want to append them all. 
alz <- tn_alz %>%
  bind_rows(ut_alz, fl_alz, ca_alz, ny_alz)
alz

# We want to see the state id first, so we use a select statement to 
# order the columns

alz <- alz %>%
  select(State_ID, all_colnames)
summary(alz)

# This data is already tidy, but it has unwanted commas, and the columns are
# character class. We convert to numbers,and remove commas. 
alz$Year <- as.numeric(gsub(",", "", alz$Year ))
alz$`65-74` <- as.numeric(gsub(",", "", alz$`65-74`))
alz$`75-84` <- as.numeric(gsub(",", "", alz$`75-84`))
alz$`85+` <- as.numeric(gsub(",", "", alz$`85+`))
alz$TOTAL <- as.numeric(gsub(",", "", alz$TOTAL))

# Now we want to combine the two data sets, alz and population. 
# We don't want every column in population, just the "Pop" column
# We will filter all else out, and only include the 5 states we are looking at
# We will also rename the State column, to match our State_ID 
# that we created before

state_population <- population %>%
  rename("State_ID" = "State") %>%
  select(State_ID, Pop) %>%
  filter(State_ID == "California" |
         State_ID == "Tennessee" |
         State_ID == "Florida" | 
         State_ID == "New York" |
         State_ID == "Utah")

# Did it work?
summary(state_population)
# Yes!!

# Now let's join these datasets! 
# I've set it up so that there shouldn't be any missing values, and we will
# join by State_ID
# We notice that the data is no longer tidy, but we also don't want to look at
# the extrapolation for 2025, so instead of doing a pivot to make tidy data,
# we will throw out the 2025 predictions
# Next we want to create a column with the percentage of people with alzheimers

state_population_end <- state_population %>%
  inner_join(alz, by = "State_ID") %>%
  filter(Year == "2020") %>%
  mutate("Percent" = TOTAL / Pop)

# round the Percent so that it is easier to read
state_population_end$Percent <- round(state_population_end$Percent, 5)

state_population_end
write_csv(state_population_end, "StateCases.csv")
