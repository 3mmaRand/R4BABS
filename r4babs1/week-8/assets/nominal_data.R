
# overview ----------------------------------------------------------------
# This script shows how to import, summarise and plot data which is nominal
# (categorical). It uses an example which is the coat colour 62 cats.
# The data file is called cat-coats.csv which is a comma separated values file
# and the script assumes it is in a folder called data-raw inside the working 
# directory. 
# The data file also contains a continuous variable, mass, which is not 
# used here.

# packages ----------------------------------------------------------------
# load tidyverse: functions for data import, summary and plotting
library(tidyverse)

# import ------------------------------------------------------------------
# import data from csv
cats <- read_csv("data-raw/cat-coats.csv")


# summarise ---------------------------------------------------------------
# count the number of cats in each category
cats |> 
  group_by(coat) |> 
  count()

# plot --------------------------------------------------------------------
# barchart of the raw data (no need to tabulate first)
ggplot(cats, aes(x = coat)) +
  geom_bar()



