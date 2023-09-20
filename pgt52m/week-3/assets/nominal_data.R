
# overview ----------------------------------------------------------------
# This script shows how to import, summarise and plot data which is nominal
# (categorical). It uses an example which is the coat colour 62 cats.
# The data file is called cat-coats.csv which is a comma separated values file
# and the script assumes it is in a folder called data-raw inside the working 
# directory. 
# The data file also contains a continuous variable, mass, which is not 
# used here.

# Carried out with
# R Core Team (2023). R: A language and environment for statistical computing.
# R Foundation for Statistical Computing, Vienna, Austria. 
# URL https://www.R-project.org/.
# Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, 
# Hayes A,Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K,
# Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, 
# Woo K, Yutani H (2019). “Welcome to the tidyverse.” Journal of Open Source 
# Software, 4(43), 1686. doi:10.21105/joss.01686 
# <https://doi.org/10.21105/joss.01686>.


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



