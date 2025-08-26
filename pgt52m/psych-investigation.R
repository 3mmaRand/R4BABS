# I plan to use:
# - a subset of w#_cbt_intervention.csv in place of seals in week 4
# - a different subset of w#_cbt_intervention.csv in place of bees in week 5
# - a subset of w#_#diagnosis_stress_activity.csv in place of plants in week 6
# - a different subset of w#_#diagnosis_stress_activity.csv in place of Adiponectin in week 7




library(tidyverse)
cbt_intervention <- read_csv("pgt52m/data-raw-temp/w1_cbt_intervention.csv") |> 
  janitor::clean_names()

cbt_intervention |> 
  filter(therapy == "CBT") |> 
  filter(time != "MidTreatment") |> 
  mutate(stress = round(stress, 1)) |> 
  select(time, stress) |> 
  write_csv(file = "pgt52m/week-4/data-raw/cbt_intervention.csv")

