library(tidyverse)

# - nitrate = sodium umol/L
# - phosphate = potassium umol/L
# - oestrogen = b12 pmol/L
# - bacterial counts = wbc 	10^9 /L
# - algal counts = rbc count 	10^12 /L
# - microplastics count = platlet counts	10^9 /L
# - presence/absence = inflamation marker 0 or 1 has to go in after the summary


# 5 measure from one person
# 50 people: 25 before treatment and 25 people after notte, different people


n <- 25
reps <- 5


# generate raw data before
na_m <- 100
na_sd <- 30

k_m <- 4.8
k_sd <- 0.9

b12_m <- 170
b12_sd <- 100

wbc_m <- 	5

rbc_m <- 4.5

plate_m <- 300


before <- data.frame(sodium = round(rnorm(n * reps, na_m, na_sd), 1),
                     potassium = round(rnorm(n * reps, k_m, k_sd), 3),
                     b12 = round(rnorm(n * reps, b12_m, b12_sd), 1),
                     wbc = rpois(n * reps, wbc_m),
                     rbc = rpois(n * reps, rbc_m),
                     platlet = rpois(n * reps, plate_m),
                     status = "before",
                     patient = rep(1:n, each = reps))


# generate raw data before
na_m <- 120
k_m <- 4.6
b12_m <- 224
wbc_m <- 	10
rbc_m <- 6.5
plate_m <- 400

after <- data.frame(sodium = round(rnorm(n * reps, na_m, na_sd), 1),
                     potassium = round(rnorm(n * reps, k_m, k_sd), 3),
                     b12 = round(rnorm(n * reps, b12_m, b12_sd), 1),
                     wbc = rpois(n * reps, wbc_m),
                     rbc = rpois(n * reps, rbc_m),
                     platlet = rpois(n * reps, plate_m),
                     status = "after",
                     patient = rep(1:n, each = reps))


# summarise data
before <- 
  before |> 
  group_by(patient) |> 
  summarise(sodium = mean(sodium),
            potassium = mean(potassium),
            b12 = mean(b12), 
            wbc = mean(wbc),
            rbc = mean(rbc),
            platlet = mean(platlet))
before$status = "before"
# add inflamation marker
before$inflam <- rbinom(n = n, prob = 0.8, size = 1)


after <- 
  after |> 
  group_by(patient) |> 
  summarise(sodium = mean(sodium),
            potassium = mean(potassium),
            b12 = mean(b12), 
            wbc = mean(wbc),
            rbc = mean(rbc),
            platlet = mean(platlet))
after$status = "after"
# add inflamation marker
# add inflamation marker
after$inflam <- rbinom(n = n, prob = 0.3, size = 1)


# combine
bloods <- bind_rows(before, after)

# write to file
write_csv(bloods, "r4babs1/week-9/data-raw/blood.csv")

# check some shit
GGally::ggpairs(bloods, aes(colour = status))

# ----- Sodium
blood_summary_na <- bloods |> 
  group_by(status) |> 
  summarise(mean = mean(sodium),
            sd = sd(sodium),
            n = length(sodium),
            se = sd/sqrt(n))

ggplot() +
  geom_point(data = bloods, aes(x = status, y = sodium),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = blood_summary_na, 
                aes(x = status, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = blood_summary_na, 
                aes(x = status, ymin = mean, ymax = mean),
                width = 0.2) +
  scale_y_continuous(name = "Sodium (umol/L)", 
                     limits = c(0, 150), 
                     expand = c(0, 0)) +
  theme_classic()

# ----- Potassium
blood_summary_k <- bloods |> 
  group_by(status) |> 
  summarise(mean = mean(potassium),
            sd = sd(potassium),
            n = length(potassium),
            se = sd/sqrt(n))

ggplot() +
  geom_point(data = bloods, aes(x = status, y = potassium),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = blood_summary_k, 
                aes(x = status, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = blood_summary_k, 
                aes(x = status, ymin = mean, ymax = mean),
                width = 0.2) +
  scale_y_continuous(name = "Potassium (umol/L)", 
                     limits = c(0, 7), 
                     expand = c(0, 0)) +
  theme_classic()


# ----- b12
blood_summary_b12 <- bloods |> 
  group_by(status) |> 
  summarise(mean = mean(b12),
            sd = sd(b12),
            n = length(b12),
            se = sd/sqrt(n))

ggplot() +
  geom_point(data = bloods, aes(x = status, y = b12),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = blood_summary_b12, 
                aes(x = status, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = blood_summary_b12, 
                aes(x = status, ymin = mean, ymax = mean),
                width = 0.2) +
  scale_y_continuous(name = "B12 (pmol/L)", 
                     limits = c(0, 350), 
                     expand = c(0, 0)) +
  theme_classic()


# ----- wbc
blood_summary_wbc <- bloods |> 
  group_by(status) |> 
  summarise(mean = mean(wbc),
            sd = sd(wbc),
            n = length(wbc),
            se = sd/sqrt(n))

ggplot() +
  geom_point(data = bloods, aes(x = status, y = wbc),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = blood_summary_wbc, 
                aes(x = status, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = blood_summary_wbc, 
                aes(x = status, ymin = mean, ymax = mean),
                width = 0.2) +
  scale_y_continuous(name = "wbc (10^9/L)", 
                     limits = c(0, 15), 
                     expand = c(0, 0)) +
  theme_classic()


# ----- rbc
blood_summary_rbc <- bloods |> 
  group_by(status) |> 
  summarise(mean = mean(rbc),
            sd = sd(rbc),
            n = length(rbc),
            se = sd/sqrt(n))

ggplot() +
  geom_point(data = bloods, aes(x = status, y = rbc),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = blood_summary_rbc, 
                aes(x = status, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = blood_summary_rbc, 
                aes(x = status, ymin = mean, ymax = mean),
                width = 0.2) +
  scale_y_continuous(name = "rbc (10^9/L)", 
                     limits = c(0, 11), 
                     expand = c(0, 0)) +
  theme_classic()


bloods |> 
  ggplot(aes(x = sodium, y = potassium, colour = status)) +
  geom_point() +
  scale_y_continuous( name = "Potassium (umol/L)",
                      expand = c(0, 0)) +
  scale_x_continuous(name = "Potassium (umol/L)", 
                     expand = c(0, 0)) +
  theme_classic()


# bone length create ------------------------------------------------------

bones <- read.table("clipboard") |> select(ulna = V1, height = V2)

bones |> ggplot(aes(x = ulna, y = height)) +
  geom_point()

lm(data = bones, height ~ ulna)

bone <- data.frame(ulna = rnorm(30, mean = mean(bones$ulna), sd = sd(bones$ulna)))
bone$height <- (0.65293 + bone$ulna *  0.035 ) + rnorm(30, 0, 0.2)

bone$height <- round(bone$height, 2)
bone$ulna <- round(bone$ulna, 1)

bone |> ggplot(aes(x = ulna, y = height)) +
  geom_point()  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 35)) 

write_delim(bone, "r4babs1/week-9/data-raw/height.txt")
