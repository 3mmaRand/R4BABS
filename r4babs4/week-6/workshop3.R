

library(tidyverse)

## import --------------------------------------------------------------
clean_trans <- read_csv("r4babs4/week-4/data-processed/ai_clean_logicle_trans.csv")



## scatter ss v fs -----------------------------------------------------
ggplot(clean_trans, aes(x = FS_Lin, y = SS_Lin)) +
  geom_hex(bins = 128) +
  scale_fill_viridis_c() +
  facet_grid(antibody ~ treatment) +
  theme_bw()


## gating live cells ---------------------------------------------------

# fs and ss cutoffs for live cells
xmin <- 25000
xmax <- 40000
ymin <- 5000
ymax <- 18000


# create a data frame with the coordinates of the box
box <- data.frame(x = c(xmin, xmin, xmax, xmax),
                  y = c(ymin, ymax, ymax, ymin))


## scatter ss v fs with live cell gate ---------------------------------
ggplot(clean_trans, aes(x = FS_Lin, y = SS_Lin)) +
  geom_hex(bins = 128) +
  scale_fill_viridis_c() +
  geom_polygon(data = box, aes(x = x, y = y), 
               fill = NA, 
               color = "red",
               linewidth = 1) +
  facet_grid(antibody ~ treatment) +
  theme_bw()


## gate the data -------------------------------------------------------
# filter out the debris
clean_trans_nondebris <- clean_trans |> 
  filter(between(FS_Lin, xmin, xmax),
         between(SS_Lin, ymin, ymax)) 



## summarise each sample after gating ----------------------------------
# number of cells in each sample after AI cleaning but before gating
clean_trans_n <-  clean_trans |> 
  group_by(antibody, treatment) |> 
  summarise(n = n()) 


# number of cells in each sample after gating
clean_trans_nondebris_n <-  clean_trans_nondebris |> 
  group_by(antibody, treatment) |> 
  summarise(n_nondebris = n()) 

# join the two summaries and calculate the percentage of cells
# that are not debris
clean_trans_nondebris_n <- clean_trans_nondebris_n |> 
  left_join(clean_trans_n, by = c("antibody", "treatment")) |> 
  mutate(perc_nondebris = round(n_nondebris/n * 100, 1) )



## fig1 ----------------------------------------------------------------
(fig1 <- ggplot(clean_trans, aes(x = FS_Lin, y = SS_Lin)) +
  geom_hex(bins = 128) +
  scale_fill_viridis_c() +
  geom_polygon(data = box, aes(x = x, y = y), 
               fill = NA, 
               color = "red",
               linewidth = 1) +
  geom_text(data = clean_trans_nondebris_n, 
            aes(label = paste0(perc_nondebris, "%")), 
            x = 20000, 
            y = 50000,
            colour = "red",
            size = 4) +
  scale_y_continuous(expand = c(0, 0),
                     name = "SSC") +
  scale_x_continuous(expand = c(0, 0),
                     name = "FSC") +
  facet_grid(antibody ~ treatment) +
  theme_bw() +
  theme(legend.position = "none"))

ggsave("r4babs4/week-6/figures/1-live-cell-gating.png",
       device = "png",
       plot = fig1,
       width = 5,
       height = 3.4,
       units = "in",
       dpi = 300)
# Scatterplots with SSC on the Y-axis and FSC on the X-axis for each of the six treatment-antibody combinations. The live cells are gated with a rectangular gate shown as a red box.  Dead cells and debris are outside the gate. The percentage of cells in the gate in each sample is shown in red text. 

## working out the APC gate --------------------------------------------
apc_cut <- 2.1

ggplot(clean_trans_nondebris, aes(x = E_coli_FITC_Lin, 
                                  y = TNFa_APC_Lin)) +
  geom_hex(bins = 128) +
  scale_fill_viridis_c() +
  geom_hline(yintercept = apc_cut, 
             color = "red") +
  facet_grid(antibody ~ treatment) +
  theme_bw()



## add variable for whether cells are TNF-α +'ve cells or not ----------
clean_trans_nondebris <- clean_trans_nondebris |> 
  mutate(tnfa = case_when(TNFa_APC_Lin < apc_cut ~ "TNF-α -'ve",
                          TNFa_APC_Lin >= apc_cut ~ "TNF-α +'ve"))

## summarise the number of TNF-α +'ve cells in each sample  
clean_trans_nondebris_tfna_pos <- clean_trans_nondebris |> 
  filter(tnfa == "TNF-α +'ve") |>
  group_by(antibody, treatment) |>
  summarise(n_pos_tnfa = n(),
            mean_apc = round(mean(TNFa_APC_Lin), 2))

## join the summary with the summary of the number of cells in each sample
## and calculate the percentage of cells that are TNF-α +'ve
clean_trans_nondebris_tfna_pos <- 
  clean_trans_nondebris_tfna_pos |> 
  left_join(clean_trans_nondebris_n, by = c("antibody", "treatment")) |> 
  mutate(perc_tfna_pos = round(n_pos_tnfa/n_nondebris * 100, 1) )

## fig2 ----------------------------------------------------------------
(fig2 <- clean_trans_nondebris |> 
   filter(treatment == "MEDIA") |>
   ggplot(aes(x = TNFa_APC_Lin)) +
   geom_histogram(bins = 100) +
   geom_vline(xintercept = apc_cut, 
              color = "red") +
   geom_text(data = clean_trans_nondebris_tfna_pos |> 
               filter(treatment == "MEDIA"), 
             aes(label = paste0(perc_tfna_pos, 
                                "% cells\nTNF-α +'ve\nMFI = ",
                                mean_apc)), 
             x = 2.5, 
             y = 125,
             colour = "red",
             size = 3) +
   scale_y_continuous(expand = c(0, 0),
                      limits = c(0, 155)) +
   scale_x_continuous(name = "Logicle transformed APC TNF-α signal") +
   facet_grid(~ antibody) +
   ggtitle("MEDIA treated") +
   theme_bw()  )

ggsave("r4babs4/week-6/figures/2-tnf-positive-media.png",
       device = "png",
       plot = fig2,
       width = 4,
       height = 2.5,
       units = "in",
       dpi = 300)


(fig2_alt <- clean_trans_nondebris |> 
  filter(treatment == "MEDIA") |>
  ggplot(aes(x = TNFa_APC_Lin)) +
  geom_density(fill = "gray80") +
  geom_vline(xintercept = apc_cut, 
             color = "red") +
  geom_text(data = clean_trans_nondebris_tfna_pos |> 
              filter(treatment == "MEDIA"), 
            aes(label = paste0(perc_tfna_pos, 
                               "% cells\nTNF-α +'ve\nMFI = ",
                               mean_apc)), 
            x = 2.5, 
            y = 2.5,
            colour = "red",
            size = 3) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 3.5),
                     name = "Density") +
    scale_x_continuous(name = "Logicle transformed APC TNF-α signal") +
    facet_grid(~ antibody) +
    ggtitle("MEDIA treated") +
  theme_bw())

ggsave("r4babs4/week-6/figures/2-alt-tnf-positive-media.png",
       device = "png",
       plot = fig2_alt,
       width = 4,
       height = 2.5,
       units = "in",
       dpi = 300)

# Two density plots showing the distribution of the TNFalpha APC signal (x-axes) for media treated cells from an isotope control sample (left) and an anti-TNFalpha stained sample (right). The red gate is drawn based on the isotope control to show TNFalpha positive cells. The percentage of cells which are classed TNFalpha positive an the mean fluorescence intensity in  TNFalpha positive cells is shown on each density plot



## fig 3 and and b -----------------------------------------------------
(fig3a <- clean_trans_nondebris |> 
   filter(treatment == "MEDIA") |>
   ggplot(aes(x = TNFa_APC_Lin, fill = antibody)) +
   geom_density(alpha = 0.3) +
   geom_vline(xintercept = apc_cut, 
              color = "red") +
   scale_fill_viridis_d(name = NULL) +
   scale_y_continuous(expand = c(0, 0),
                      limits = c(0, 3.6)) +
   scale_x_continuous(name = "Logicle transformed APC TNF-α signal") +
   theme_bw() +
   ggtitle("MEDIA treated") +
   theme(legend.position = c(0.8, 0.85)) )

ggsave("r4babs4/week-6/figures/3a-tnf-positive-media-antibody-overlay.png",
       device = "png",
       plot = fig3a,
       width = 3.5,
       height = 3.5,
       units = "in",
       dpi = 300)
# An "overlay" with two distributions on one plot. These are the density plots of  the TNFalpha APC signal for media treated cells from an isotope control sample (in purple) and an anti-TNFalpha stained sample (in yellow). The red gate is drawn based on the isotope control to show TNFalpha positive cells. 

(fig3b <- clean_trans_nondebris |> 
    filter(antibody == "TNFAPC") |>
    ggplot(aes(x = TNFa_APC_Lin, fill = treatment)) +
    geom_density(alpha = 0.3) +
    geom_vline(xintercept = apc_cut, 
               color = "red") +
    scale_fill_viridis_d(name = NULL) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 3.6)) +
    scale_x_continuous(name = "Logicle transformed APC TNF-α signal") +
    theme_bw() +
    ggtitle("Anti TNF-α stained") +
    theme(legend.position = c(0.75, 0.85),
          legend.margin = margin(0, 4, 4, 0)) )

ggsave("r4babs4/week-6/figures/3b-tnf-positive-tnfa-treatment-overlay.png",
       device = "png",
       plot = fig3b,
       width = 3.5,
       height = 3.5,
       units = "in",
       dpi = 300)
# An "overlay" with three distributions on one plot. These are the density plots of  the TNFalpha APC signal for anti-TNFalpha stained samples for each of the three treaments: Media (in purple), LPS (in blue) and EColi (in yellow). The red gate is drawn based on the isotope control to show TNFalpha positive cells. 

## working out the FITC gate -------------------------------------------
fitc_cut <- 1.8
ggplot(clean_trans_nondebris, aes(x = E_coli_FITC_Lin, 
                                  y = TNFa_APC_Lin)) +
  geom_hex(bins = 128) +
  geom_hline(yintercept = apc_cut, 
             color = "red") +
  geom_vline(xintercept = fitc_cut, 
             color = "red") +
  scale_fill_viridis_c() +
  facet_grid(antibody ~ treatment) +
  theme_bw() +
  theme(legend.position = "none")







## add variable for whether cells are FTIC +'ve cells or not -----------
clean_trans_nondebris <- clean_trans_nondebris |> 
  mutate(fitc = case_when(E_coli_FITC_Lin < fitc_cut ~ "FITC -'ve",
                          E_coli_FITC_Lin >= fitc_cut ~ "FITC +'ve"))

## summarise the number of FITC +'ve cells in each sample  
clean_trans_nondebris_fitc_pos <- clean_trans_nondebris |>
  filter(fitc == "FITC +'ve") |>
  group_by(antibody, treatment, .drop = FALSE) |>
  summarise(n_pos_fitc = n(),
            mean_fitc = round(mean(E_coli_FITC_Lin), 2))

## join the summary with the summary of the number of cells in each sample
## and calculate the percentage of cells that are FITC +'ve
clean_trans_nondebris_fitc_pos <- 
  clean_trans_nondebris_fitc_pos |> 
  left_join(clean_trans_nondebris_n, by = c("antibody", "treatment")) |> 
  mutate(perc_fitc_pos = round(n_pos_fitc/n_nondebris * 100, 1) )




## calculate the percentage of cells in each quadrant
all_combin_n <- clean_trans_nondebris |> 
  group_by(antibody, treatment, tnfa, fitc, .drop = FALSE) |>
  summarise(n = n())

all_combin_perc <- clean_trans_nondebris_n |> 
  select(n_nondebris, antibody, treatment) |> 
  right_join(all_combin_n, by = c("antibody", "treatment")) |> 
  mutate(perc = round(n / n_nondebris * 100, 1)) |> 
  filter(perc > 0)

## ---------------------------------------------------------------------
(fig5 <- ggplot(data = clean_trans_nondebris, 
       aes(x = E_coli_FITC_Lin,
           y = TNFa_APC_Lin)) +
  geom_hex(bins = 128) +
  geom_hline(yintercept = apc_cut,
             color = "red") +
  geom_vline(xintercept = fitc_cut,
             color = "red") +
  scale_fill_viridis_c() +
  geom_text(data = all_combin_perc |>
              filter(tnfa == "TNF-α +'ve",
                     fitc == "FITC +'ve"),
            aes(label = paste0(perc, "%")),
            x = 3.5,
            y = 3.1,
            size = 3) +
  geom_text(data = all_combin_perc |>
              filter(tnfa == "TNF-α +'ve",
                     fitc == "FITC -'ve"),
            aes(label = paste0(perc, "%")),
            x = 1.25,
            y = 3.1,
            size = 3) +
  geom_text(data = all_combin_perc |>
              filter(tnfa == "TNF-α -'ve",
                     fitc == "FITC +'ve"),
            aes(label = paste0(perc, "%")),
            x = 3.5,
            y = 1.25,
            size = 3) +
  geom_text(data = all_combin_perc |>
              filter(tnfa == "TNF-α -'ve",
                     fitc == "FITC -'ve"),
            aes(label = paste0(perc, "%")),
            x = 1.25,
            y = 1.25,
            size = 3) +
  facet_grid(antibody ~ treatment) +
  scale_y_continuous(name = "Logicle transformed APC TNF-α signal") +
  scale_x_continuous(name = "Logicle transformed FITC signal") +
  theme_bw() +
  theme(legend.position = "none"))


ggsave("r4babs4/week-6/figures/5-apc-v-fitc-quadgate_pc.png",
       device = "png",
       plot = fig5,
       width = 6.5,
       height = 3.5,
       units = "in",
       dpi = 300)
# Scatter plots of TNFalpha APC (y axes) against FITC (x axes) for each of the 6 treatment-antibody combinations. Horizontal and vertical lines indicate the gates (cut offs) for each variable and divide the scatterplots into quadrants. The percentage of cells in each quadrant is annotated on the plots where that percentage is greater than 0.
## fig 6 ---------------------------------------------------------------
(fig6 <- clean_trans_nondebris |> 
   filter(antibody == "TNFAPC") |>
   ggplot(aes(x = E_coli_FITC_Lin, fill = treatment)) +
   geom_density(alpha = 0.3) +
   geom_vline(xintercept = fitc_cut, 
              color = "red") +
   scale_fill_viridis_d(name = NULL, end = 0.7) +
   scale_y_continuous(expand = c(0, 0),
                      limits = c(0, 4.5)) +
   scale_x_continuous(name = "Logicle transformed FITC signal") +
   geom_text(data = clean_trans_nondebris_fitc_pos |> 
               filter(antibody == "TNFAPC",
                      treatment == "MEDIA"), 
             aes(label = paste0(perc_fitc_pos, 
                                "% FITC +'ve, MFI = ",
                                mean_fitc)), 
             x = 2.5, 
             y = 3.1,
             size = 3,
             colour = viridisLite::viridis(3, end = 0.7)[1]) +
   geom_text(data = clean_trans_nondebris_fitc_pos |> 
               filter(antibody == "TNFAPC",
                      treatment == "LPS"), 
             aes(label = paste0(perc_fitc_pos, 
                                "% FITC +'ve, MFI = ",
                                mean_fitc)), 
             x = 2.5, 
             y = 2.85,
             size = 3,
             colour = viridisLite::viridis(3, end = 0.7)[2]) +
   geom_text(data = clean_trans_nondebris_fitc_pos |> 
               filter(antibody == "TNFAPC",
                      treatment == "ECOLIGreen"), 
             aes(label = paste0(perc_fitc_pos, 
                                "% FITC +'ve, MFI = ",
                                mean_fitc)), 
             x = 2.5, 
             y = 2.65,
             size = 3,
             colour = viridisLite::viridis(3, end = 0.7)[3]) +
   theme_bw() +
   ggtitle("Anti TNF-α stained") +
   theme(legend.position = c(0.5, 0.85),
         legend.margin = margin(0, 4, 4, 0)) )

ggsave("r4babs4/week-6/figures/6-fitc-positive-fitc-treatment-overlay.png",
       device = "png",
       plot = fig6,
       width = 4,
       height = 4,
       units = "in",
       dpi = 300)

# An "overlay" with three distributions on one plot. These are the density plots of  the FITC signal for anti-TNFalpha stained samples for each of the three treatments: Media (in purple), LPS (in blue) and EColi (in green). The red gate shows FITC positive cells. The percentage of FITC positive cells and the mean FITC signal of  FITC positive cells for each treatment is annotated on the plot.