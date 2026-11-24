
# get weighted mean of the indicator 'andel_qnormal_klasse' including standard error  
# (weighted in order to give equal weight to each vannregion)  

library(here)
library(ggplot2)

# functions
source(here("R/NIVA_Fjord_functions.R"))

# read data
dat <- read_csv(here("data/NIVA_fjord/regulerte_elver_kyst.csv"))

cat("number of rivers", nrow(dat))

ggplot(dat, aes(okoregion, andel_qnormal_prosent)) +
  geom_jitter(width = 0.2) +
  scale_y_log10() +
  coord_flip()  

ggplot(dat, aes(andel_qnormal_klasse, andel_qnormal_prosent)) +
  geom_jitter(width = 0.2) +
  scale_y_log10() +
  coord_flip()  

# for simplicity, we aggregate by waterbody first 
# (instead of doing a proper statistical hierarchical approach, which would have given
#  a slightly lower standard error)
# code from the 'compute_indeks' function
by_vannforekomst <- dat %>%
  mutate(andel_qnormal_klasse = andel_qnormal_klasse / 100) %>%
  group_by(vannforekomstid,okoregion) %>%
  summarise(andel_qnormal_klasse = mean(andel_qnormal_klasse), .groups = "drop")

# by_vannforekomst |> filter(okoregion == "") |> pull(vannforekomstid)
# "0280020402-2-C" - this is a lake, Øystrebøvatnet 
# We drop this (the corresponding marine watre body exists in the data):
# by_vannforekomst |> filter(vannforekomstid == "0280020402-1-C")

# remove the lake (only one)
by_vannforekomst <- by_vannforekomst |> 
  filter(okoregion != "")

cat("number of water bodies", nrow(by_vannforekomst))

# plot
ggplot(by_vannforekomst, aes(x = okoregion, y = andel_qnormal_klasse)) +
  geom_boxplot()

ggplot(by_vannforekomst, aes(x = fct_rev(okoregion), y = andel_qnormal_klasse)) +
  geom_jitter(width = 0.2) +
  coord_flip() +
  labs(
    x = "Region", y = "Indikator NIVA_Fjord"
  ) +
  theme_bw()

# dir.create("NIVA_Fjord_results")
# writexl::write_xlsx(by_vannforekomst, "NIVA_Fjord_results/NIVA_Fjord_indikator.xlsx")

# weights - use inverse weightin in order to give equal weight
# to each ecoregion
n_ecoregion <- table(by_vannforekomst$okoregion)

# inverse weighting method
w <- 1/n_ecoregion
w <- w/sum(w)

# add to data
lookup_weights <- data.frame(w) |> 
  rename(okoregion = Var1, w = Freq)
by_vannforekomst <- by_vannforekomst |> 
  left_join(lookup_weights, by = "okoregion")

# unweighted mean  
mod <- lm(andel_qnormal_klasse ~  1, by_vannforekomst)
summary(mod)$coef[,1:2]

# 29.06.2026:
# Estimate Std. Error 
# 0.75270325 0.01884167 

# weighted mean  
mod <- lm(andel_qnormal_klasse ~  1, by_vannforekomst, weights = by_vannforekomst$w)
summary(mod)$coef[,1:2]

# 29.06.2026:
# Estimate Std. Error 
# 0.74554846 0.01897685 

