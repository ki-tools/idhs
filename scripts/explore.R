library(idhs)
# load_all()

# NOTE: before running this script run the code in data.R

# --------------- read in preprocessed data -------------- #

dd <- readRDS("ext-data/idhs/extract2/dd.rds")

# html page showing variable names and their descriptions
view_var_descs(dd)
view_var_descs(dd, include_geo = TRUE)

# ---------- get regional yearly STI prevalence ---------- #

# numerator is stianyr (had STI in last 12 months) = "Yes"
attributes(dd$stianyr)[c("labels", "label")]
# denominator is all responses except sexactiv4wk = "never had intercourse"
attributes(dd$sexactiv4wk)[c("labels", "label")]

sti_rates <- calc_rates(dd,
  geo_dir = "ext-data/idhs/geo/",
  num_var = "stianyr",
  num_cond = 1,
  denom_var = "sexactiv4wk",
  denom_cond = c(1:9)
)

# -------------------- visualizations -------------------- #

# individual scatter
scatter_vis_all(sti_rates)

# individual geo
geo_vis_all(sti_rates,
  title = "STI rates",
  geo_dir = "ext-data/idhs/geo/",
  bins = c(0, 2, 4, 6, 10, 15, 35)
)

# joint
geo_scatter_vis(
  sti_rates,
  title = "STI rates",
  geo_dir = "ext-data/idhs/geo/",
  bins = c(0, 2, 4, 6, 10, 15, 35)
)
