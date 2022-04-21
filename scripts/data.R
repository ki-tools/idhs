# ---------- preprocess the data (only run once) --------- #

dd <- preprocess_ipums(
  input_xml = "ext-data/idhs/extract2/idhs_00002.xml",
  output_file = "ext-data/idhs/extract2/dd.rds"
)

preprocess_geo(dd, output_dir = "ext-data/idhs/geo/")

ipumsr::ipums_conditions()
# view extract information
ipumsr::ipums_view(attr(dd, "ipums_ddi"))


