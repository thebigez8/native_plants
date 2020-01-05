library(tidyverse)
dat <- readxl::read_excel("data/native_plants.xlsx") %>%
  mutate_if(is.numeric, ~ replace(.x, is.na(.x), 0))

genus.idx <- dat$Genus == "GENUS"
species.idx <- dat$Species == "SPECIES"

features <- c(
  butterfly_nectar = 24,
  butterfly_larval = 9,
  Hummingbirds = 14,
  Moist_Soil = 40 + 1, # plus wildcard for ferns
  Rock_sunny = 32,
  Rock_shady = 17,
  Groundcover = 15,
  Deer_resistant = 32 + 2,
  Coniferous_forest = 27,
  Shade_garden = 39,
  Prairie = 56,
  Deciduous_forest = 43,
  Mixed_border = 44,
  Front_yard_sunny = 25,
  Front_yard_shady = 24
)
for(feat in names(features))
{
  genus <- setdiff(dat$Genus[species.idx & dat[[feat]] == 1], "GENUS")
  type <- dat$Type[species.idx & dat[[feat]] == 1 & genus.idx]
  bad.common <- dat$Common[dat$Genus %in% genus & dat[[feat]] == 1 & !species.idx]
  if(length(bad.common)) warning(paste0(feat, ": ", paste0(bad.common, collapse = ", ")))
  if(sum(dat[[feat]]) != features[feat]) stop(feat, ": ", sum(dat[[feat]]))

  dat[[feat]][dat$Genus %in% genus] <- 1
  dat[[feat]][dat$Type %in% type] <- 1
}

write.csv(dat, "data/native_plants_qc.csv", row.names = FALSE, na = "")
