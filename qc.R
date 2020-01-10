library(tidyverse)
dat <- readxl::read_excel("data/native_plants.xlsx") %>%
  mutate_if(is.numeric, ~ replace(.x, is.na(.x), 0))

genus.idx <- dat$Genus == "GENUS"
species.idx <- dat$Species == "SPECIES"

features <- c(
  Butterfly_nectar = 36,
  Butterfly_larval = 14,
  Hummingbirds = 17,
  Moist_soil = 62 + 1, # plus wildcard for ferns
  Pond = 1,
  Shallow_water = 6,
  Rock_sunny = 32,
  Rock_shady = 17,
  Groundcover = 15,
  Deer_resistant = 49,
  Coniferous_forest = 53,
  Shade_garden = 47,
  Prairie = 75,
  Deciduous_forest = 61,
  Mixed_border = 56,
  Front_yard_sunny = 25,
  Front_yard_shady = 24,
  Front_yard_woody = 20
)
for(feat in names(features))
{
  genus <- setdiff(dat$Genus[species.idx & dat[[feat]] == 1], "GENUS")
  type <- dat$Type[species.idx & dat[[feat]] == 1 & genus.idx]
  bad.common <- dat$Common[dat$Genus %in% genus & dat[[feat]] == 1 & !species.idx]
  if(length(bad.common) && feat != "Coniferous_forest")
    warning(paste0(feat, ": ", paste0(bad.common, collapse = ", ")))
  if(sum(dat[[feat]]) != features[feat]) stop(feat, ": ", sum(dat[[feat]]))

  dat[[feat]][dat$Genus %in% genus] <- 1
  dat[[feat]][dat$Type %in% type] <- 1
}

dup.type <- xtabs(~ Genus + Type, data = dat) %>%
  as.data.frame() %>%
  filter(Freq > 0) %>%
  pull(Genus) %>%
  (function(x) x[duplicated(x)]) %>%
  as.character()
if(length(setdiff(dup.type, c("Salix", "Prunus", "Cornus", "Potentilla")))) stop("Some type/genus mismatches")
if(anyNA(dat$Type)) stop("Missing type")
write.csv(dat, "data/native_plants_qc.csv", row.names = FALSE, na = "")
