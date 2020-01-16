library(tidyverse)
library(arsenal)

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
if(any(str_extract(dat$Height, "[a-z]+$") %nin% c("in", "ft", NA))) stop("Height units")
if(any(str_extract(dat$Width, "[a-z]+$") %nin% c("in", "ft", NA))) stop("Width units")
write.csv(dat, "data/native_plants_qc.csv", row.names = FALSE, na = "")

get_MN <- function(x)
{
  # 1 2 3
  # 4 5 6
  # 7 8 9
  out <- character(0)
  for(i in x)
  {
    out <- c(out, switch(
      i,
      "E MN" = c(3, 6, 9), "N MN" = 1:3, "MN-Iowa border" =, "S MN" = c(7, 8, 9), "W MN" = c(1, 4, 7),
      "far NW MN" = , "NW MN" = 1, "far NE MN" = , "Lake Superior MN" = , "NE MN" = 3,
      "far SE MN" = , "SE border MN" =, "SE MN" = 9, "SW MN" = 7,
      "throughout MN" = 1:9,
      "all but far N MN" = , "S 2/3 MN" = 4:9,
      "all but S MN" = , "N 2/3 MN" = 1:6,
      "all but SW and W MN" = , "E 2/3 MN" = c(2:3, 5:6, 8:9), "central MN" = 5,
      "south-central MN" = 8, "east-central MN" = 6, "band NW to SE MN" = c(1, 5, 9),
      "band from east-central to SE MN" = c(5, 9),
      "all but far SW MN" = , "all but SW MN" = c(1:6, 8:9),
      "all but far SE MN" = , "all but SE MN" = 1:8, "all but NW MN" = 2:9,
      "all but north-central MN" = c(1, 3:9), "all but NE MN" = , "all but far NE MN" = c(1:2, 4:9),
      NA
    ))
    if(anyNA(out)) print(i)
  }
  lapply(1:9, function(j) if(length(out) == 0) NA_integer_ else +(j %in% out)) %>%
    set_names(paste0("MN_", 1:9)) %>%
    as_tibble()
}

get_zone <- function(x)
{
  tibble(
    Zone_1 = +any(grepl("1", x)),
    Zone_2 = +any(grepl("2", x)),
    Zone_3 = +any(grepl("3", x)),
    Zone_4 = +any(grepl("4", x))
  )
}
rplc <- function(y, v, z) replace(y, y == v, z)

get_sun <- function(x)
{
  out <- x %>%
    strsplit(" to ") %>%
    "[["(1) %>%
    rplc("deep shade", 0) %>%
    rplc("heavy shade", 1) %>%
    rplc("full shade", 2) %>% rplc("shade", 2) %>%
    rplc("moderate shade", 3) %>%
    rplc("partial shade", 4) %>%
    rplc("light shade", 5) %>%
    rplc("very light shade", 6) %>%
    rplc("spring sun", 7) %>%
    rplc("partial sun", 8) %>%
    rplc("sun", 9) %>%
    rplc("full sun", 10) %>%
    as.numeric() %>%
    (function(i) if(length(i) == 2) (i[1]):(i[2]) else i)

  lapply(0:10, function(j) if(anyNA(out)) NA_integer_ else +(j %in% out)) %>%
    set_names(paste0("Sun_", 0:10)) %>%
    as_tibble()
}

get_moisture <- function(x)
{
  if(identical(x, "not wet or boggy"))
  {
    out <- 0:3
  } else if(length(x) > 0)
  {
    out <- x %>%
      strsplit(" to ") %>%
      "[["(1) %>%
      rplc("dry", 0) %>%
      rplc("average", 1) %>%
      rplc("slightly moist", 2) %>%
      rplc("damp", 3) %>% rplc("moist", 3) %>%
      rplc("wet", 4) %>%
      rplc("submerged", 5) %>%
      as.numeric() %>%
      (function(i) if(length(i) == 2) (i[1]):(i[2]) else (i))
  } else out <- NA_integer_

  lapply(0:5, function(j) if(anyNA(out)) NA_integer_ else +(j %in% out)) %>%
    set_names(paste0("Moisture_", 0:5)) %>%
    as_tibble()
}

get_ph <- function(x)
{
  if(length(x) > 0)
  {
    out <- x %>%
      strsplit(" to ") %>%
      "[["(1) %>%
      rplc("acidic", 0) %>%
      rplc("moderately acidic", 1) %>%
      rplc("slightly acidic", 2) %>%
      rplc("neutral", 3) %>%
      rplc("slightly alkaline", 4) %>%
      rplc("limy", 5) %>%
      rplc("alkaline", 6) %>%
      as.numeric() %>%
      (function(i) if(length(i) == 2) (i[1]):(i[2]) else (i))
  } else out <- NA_integer_


  lapply(0:6, function(j) if(anyNA(out)) NA_integer_ else +(j %in% out)) %>%
    set_names(paste0("PH_", 0:6)) %>%
    as_tibble()
}

dat2 <- dat %>%
  separate(Height, c("Height2", "Height_units"), sep = " ", remove = FALSE) %>%
  separate(Width, c("Width2", "Width_units"), sep = " ", remove = FALSE) %>%
  filter(Zone != "NR") %>%
  mutate(
    Height_multiplier = if_else(Height_units == "in", 1/12, 1),
    Min_height = Height_multiplier*as.numeric(if_else(grepl("-", Height2), str_extract(Height2, "^\\d+(\\.5)?"), Height2)),
    Max_height = Height_multiplier*as.numeric(if_else(grepl("-", Height2), str_extract(Height2, "\\d+(\\.5)?$"), Height2)),
    Width_multiplier = if_else(Width_units == "in", 1/12, 1),
    Min_width = Width_multiplier*as.numeric(if_else(grepl("-", Width2), str_extract(Width2, "^\\d+(\\.5)?"), Width2)),
    Max_width = Width_multiplier*as.numeric(if_else(grepl("-", Width2), str_extract(Width2, "\\d+(\\.5)?$"), Width2)),

    MN = map(strsplit(Habitat, ", "), str_subset, pattern = "MN"),
    MN = map(MN, get_MN),
    Zone_tmp = map(strsplit(Zone, ", "), get_zone),
    Sun_tmp = map(Sun, get_sun),

    moisture_tmp = map(strsplit(Soil, ", "), str_subset, pattern = "moist|wet|dry|bog|submerged"),
    moisture_tmp = map(moisture_tmp, get_moisture),

    ph_tmp = map(strsplit(Soil, ", "), str_subset, pattern = "acid|alka|neutr|limy"),
    ph_tmp = map(ph_tmp, get_ph)
  ) %>%
  unnest(MN, Zone_tmp, Sun_tmp, moisture_tmp, ph_tmp) %>%
  select(-Height2, -Height_units, -Height_multiplier, -Width2, -Width_units, -Width_multiplier)
write.csv(dat2, "data/native_plants_qc_split.csv", row.names = FALSE, na = "")
