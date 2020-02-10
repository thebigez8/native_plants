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
  unnest(c(MN, Zone_tmp, Sun_tmp, moisture_tmp, ph_tmp)) %>%
  select(-Height2, -Height_units, -Height_multiplier, -Width2, -Width_units, -Width_multiplier) %>%
  mutate(Type = factor(Type, levels = sort(unique(Type))))

snas <- "https://raw.githubusercontent.com/thebigez8/scientific_natural_areas/master/snas.json" %>%
  jsonlite::read_json() %>%
  (function(x) tibble(
    id = map_chr(x, "id"),
    name = map_chr(x, "name"),
    wildflower = map(x, c("species", "wildflower")),
    grass_sedge = map(x, c("species", "grass_sedge")),
    tree_shrub = map(x, c("species", "tree_shrub"))
  )) %>%
  pivot_longer(c(wildflower, grass_sedge, tree_shrub), names_to = "Type", values_to = "Species") %>%
  filter(lengths(Species) > 0) %>%
  unnest(c(Species)) %>%
  mutate(
    sname = map_chr(Species, "sname"),
    cname = tolower(map_chr(Species, "cname"))
  ) %>%
  select(-Species) %>%
  mutate_if(is.character, str_replace_all, pattern = "Â", replacement = "") %>%
  mutate(
    sname = case_when(
      name %in% c("Antelope Valley SNA", "Cedar Rock SNA", "Shooting Star Prairie SNA", "Lester Lake SNA") ~ cname,
      sname %in% c("Aromatic aster", "Bicknell's sedge", "Big bluestem", "Black ash", "Black spruce",
                   "Bladder sedge", "Blue cohosh", "Blue grama grass", "Bluebead lily", "Bluets",
                   "Yarrow", "Yellow birch", "Woolly sedge", "Woodbine") ~ cname,
      TRUE ~ sname
    ),
    sname = str_to_sentence(sname),
    sname = case_when(
      sname == "Aristida basimirea" ~ "Aristida basiramea",
      sname == "Apocynum canabinnum" ~ "Apocynum cannabinum",
      sname == "Asclepias syraca" ~ "Asclepias syriaca",
      sname == "Asclepias verticillatus" ~ "Asclepias verticillata",
      sname == "Astragalus candadensis" ~ "Astragalus canadensis",
      sname == "Berula erect" ~ "Berula erecta",
      sname %in% c("Amorpha canascens", "Amorpho canescens") ~ "Amorpha canescens",
      sname == "Apocynum androsaemifolia" ~ "Apocynum androsaemifolium",
      sname == "Betula allegheniensis" ~ "Betula alleghaniensis",
      sname == "Baptisa alba" ~ "Baptisia alba",
      sname == "Andropogon gerardi" ~ "Andropogon gerardii",
      sname == "Ambrosia artemesiifolia" ~ "Ambrosia artemisiifolia",
      sname == "Antennaria plantaginafolia" ~ "Antennaria plantaginifolia",
      sname == "Asclepias verticilatta" ~ "Asclepias verticillata",
      sname == "Cyclachaena xanthifolia" ~ "Cyclachaena xanthiifolia",
      sname == "Quecrus macrocarpa" ~ "Quercus macrocarpa",
      sname %in% c("Salix pedicillaris", "Salix pedicellari") ~ "Salix pedicellaris",
      sname == "Salix serisssima" ~ "Salix serissima",
      sname == "Silene stellate" ~ "Silene stellata",
      sname == "Ziza aurea" ~ "Zizia aurea",
      sname == "Veronicastrum virginiana" ~ "Veronicastrum virginicum",
      sname == "Vernonia faciculata" ~ "Vernonia fasciculata",
      sname == "Vaccinium oxycoccus" ~ "Vaccinium oxycoccos",
      sname == "Thalicrum dasycarpum" ~ "Thalictrum dasycarpum",
      sname %in% c("Taraxanum officinale", "Taraxacum officianale") ~ "Taraxacum officinale",
      sname == "Symphyotrichum sericeus" ~ "Symphyotrichum sericeum",
      sname == "Symphyotrichum ciliolatus" ~ "Symphyotrichum ciliolatum",
      sname == "Symphyotrichum laevis" ~ "Symphyotrichum laeve",
      sname == "Quercus macrocarpo" ~ "Quercus macrocarpa",
      sname == "Pycnanthemum virginiana" ~ "Pycnanthemum virginianum",
      sname == "Prenanthes racemose" ~ "Prenanthes racemosa",
      sname == "Populus deltoids" ~ "Populus deltoides",
      sname == "Physalis heterophyla var heterophyla" ~ "Physalis heterophylla var heterophylla",
      sname == "Penstemen albidis" ~ "Penstemon albidus",
      sname %in% c("Patinica sativa", "Pastinica sativa") ~ "Pastinaca sativa",

      sname %in% c("Melilotus officinale", "Melilotus offisinalis") ~ "Melilotus officinalis",
      sname == "Lysimachia terresrris" ~ "Lysimachia terrestris",
      sname == "Lotus corniculata" ~ "Lotus corniculatus",
      sname == "Liatris ligulisytlis" ~ "Liatris ligulistylis",

      TRUE ~ sname
    ),

    Genus = str_extract(sname, "^[A-z]+"),
    Species = case_when(
      grepl(" ", sname) ~ tolower(str_replace(sname, "[A-z]+\\s+((x )?[A-z-]+).*", "\\1")),
      TRUE ~ ""
    )
  )

snas2 <- snas %>%
  mutate(name = sub("\\s+SNA\\s*$", "", name)) %>%
  group_by(Genus, Species) %>%
  summarize(
    n = length(unique(name)),
    name = paste0(name, collapse = ", "),
    cname = paste0(unique(cname), collapse = ", "),
    sname = paste0(unique(sname), collapse = ", "),
  )

if(FALSE)
{
  dat2 %>%
    anti_join(snas2, by = c("Genus", "Species")) %>%
    View()
}
snas3 <- snas %>%
  mutate(name = sub("\\s+SNA\\s*$", "", name)) %>%
  select(Genus, Species, SNA = name) %>%
  group_by(Genus, Species) %>%
  nest(SNA = c(SNA)) %>%
  mutate(SNA = map(SNA, unlist, use.names = FALSE))

snas3 %>%
  mutate(SNA = map_chr(SNA, paste0, collapse = ", ")) %>%
  left_join(x = dat2, by = c("Genus", "Species")) %>%
  write.csv("data/native_plants_qc_split.csv", row.names = FALSE, na = "")

snas3 %>%
  mutate(SNA = map_chr(SNA, paste0, collapse = ", ")) %>%
  left_join(x = dat, by = c("Genus", "Species")) %>%
  write.csv("data/native_plants_qc.csv", row.names = FALSE, na = "")


###########################################

# I know I just undid all this above
dat2 %>%
  nest(
    Zone_list = c(starts_with("Zone_")),
    Sun_list = c(starts_with("Sun_")),
    MN_list = c(starts_with("MN_")),
    Moisture_list = c(starts_with("Moisture_")),
    PH_list = c(starts_with("PH_")),
    Feature_list = c(Prairie:Front_yard_woody)
  ) %>%
  left_join(snas3, by = c("Genus", "Species")) %>%
  mutate(id = paste(Genus, Species, sep = "-")) %>%
  select(id, everything()) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character, ~ if_else(is.na(.x), '""', paste0('"', .x, '"'))) %>%
  mutate(SNA = map_chr(SNA, ~ paste0("[", paste0('"', .x, '"', collapse = ", "), "]"))) %>%
  mutate_if(is.list, map_chr, function(L) {
    L <- unlist(L)
    L[is.na(L)] <- 2
    n <- if(all(grepl("_\\d+", names(L)))) as.numeric(str_extract(names(L), "\\d+$")) else names(L)
    out <- n[L == 1]
    if(is.character(out) && length(out)) out <- paste0('"', out, '"')
    paste0("[", paste0(out, collapse = ", "), "]")
  }) %>%
  (function(x) {
    lapply(seq_len(nrow(x)), function(i) {
      a <- unlist(x[i, ])
      out <- paste0('    "', names(a), '": ', if_else(is.na(a), "null", a), collapse = ',\n')
      paste0("  {\n", out, "\n  }")
    })
  }) %>%
  paste0(collapse = ",\n") %>%
  paste0("[\n", ., "\n]\n") %>%
  cat(file = "html/native_plants.json")







###########################################

dat.hide <- dat2 %>%
  select(Genus, Species) %>%
  unite(id, Genus, Species, sep = "-") %>%
  pull(id) %>%
  paste0('<tr id="', ., '">\n')

dat.boxes <- dat2 %>%
  "["(c("Genus", "Species", names(features))) %>%
  gather("N", "Y", -Genus, -Species) %>%
  filter(Y == 1) %>%
  select(-Y) %>%
  mutate(
    N = gsub("_", " ", tolower(N)),
    N = case_when(
      N == "deer resistant" ~ "deer resistance",
      N == "butterfly larval" ~ "butterfly larvae",
      N == "pond" ~ "ponds",
      N %in% c("mixed border", "shade garden") ~ paste0(N, "s"),
      TRUE ~ str_replace(N, " (shady|sunny|woody)", "s (\\1)")
    ),
    N = case_when(
      N %in% c("coniferous forest", "deciduous forest", "prairie") ~ paste("native to", N),
      TRUE ~ paste("good for", N)
    )
  ) %>%
  group_by(Genus, Species) %>%
  summarize(`Notable Features` = paste0(N, collapse = ", ")) %>%
  ungroup()

dat.show <- dat2 %>%
  select_if(~ is.character(.x) || is.factor(.x)) %>%
  (function(x) {if(anyNA(select(x, Genus, Species, Zone, Type))) stop('NAs'); x}) %>%
  mutate(Type = as.character(Type)) %>%
  left_join(dat.boxes, by = c("Genus", "Species")) %>%
  mutate_all(~ replace(.x, is.na(.x), "-")) %>%
  unite("Scientific Name", Genus, Species, sep = " ") %>%
  mutate(`Scientific Name` = paste0("<i>", `Scientific Name`, "</i>")) %>%
  mutate_all(~ paste0("  <td>", .x, "</td>\n")) %>%
  as.list() %>%
  do.call(what = paste0) %>%
  paste0(dat.hide, ., "</tr>", collapse = "\n")

nms <- dat2 %>%
  select_if(~ is.character(.x) || is.factor(.x)) %>%
  names() %>%
  (function(x) c("Scientific Name", "Common Name", x[-(1:3)], "Notable Features")) %>%
  paste0("  <th>", ., "</th>", collapse = "\n") %>%
  paste0("<tr>\n", ., "\n</tr>")

cat(
  '<!DOCTYPE html>',
  '<html lang="en">',
  '<head>',
  '  <title>MN Native Plants</title>',
  '</head>',
  '<body>',
  '<table>',
  '<thead>',
  nms,
  '</thead>',
  '<tbody>',
  dat.show,
  '</tbody>',
  '</table>',
  '</body>',
  '</html>',
  sep = "\n", file = "html/native_plants.html"
)

