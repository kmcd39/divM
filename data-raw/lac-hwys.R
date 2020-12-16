library(dplyr)
# defines FCLASS codes that delimit
# NHPN hwy data
# https://catalog.data.gov/dataset/national-highway-planning-network-nhpn
# FGDC Metadata


# includes "urban principal arterials" designated interstates or "other freeways and
# expressways" (FCLASS 11 & 12) and all rural principal arterials (FCLASS 1 & 2)
lac_codes <- c(1, 2, 11, 12)

# Some Interstates are classified outside of these designations, so whole filter
# command to include these is:

lac <- hwys %>% filter(FCLASS %in% lac_codes  |
                         SIGNT1 == "I") # all limited-access
nrow(hwys)
nrow(lac)




# write code list --------------------------------------------------------------
usethis::use_data(lac_codes,
                  overwrite = T)


# To see these hwys that are Interstates but not included in the LAC/major arterial
# classifications:
'
library(sf)
tmp <- hwys %>%
  filter(!FCLASS %in% lac_codes &
           SIGNT1 == "I")

tmp %>%
  mapview::mapview(zcol = "FCLASS")

tmp %>%
  filter(SIGN1 == "I86") %>%
  mapview::mapview(zcol = "FCLASS")\
'
