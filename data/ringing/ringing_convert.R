library(tidyverse)
library(readxl)

r <- read_xlsx("data/ringing/Woodie Trapping Data 20240831.xlsx")

o <- r %>%
  transmute(
    ring_numer = `Ring Number`,
    tag_id = "18LX",
    datetime = as.POSIXct(paste(format(`Sampling Date`, "%Y-%m-%d"), format(`Sampling Time`, "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S"),
    location_name = Locality,
    latitude = as.numeric(sapply(strsplit(`Co-ordinates`, ","), `[`, 1)),
    longitude = as.numeric(sapply(strsplit(`Co-ordinates`, ","), `[`, 2)),
    observation_type = "",
    device_status = "present",
    observer = Ringer,
    catching_method = `Trapping Method`,
    life_stage = Age,
    sex = Sex,
    condition = "alive",
    mass = Mass,
    wing_length = `Wing Length`,
    additional_metric = "",
    observation_comments = glue::glue("{`Additional Notes`} | color_ring: {`Colour Ring`}")
  )


write_csv(o, "data/observations_model.csv")
