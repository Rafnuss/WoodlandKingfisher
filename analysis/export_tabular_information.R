# Extract summury result of the birds trajectory
# - pml: path most likely: information at the stationary period information
# - eml: edge most likely: imformation at the level of each flight
# - journey: information at the level of pre-breeding, post-breeding, non-breeding, breeding level.


library(tidyverse)
library(GeoPressureR)

# Define tag id that you want to compute
list_id <- tail(names(yaml::yaml.load_file("config.yml", eval.expr = FALSE)), -1)
# list_id <- c("16LN", "16LO", "16LP", "20IK", "22NO")

# Define Congo limit
lat_min <- -3
lat_max <- 3

EMl <- list()
PMl <- list()
JOURNEy <- list()

for (i in seq(1, length(list_id))) {
  # load data
  load(paste0("data/interim/", list_id[i], ".Rdata"))

  # Take the most likely path information and add more info
  pml <- path_most_likely
  pml$duration <- stap2duration(pml, return_numeric = F)

  istap_winter <- which(pml$duration > 50)[2]
  pml$status <- ""
  pml$status[seq(2, istap_winter - 1)] <- "post-breeding"
  pml$status[seq(istap_winter, nrow(pml))] <- "pre-breeding"
  pml$status[istap_winter] <- "non-breeding"
  pml$status[1] <- "breeding"
  pml$status[nrow(pml)] <- "breeding"

  # Define stap over the congo
  pml$congo <- pml$lat > lat_min & pml$lat < lat_max

  # Keep id on the dataset
  pml$id <- list_id[i]

  # flight data from edges
  eml <- edge_most_likely
  eml$id <- list_id[i]

  # Add status + congo info
  eml$status <- ""
  eml$status[eml$stap_s < istap_winter] <- "post-breeding"
  eml$status[eml$stap_t > istap_winter] <- "pre-breeding"

  # Add Congo information
  eml$congo <- pml$congo[eml$stap_s] + pml$congo[eml$stap_t]

  # Compute wind support
  eml$ws_support <- windsupport(eml$ws, eml$gs)
  eml$aws <- abs(eml$ws)
  eml$ags <- abs(eml$gs)
  eml$aas <- abs(eml$gs - eml$ws)

  journey <- pml %>%
    group_by(status) %>%
    summarise(
      start = first(start),
      end = last(end),
      duration = last(end) - first(start),
      stap_s = min(stap_id) - 1,
      stap_t = max(stap_id) + 1,
      n = n()
    ) %>%
    merge(
      pml %>%
        filter(congo) %>%
        group_by(status) %>%
        summarise(
          congo_duration = max(end) - min(start),
          congo_n = n()
        ),
      by = "status"
    ) %>%
    merge(
      eml %>%
        group_by(status) %>%
        summarise(
          flight_distance = sum(distance),
          flight_duration = sum(duration),
          flight_number = sum(n)
        ),
      by = "status"
    )

  # Add elevation to pressurepath
  elevation <- path2elevation(pml, scale = tag$param$scale)
  pressurepath$elevation <- approx(elevation$stap_id, elevation$X50, pressurepath$stap_id)$y

  # Altitude
  eml <- eml %>%
    merge(
      pressurepath %>%
        filter(stap_id != round(stap_id)) %>%
        mutate(stap_s = floor(stap_id)) %>%
        group_by(stap_s) %>%
        summarise(
          mean_alt_sl = mean(altitude),
          min_alt_sl = min(altitude),
          max_alt_sl = max(altitude),
          std_alt_sl = sd(altitude),
          mean_alt_gl = mean(altitude - elevation),
          min_alt_gl = min(altitude - elevation),
          max_alt_gl = max(altitude - elevation),
          std_alt_gl = sd(altitude - elevation),
        )
    )



  eml$tag_id <- tag$param$id
  pml$tag_id <- tag$param$id
  journey$tag_id <- tag$param$id

  EMl[[i]] <- eml
  PMl[[i]] <- pml
  JOURNEy[[i]] <- journey
}


EML <- do.call(rbind, EMl)
PML <- do.call(rbind, PMl)
JOURNEY <- do.call(rbind, JOURNEy)

write.csv(EML, file = "output/summary_flight.csv")
write.csv(PML, file = "output/summary_stap.csv")
write.csv(JOURNEY, file = "output/summary_journey.csv")
