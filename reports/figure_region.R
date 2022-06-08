library(GeoPressureR)
library(raster)
library(dplyr)
library(ggplot2)

gdl_list = c('22BS','22BT','24FD')
_


for (gdl in gdl_list){
  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))

  for (r in regions){
    # Find the extent of the map for the 90% percentile
    thr_prob_percentile <- .90
    probt <- as.data.frame(pressure_prob[[i_s]], xy = T)
    probt$layer[is.na(probt$layer)] <- 0
    probt$layer <- probt$layer / sum(probt$layer, na.rm = T)
    ls <- sort(probt$layer)
    id_prob_percentile <- sum(cumsum(ls) <= (1 - thr_prob_percentile))
    thr_prob <- ls[id_prob_percentile + 1]
    probtc <- subset(probt, layer >= thr_prob)
    extent_sm <- c(min(probtc$x), max(probtc$x), min(probtc$y), max(probtc$y))

    # request the pressure at the max resolution and take higher samples
    pressure_maps_sm <- geopressure_map(subset(pam$pressure, sta_id == i_sta),
                                        extent = c(extent_sm[4], extent_sm[1], extent_sm[3], extent_sm[2]),
                                        scale = 10,
                                        max_sample = 100,
                                        margin = 20
    )

    pressure_prob_sm <- geopressure_prob_map(pressure_maps_sm,
                                             s = gpr$prob_map_s,
                                             thr = gpr$prob_map_thr
    )
    # Take only the first value
    pressure_prob_sm <- pressure_prob_sm[[1]]

    # set prob=0 to NA
    tmp <- values(pressure_prob_sm)
    tmp[is.na(tmp)] <- 0
    tmp <- tmp / sum(tmp)
    tmps <- sort(tmp)
    id_prob_percentile <- sum(cumsum(tmps) <= (1 - thr_prob_percentile))
    thr_prob <- tmps[id_prob_percentile + 1]
    tmp[tmp < thr_prob] <- NA
    values(pressure_prob_sm) <- tmp

    pressure_prob_lg <- c(pressure_prob_lg, pressure_prob_sm)
  }
}



