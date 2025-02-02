library(GeoPressureR)
library(terra)
library(glue)

library(tidyverse)
library(lubridate)

library(ggplot2)
library(ggmap)
library(ggthemes)
library(ggnewscale)
library(cowplot)
library(colorspace)
library(gganimate)
library(move2)
library(ggspatial)
library(sf)
library(tidyterra)

# devtools::install_github("16EAGLE/moveVis")
library(moveVis)

list_id <- tail(names(yaml::yaml.load_file("config.yml", eval.expr = FALSE)), -1)


# Trajectory ----

p <- list()
for (i in seq_len(length(list_id))) {
  load(glue::glue("./data/interim/{list_id[i]}.RData"))
  path_most_likely$tag_id <- tag$param$id
  istap_winter <- which(stap2duration(path_most_likely) > 50)[2]
  path_most_likely$status <- "post-breeding"
  path_most_likely$status[path_most_likely$stap_id > istap_winter] <- "pre-breeding"
  path_most_likely$status[path_most_likely$stap_id == istap_winter] <- "wintering"
  path_most_likely$duration <- stap2duration(path_most_likely)
  p[[i]] <- path_most_likely
}
# p2 = do.call(rbind, p)

p0 <- ggplot() +
  annotation_map_tile("https://services.arcgisonline.com/arcgis/rest/services/World_Physical_Map/MapServer/tile/${z}/${y}/${x}.jpeg", zoomin = 0)
# +  annotation_map_tile('https://tiles.stadiamaps.com/tiles/stamen_toner_lines/${z}/${y}/${x}${r}.png', zoomin = -1)

pl <- p0
for (pi in p) {
  pl <- pl +
    geom_sf(data = pi[c("lon", "lat")] %>%
      as.matrix() %>%
      st_linestring() %>%
      st_sfc(crs = 4326), color = "black") +
    geom_point(data = pi %>% filter(status == "wintering"), aes(x = lon, y = lat))
}
pl <- pl +
  geom_point(data = pi %>% filter(stap_id == 1), aes(x = lon, y = lat))

pf <- pl +
  annotation_scale() +
  coord_sf(
    xlim = config::get("extent")[c(1, 2)],
    ylim = config::get("extent")[c(3, 4)], crs = 4326
  ) +
  coord_sf(
    xlim = c(-17, 49),
    ylim = c(-33, 35), crs = 4326
  )

ggsave(plot = pf, "output/figure_print/trajectory_full.eps", device = "eps", width = 6, height = 6)
ggsave(plot = pf, "output/figure_print/trajectory_full.png", width = 6, height = 6)


for (pi in p) {
  pp <- p0 +
    geom_sf(data = pi %>%
      filter(status != "pre-breeding") %>%
      select(c("lon", "lat")) %>%
      as.matrix() %>%
      st_linestring() %>%
      st_sfc(crs = 4326), color = "#32CD32")

  if ("pre-breeding" %in% pi$status) {
    pp <- pp +
      geom_sf(data = pi %>%
        filter(status != "post-breeding") %>%
        select(c("lon", "lat")) %>%
        as.matrix() %>%
        st_linestring() %>%
        st_sfc(crs = 4326), color = "#D2691E")
  }

  pp <- pp +
    geom_point(data = pi, aes(x = lon, y = lat, size = duration^(0.5) * 6)) +
    coord_sf(
      xlim = c(20, 34),
      ylim = c(-27, 13), crs = 4326
    )
  ggsave(plot = pp, glue("output/figure_print/trajectory_{pi$tag_id[1]}.eps"), device = "eps", width = 6, height = 6)
}

ggsave(plot = p0 +
  coord_sf(
    xlim = c(20, 34),
    ylim = c(-27, 13), crs = 4326
  ), "output/figure_print/trajectory_full0.png", width = 6, height = 6)








# Figure non-breeding site ----

p0 <- get_googlemap(center = c(lon = 27.5, lat = 8.75), zoom = 7, maptype = "hybrid", size = c(1280, 1280), scale = 4, default_crs = NULL) %>%
  ggmap() + theme_map()

# p0 + coord_fixed(
#   xlim=c(20, 37),
#   ylim=c(4, 12),
#   ratio=1/cos(pi*41.39/180),
#   expand = F
# )+ scalebar(x.min= 20, x.max = 37, y.min=4, y.max=12, transform=T,
#            location = "topright", dist = 100, height = 0.1, model = "WGS84", dist_unit = "km")

p <- p0
for (i in seq(1, length(list_id))) {
  load(paste0("data/interim/", list_id[i], ".Rdata"))

  istap_winter <- which(stap2duration(tag$stap) > 50)[2]

  # tag2map(tag)
  df <- rast.map(marginal)[[istap_winter]] %>%
    disagg(5, method = "bilinear") %>%
    as.data.frame(xy = TRUE) %>%
    rename(layer = 3) %>%
    filter(!is.na(layer)) %>%
    arrange(desc(layer)) %>%
    mutate(layerP = 1 - cumsum(layer) / sum(layer))

  p <- p + new_scale_colour() +
    geom_contour(data = df, aes(x = x, y = y, z = layerP, color = after_stat(level)), linewidth = 2, breaks = c(.1)) +
    scale_colour_gradient(
      high = config::get("color", list_id[i]),
      low = "white",
      limits = c(0, .1),
      guide = "none"
    )
}
print(p)

plot_inset <- ggplot() +
  borders("world", colour = "gray90", fill = "gray50", linewidth = 0.1) +
  coord_quickmap(
    # xlim=c(gpr$extent_W, gpr$extent_E),
    # ylim=c(gpr$extent_S, gpr$extent_N),
    xlim = c(-18, 51),
    ylim = c(-35, 37),
    expand = F
  ) +
  geom_rect(
    aes(
      xmin = layer_scales(p0)$x$range$range[1],
      xmax = layer_scales(p0)$x$range$range[2],
      ymin = layer_scales(p0)$y$range$range[1],
      ymax = layer_scales(p0)$y$range$range[2]
    ),
    color = "red", alpha = 0.1, linewidth = 1
  ) +
  theme_map() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "#FFFFFF")
  )

pf <- ggdraw() +
  draw_plot(p) +
  draw_plot(plot_inset,
    x = .15,
    y = .06,
    width = 0.35, height = 0.35
  )

ggsave(plot = pf, "output/figure_print/wintering_location.png")
ggsave(plot = pf, "output/figure_print/wintering_location.eps", device = "eps")









# Cumulative flight duration ----
d <- list()

d <- lapply(list_id, function(id) {
  load(paste0("data/interim/", id, ".Rdata"))

  fl <- stap2flight(tag$stap)

  if (id == "20IK") {
    fl$start[1] <- fl$start[2] - 22 * 60 * 60 * 24
  }

  fl$duration[month(fl$end) > 8] <- -fl$duration[month(fl$end) > 8]

  data.frame(
    x = c(fl$start, tail(fl$end, 1)),
    y = c(0, cumsum(fl$duration)),
    id = id,
    color = darken(config::get("color", id), 0.2)
  )
})

d2 <- do.call("rbind", d)

l <- d2 %>%
  select(color, id) %>%
  unique()

d2 %>%
  mutate(
    x = `year<-`(x, 2000),
  ) %>%
  ggplot(aes(x = x, y = y, color = color, group = id)) +
  geom_step(size = 1) +
  scale_color_identity(
    name = "Track",
    labels = l$id,
    breaks = l$color,
    guide = "legend"
  ) +
  scale_x_datetime(date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  scale_y_continuous(breaks = seq(0, 210, by = 20)) +
  coord_cartesian(
    xlim = as.POSIXct(c("2000-03-15 UTC", "2000-12-1 UTC"))
  ) +
  ylab("Cumulative Hours of flight") +
  xlab("Date") +
  theme_bw() +
  theme(
    legend.position = c(.5, .1),
    legend.box.background = element_rect(colour = "black"),
    legend.direction = "horizontal"
  )

ggsave("output/figure_print/cumulative_flight.png", width = 8, height = 4)
ggsave("output/figure_print/cumulative_flight.eps", device = "eps", width = 8, height = 4)





# NDVI ----

ndvi <- readr::read_csv("data/NDVI/ndvi.csv", show_col_types = FALSE) %>%
  mutate(date = as.POSIXct(paste("2000-", doy), format = "%Y-%j"))
d <- lapply(list_id, function(id) {
  load(paste0("data/interim/", id, ".Rdata"))
  col <- config::get("color", id)
  rbind(
    path_most_likely,
    tail(path_most_likely, 1) %>% mutate(start = end)
  ) %>%
    mutate(
      id = id,
      color = colorspace::darken(col, 0.2),
      start = `year<-`(start, 2000),
    )
})
d2 <- do.call("rbind", d)

ggplot(ndvi, aes(date, lat, fill = ndvi)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "darkgreen", limits = c(0, 10000)
  ) +
  geom_step(data = d2, aes(x = start, y = lat, group = id, fill = 1), color = "black", linewidth = 1) +
  scale_x_datetime(date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  coord_cartesian(
    xlim = as.POSIXct(c("2000-01-01 UTC", "2000-12-31 UTC")),
    ylim = c(min(ndvi$lat), max(ndvi$lat))
  ) +
  ylab("Latitude") +
  xlab("Date") +
  theme_bw()


ggsave("output/figure_print/NDVI_lat.eps", device = "eps", width = 8, height = 4)

















# Gif of uncertainty ----

p0 <- get_googlemap(
  center = c(
    lon = mean(config::get("extent")[c(1, 2)]),
    lat = mean(config::get("extent")[c(3, 4)])
  ),
  zoom = 4, maptype = "satellite"
) %>% ggmap() +
  borders("world", colour = "gray90", size = 0.1) +
  coord_quickmap(
    xlim = config::get("extent")[c(1, 2)],
    ylim = config::get("extent")[c(3, 4)],
    expand = F
  ) +
  theme_map() +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

for (i in seq(3, length(list_id))) {
  id <- list_id[i]

  load(paste0("data/interim/", id, ".Rdata"))

  dfr <- marginal %>%
    rast() %>%
    as.data.frame(xy = TRUE) %>%
    pivot_longer(cols = -c(x, y), values_to = "layer", names_to = "stap_id") %>%
    group_by(stap_id) %>%
    mutate(
      stap_id = as.numeric(str_remove(stap_id, "#")),
      layer = layer / max(layer)
    )

  dfrf <- dfr %>%
    filter(stap_id %in% which(stap2duration(tag$stap) > 2))

  p <- p0 +
    geom_tile(data = dfrf, aes(x = x, y = y, fill = layer, group = seq_along(stap_id))) +
    scale_fill_gradient(
      high = config::get("color", list_id[i]),
      low = paste0(config::get("color", list_id[i]), "00"),
      limits = c(0, 1),
      guide = "none"
    ) +
    # + geom_rect(data=dfr, aes(xmin = gpr$extent_W, xmax = mean(gpr$extent_W+rect_norm_start), ymin = gpr$extent_S, ymax = gpr$extent_S+.1))
    # geom_text(data = dfr, aes(gpr$extent_W, gpr$extent_N, label=start),nudge_y=-1, nudge_x=.1, size = 8, hjust = 0, color = config::get("color", list_id[i])) +
    transition_states(stap_id) +
    enter_fade() +
    exit_shrink() +
    ggtitle("Stationary period {closest_state}")

  gganimate::animate(p, height = 800, width = 600)
  anim_save(paste0("output/figure_print/marginal_animation_", id, ".gif"))
}









# moveVis 1: sattelite view ----

d <- lapply(list_id, function(id) {
  load(paste0("data/interim/", id, ".Rdata"))

  df <- path_most_likely %>%
    mutate(
      color = config::get("color", id),
      track_id = id
    )

  if (id == "20IK") {
    df$start[1] <- as.POSIXct("2018-01-01 00:00:00", tz = "UTC")
  }

  year(df$end) <- year(df$end) - year(df$start[1])
  year(df$start) <- year(df$start) - year(df$start[1])

  df$start[1] <- as.POSIXct("0000-01-01 00:00:00", tz = "UTC")
  df$start[1] <- as.POSIXct("0000-01-01 00:00:00", tz = "UTC")

  if (id == "22NO") { # track finished earlier
    df$end[nrow(df)] <- as.POSIXct("0000-09-28 00:00:00", tz = "UTC")
  } else {
    df$end[nrow(df)] <- "0000-12-29 00:00 UTC"
  }

  rbind(
    df %>% mutate(time = start),
    df %>% mutate(time = end)
  )
})
d <- do.call("rbind", d)


d2 <- d %>%
  # filter(track_id=="16LP") %>%
  df2move("WGS84", x = "lon", y = "lat", time = "time", track_id = "track_id") %>%
  align_move(res = 24, unit = "hours") # Use midday position rather than midnight (while the bird could be flying)
# subset_move(from = "0000-01-1", to = "0000-12-30") # remove the long equipement and retrival period duration

# view_spatial(d2)


frames <- d2 %>%
  frames_spatial(
    # equidistant = T,
    ext = st_bbox(c(
      xmin = tag$param$extent[1],
      xmax = tag$param$extent[2],
      ymax = tag$param$extent[4],
      ymin = tag$param$extent[3]
    ), crs = "WGS84"),
    path_colours = unique(d$color), trace_colour = unique(d$color),
    path_legend = F,
    map_service = "mapbox", map_type = "satellite", map_token = "pk.eyJ1IjoicmFmbnVzcyIsImEiOiIzMVE1dnc0In0.3FNMKIlQ_afYktqki-6m0g"
  )


frames[[100]]

frames_cus <- frames %>%
  add_labels(x = NULL, y = NULL) %>% # add some customizations, such as axis labels
  # add_timestamps(type = "label", size = 6) %>%
  add_text(
    labels = format(get_frametimes(frames), "%d-%b"),
    x = frames$aesthetics$gg.ext[1] + ((frames$aesthetics$gg.ext[3] - frames$aesthetics$gg.ext[1]) / 2),
    y = frames$aesthetics$gg.ext[4] - ((frames$aesthetics$gg.ext[4] - frames$aesthetics$gg.ext[2]) * 0.05),
    type = "label", size = 6
  ) %>%
  add_progress()


frames_cus[[360]]

animate_frames(frames_cus, out_file = paste0("output/figure_print/movevis3.gif"), overwrite = T)


# moveVis 1: NDVI changing view ----

fl_terra <- terra::rast("data/NDVI/ndvi.tif")
fl_terra_proj <- project(fl_terra, "WGS84")

colors <- c(
  "#FFFFFF", "#CE7E45", "#DF923D", "#F1B555", "#FCD163", "#99B718", "#74A901",
  "#66A000", "#529400", "#3E8601", "#207401", "#056201", "#004C00", "#023B01",
  "#012E01", "#011D01", "#011301"
)
from <- seq(1, 90)
to <- t(col2rgb(colorRampPalette(colors)(length(from))))

r_list <- list()
for (i in seq_len(nlyr(fl_terra_proj))) {
  r_list[[i]] <- subst(max(round(fl_terra_proj[[i]] / 100), 0), from, to, names = c("red", "green", "blue"))
}

plotRGB(r_list[[i]])

r_times <- as.POSIXct(gsub("_NDVI_median", "", names(fl_terra)),
  format = "%Y_%m_%d", tz = "UTC"
)
year(r_times) <- 0000


frames <- d2 %>%
  frames_spatial(
    r_list = r_list,
    r_times = r_times,
    # fade_raster = TRUE, not working!
    # ext = st_bbox(c(xmin = 15,xmax = 38,ymax = 13,ymin = -24), crs = "WGS84"),
    ext = st_bbox(c(xmin = -20, xmax = 52, ymax = 40, ymin = -37), crs = "WGS84"),
    path_colours = unique(d$color), trace_colour = unique(d$color),
    path_legend = F
  )

frames[[100]]
frames[[200]]

frames_cus <- frames %>%
  add_labels(x = NULL, y = NULL) %>% # add some customizations, such as axis labels
  # add_timestamps(type = "label", size = 6) %>%
  add_text(
    labels = format(get_frametimes(frames), "%d-%b"),
    x = frames$aesthetics$gg.ext[1] + ((frames$aesthetics$gg.ext[3] - frames$aesthetics$gg.ext[1]) / 2),
    y = frames$aesthetics$gg.ext[4] - ((frames$aesthetics$gg.ext[4] - frames$aesthetics$gg.ext[2]) * 0.05),
    type = "label", size = 6
  ) %>%
  add_progress()

frames_cus[[360]]

animate_frames(frames_cus, out_file = paste0("output/figure_print/movevis_ndvi2.gif"), overwrite = T, fps = 50)



# Elevation during flight ----


d <- lapply(list_id, function(id) {
  load(paste0("data/interim/", id, ".Rdata"))

  elevation <- path2elevation(path_most_likely, scale = tag$param$scale, sampling_scale = tag$param$scale)

  # Compute distance along the path for pressurepath (to be able to plot it with elevation)
  lonlat <- data.frame(
    lon = pressurepath$lon,
    lat = pressurepath$lat
  )
  distance <- geosphere::distHaversine(tail(lonlat, -1), head(lonlat, -1))
  pressurepath$distance <- c(0, cumsum(distance))

  # Get also a point per stap_id
  # exclude flight
  pp <- pressurepath[pressurepath$stap_id == round(pressurepath$stap_id), ]
  # compute average flight and distance
  pp_stap <- merge(
    tag$stap,
    data.frame(
      stap_id = sapply(split(pp$stap_id, pp$stap_id), median),
      altitude = sapply(split(pp$altitude, pp$stap_id), \(x) round(mean(x), 1)),
      distance = sapply(split(pp$distance, pp$stap_id), \(x) round(mean(x), 1))
    )
  )
  pp_stap$duration <- stap2duration(pp_stap)


  p <- ggplot() +
    geom_line(data = elevation, aes(x = distance, y = X50, color = "ground")) +
    geom_line(data = pressurepath, aes(x = distance / 1000, y = altitude, color = "flight")) +
    geom_point(data = pp_stap, aes(x = distance / 1000, y = altitude, color = "stap", name = stap_id, size = duration^(0.25) * 6)) +
    geom_point(data = pressurepath %>% filter(lat > -3 & lat < 3), aes(x = distance / 1000, y = 0, color = "rainforest")) +
    theme_bw() +
    ylab("altitude/elevation (m a.s.l.)") +
    xlab("Distance along trajectory (km)") +
    scale_color_manual(
      values = c(ground = "brown", flight = "black", stap = "blue", rainforest = "green"),
      labels = c(ground = "Ground elevation (median over 0.25°)", flight = "Bird flight altitude", stap = "Stationary period")
    ) +
    # scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
    guides(size = FALSE)

  # Interactive plot
  plotly::layout(plotly::ggplotly(p), legend = list(orientation = "h"))

  ggsave(glue::glue("output/figure_print/elevation_{id}.png"), device = "png", width = 12, height = 4)
})









# Expected habitat use ----
p0 <- get_googlemap(
  center = c(
    lon = mean(config::get("extent")[c(1, 2)]),
    lat = mean(config::get("extent")[c(3, 4)])
  ),
  zoom = 4, maptype = "satellite"
) %>%
  ggmap(darken = c(0.4, "black")) +
  borders("world", colour = "gray40", size = 0.2) +
  coord_quickmap(
    xlim = config::get("extent")[c(1, 2)],
    ylim = config::get("extent")[c(3, 4)],
    expand = F
  ) +
  theme_map() +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

for (i in seq(1, length(list_id))) {
  load(paste0("data/interim/", list_id[i], ".Rdata"))

  tag$stap$duration <- stap2duration(tag$stap)
  istap_winter <- which(stap2duration(tag$stap) > 50)[2]

  marginal_duration <- mapply(function(x, duration) {
    x * duration
  }, marginal$data, tag$stap$duration, SIMPLIFY = FALSE)

  s <- c("pre-breeding", "post-breeding")
  for (i_s in c(1, 2)) {
    if (i_s == 1) {
      s_in <- seq(2, istap_winter - 1)
      s_out <- seq(1, istap_winter)
    } else {
      if (istap_winter == nrow(tag$stap)) next
      s_in <- seq(istap_winter + 1, nrow(tag$stap) - 1)
      s_out <- seq(istap_winter, nrow(tag$stap))
    }

    d <- Reduce("+", marginal_duration[tag$stap$stap_id %in% s_in])
    d[d == 0] <- NA
    r <- terra::rast(d, extent = marginal$extent, crs = "epsg:4326")

    p <- p0 +
      geom_path(data = path_simulation %>% filter(stap_id %in% s_out & j < 10) %>% arrange(stap_id), aes(x = lon, y = lat, group = j), alpha = .5, color = "white") +
      geom_path(data = path_most_likely %>% filter(stap_id %in% s_out) %>% arrange(stap_id), aes(x = lon, y = lat, group = j), alpha = .7, color = "white", linewidth = 2, lineend = "round") +
      # geom_tile(data = res, aes(x = x, y = y, fill = (layer))) +
      geom_spatraster(data = r) +
      scale_fill_gradient(
        high = config::get("color", list_id[i]),
        low = paste0(config::get("color", list_id[i]), "00"),
        na.value = NA,
        # limits=c(0,.1),
        guide = "none"
      ) +
      coord_sf(
        xlim = config::get("extent")[c(1, 2)],
        ylim = config::get("extent")[c(3, 4)]
      )

    ggsave(paste0("output/figure_print/expected_use/", s[i_s], "_", list_id[i], ".png"), plot = p, width = 7.8, height = 11.5)
  }
}

do.call("grid.arrange", c(p[[1]], nrow = 1))
do.call("grid.arrange", c(p[[2]], nrow = 1))

d2 <- do.call("rbind", d)

d2 %>%
  ggplot(aes(x = lat, y = dens, color = id, group = paste0(id, "_", season))) +
  geom_line()


landcover <- raster("data/land_cover/land_cover.tif")
restt <- Reduce("+", rest)
plot(landcover)
plot(restt)



























### Flight timing ----
library(tidyverse)
library(lubridate)
library(GeoPressureR)
library(suntools)

# read flights
flights <- read_csv(file = "output/summary_flight.csv") %>%
  mutate(
    start = as.POSIXct(start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    end = as.POSIXct(end, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  )

# Load all pressurepath, compute sunrise and sunset per day and combine in a single data.frame for all tracks
list_id <- tail(names(yaml::yaml.load_file("config.yml", eval.expr = FALSE)), -1)
pp <- purrr::map(list_id, \(id){
  load(glue::glue("./data/interim/{id}.RData"))

  pp <- pressurepath_most_likely %>%
    mutate(
      date = round_date(date, "day")
    ) %>%
    group_by(date) %>%
    reframe(
      sunrise = median(sunrise),
      sunset = median(sunset)
    ) %>%
    mutate(
      tag_id = id
    )

  # filter for flight
  fpost <- flights %>% filter((tag_id == tag$param$id) & (status == "post-breeding"))
  fpre <- flights %>% filter((tag_id == tag$param$id) & (status == "pre-breeding"))

  pp$status <- ""
  pp$status[(pp$date > (min(fpost$start) - days(5))) & (pp$date < (max(fpost$end) + days(5)))] <- "post-breeding"
  pp$status[(pp$date > (min(fpre$start) - days(5))) & (pp$date < (max(fpre$end) + days(5)))] <- "pre-breeding"
  # pp <- pp %>% filter(status != "")
  pp
}) %>%
  bind_rows() %>%
  mutate(
    sunrise = as.POSIXct(sunrise, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    sunset = as.POSIXct(sunset, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    sunrise_decimal = hour(sunrise) + minute(sunrise) / 60 + second(sunrise) / 60 / 60,
    sunset_decimal = hour(sunset) + minute(sunset) / 60 + second(sunset) / 60 / 60
  )

ppy <- pp %>%
  mutate(
    date = `year<-`(date, 2000),
  )

tmp <- flights %>%
  mutate(
    middle_date = round_date(start + difftime(end, start), "day"),
    start_hms = hms(format(start, "%H:%M:%S")),
    end_hms = hms(format(end, "%H:%M:%S")),
    start_decimal = hour(start) + minute(start) / 60 + second(start) / 60 / 60,
    end_decimal = hour(end) + minute(end) / 60 + second(end) / 60 / 60
  ) %>%
  mutate(
    middle_date = `year<-`(middle_date, 2000),
  )

# function to convert the x axis to have night center
t_h <- \(x) (x + 12 + 2) %% 24



ggplot() +
  geom_ribbon(data = ppy, aes(x = date, ymin = t_h(sunset_decimal), ymax = t_h(sunrise_decimal)), colour = "grey", alpha = 0.1) +
  geom_segment(data = tmp %>% filter(congo > 0), aes(x = middle_date, y = t_h(start_decimal), yend = t_h(end_decimal)), colour = "darkgreen") +
  geom_segment(data = tmp %>% filter(congo == 0), aes(x = middle_date, y = t_h(start_decimal), yend = t_h(end_decimal))) +
  geom_path(data = ppy, aes(x = date, y = t_h(sunrise_decimal))) +
  geom_path(data = ppy, aes(x = date, y = t_h(sunset_decimal))) +
  scale_y_continuous(
    breaks = seq(0, 24, 2),
    minor_breaks = seq(0, 24, 1),
    labels = c("12:00", "14:00", "16:00", "18:00", "20:00", "22:00", "00:00", "02:00", "04:00", "06:00", "08:00", "10:00", "12:00"),
    limits = c(t_h(15), t_h(5)),
    expand = c(0, 0)
  ) +
  scale_x_datetime(
    date_breaks = "1 month",
    date_minor_breaks = "1 day",
    date_labels = "%b",
    limits = as.POSIXct(c("2000-3-1", "2000-12-15")),
    expand = c(0, 0)
  ) +
  labs(
    title = "Flight timing",
    y = "Time of day (UTC+2)",
    x = "Day of year"
  ) +
  theme_minimal() +
  theme(
    # panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.3)
  ) +
  facet_wrap(~tag_id, scales = "free_y", ncol = 1, strip.position = "right")


ggsave("output/figure_print/flight_timing.eps", device = "eps", width = 8, height = 12)





flights$dusk <- crepuscule(
  matrix(c(flights$lon_s, flights$lat_s), ncol = 2),
  dateTime = round_date(flights$start, "day")-days(1),
  solarDep = 6,
  direction = "dusk",
  POSIXct.out = TRUE
)$time

flights <- flights %>% mutate(
  since_dusk = as.numeric(flights$start - flights$dusk,units="mins"),
  since_dusk_cat = cut(since_dusk, breaks = seq(-15, 700, by = 15), include.lowest = TRUE, right = FALSE),
  duration_category = cut(duration, breaks = c(0, 1, 3, 5, 10, 12), include.lowest = TRUE, right = FALSE)
)

flight_first <- flights %>%
  group_by(tag_id, status) %>%
  arrange(stap_s) %>%
  slice(1) %>%
  group_by(since_dusk_cat) %>%
  summarise(
    n = n(),
    duration_category = first(duration_category)
  )

flights %>%
  ggplot(aes(x = since_dusk_cat, fill= duration_category)) +
  geom_bar() +
  geom_point(data = flight_first, aes(y=n)) +
  labs(
    title = "Histogram of Time Differences",
    x = "Departure since civil twilight (6°) (minutes)",
    y = "Number of flight",
    fill = " Duration category (hr)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




