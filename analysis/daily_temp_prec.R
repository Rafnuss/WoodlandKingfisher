library(tidyverse)
library(lubridate)
library(GeoPressureR)

dtp <- rbind(
  read_csv("data/daily_temp_prec/breeding.csv", show_col_types = FALSE) %>% mutate(cat = "breed"),
  read_csv("data/daily_temp_prec/wintering.csv", show_col_types = FALSE) %>% mutate(cat = "wint")
) %>%
  mutate(
    date_doy = update(date, year = 2017),
    mean_2m_air_temperature = mean_2m_air_temperature - 273.15,
    total_precipitation = ifelse(total_precipitation < 0, 0, total_precipitation * 1000)
  )

dtp_avg <- dtp %>%
  group_by(date_doy, cat) %>%
  summarise(
    total_precipitation = mean(total_precipitation),
    total_precipitation_q10 = quantile(total_precipitation, .1),
    total_precipitation_q90 = quantile(total_precipitation, .9),
    mean_2m_air_temperature = mean(mean_2m_air_temperature)
  )


breed_loc <- c(28.768573249999992, -22.724631630000005)
wint_loc <- c(28.051387619881588, 8.435382674874637)


# ----

dtp_avg %>%
  ggplot(aes(x = date_doy, y = mean_2m_air_temperature, col = cat)) +
  geom_point(data = dtp, alpha = 0.1) +
  geom_step() +
  geom_smooth(method = "gam") +
  labs(y = "Temperature [°C]") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_color_manual(values = c("#999999", "#E69F00")) +
  labs(x = "Day of the Year")

ggsave(file = "output/figure_print/daily_temp.eps", width = 140, height = 50, unit = "mm")

dtp_avg %>%
  ggplot(aes(x = date_doy, y = total_precipitation, col = cat)) +
  geom_point(data = dtp, alpha = 0.1) +
  geom_step() +
  geom_smooth(method = "gam") +
  labs(y = "Total precipication [mm]") +
  ylim(c(0, 10)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_color_manual(values = c("#999999", "#E69F00")) +
  labs(x = "Day of the Year")

ggsave(file = "output/figure_print/daily_precip.eps", width = 140, height = 50, unit = "mm")



dtp_avg$doy <- as.numeric(dtp_avg$date_doy)
a <- mgcv::gam(total_precipitation ~ s(doy), data = dtp_avg)
dtp_avg$total_precipitation_smooth <- predict(a, dtp_avg)

dtp_avg %>%
  ggplot(aes(x = date_doy, y = total_precipitation, col = cat)) +
  geom_bar(stat = "identity") +
  labs(y = "Total precipication [mm]") +
  ylim(c(0, 10)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_color_manual(values = c("#999999", "#E69F00")) +
  labs(x = "Day of the Year")

ggsave(file = "output/figure_print/daily_precip.eps", width = 140, height = 50, unit = "mm")

# ----


list_id <- tail(names(yaml::yaml.load_file("config.yml", eval.expr = FALSE)), -1)
did <- list()
for (i in seq(1, length(list_id))) {
  load(paste0("data/interim/", list_id[i], ".Rdata"))

  istap_winter <- which(stap2duration(tag$stap) > 50)[2]
  tmp <- c(
    tag$stap$end[1],
    tag$stap$start[istap_winter],
    tag$stap$end[istap_winter],
    tail(tag$stap$start, 1)
  )
  tmp <- update(tmp, year = 2017)
  did[[i]] <- data.frame(
    date = tmp,
    cat2 = c("breed", "wint", "wint", "breed"),
    cat = list_id[i]
  )
}

did[[5]] <- did[[5]][c(1, 2), ]

did <- do.call(rbind, did)

ggplot(did, aes(x = as.Date(date), y = cat)) +
  geom_point() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", limits = as.Date(c("2017-01-01", "2018-01-01"))) +
  labs(x = "Day of the Year")

ggsave(file = "output/figure_print/daily_tag.eps", width = 140, height = 50, unit = "mm")
