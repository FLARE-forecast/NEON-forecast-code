library(tidyverse)
library(lubridate)
library(patchwork)
library(egg)
library(broom)

lake_directory <- here::here()

lake_info <- read_csv(file.path(lake_directory,"workflows","neon_lakes_ms", "LakeTable_15Nov2021.csv"))
score_directory <- "s3"


library(arrow)
library(dplyr)
source(file.path(lake_directory,"workflows","neon_lakes_ms","scoring.R"))

if(score_directory == "s3"){
Sys.setenv("AWS_EC2_METADATA_DISABLED"="TRUE")
Sys.unsetenv("AWS_ACCESS_KEY_ID")
Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
Sys.unsetenv("AWS_DEFAULT_REGION")
Sys.unsetenv("AWS_S3_ENDPOINT")

s <- score_schema()
s3 <- s3_bucket(bucket = "scores",
                endpoint_override = "s3.flare-forecast.org",
                anonymous=TRUE)
ds <- open_dataset(s3, schema=s, format = "csv", skip_rows = 1)

## Test
combined <- ds %>% filter(team == "ms1_glm_flare" | team == "ms1_climatology") %>% collect()
}else{
#Read in scored forecasts
scores_files <- fs::dir_ls(score_directory, type="file",recurse = TRUE)

scores_files1 <- scores_files[stringr::str_detect(scores_files, "ms1_glm_flare")]
scores_files2 <- scores_files[stringr::str_detect(scores_files, "ms1_climatology")]
scores_files <- c(scores_files1, scores_files2)

combined <- readr::read_csv(scores_files, progress = FALSE)
}

combined <- combined %>%
  rename("siteID" = theme) %>%
  mutate(resid = observed - mean,
         sq_error = (resid)^2) %>%
  select(siteID, team, issue_date, time, forecast_start_time, horizon, target,depth, mean, sd, observed, crps, logs, resid, sq_error, quantile10, quantile90) %>%
  mutate(region = NA,
         region = ifelse(siteID %in% c("BARC","SUGG"), "Southeast", region),
         region = ifelse(siteID %in% c("CRAM","LIRO"), "Great Lakes", region),
         region = ifelse(siteID %in% c("PRLA","PRPO"), "Northern Plains", region),
         region = factor(region, levels = c("Northern Plains","Great Lakes","Southeast")),
         siteID = factor(siteID, levels = c("PRLA","PRPO","CRAM","LIRO","BARC","SUGG"))) %>%
  rename(Model = team) %>%
  filter(Model != "ms_persistence") %>%
  mutate(Model = ifelse(Model == "ms_climatology", "Day-of-year mean", Model),
         Model = ifelse(Model == "ms_glm_flare","FLARE-GLM",Model)) %>%
  mutate(season = ifelse(time < as_date("2021-09-01"), "summer", "fall")) %>%
  filter(time < as_date("2021-10-24")) %>%
  mutate(Model = factor(Model, levels = c("FLARE-GLM","Day-of-year mean")))

horizon10rmse <- combined %>%
  filter(Model == 'FLARE-GLM') %>%
  select(siteID, Model, region, issue_date, time, season, forecast_start_time, horizon, target,depth, sq_error) %>%
  pivot_wider(names_from = Model, values_from = sq_error, values_fill = NA) %>%
  na.omit() %>%
  filter(depth == 0.1) %>%
  filter(horizon <= 10) %>%
  pivot_longer(cols = -c("siteID", "issue_date", "season", "time", "forecast_start_time", "horizon", "target","depth", "region"), names_to = "Model",values_to = "sq_error") %>%
  group_by(target, Model, siteID) %>%  # average over siteID
  summarise(rmse = mean(sq_error, na.rm =TRUE),
            .groups = "drop") %>%
  mutate(rmse = sqrt(rmse)) %>%
  left_join(lake_info, by = "siteID") %>%
  rename(region = state)

first10days <- combined %>%
  filter(Model == 'FLARE-GLM') %>%
  select(siteID, Model, region, issue_date, time, season, forecast_start_time, horizon, target,depth, sq_error) %>%
  pivot_wider(names_from = Model, values_from = sq_error, values_fill = NA) %>%
  na.omit() %>%
  filter(depth == 0.1) %>%
  pivot_longer(cols = -c("siteID", "issue_date", "season", "time", "forecast_start_time", "horizon", "target","depth", "region"), names_to = "Model",values_to = "sq_error") %>%
  group_by(target, Model, siteID, horizon) %>%  # average over siteID
  summarise(rmse = mean(sq_error, na.rm =TRUE),
            .groups = "drop") %>%
  mutate(rmse = sqrt(rmse)) %>%
  filter(horizon < 10) %>%
  group_by(siteID)

slope10day <- do(first10days, tidy(lm(rmse ~ horizon, data = .))) %>%
  filter(term == "horizon") %>%
  rename(slope = estimate) %>%
  select(siteID, slope)

horizon10rmse <- left_join(horizon10rmse, slope10day)

corr_table <- matrix(NA, nrow = 10, ncol = 2)
corr_table[1,1] <- cor(-horizon10rmse$rmse, horizon10rmse$max_depth_m, method = )
corr_table[2,1] <- cor(-horizon10rmse$rmse, horizon10rmse$fetch_m, method = "spearman")
corr_table[3,1] <- cor(-horizon10rmse$rmse, horizon10rmse$volume_m3, method = "spearman")
corr_table[4,1] <- cor(-horizon10rmse$rmse, horizon10rmse$surface_area_km2, method = "spearman")
corr_table[5,1] <- cor(-horizon10rmse$rmse, horizon10rmse$mean_secchi_depth_m, method = "spearman")
corr_table[6,1] <- cor(-horizon10rmse$rmse, horizon10rmse$mean_annual_temperature_degreeC, method = "spearman")
corr_table[7,1] <- cor(-horizon10rmse$rmse, horizon10rmse$mean_annual_precipitation_mm, method = "spearman")
corr_table[8,1] <- cor(-horizon10rmse$rmse, horizon10rmse$air_temp_10day_sd_degreeC, method = "spearman")
corr_table[9,1] <- cor(-horizon10rmse$rmse, horizon10rmse$mean_hydraulic_residence_time_yrs, method = "spearman")
corr_table[10,1] <- cor(-horizon10rmse$rmse, horizon10rmse$catchment_size_km2, method = "spearman")

corr_table[1,2] <- cor(horizon10rmse$slope, horizon10rmse$max_depth_m, method = "spearman")
corr_table[2,2] <- cor(horizon10rmse$slope, horizon10rmse$fetch_m, method = "spearman")
corr_table[3,2] <- cor(horizon10rmse$slope, horizon10rmse$volume_m3, method = "spearman")
corr_table[4,2] <- cor(horizon10rmse$slope, horizon10rmse$surface_area_km2, method = "spearman")
corr_table[5,2] <- cor(horizon10rmse$slope, horizon10rmse$mean_secchi_depth_m, method = "spearman")
corr_table[6,2] <- cor(horizon10rmse$slope, horizon10rmse$mean_annual_temperature_degreeC, method = "spearman")
corr_table[7,2] <- cor(horizon10rmse$slope, horizon10rmse$mean_annual_precipitation_mm, method = "spearman")
corr_table[8,2] <- cor(horizon10rmse$slope, horizon10rmse$air_temp_10day_sd_degreeC, method = "spearman")
corr_table[9,2] <- cor(horizon10rmse$slope, horizon10rmse$mean_hydraulic_residence_time_yrs, method = "spearman")
corr_table[10,2] <- cor(horizon10rmse$slope, horizon10rmse$catchment_size_km2, method = "spearman")

corr_table_pvalue <- matrix(NA, nrow = 10, ncol = 2)
corr_table_pvalue[1,1] <- cor.test(-horizon10rmse$rmse, horizon10rmse$max_depth_m, method = "spearman")$p.value
corr_table_pvalue[2,1] <- cor.test(-horizon10rmse$rmse, horizon10rmse$fetch_m, method = "spearman")$p.value
corr_table_pvalue[3,1] <- cor.test(-horizon10rmse$rmse, horizon10rmse$volume_m3, method = "spearman")$p.value
corr_table_pvalue[4,1] <- cor.test(-horizon10rmse$rmse, horizon10rmse$surface_area_km2, method = "spearman")$p.value
corr_table_pvalue[5,1] <- cor.test(-horizon10rmse$rmse, horizon10rmse$mean_secchi_depth_m, method = "spearman")$p.value
corr_table_pvalue[6,1] <- cor.test(-horizon10rmse$rmse, horizon10rmse$mean_annual_temperature_degreeC, method = "spearman")$p.value
corr_table_pvalue[7,1] <- cor.test(-horizon10rmse$rmse, horizon10rmse$mean_annual_precipitation_mm, method = "spearman")$p.value
corr_table_pvalue[8,1] <- cor.test(-horizon10rmse$rmse, horizon10rmse$air_temp_10day_sd_degreeC, method = "spearman")$p.value
corr_table_pvalue[9,1] <- cor.test(-horizon10rmse$rmse, horizon10rmse$mean_hydraulic_residence_time_yrs, method = "spearman")$p.value
corr_table_pvalue[10,1] <- cor.test(-horizon10rmse$rmse, horizon10rmse$catchment_size_km2, method = "spearman")$p.value

corr_table_pvalue[1,2] <- cor.test(horizon10rmse$slope, horizon10rmse$max_depth_m, method = "spearman")$p.value
corr_table_pvalue[2,2] <- cor.test(horizon10rmse$slope, horizon10rmse$fetch_m, method = "spearman")$p.value
corr_table_pvalue[3,2] <- cor.test(horizon10rmse$slope, horizon10rmse$volume_m3, method = "spearman")$p.value
corr_table_pvalue[4,2] <- cor.test(horizon10rmse$slope, horizon10rmse$surface_area_km2, method = "spearman")$p.value
corr_table_pvalue[5,2] <- cor.test(horizon10rmse$slope, horizon10rmse$mean_secchi_depth_m, method = "spearman")$p.value
corr_table_pvalue[6,2] <- cor.test(horizon10rmse$slope, horizon10rmse$mean_annual_temperature_degreeC, method = "spearman")$p.value
corr_table_pvalue[7,2] <- cor.test(horizon10rmse$slope, horizon10rmse$mean_annual_precipitation_mm, method = "spearman")$p.value
corr_table_pvalue[8,2] <- cor.test(horizon10rmse$slope, horizon10rmse$air_temp_10day_sd_degreeC, method = "spearman")$p.value
corr_table_pvalue[9,2] <- cor.test(horizon10rmse$slope, horizon10rmse$mean_hydraulic_residence_time_yrs, method = "spearman")$p.value
corr_table_pvalue[10,2] <- cor.test(horizon10rmse$slope, horizon10rmse$catchment_size_km2, method = "spearman")$p.value



variables <- c("Maximum\ndepth", "Fetch", "Volume", "Surface\narea","Water clarity\n(Secchi depth)","Mean annual air\ntemperature", "Mean annual\nprecipitation","10-day variance in\n air temperature","Mean hydrologic\nresidence time","Catchment\nsize")
corr_tibble <- tibble(rmse = corr_table[,1], slope = corr_table[,2], variable = as.factor(variables)) %>%
  pivot_longer(cols = -variable, names_to = "Metric",values_to = "value") %>%
  mutate(Metric = ifelse(Metric == "rmse",  "accuracy", "rate of accuracy\ndegradation")) %>%
  arrange(desc(abs(value)))

corr_names <- rev(as.character(unique(corr_tibble$variable)))

corr_tibble <- corr_tibble %>%
  mutate(variable = factor(variable, levels = corr_names))

#Figure 1 is another script

#Figure 2 (0.1 m)
y_label <- expression(paste('RMSE (',degree,'C)', sep = ""))
p <- combined %>%
  select(siteID, Model, region, issue_date, time, season, forecast_start_time, horizon, target,depth, sq_error) %>%
  pivot_wider(names_from = Model, values_from = sq_error, values_fill = NA) %>%
  na.omit() %>%
  filter(depth == 0.1) %>%
  pivot_longer(cols = -c("siteID", "issue_date", "season", "time", "forecast_start_time", "horizon", "target","depth", "region"), names_to = "Model",values_to = "sq_error") %>%
  group_by(target, region, Model, horizon, siteID) %>%  # average over siteID
  summarise(rmse = mean(sq_error, na.rm =TRUE),
            .groups = "drop") %>%
  mutate(rmse = sqrt(rmse)) %>%
  mutate(Model = factor(Model, levels = c("FLARE-GLM","Day-of-year mean"))) %>%
  ggplot(aes(horizon, rmse, col=Model)) +
  geom_line() +
  ylim(0,3.75) +
  #geom_point() +
  labs(x = "Horizon (days)", y = y_label) +
  #geom_hline(yintercept = 1.48) + #https://www.doi.org/10.3389/fenvs.2021.707874
  facet_wrap(~region + siteID, nrow = 3, scale = "free") +
  theme_bw() +
  theme(legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.justification="center",
        #legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.15),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


p <- tag_facet(p, fontface = 1)
p <- p + theme(strip.text = element_text())

y_label <- expression(paste('RMSE (',degree,'C)', sep = ""))
p_region <- combined %>%
  select(siteID, Model, region, issue_date, time, season, forecast_start_time, horizon, target,depth, sq_error) %>%
  pivot_wider(names_from = Model, values_from = sq_error, values_fill = NA) %>%
  na.omit() %>%
  filter(depth == 0.1) %>%
  pivot_longer(cols = -c("siteID", "issue_date", "season", "time", "forecast_start_time", "horizon", "target","depth", "region"), names_to = "Model",values_to = "sq_error") %>%
  filter(Model == "FLARE-GLM") %>%
  group_by(target, Model, horizon, region) %>%  # average over siteID
  summarise(rmse = mean(sq_error, na.rm =TRUE),
            .groups = "drop") %>%
  mutate(rmse = sqrt(rmse)) %>%
  ggplot(aes(horizon, rmse, col = region)) +
  geom_line() +
  #geom_point() +
  ylim(0,3.25) +
  labs(x = "Horizon (days)", y = y_label, tag = "(g)", title = " ") +
  theme_bw() +
  guides(color = guide_legend(title = "NEON\nDomain")) +
  scale_color_manual(values=c("brown", "purple", "darkgray")) +
  theme(legend.position= c(0.97,0.02),
        legend.justification=c(1,0),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.15),
        legend.key = element_rect(fill = "white"),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.tag.position = c(0.25,0.83))

p_season <- combined %>%
  select(siteID, Model, issue_date, time, season, forecast_start_time, horizon, target,depth, sq_error) %>%
  pivot_wider(names_from = Model, values_from = sq_error, values_fill = NA) %>%
  na.omit() %>%
  filter(depth == 0.1) %>%
  pivot_longer(cols = -c("siteID", "issue_date", "season", "time", "forecast_start_time", "horizon", "target","depth"), names_to = "Model",values_to = "sq_error") %>%
  filter(Model == "FLARE-GLM") %>%
  group_by(target, Model, horizon, season) %>%  # average over siteID
  summarise(rmse = mean(sq_error, na.rm =TRUE),
            .groups = "drop") %>%
  mutate(rmse = sqrt(rmse)) %>%
  ggplot(aes(horizon, rmse, col = season)) +
  geom_line() +
  #geom_point() +
  ylim(0,3.25) +
  labs(x = "Horizon (days)", y = y_label, tag = "(h)", title = " ") +
  theme_bw()+
  scale_color_manual(values=c("black", "green")) +
  guides(color = guide_legend(title = "Season")) +
  theme(legend.position= c(0.97,0.02),
        legend.justification=c(1,0),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.15),
        legend.key = element_rect(fill = "white"),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y=element_blank(),
        axis.title.y = element_blank(),
        plot.tag.position = c(0.13,0.83))


p_combined <- p / (p_region + p_season)

p_combined <- p_combined + plot_layout(widths = c(4.5, 4.5), heights = unit(c(4, 1), c('in', 'null')))

ggsave(filename = file.path(lake_directory, "notebook","Figure2.jpg"), device = "jpeg", plot = p_combined, width = 4.5, height = 7.5, units = "in")

#Figure 3

p<- ggplot(corr_tibble, aes(x = value, y = variable, col = Metric)) +
  geom_point() +
  geom_vline(xintercept = 0.5) +
  geom_vline(xintercept = -0.5) +
  geom_hline(yintercept = 4.5, linetype = "dashed") +
  xlim(-1,1) +
  labs(x = "Spearman's correlation coefficient") +
  scale_color_manual(values=c("red", "blue")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position= "bottom",
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))

ggsave(filename = file.path(lake_directory, "notebook","Figure3.jpg"), device = "jpeg", plot = p, width = 4.5, height = 4.5, units = "in")

# Other figures

#Figure SI1 (<= 2 m)
y_label <- expression(paste('RMSE (',degree,'C)', sep = ""))
p <- combined %>%
  select(siteID, Model, region, issue_date, time, season, forecast_start_time, horizon, target,depth, sq_error) %>%
  pivot_wider(names_from = Model, values_from = sq_error, values_fill = NA) %>%
  na.omit() %>%
  filter(depth <= 2) %>%
  pivot_longer(cols = -c("siteID", "issue_date", "season", "time", "forecast_start_time", "horizon", "target","depth", "region"), names_to = "Model",values_to = "sq_error") %>%
  group_by(target, region, Model, horizon, siteID) %>%  # average over siteID
  summarise(rmse = mean(sq_error, na.rm =TRUE),
            .groups = "drop") %>%
  mutate(rmse = sqrt(rmse)) %>%
  mutate(Model = factor(Model, levels = c("FLARE-GLM","Day-of-year mean"))) %>%
  ggplot(aes(horizon, rmse, col=Model)) +
  geom_line() +
  ylim(0,3.75) +
  #geom_point() +
  labs(x = "Horizon (days)", y = y_label) +
  #geom_hline(yintercept = 1.48) + #https://www.doi.org/10.3389/fenvs.2021.707874
  facet_wrap(~region + siteID, nrow = 3, scale = "free") +
  theme_bw() +
  theme(legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.justification="center",
        #legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.15),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


p <- tag_facet(p, fontface = 1)
p <- p + theme(strip.text = element_text())

ggsave(filename = file.path(lake_directory, "notebook","FigureSI1_2mless.jpg"), device = "jpeg", plot = p, width = 4.5, height = 4.5, units = "in")


#Figure SI2 (all depths)

y_label <- expression(paste('RMSE (',degree,'C)', sep = ""))
p <- combined %>%
  select(siteID, Model, region, issue_date, time, season, forecast_start_time, horizon, target,depth, sq_error) %>%
  pivot_wider(names_from = Model, values_from = sq_error, values_fill = NA) %>%
  na.omit() %>%
  pivot_longer(cols = -c("siteID", "issue_date", "season", "time", "forecast_start_time", "horizon", "target","depth", "region"), names_to = "Model",values_to = "sq_error") %>%
  group_by(target, region, Model, horizon, siteID) %>%  # average over siteID
  summarise(rmse = mean(sq_error, na.rm =TRUE),
            .groups = "drop") %>%
  mutate(rmse = sqrt(rmse)) %>%
  mutate(Model = factor(Model, levels = c("FLARE-GLM","Day-of-year mean"))) %>%
  ggplot(aes(horizon, rmse, col=Model)) +
  geom_line() +
  ylim(0,3.75) +
  #geom_point() +
  labs(x = "Horizon (days)", y = y_label) +
  #geom_hline(yintercept = 1.48) + #https://www.doi.org/10.3389/fenvs.2021.707874
  facet_wrap(~region + siteID, nrow = 3, scale = "free") +
  theme_bw() +
  theme(legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.justification="center",
        #legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.15),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

p <- tag_facet(p, fontface = 1)
p <- p + theme(strip.text = element_text())

ggsave(filename = file.path(lake_directory, "notebook","FigureSI2_alldepths.jpg"), device = "jpeg", plot = p, width = 4.5, height = 4.5, units = "in")

#CRPS 0.1 over horizon

y_label <- expression(paste('Continous Ranked Probability Score (',degree,'C)', sep = ""))
p <- combined %>%
  select(siteID, Model, region, issue_date, time, season, forecast_start_time, horizon, target,depth, crps) %>%
  pivot_wider(names_from = Model, values_from = crps, values_fill = NA) %>%
  na.omit() %>%
  filter(depth == 0.1) %>%
  pivot_longer(cols = -c("siteID", "issue_date", "season", "time", "forecast_start_time", "horizon", "target","depth", "region"), names_to = "Model",values_to = "crps") %>%
  group_by(target, region, Model, horizon, siteID) %>%  # average over siteID
  summarise(crps = mean(crps, na.rm =TRUE),
            .groups = "drop") %>%
  mutate(Model = factor(Model, levels = c("FLARE-GLM","Day-of-year mean"))) %>%
  ggplot(aes(horizon, crps, col=Model)) +
  geom_line() +
  ylim(0,2) +
  #geom_point() +
  labs(x = "Horizon (days)", y = y_label) +
  facet_wrap(~region + siteID, nrow = 3, scale = "free") +
  theme_bw() +
  theme(legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.justification="center",
        #legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.15),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


p <- tag_facet(p, fontface = 1)
p <- p + theme(strip.text = element_text())

ggsave(filename = file.path(lake_directory, "notebook","FigureSI3_crps0.1.jpg"), device = "jpeg", plot = p, width = 4.5, height = 4.5, units = "in")

### Numbers for next

rmse_all_mean <- combined %>%
  select(siteID, Model, region, issue_date, time, season, forecast_start_time, horizon, target,depth, sq_error) %>%
  pivot_wider(names_from = Model, values_from = sq_error, values_fill = NA) %>%
  na.omit() %>%
  filter(depth == 0.1) %>%
  pivot_longer(cols = -c("siteID", "issue_date", "season", "time", "forecast_start_time", "horizon", "target","depth", "region"), names_to = "Model",values_to = "sq_error") %>%
  group_by(Model, horizon) %>%  # average over siteID
  summarise(rmse = mean(sq_error, na.rm =TRUE),
            .groups = "drop") %>%
  mutate(rmse = sqrt(rmse)) %>%
  filter(Model == "FLARE-GLM")

rmse_site_mean <- combined %>%
  select(siteID, Model, region, issue_date, time, season, forecast_start_time, horizon, target,depth, sq_error) %>%
  pivot_wider(names_from = Model, values_from = sq_error, values_fill = NA) %>%
  na.omit() %>%
  filter(depth == 0.1) %>%
  pivot_longer(cols = -c("siteID", "issue_date", "season", "time", "forecast_start_time", "horizon", "target","depth", "region"), names_to = "Model",values_to = "sq_error") %>%
  group_by(Model, siteID, horizon) %>%  # average over siteID
  summarise(rmse = mean(sq_error, na.rm =TRUE),
            .groups = "drop") %>%
  mutate(rmse = sqrt(rmse)) %>%
  filter(Model == "FLARE-GLM") %>%
  pivot_wider(names_from = siteID, values_from = rmse)

clim_vs_flare_horizon <- combined %>%
  select(siteID, Model, region, issue_date, time, season, forecast_start_time, horizon, target,depth, sq_error) %>%
  pivot_wider(names_from = Model, values_from = sq_error, values_fill = NA) %>%
  na.omit() %>%
  filter(depth == 0.1) %>%
  pivot_longer(cols = -c("siteID", "issue_date", "season", "time", "forecast_start_time", "horizon", "target","depth", "region"), names_to = "Model",values_to = "sq_error") %>%
  group_by(Model, horizon, siteID) %>%  # average over siteID
  summarise(rmse = mean(sq_error, na.rm =TRUE),
            .groups = "drop") %>%
  mutate(rmse = sqrt(rmse)) %>%
  pivot_wider(names_from = Model, values_from = rmse) %>%
  mutate(diff = climatology - `FLARE-GLM`) %>%
  filter(diff < 0) %>%
  group_by(siteID) %>%
  summarize(min = min(horizon, na.rm = TRUE))

slope10day
