
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

# Cramption Lake
cram_path <- "./forecast_output/CRAM"
cram_forecasts <- list.files(cram_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(cram_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))

cram_1wk <- cram_forecasts %>%
  filter(days <= 7)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1wk")

cram_2wk <- cram_forecasts %>%
  filter(days > 7)%>%
  filter(days <= 14)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "2wk")

cram_3wk <- cram_forecasts %>%
  filter(days > 14)%>%
  filter(days <= 21)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "3wk")

cram_4wk <- cram_forecasts %>%
  filter(days > 21)%>%
  filter(days <= 28)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "4wk")

cram_eval <- bind_rows(cram_1wk, cram_2wk, cram_3wk, cram_4wk)%>%
  mutate(siteID = "C: CRAM")

# Little Rock Lake
liro_path <- "./forecast_output/LIRO"
liro_forecasts <- list.files(liro_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(liro_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))

liro_1wk <- liro_forecasts %>%
  filter(days <= 7)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1wk")

liro_2wk <- liro_forecasts %>%
  filter(days > 7)%>%
  filter(days <= 14)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "2wk")

liro_3wk <- liro_forecasts %>%
  filter(days > 14)%>%
  filter(days <= 21)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "3wk")

liro_4wk <- liro_forecasts %>%
  filter(days > 21)%>%
  filter(days <= 28)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "4wk")

liro_eval <- bind_rows(liro_1wk, liro_2wk, liro_3wk, liro_4wk)%>%
  mutate(siteID = "D: LIRO")

great_lakes_domain <- bind_rows(cram_eval, liro_eval)%>%
  mutate(domain = "GRL")


### South East Domain

# Lake Barco
barc_path <- "./forecast_output/BARC"
barc_forecasts <- list.files(barc_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(barc_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))

barc_1wk <- barc_forecasts %>%
  filter(days <= 7)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1wk")

barc_2wk <- barc_forecasts %>%
  filter(days > 7)%>%
  filter(days <= 14)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "2wk")

barc_3wk <- barc_forecasts %>%
  filter(days > 14)%>%
  filter(days <= 21)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "3wk")

barc_4wk <- barc_forecasts %>%
  filter(days > 21)%>%
  filter(days <= 28)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "4wk")

barc_eval <- bind_rows(barc_1wk, barc_2wk, barc_3wk, barc_4wk)%>%
  mutate(siteID = "A: BARC")

# Lake Suggs
sugg_path <- "./forecast_output/SUGG"
sugg_forecasts <- list.files(sugg_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(sugg_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))

sugg_1wk <- sugg_forecasts %>%
  filter(days <= 7)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1wk")

sugg_2wk <- sugg_forecasts %>%
  filter(days > 7)%>%
  filter(days <= 14)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "2wk")

sugg_3wk <- sugg_forecasts %>%
  filter(days > 14)%>%
  filter(days <= 21)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "3wk")

sugg_4wk <- sugg_forecasts %>%
  filter(days > 21)%>%
  filter(days <= 28)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "4wk")

sugg_eval <- bind_rows(sugg_1wk, sugg_2wk, sugg_3wk, sugg_4wk)%>%
  mutate(siteID = "B: SUGG")

south_east_domain <- bind_rows(barc_eval, sugg_eval)%>%
  mutate(domain = "SOE")

# Northern Plains Domain

# Prairie Pothole Lake
prpo_path <- "./forecast_output/PRPO"
prpo_forecasts <- list.files(prpo_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(prpo_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))

prpo_1wk <- prpo_forecasts %>%
  filter(days <= 7)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1wk")

prpo_2wk <- prpo_forecasts %>%
  filter(days > 7)%>%
  filter(days <= 14)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "2wk")

prpo_3wk <- prpo_forecasts %>%
  filter(days > 14)%>%
  filter(days <= 21)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "3wk")

prpo_4wk <- prpo_forecasts %>%
  filter(days > 21)%>%
  filter(days <= 28)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "4wk")

prpo_eval <- bind_rows(prpo_1wk, prpo_2wk, prpo_3wk, prpo_4wk)%>%
  mutate(siteID = "E: PRPO")

# Prairie Lake
prla_path <- "./forecast_output/PRLA"
prla_forecasts <- list.files(prla_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(prla_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))

prla_1wk <- prla_forecasts %>%
  filter(days <= 7)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1wk")

prla_2wk <- prla_forecasts %>%
  filter(days > 7)%>%
  filter(days <= 14)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "2wk")

prla_3wk <- prla_forecasts %>%
  filter(days > 14)%>%
  filter(days <= 21)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "3wk")

prla_4wk <- prla_forecasts %>%
  filter(days > 21)%>%
  filter(days <= 28)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "4wk")

prla_eval <- bind_rows(prla_1wk, prla_2wk, prla_3wk, prla_4wk)%>%
  mutate(siteID = "F: PRLA")

northern_plains_domain <- bind_rows(prpo_eval, prla_eval)%>%
  mutate(domain = "NRP")

eval_lakes <- bind_rows(south_east_domain, great_lakes_domain, northern_plains_domain)

rmse_fig <- ggplot(eval_lakes, aes(forecast_horizon, rmse, fill = domain))+
geom_boxplot(outlier.colour="red", outlier.shape=16,
             outlier.size=1, notch=FALSE)+
  theme_classic()+
  facet_wrap(~siteID)

rmse_fig

ggsave(path = ".", filename = "./forecast_output/figures/across_site_rmse.jpg", width = 10, height = 7, device='jpg', dpi=1000)
