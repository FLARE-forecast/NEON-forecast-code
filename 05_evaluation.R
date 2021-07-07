
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

cram_1dy <- cram_forecasts %>%
  filter(days == 1)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1dy")

cram_7dy <- cram_forecasts %>%
  filter(days == 7)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1wk")

cram_14dy <- cram_forecasts %>%
  filter(days == 14)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "2wk")

cram_21dy <- cram_forecasts %>%
  filter(days == 21)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "3wk")

cram_28dy <- cram_forecasts %>%
  filter(days == 28)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "4wk")

cram_eval <- bind_rows(cram_1dy, cram_7dy, cram_14dy, cram_21dy, cram_28dy)%>%
  mutate(siteID = "C: CRAM") %>%
  mutate(`Lake Depth` = "Deep")

# Little Rock Lake
liro_path <- "./forecast_output/LIRO"
liro_forecasts <- list.files(liro_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(liro_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))

liro_1dy <- liro_forecasts %>%
  filter(days == 1)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1dy")

liro_7dy <- liro_forecasts %>%
  filter(days == 7)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1wk")

liro_14dy <- liro_forecasts %>%
  filter(days == 14)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "2wk")

liro_21dy <- liro_forecasts %>%
  filter(days == 21)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "3wk")

liro_28dy <- liro_forecasts %>%
  filter(days == 28)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "4wk")

liro_eval <- bind_rows(liro_1dy, liro_7dy, liro_14dy, liro_21dy, liro_28dy)%>%
  mutate(siteID = "D: LIRO") %>%
  mutate(`Lake Depth` = "Shallow")

great_lakes_domain <- bind_rows(cram_eval, liro_eval)%>%
  mutate(domain = "GRL")

gl_fig <- ggplot(great_lakes_domain, aes(forecast_horizon, rmse, fill = `Lake Depth`))+
  geom_boxplot(outlier.colour="black", outlier.shape=21,
               outlier.size=2, notch=FALSE, outlier.fill = "grey")+
  ylab("RMSE")+
  xlab("")+
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6))+
  scale_fill_manual(values=c("darkorange", "darkviolet"))+
  theme_classic()+
  theme(axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"))+
  facet_wrap(~siteID)

### South East Domain

# Lake Barco
barc_path <- "./forecast_output/BARC"
barc_forecasts <- list.files(barc_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(barc_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))

barc_1dy <- barc_forecasts %>%
  filter(days == 1)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1dy")

barc_7dy <- barc_forecasts %>%
  filter(days == 7)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1wk")

barc_14dy <- barc_forecasts %>%
  filter(days == 14)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "2wk")

barc_21dy <- barc_forecasts %>%
  filter(days == 21)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "3wk")

barc_28dy <- barc_forecasts %>%
  filter(days == 28)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "4wk")

barc_eval <- bind_rows(barc_1dy, barc_7dy, barc_14dy, barc_21dy, barc_28dy)%>%
  mutate(siteID = "A: BARC") %>%
  mutate(`Lake Depth` = "Deep")

# Lake Suggs
sugg_path <- "./forecast_output/SUGG"
sugg_forecasts <- list.files(sugg_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(sugg_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))

sugg_1dy <- sugg_forecasts %>%
  filter(days == 1)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1dy")

sugg_7dy <- sugg_forecasts %>%
  filter(days == 7)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1wk")

sugg_14dy <- sugg_forecasts %>%
  filter(days == 14)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "2wk")

sugg_21dy <- sugg_forecasts %>%
  filter(days == 21)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "3wk")

sugg_28dy <- sugg_forecasts %>%
  filter(days == 28)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "4wk")

sugg_eval <- bind_rows(sugg_1dy, sugg_7dy, sugg_14dy, sugg_21dy, sugg_28dy)%>%
  mutate(siteID = "B: SUGG") %>%
  mutate(`Lake Depth` = "Shallow")

south_east_domain <- bind_rows(barc_eval, sugg_eval)%>%
  mutate(domain = "SOE")

se_fig <- ggplot(south_east_domain, aes(forecast_horizon, rmse, fill = `Lake Depth`))+
  geom_boxplot(outlier.colour="black", outlier.shape=21,
               outlier.size=2, notch=FALSE, outlier.fill = "grey")+
  ylab("")+
  xlab("")+
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6))+
  scale_fill_manual(values=c("darkorange", "darkviolet"))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"))+
  facet_wrap(~siteID)

# Northern Plains Domain

# Prairie Pothole Lake
prpo_path <- "./forecast_output/PRPO"
prpo_forecasts <- list.files(prpo_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(prpo_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))

prpo_1dy <- prpo_forecasts %>%
  filter(days == 1)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1dy")

prpo_7dy <- prpo_forecasts %>%
  filter(days == 7)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1wk")

prpo_14dy <- prpo_forecasts %>%
  filter(days == 14)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "2wk")

prpo_21dy <- prpo_forecasts %>%
  filter(days == 21)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "3wk")

prpo_28dy <- prpo_forecasts %>%
  filter(days == 28)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "4wk")

prpo_eval <- bind_rows(prpo_1dy, prpo_7dy, prpo_14dy, prpo_21dy, prpo_28dy)%>%
  mutate(siteID = "F: PRPO") %>%
  mutate(`Lake Depth` = "Shallow")

# Prairie Lake
prla_path <- "./forecast_output/PRLA"
prla_forecasts <- list.files(prla_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(prla_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))

prla_1dy <- prla_forecasts %>%
  filter(days == 1)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1dy")

prla_7dy <- prla_forecasts %>%
  filter(days == 7)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "1wk")

prla_14dy <- prla_forecasts %>%
  filter(days == 14)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "2wk")

prla_21dy <- prla_forecasts %>%
  filter(days == 21)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "3wk")

prla_28dy <- prla_forecasts %>%
  filter(days == 28)%>%
  group_by(forecast_start_day)%>%
  summarize(rmse = RMSE(forecast_mean,observed))%>%
  mutate(forecast_horizon = "4wk")

prla_eval <- bind_rows(prla_1dy, prla_7dy, prla_14dy, prla_21dy, prla_28dy)%>%
  mutate(siteID = "E: PRLA") %>%
  mutate(`Lake Depth` = "Deep")

northern_plains_domain <- bind_rows(prpo_eval, prla_eval)%>%
  mutate(domain = "NRP")


np_fig <- ggplot(northern_plains_domain, aes(forecast_horizon, rmse, fill = `Lake Depth`))+
geom_boxplot(outlier.colour="black", outlier.shape=21,
             outlier.size=2, notch=FALSE, outlier.fill = "grey")+
  ylab("")+
  xlab("Forecast Horizon")+
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6), limits = c(0,6))+
  scale_fill_manual(values=c("darkorange", "darkviolet"))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"))+
  facet_wrap(~siteID)

fig <- se_fig/gl_fig/np_fig
fig
ggsave(path = ".", filename = "./forecast_output/figures/across_site_rmse.jpg", width = 7, height = 10, device='jpg', dpi=1000)
