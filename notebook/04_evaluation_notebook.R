
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

setwd(here::here())

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
  mutate(forecast_horizon = "a 1",
         rmse = RMSE(forecast_mean,observed))

cram_3dy <- cram_forecasts %>%
  filter(days == 3)%>%
  mutate(forecast_horizon = "b 3",
         rmse = RMSE(forecast_mean,observed))

cram_7dy <- cram_forecasts %>%
  filter(days == 7)%>%
  mutate(forecast_horizon = "c 7",
         rmse = RMSE(forecast_mean,observed))

cram_14dy <- cram_forecasts %>%
  filter(days == 14)%>%
  mutate(forecast_horizon = "d 14",
         rmse = RMSE(forecast_mean,observed))

cram_21dy <- cram_forecasts %>%
  filter(days == 21)%>%
  mutate(forecast_horizon = "e 21",
         rmse = RMSE(forecast_mean,observed))

cram_28dy <- cram_forecasts %>%
  filter(days == 28)%>%
  mutate(forecast_horizon = "f 28",
         rmse = RMSE(forecast_mean,observed))

cram_35dy <- cram_forecasts %>%
  filter(days == 35)%>%
  mutate(forecast_horizon = "f 35",
         rmse = RMSE(forecast_mean,observed))

cram_eval <- bind_rows(cram_1dy, cram_3dy, cram_7dy, cram_14dy, cram_21dy, cram_28dy, cram_35dy)%>%
  mutate(siteID = "C: CRAM")

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
  mutate(forecast_horizon = "a 1",
         rmse = RMSE(forecast_mean,observed))

liro_3dy <- liro_forecasts %>%
  filter(days == 3)%>%
  mutate(forecast_horizon = "b 3",
         rmse = RMSE(forecast_mean,observed))

liro_7dy <- liro_forecasts %>%
  filter(days == 7)%>%
  mutate(forecast_horizon = "c 7",
         rmse = RMSE(forecast_mean,observed))

liro_14dy <- liro_forecasts %>%
  filter(days == 14)%>%
  mutate(forecast_horizon = "d 14",
         rmse = RMSE(forecast_mean,observed))

liro_21dy <- liro_forecasts %>%
  filter(days == 21)%>%
  mutate(forecast_horizon = "e 21",
         rmse = RMSE(forecast_mean,observed))

liro_28dy <- liro_forecasts %>%
  filter(days == 28)%>%
  mutate(forecast_horizon = "f 28",
         rmse = RMSE(forecast_mean,observed))

liro_35dy <- liro_forecasts %>%
  filter(days == 35)%>%
  mutate(forecast_horizon = "f 35",
         rmse = RMSE(forecast_mean,observed))

liro_eval <- bind_rows(liro_1dy,liro_3dy, liro_7dy, liro_14dy, liro_21dy, liro_28dy, liro_35dy)%>%
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

barc_1dy <- barc_forecasts %>%
  filter(days == 1)%>%
  mutate(forecast_horizon = "a 1",
         rmse = RMSE(forecast_mean,observed))

barc_3dy <- barc_forecasts %>%
  filter(days == 3)%>%
  mutate(forecast_horizon = "b 3",
         rmse = RMSE(forecast_mean,observed))

barc_7dy <- barc_forecasts %>%
  filter(days == 7)%>%
  mutate(forecast_horizon = "c 7",
         rmse = RMSE(forecast_mean,observed))

barc_14dy <- barc_forecasts %>%
  filter(days == 14)%>%
  mutate(forecast_horizon = "d 14",
         rmse = RMSE(forecast_mean,observed))

barc_21dy <- barc_forecasts %>%
  filter(days == 21)%>%
  mutate(forecast_horizon = "e 21",
         rmse = RMSE(forecast_mean,observed))

barc_28dy <- barc_forecasts %>%
  filter(days == 28)%>%
  mutate(forecast_horizon = "f 28",
         rmse = RMSE(forecast_mean,observed))

barc_35dy <- barc_forecasts %>%
  filter(days == 35)%>%
  mutate(forecast_horizon = "f 35",
         rmse = RMSE(forecast_mean,observed))

barc_eval <- bind_rows(barc_1dy,barc_3dy, barc_7dy, barc_14dy, barc_21dy, barc_28dy, barc_35dy)%>%
  mutate(siteID = "E: BARC")

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
  mutate(forecast_horizon = "a 1",
         rmse = RMSE(forecast_mean,observed))

sugg_3dy <- sugg_forecasts %>%
  filter(days == 3)%>%
  mutate(forecast_horizon = "b 3",
         rmse = RMSE(forecast_mean,observed))

sugg_7dy <- sugg_forecasts %>%
  filter(days == 7)%>%
  mutate(forecast_horizon = "c 7",
         rmse = RMSE(forecast_mean,observed))

sugg_14dy <- sugg_forecasts %>%
  filter(days == 14)%>%
  mutate(forecast_horizon = "d 14",
         rmse = RMSE(forecast_mean,observed))

sugg_21dy <- sugg_forecasts %>%
  filter(days == 21)%>%
  mutate(forecast_horizon = "e 21",
         rmse = RMSE(forecast_mean,observed))

sugg_28dy <- sugg_forecasts %>%
  filter(days == 28)%>%
  mutate(forecast_horizon = "f 28",
         rmse = RMSE(forecast_mean,observed))

sugg_35dy <- sugg_forecasts %>%
  filter(days == 35)%>%
  mutate(forecast_horizon = "f 35",
         rmse = RMSE(forecast_mean,observed))

sugg_eval <- bind_rows(sugg_1dy, sugg_3dy, sugg_7dy, sugg_14dy, sugg_21dy, sugg_28dy, sugg_35dy)%>%
  mutate(siteID = "F: SUGG")

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

prpo_1dy <- prpo_forecasts %>%
  filter(days == 1)%>%
  mutate(forecast_horizon = "a 1",
         rmse = RMSE(forecast_mean,observed))

prpo_3dy <- prpo_forecasts %>%
  filter(days == 3)%>%
  mutate(forecast_horizon = "b 3",
         rmse = RMSE(forecast_mean,observed))

prpo_7dy <- prpo_forecasts %>%
  filter(days == 7)%>%
  mutate(forecast_horizon = "c 7",
         rmse = RMSE(forecast_mean,observed))

prpo_14dy <- prpo_forecasts %>%
  filter(days == 14)%>%
  mutate(forecast_horizon = "d 14",
         rmse = RMSE(forecast_mean,observed))

prpo_21dy <- prpo_forecasts %>%
  filter(days == 21)%>%
  mutate(forecast_horizon = "e 21",
         rmse = RMSE(forecast_mean,observed))

prpo_28dy <- prpo_forecasts %>%
  filter(days == 28)%>%
  mutate(forecast_horizon = "f 28",
         rmse = RMSE(forecast_mean,observed))

prpo_35dy <- prpo_forecasts %>%
  filter(days == 35)%>%
  mutate(forecast_horizon = "f 35",
         rmse = RMSE(forecast_mean,observed))

prpo_eval <- bind_rows(prpo_1dy, prpo_3dy, prpo_7dy, prpo_14dy, prpo_21dy, prpo_28dy, prpo_35dy)%>%
  mutate(siteID = "B: PRPO")

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
  mutate(forecast_horizon = "a 1",
         rmse = RMSE(forecast_mean,observed))

prla_3dy <- prla_forecasts %>%
  filter(days == 3)%>%
  mutate(forecast_horizon = "b 3",
         rmse = RMSE(forecast_mean,observed))

prla_7dy <- prla_forecasts %>%
  filter(days == 7)%>%
  mutate(forecast_horizon = "c 7",
         rmse = RMSE(forecast_mean,observed))

prla_14dy <- prla_forecasts %>%
  filter(days == 14)%>%
  mutate(forecast_horizon = "d 14",
         rmse = RMSE(forecast_mean,observed))

prla_21dy <- prla_forecasts %>%
  filter(days == 21)%>%
  mutate(forecast_horizon = "e 21",
         rmse = RMSE(forecast_mean,observed))

prla_28dy <- prla_forecasts %>%
  filter(days == 28)%>%
  mutate(forecast_horizon = "f 28",
         rmse = RMSE(forecast_mean,observed))

prla_35dy <- prla_forecasts %>%
  filter(days == 35)%>%
  mutate(forecast_horizon = "f 35",
         rmse = RMSE(forecast_mean,observed))

prla_eval <- bind_rows(prla_1dy, prla_3dy, prla_7dy, prla_14dy, prla_21dy, prla_28dy, prla_35dy)%>%
  mutate(siteID = "A: PRLA")

northern_plains_domain <- bind_rows(prpo_eval, prla_eval)%>%
  mutate(domain = "NRP")


### FIGS ###

library(patchwork)

gl_fig <- ggplot(great_lakes_domain, aes(forecast_horizon, forecast_sd, fill = domain))+
  geom_boxplot(outlier.colour="black", outlier.shape=21,
               outlier.size=2, notch=FALSE, outlier.fill = "grey")+
  ylab("Forecast SD")+
  xlab("")+
  labs(title = "Great Lakes (46.113892, -89.591314)")+
  scale_y_continuous(breaks = c(0,1,2,3,4), limits = c(0,4))+
  scale_fill_manual(values=c("darkorange"))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y =element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 20, color = "black"))+
  facet_wrap(~siteID)

se_fig <- ggplot(south_east_domain, aes(forecast_horizon, forecast_sd, fill = domain))+
  geom_boxplot(outlier.colour="black", outlier.shape=21,
               outlier.size=2, notch=FALSE, outlier.fill = "grey")+
  ylab("")+
  xlab("Forecast Horizon (days)")+
  labs(title = "Southeast (29.706884, -81.977121)")+
  scale_y_continuous(breaks = c(0,1,2,3,4), limits = c(0,4))+
  scale_fill_manual(values=c("darkviolet"))+
  scale_x_discrete(labels = c(
    "a 1" = "1",
    "b 3" = "3",
    "c 7" = "7",
    "d 14" = "14",
    "e 21" = "21",
    "f 28" = "28",
    "f 35" = "35"))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 20, color = "black"))+
  facet_wrap(~siteID)

np_fig <- ggplot(northern_plains_domain, aes(forecast_horizon, forecast_sd, fill = domain))+
geom_boxplot(outlier.colour="black", outlier.shape=21,
             outlier.size=2, notch=FALSE, outlier.fill = "grey")+
  ylab("")+
  xlab("")+
  labs(title = "Northern Plains (47.210446, -99.273900)")+
  scale_y_continuous(breaks = c(0,1,2,3,4), limits = c(0,4))+
  scale_fill_manual(values=c("coral1"))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 20, color = "black"))+
  facet_wrap(~siteID)

fig1 <- np_fig/gl_fig/se_fig
fig1
ggsave(path = ".", filename = "forecast_output/figures/across_site_sd.jpg", width = 7, height = 10, device='jpg', dpi=1000)


gl_fig_rmse <- ggplot(great_lakes_domain, aes(forecast_horizon, rmse, fill = domain))+
  geom_boxplot(outlier.colour="black", outlier.shape=21,
               outlier.size=2, notch=FALSE, outlier.fill = "grey")+
  ylab("Forecast RMSE")+
  xlab("")+
  labs(title = "Great Lakes (46.113892, -89.591314)")+
  scale_y_continuous(breaks = c(0,1,2,3,4), limits = c(0,4))+
  scale_fill_manual(values=c("darkorange"))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y =element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 20, color = "black"))+
  facet_wrap(~siteID)

se_fig_rmse <- ggplot(south_east_domain, aes(forecast_horizon, rmse, fill = domain))+
  geom_boxplot(outlier.colour="black", outlier.shape=21,
               outlier.size=2, notch=FALSE, outlier.fill = "grey")+
  ylab("")+
  xlab("Forecast Horizon (days)")+
  labs(title = "Southeast (29.706884, -81.977121)")+
  scale_y_continuous(breaks = c(0,1,2,3,4), limits = c(0,4))+
  scale_fill_manual(values=c("darkviolet"))+
  scale_x_discrete(labels = c(
    "a 1" = "1",
    "b 3" = "3",
    "c 7" = "7",
    "d 14" = "14",
    "e 21" = "21",
    "f 28" = "28",
    "f 35" = "35"))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 20, color = "black"))+
  facet_wrap(~siteID)

np_fig_rmse <- ggplot(northern_plains_domain, aes(forecast_horizon, rmse, fill = domain))+
  geom_boxplot(outlier.colour="black", outlier.shape=21,
               outlier.size=2, notch=FALSE, outlier.fill = "grey")+
  ylab("")+
  xlab("")+
  labs(title = "Northern Plains (47.210446, -99.273900)")+
  scale_y_continuous(breaks = c(0,1,2,3,4), limits = c(0,4))+
  scale_fill_manual(values=c("coral1"))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 20, color = "black"))+
  facet_wrap(~siteID)

fig2 <- np_fig_rmse/gl_fig_rmse/se_fig_rmse
fig2

ggsave(path = ".", filename = "forecast_output/figures/across_site_rmse.jpg", width = 7, height = 10, device='jpg', dpi=1000)





cram_rmse <- list.files(cram_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(cram_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))%>%
  group_by(days)%>%
  summarise(rmse = RMSE(forecast_mean, observed),
            sd = mean(forecast_sd, na.rm = T))%>%
  mutate(siteID = "C: CRAM")

liro_rmse <- list.files(liro_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(liro_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))%>%
  group_by(days)%>%
  summarise(rmse = RMSE(forecast_mean, observed),
            sd = mean(forecast_sd, na.rm = T))%>%
  mutate(siteID = "D: LIRO")

prla_rmse <- list.files(prla_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(prla_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))%>%
  group_by(days)%>%
  summarise(rmse = RMSE(forecast_mean, observed),
            sd = mean(forecast_sd, na.rm = T))%>%
  mutate(siteID = "A: PRLA")

prpo_rmse <- list.files(prpo_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(prpo_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))%>%
  group_by(days)%>%
  summarise(rmse = RMSE(forecast_mean, observed),
            sd = mean(forecast_sd, na.rm = T))%>%
  mutate(siteID = "B: PRPO")

barc_rmse <- list.files(barc_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(barc_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))%>%
  group_by(days)%>%
  summarise(rmse = RMSE(forecast_mean, observed),
            sd = mean(forecast_sd, na.rm = T))%>%
  mutate(siteID = "E: BARC")

sugg_rmse <- list.files(sugg_path, pattern = ".csv")%>%
  map_df(~ read_csv(file.path(sugg_path, .))) %>%
  filter(depth == 0.5)%>%
  group_by(forecast_start_day)%>%
  filter(date >= forecast_start_day)%>%
  mutate(days = seq_along(date))%>%
  group_by(days)%>%
  summarise(rmse = RMSE(forecast_mean, observed),
            sd = mean(forecast_sd, na.rm = T))%>%
  mutate(siteID = "F: SUGG")


rmse_fig_1 <- bind_rows(barc_rmse, sugg_rmse, cram_rmse, liro_rmse, prla_rmse, prpo_rmse) %>%
  filter(days <= 35) %>%
  ggplot(., aes(x = days, y = rmse, color = siteID, group = siteID))+
  geom_line(lwd = 2)+
  ylab("Forecast RMSE")+
  xlab("Forecast Horizon (days)")+
  labs(title = "1 to 35-day forecast horizon RMSE")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        plot.title = element_text(size = 20, color = "black"))+
  viridis::scale_color_viridis(option = "C", discrete = T)

rmse_fig_2 <- bind_rows(barc_rmse, sugg_rmse, cram_rmse, liro_rmse, prla_rmse, prpo_rmse) %>%
  filter(days <= 35) %>%
  ggplot(., aes(x = days, y = sd, color = siteID, group = siteID))+
  geom_line(lwd = 2)+
  ylab("Forecast SD")+
  xlab("Forecast Horizon (days)")+
  labs(title = "1 to 35-day forecast horizon SD")+
  theme_classic()+
  theme(legend.position = c(0.1,0.85),
        axis.text.x = element_text(size = 15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        plot.title = element_text(size = 20, color = "black"))+
  viridis::scale_color_viridis(option = "C", discrete = T)

rmse_together <- rmse_fig_2+rmse_fig_1
rmse_together

ggsave(path = ".", filename = "forecast_output/figures/aggregated_rmse_sd.jpg", width = 20, height = 8, device='jpg', dpi=1000)

