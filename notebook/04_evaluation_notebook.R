
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
  mutate(forecast_horizon = "a 1")

cram_3dy <- cram_forecasts %>%
  filter(days == 3)%>%
  mutate(forecast_horizon = "b 3")

cram_7dy <- cram_forecasts %>%
  filter(days == 7)%>%
  mutate(forecast_horizon = "c 7")

cram_14dy <- cram_forecasts %>%
  filter(days == 14)%>%
  mutate(forecast_horizon = "d 14")

cram_21dy <- cram_forecasts %>%
  filter(days == 21)%>%
  mutate(forecast_horizon = "e 21")

cram_28dy <- cram_forecasts %>%
  filter(days == 28)%>%
  mutate(forecast_horizon = "f 28")

cram_eval <- bind_rows(cram_1dy, cram_3dy, cram_7dy, cram_14dy, cram_21dy, cram_28dy)%>%
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
  mutate(forecast_horizon = "a 1")

liro_3dy <- liro_forecasts %>%
  filter(days == 3)%>%
  mutate(forecast_horizon = "b 3")

liro_7dy <- liro_forecasts %>%
  filter(days == 7)%>%
  mutate(forecast_horizon = "c 7")

liro_14dy <- liro_forecasts %>%
  filter(days == 14)%>%
  mutate(forecast_horizon = "d 14")

liro_21dy <- liro_forecasts %>%
  filter(days == 21)%>%
  mutate(forecast_horizon = "e 21")

liro_28dy <- liro_forecasts %>%
  filter(days == 27)%>%
  mutate(forecast_horizon = "f 28")

liro_eval <- bind_rows(liro_1dy,liro_3dy, liro_7dy, liro_14dy, liro_21dy, liro_28dy)%>%
  mutate(siteID = "D: LIRO")

great_lakes_domain <- bind_rows(cram_eval, liro_eval)%>%
  mutate(domain = "GRL")

gl_fig <- ggplot(great_lakes_domain, aes(forecast_horizon, forecast_sd, fill = domain))+
  geom_boxplot(outlier.colour="black", outlier.shape=21,
               outlier.size=2, notch=FALSE, outlier.fill = "grey")+
  ylab("Forecast SD")+
  xlab("")+
  labs(title = "Great Lakes")+
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
  mutate(forecast_horizon = "a 1")

barc_3dy <- barc_forecasts %>%
  filter(days == 3)%>%
  mutate(forecast_horizon = "b 3")

barc_7dy <- barc_forecasts %>%
  filter(days == 7)%>%
  mutate(forecast_horizon = "c 7")

barc_14dy <- barc_forecasts %>%
  filter(days == 14)%>%
  mutate(forecast_horizon = "d 14")

barc_21dy <- barc_forecasts %>%
  filter(days == 21)%>%
  mutate(forecast_horizon = "e 21")

barc_28dy <- barc_forecasts %>%
  filter(days == 28)%>%
  mutate(forecast_horizon = "f 28")

barc_eval <- bind_rows(barc_1dy,barc_3dy, barc_7dy, barc_14dy, barc_21dy, barc_28dy)%>%
  mutate(siteID = "A: BARC")

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
  mutate(forecast_horizon = "a 1")

sugg_3dy <- sugg_forecasts %>%
  filter(days == 3)%>%
  mutate(forecast_horizon = "b 3")

sugg_7dy <- sugg_forecasts %>%
  filter(days == 7)%>%
  mutate(forecast_horizon = "c 7")

sugg_14dy <- sugg_forecasts %>%
  filter(days == 14)%>%
  mutate(forecast_horizon = "d 14")

sugg_21dy <- sugg_forecasts %>%
  filter(days == 21)%>%
  mutate(forecast_horizon = "e 21")

sugg_28dy <- sugg_forecasts %>%
  filter(days == 28)%>%
  mutate(forecast_horizon = "f 28")

sugg_eval <- bind_rows(sugg_1dy, sugg_3dy, sugg_7dy, sugg_14dy, sugg_21dy, sugg_28dy)%>%
  mutate(siteID = "B: SUGG")

south_east_domain <- bind_rows(barc_eval, sugg_eval)%>%
  mutate(domain = "SOE")

se_fig <- ggplot(south_east_domain, aes(forecast_horizon, forecast_sd, fill = domain))+
  geom_boxplot(outlier.colour="black", outlier.shape=21,
               outlier.size=2, notch=FALSE, outlier.fill = "grey")+
  ylab("")+
  xlab("")+
  labs(title = "Southeast")+
  scale_y_continuous(breaks = c(0,1,2,3,4), limits = c(0,4))+
  scale_fill_manual(values=c("darkviolet"))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y =element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 20, color = "black"))+
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
  mutate(forecast_horizon = "a 1")

prpo_3dy <- prpo_forecasts %>%
  filter(days == 3)%>%
  mutate(forecast_horizon = "b 3")

prpo_7dy <- prpo_forecasts %>%
  filter(days == 7)%>%
  mutate(forecast_horizon = "c 7")

prpo_14dy <- prpo_forecasts %>%
  filter(days == 14)%>%
  mutate(forecast_horizon = "d 14")

prpo_21dy <- prpo_forecasts %>%
  filter(days == 21)%>%
  mutate(forecast_horizon = "e 21")

prpo_28dy <- prpo_forecasts %>%
  filter(days == 28)%>%
  mutate(forecast_horizon = "f 28")

prpo_eval <- bind_rows(prpo_1dy, prpo_3dy, prpo_7dy, prpo_14dy, prpo_21dy, prpo_28dy)%>%
  mutate(siteID = "F: PRPO")

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
  mutate(forecast_horizon = "a 1")

prla_3dy <- prla_forecasts %>%
  filter(days == 3)%>%
  mutate(forecast_horizon = "b 3")

prla_7dy <- prla_forecasts %>%
  filter(days == 7)%>%
  mutate(forecast_horizon = "c 7")

prla_14dy <- prla_forecasts %>%
  filter(days == 14)%>%
  mutate(forecast_horizon = "d 14")

prla_21dy <- prla_forecasts %>%
  filter(days == 21)%>%
  mutate(forecast_horizon = "e 21")

prla_28dy <- prla_forecasts %>%
  filter(days == 28)%>%
  mutate(forecast_horizon = "f 28")

prla_eval <- bind_rows(prla_1dy, prla_3dy, prla_7dy, prla_14dy, prla_21dy, prla_28dy)%>%
  mutate(siteID = "E: PRLA")

northern_plains_domain <- bind_rows(prpo_eval, prla_eval)%>%
  mutate(domain = "NRP")


np_fig <- ggplot(northern_plains_domain, aes(forecast_horizon, forecast_sd, fill = domain))+
geom_boxplot(outlier.colour="black", outlier.shape=21,
             outlier.size=2, notch=FALSE, outlier.fill = "grey")+
  ylab("")+
  xlab("Forecast Horizon (days)")+
  labs(title = "Northern Plains")+
  scale_y_continuous(breaks = c(0,1,2,3,4), limits = c(0,4))+
  scale_fill_manual(values=c("coral1"))+
  scale_x_discrete(labels = c(
    "a 1" = "1",
    "b 3" = "3",
    "c 7" = "7",
    "d 14" = "14",
    "e 21" = "21",
    "f 28" = "28"))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        plot.title = element_text(size = 20, color = "black"))+
  facet_wrap(~siteID)

fig2 <- se_fig/gl_fig/np_fig
fig2
ggsave(path = ".", filename = "forecast_output/figures/across_site_sd.jpg", width = 7, height = 10, device='jpg', dpi=1000)
