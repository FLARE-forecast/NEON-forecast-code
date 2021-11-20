lake_directory <- here::here()

#Read in scored forecasts
scores_files <- fs::dir_ls(file.path(lake_directory,"scores"), type="file",recurse = TRUE)

scores_files <- scores_files[stringr::str_detect(scores_files, "ms_")]

combined <- readr::read_csv(scores_files, progress = FALSE) %>%
  rename("siteID" = theme) %>%
  mutate(resid = observed - mean) %>%
  select(siteID, team, issue_date, time, forecast_start_time, horizon, target,depth, mean, sd, observed, crps, logs, resid, quantile10, quantile90)

skill_logs_table <- combined %>%
  filter(team != "ms_persistence") %>%
  select(siteID, team, issue_date, time, forecast_start_time, horizon, target,depth, logs) %>%
  pivot_wider(names_from = team, values_from = logs, values_fill = NA) %>%
  na.omit() %>%
  mutate(skill = 1 - (ms_glm_flare/ms_climatology)) %>%
  group_by(target, horizon, depth, siteID, forecast_start_time) %>%  # average over siteID
  summarise(skill = mean(skill, na.rm =TRUE),
            .groups = "drop") %>%
  mutate(team = "ms_glm_flare") %>%
  select(siteID, team, forecast_start_time, horizon, target, depth, skill) %>%
  filter(horizon %in% c(1,5,10, 7,14,21,28,34),
         depth <= 2.0) %>%
  group_by(siteID, horizon) %>%
  summarize(skill = median(100 * skill, na.rm = TRUE)) %>%
  pivot_wider(names_from = siteID, values_from = skill)


skill_mae_table <- combined %>%
  filter(team != "ms_presistence") %>%
  mutate(abs_error = abs(resid)) %>%
  select(siteID, team, issue_date, time, forecast_start_time, horizon, target,depth, abs_error) %>%
  pivot_wider(names_from = team, values_from = abs_error, values_fill = NA) %>%
  na.omit() %>%
  mutate(skill = 1 - (ms_glm_flare/ms_climatology)) %>%
  group_by(target, horizon, depth, siteID, forecast_start_time) %>%  # average over siteID
  summarise(skill = mean(skill, na.rm =TRUE),
            .groups = "drop") %>%
  mutate(team = "ms_glm_flare") %>%
  select(siteID, team, forecast_start_time, horizon, target, depth, skill) %>%
  filter(horizon %in% c(1,5,7,14,21,28,34),
         depth <= 2.0) %>%
  group_by(siteID, horizon) %>%
  summarize(skill = median(100 * skill, na.rm = TRUE)) %>%
  pivot_wider(names_from = siteID, values_from = skill)

coverage80 <- combined %>%
  mutate(in80 = ifelse(observed > quantile10 & observed < quantile90, 1, 0)) %>%
  mutate(obs_exits = ifelse(!is.na(observed), 1, 0)) %>%
  group_by(team) %>%
  summarize(in80 = sum(in80, na.rm = TRUE),
            total = sum(obs_exits)) %>%
  mutate(prop = in80/total)


combined %>%
  filter(target == "temperature",
         horizon == 30,
         team != "ms_persistence",
         depth <= 6) %>%
  #group_by(time, siteID, team, depth) %>%
  ggplot() +
  geom_line(aes(time, mean, col = team)) +
  geom_point(aes(time, observed)) +
  geom_ribbon(aes(x = time, ymin = quantile10, ymax = quantile90,
                  fill = team),
              alpha = 0.2) +
  labs(y = "temperature") +
  facet_grid(depth~siteID) +
  theme(axis.text.x = element_text( angle = 90, hjust = 0.5, vjust = 0.5)) +
  ggtitle("30 day ahead forecast")

combined %>%
  filter(team != "ms_persistence") %>%
  group_by(target, team, horizon) %>%  # average over siteID
  summarise(mean_crps = mean(crps, na.rm =TRUE),
            mean_logs = mean(logs, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(cols = c("mean_crps", "mean_logs"),
               names_to = "metric", values_to="score") %>%
  filter(metric == "mean_logs") %>%
  filter(!is.na(score)) %>%
  ggplot(aes(horizon, score, col=team)) +
  geom_line() +
  geom_point() +
  coord_trans(y='log2') +
  labs(x = "score", title = "score by horizon")

matching_crps <- combined %>%
  filter(team != "ms_persistence") %>%
  select(siteID, team, issue_date, time, forecast_start_time, horizon, target,depth, crps) %>%
  pivot_wider(names_from = team, values_from = crps, values_fill = NA) %>%
  na.omit() %>%
  pivot_longer(cols = -c("siteID", "issue_date", "time", "forecast_start_time", "horizon", "target","depth"), names_to = "team",values_to = "crps") %>%
  group_by(target, team, horizon, depth) %>%  # average over siteID
  summarise(mean_crps = mean(crps, na.rm =TRUE),
            .groups = "drop") %>%
  filter(depth == 1) %>%
  ggplot(aes(horizon, mean_crps, col=team)) +
  geom_line() +
  geom_point() +
  labs(x = "score", title = "score by horizon")

matching_logs <- combined %>%
  filter(team != "ms_persistence") %>%
  select(siteID, team, issue_date, time, forecast_start_time, horizon, target,depth, logs) %>%
  pivot_wider(names_from = team, values_from = logs, values_fill = NA) %>%
  na.omit() %>%
  pivot_longer(cols = -c("siteID", "issue_date", "time", "forecast_start_time", "horizon", "target","depth"), names_to = "team",values_to = "logs") %>%
  group_by(target, team, horizon, depth) %>%  # average over siteID
  summarise(mean_logs = mean(logs, na.rm =TRUE),
            .groups = "drop") %>%
  filter(depth <= 2) %>%
  ggplot(aes(horizon, mean_logs, col=team)) +
  geom_line() +
  geom_point() +
  coord_trans(y='log2') +
  labs(x = "score", title = "score by horizon") +
  facet_wrap(~depth)

matching_rmse <- combined %>%
  filter(team != "ms_persistence") %>%
  mutate(sq_error = (resid)^2) %>%
  select(siteID, team, issue_date, time, forecast_start_time, horizon, target,depth, sq_error) %>%
  pivot_wider(names_from = team, values_from = sq_error, values_fill = NA) %>%
  na.omit() %>%
  pivot_longer(cols = -c("siteID", "issue_date", "time", "forecast_start_time", "horizon", "target","depth"), names_to = "team",values_to = "sq_error") %>%
  group_by(target, team, horizon, depth, siteID) %>%  # average over siteID
  summarise(rmse = mean(sq_error, na.rm =TRUE),
            .groups = "drop") %>%
  mutate(rmse = sqrt(rmse)) %>%
  filter(depth <= 6) %>%
  ggplot(aes(horizon, rmse, col=team)) +
  geom_line() +
  geom_point() +
  labs(x = "score", title = "score by horizon") +
  geom_hline(yintercept = 1.48) + #https://www.doi.org/10.3389/fenvs.2021.707874
  facet_grid(depth~siteID)

matching_skill <- combined %>%
  filter(team != "ms_presistence") %>%
  select(siteID, team, issue_date, time, forecast_start_time, horizon, target,depth, logs) %>%
  pivot_wider(names_from = team, values_from = logs, values_fill = NA) %>%
  na.omit() %>%
  mutate(skill = 1 - (ms_glm_flare/ms_climatology)) %>%
  group_by(target, horizon, depth, siteID) %>%  # average over siteID
  summarise(skill = mean(skill, na.rm =TRUE),
            .groups = "drop") %>%
  filter(depth >= 1 & depth <= 2) %>%
  ggplot(aes(horizon, skill)) +
  geom_smooth() +
  geom_point() +
  labs(x = "skill", title = "score by horizon") +
  geom_hline(yintercept = 0) +
  facet_grid(depth~siteID)

combined %>%
  filter(issue_date == "2021-06-08",
         depth == 1,
         team != "ms_persistence") %>%
  ggplot(aes(x = horizon)) +
  geom_line(aes(y = logs, color = team)) +
  #geom_point(aes(y = observed)) +
  facet_grid(depth~siteID) +
  theme_bw()
