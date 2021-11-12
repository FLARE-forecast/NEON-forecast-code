lake_directory <- here::here()

scores_files <- fs::dir_ls(file.path(lake_directory,"scores"), type="file", recurse = TRUE)
combined <- readr::read_csv(scores_files, progress = FALSE) %>%
  rename("siteID" = theme) %>%
  rename("depth_bins" = depth) %>%
  mutate(lower = as.numeric( sub("\\((.+),.*", "\\1", depth_bins) ),
         upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", depth_bins)),
         depth = (lower + upper)/2)


  combined %>%
  #filter(siteID %in% sites[-2]) %>%
  filter(target == "temperature",
         horizon == 7,
         depth < 2) %>%
  group_by(time, siteID, team, depth) %>%
  #slice_min(horizon) %>%
  ggplot() +
  geom_line(aes(time, mean, col = team)) +
  geom_point(aes(time, observed)) +
  geom_ribbon(aes(x = time, ymin = lower95, ymax = upper95,
                              fill = team),
                          alpha = 0.2) +
  labs(y = "temperature") +
  facet_wrap(depth~siteID, ncol=2) +
  theme(axis.text.x = element_text( angle = 90, hjust = 0.5, vjust = 0.5)) +
  ggtitle("7th day ahead forecast")

  combined %>%
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

  combined %>%
    group_by(target, team, horizon) %>%  # average over siteID
    summarise(mean_crps = mean(crps, na.rm =TRUE),
              mean_logs = mean(logs, na.rm = TRUE),
              .groups = "drop") %>%
    pivot_longer(cols = c("mean_crps", "mean_logs"),
                 names_to = "metric", values_to="score") %>%
    filter(metric == "mean_crps") %>%
    filter(!is.na(score)) %>%
    ggplot(aes(horizon, score, col=team)) +
    geom_line() +
    geom_point() +
    labs(x = "score", title = "score by horizon")




combined %>%
  filter(issue_date == "2021-05-18",
         depth < 2) %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = crps, color = team)) +
  #geom_point(aes(y = observed)) +
  facet_grid(depth~siteID) +
  theme_bw()
