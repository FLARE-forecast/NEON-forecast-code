

sites <- c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG")
neonstore::neon_download(product = "DP1.20252.001", site = sites, start_date = NA)

d <- neonstore::neon_index(product = "DP1.20252.001")
neonstore::neon_store(table = "dep_secchi-basic")

d <- neonstore::neon_table(table = "dep_secchi-basic")

ggplot(d, aes(date, 1.7/ as.numeric(secchiMeanDepth))) +
  geom_line() +
  facet_wrap(~siteID, scale = "free")

median_secchi <- d %>%
  filter(date < as_date("2021-04-18")) %>%
  mutate(secchiMeanDepth = as.numeric(secchiMeanDepth)) %>%
  group_by(siteID) %>%
  summarize(secchi = mean(secchiMeanDepth, na.rm= TRUE))

kw <- median_secchi %>%
  mutate(kw = 1.7 / secchi)
