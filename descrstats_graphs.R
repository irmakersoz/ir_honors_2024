### Stanford University ###
### IR Honors Thesis ###
##### 2023-2024 #####
#### Irmak Ersoz #### 

# Descriptive Statistics: Replication Code


# Load necessary libraries
library(dplyr)
library(tidyverse)
library(ggplot2)

ido_counts <- as.data.frame(table(ppd_wgi_merge$donor))

colnames(ido_counts) <- c("IDO", "count")


get_random_color <- function() {
  rgb(runif(1), runif(1), runif(1))
}

# Create a bar graph with random colors
ggplot(ido_counts, aes(x = IDO, y = count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Bar Graph of Project Counts by IDO",
       x = "IDO",
       y = "Count")
ido_counts$IDO <- factor(ido_counts$IDO, levels = ido_counts$IDO[order(ido_counts$count, decreasing = TRUE)])

# Create a bar graph with all bars in light blue and ordered from highest to lowest
ggplot(ido_counts, aes(x = IDO, y = count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +
  labs(title = "Bar Graph of Project Counts by IDO",
       x = "IDO",
       y = "Count")

sector_by_ido <- as.data.frame(table(ppd_wgi_merge$donor, ppd_wgi_merge$pseudo_sector))

colnames(sector_by_ido) <- c("donor", "sector", "count")
library(dplyr)

region_counts <- as.data.frame(table(ppd_wgi_merge$region))
colnames(region_counts) <- c("region", "count")
region_counts$region <- factor(region_counts$region, levels = region_counts$region[order(region_counts$count, decreasing = TRUE)])

ggplot(data = region_counts, aes(x = region, y = count)) +
  geom_bar(stat= "identity", fill = "lightblue") +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +
  labs(x = "Region",
       y = "Project Count")

region_financing <- as.data.frame(unique(ppd_wgi_merge$region))
region_financing$budget <- financing

financing <- c(90945576913, 82120128109, 30026955607, 95730014677, 106785827203, 121924660527, NA, 0)

colnames(region_financing) <- c("region", "budget")
region_financing <- na.omit(region_financing)

region_financing$budget <- region_financing$budget / 1000000000

region_financing$region <- factor(region_financing$region, levels = region_financing$region[order(region_financing$budget, decreasing = TRUE)])
ggplot(data = region_financing, aes(x = region, y = budget)) +
  geom_bar(stat= "identity", fill = "lightblue") +
  geom_text(aes(label = budget), vjust = -0.5, color = "black") +
  labs(x = "Region",
       y = "Financing (in $b)")


donor_budgets$budget <- donor_budgets$budget / 1000000000

ggplot(data = donor_budgets, aes(x = donor, y = budget)) +
  geom_bar(stat= "identity", fill = "lightblue") +
  geom_text(aes(label = budget), vjust = -0.5, color = "black") +
  labs(x = "IDO",
       y = "Financing (in $b)")


ggplot(data = ppd_wgi_merge, aes(x = six_overall_rating)) +
  geom_histogram(bins = 12, fill = "lightblue", color = "black", size = 0.1) +
  labs(x = "Performance Score (0-6)",
       y = "Count")


# Create a summary data frame with median and mean values for each year
pdo_year_summary <- ppd_wgi_merge %>%
  group_by(startyear) %>%
  summarize(median_score = median(six_overall_rating),
            mean_score = mean(six_overall_rating))

# Plot the data using ggplot2
ggplot(pdo_year_summary, aes(x = as.factor(startyear))) +
  geom_line(aes(y = median_score, color = "Median", group = 1), size = 1.5) +
  geom_line(aes(y = mean_score, color = "Mean", group = 1), size = 1.5) +
  labs(x = "Start Year",
       y = "Performance Score") +
  scale_color_manual(values = c("Median" = "lightblue", "Mean" = "darkblue")) +
  theme_minimal() +
  ylim(0,5)


pdo_region_summary <- ppd_wgi_merge %>%
  group_by(region) %>%
  summarize(median_score = median(six_overall_rating),
            mean_score = mean(six_overall_rating))

pdo_region_summary <- pdo_region_summary%>% 
  filter(!is.na(region))

pdo_region_stdev <- ppd_wgi_merge %>%
  group_by(region) %>%
  summarize(standard_dev = sd(six_overall_rating))

pdo_region_stdev <- pdo_region_stdev %>%
  filter(!is.na(region))

pdo_region_stdev <- pdo_region_stdev %>%
  filter(!is.na(standard_dev))

pdo_region_summary$region <- factor(pdo_region_summary$region, levels = pdo_region_summary$region[order(pdo_region_summary$median_score, decreasing = T)])

ggplot(pdo_region_summary, aes(x = as.factor(region))) +
  geom_bar(aes(y = median_score), stat = "identity", fill = "lightblue") +
  geom_bar(aes(y = mean_score), stat = "identity", fill = "darkblue") +
  labs(x = "Region",
       y = "Performance Score")


ggplot(pdo_region_summary, aes(x = as.factor(region))) +
  geom_col(aes(y = median_score, fill = "Median"), position = "dodge", color = "black") +
  geom_col(aes(y = mean_score, fill = "Mean"), position = "dodge", color = "black") +
  labs(x = "Region",
       y = "Performance Score") +
  scale_fill_manual(values = c("Median" = "lightblue", "Mean" = "darkblue")) +
  theme_minimal()


ggplot(pdo_region_summary, aes(x = as.factor(region))) +
  geom_col(aes(y = median_score, fill = "Median"), position =  position_nudge(x = -0.2), width = 0.4) +
  geom_col(aes(y = mean_score, fill = "Mean"), position =  position_nudge(x = 0.2), width = 0.4) +
  geom_text(aes(y = median_score, label = round(median_score, 2)), position = position_nudge(x = -0.2), vjust = -0.5, color = "black") +
  geom_text(aes(y = mean_score, label = round(mean_score, 2)), position = position_nudge(x = 0.2), vjust = -0.5, color = "black") +
  labs(x = "Region",
       y = "Performance Score") +
  scale_fill_manual(values = c("Median" = "lightblue", "Mean" = "darkblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(pdo_region_stdev, aes(x = region, y = standard_dev)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = round(standard_dev, 3)), vjust = -0.5, color = "black") +
  labs(x = "Region", 
       y = "Standard Deviation of Performance Scores") +
  ylim(0,6)


# Filter out specific regions
filtered_regions <- ppd_wgi_merge %>%
  filter(region %in% c("East Asia & Pacific", "Europe & Central Asia", "Sub-Saharan Africa", "Latin America & Caribbean", "Middle East & North Africa", "South Asia"))


ggplot(filtered_regions, aes(x = startyear, y = six_overall_rating, color = region)) +
  
  # Add lines for median trends by region
  stat_summary(fun = median, geom = "line", size = 0.7) +
  
  # Customize the appearance of the plot
  labs(x = " Year", y = "Median Performance Score") +
  theme_minimal() +
  
  # Adjust color scale for better visibility
  scale_color_manual(values = c("Europe & Central Asia" = "plum4", 
                                "South Asia" = "hotpink3", 
                                "Sub-Saharan Africa" = "skyblue3", 
                                "Latin America & Caribbean" = "gold1",
                                "Middle East & North Africa" = "palegreen3",
                                "East Asia & Pacific" = "tomato3")) +
  
  # Add legend
  guides(color = guide_legend(title = "Region")) +
  ylim(0,6)



pdo_org_summary <- ppd_wgi_merge %>%
  group_by(donor) %>%
  summarize(median_score = median(six_overall_rating),
            mean_score = mean(six_overall_rating))


pdo_org_summary$donor <- factor(pdo_org_summary$donor, levels = pdo_org_summary$donor[order(pdo_org_summary$median_score, decreasing = T)])


ggplot(pdo_org_summary, aes(x = as.factor(donor))) +
  geom_col(aes(y = median_score, fill = "Median"), position =  position_nudge(x = -0.2), width = 0.3) +
  geom_col(aes(y = mean_score, fill = "Mean"), position =  position_nudge(x = 0.2), width = 0.3) +
  geom_text(aes(y = median_score, label = round(median_score, 2)), position = position_nudge(x = -0.22), vjust = -0.5, color = "black") +
  geom_text(aes(y = mean_score, label = round(mean_score, 2)), position = position_nudge(x = 0.22), vjust = -0.5, color = "black") +
  labs(x = "IDO",
       y = "Performance Score",
       fill = " ") +
  scale_fill_manual(values = c("Median" = "lightblue", "Mean" = "darkblue")) +
  theme_minimal()


pdo_org_stdev <- ppd_wgi_merge %>%
  group_by(donor) %>%
  summarize(standard_dev = sd(six_overall_rating))

pdo_org_stdev$donor <- factor(pdo_org_stdev$donor, levels = pdo_org_stdev$donor[order(pdo_org_stdev$standard_dev, decreasing = T)] )

ggplot(pdo_org_stdev, aes(x = donor, y = standard_dev)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = round(standard_dev, 3)), vjust = -0.5, color = "black") +
  labs(x = "IDO", 
       y = "Standard Deviation of Performance Scores") +
  ylim(0,6) +
  theme_minimal()



ggplot(ppd_wgi_merge, aes(x = startyear, y = six_overall_rating, color = donor)) +
  
  # Add lines for median trends by region
  stat_summary(fun = median, geom = "line", size = 0.7) +
  
  # Customize the appearance of the plot
  labs(x = " Year", y = "Median Performance Score") +
  theme_minimal() +
  
  # Adjust color scale for better visibility
  scale_color_manual(values = c("WB" = "plum3",
                     "GFATM" = "skyblue2",
                     "JICA" = "forestgreen",
                     "AsianDB" = "tomato2",
                     "DFID" = "hotpink", 
                     "KfW" = "khaki2",
                     "GEF" = "burlywood4",
                     "GiZ" = "seashell4",
                     "IFAD" = "chocolate1",
                     "DFAT" = "pink",
                     "CDB" = "lightslateblue",
                     "AfricanDB" = "gray0"))+
  
  # Add legend
  guides(color = guide_legend(title = "IDO")) +
  ylim(0,6)



ggplot(ppd_wgi_merge, aes(x = startyear, y = six_overall_rating)) +
  
  # Add lines for median trends by region
  stat_summary(fun = median, geom = "line", size = 0.7, color = "lightblue") +
  
  # Add mean lines for each donor in facets
  stat_summary(fun = mean, geom = "line", aes(group = donor), size = 0.7, color = "darkblue") +
  
  # Facet by donor
  facet_wrap(~donor, ncol = 2, scales = "free_x") +
  
  # Customize the appearance of the plot
  labs(x = "Year", y = "Performance Score") +
  theme_minimal() +
  ylim(0, 6)


# Trends of good governance

wgi_medians <- ppd_wgi_merge %>%
  summarize(corruption_median = median(ppd_wgi_merge$wgi_corruption_score, na.rm = T),
            goveff_median = median(ppd_wgi_merge$wgi_goveff_score, na.rm = T),
            polstab_median = median(ppd_wgi_merge$wgi_polstability_score, na.rm = T),
            regqual_median = median(ppd_wgi_merge$wgi_regquality_score, na.rm = T),
            rol_median = median(ppd_wgi_merge$wgi_ruleoflaw_score, na.rm = T),
            voacc_median = median(ppd_wgi_merge$wgi_voiceacct_score, na.rm = T))

wgi_indices <- colnames(wgi_medians)
wgimedians <- as.numeric(wgi_medians[1, ])



wgi_medians <- data.frame(wgi_indices, wgimedians)

library(ggplot2)
ggplot(wgi_medians, aes(x = wgi_indices, y = wgimedians)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = round(wgimedians, 3)), vjust = -0.5, color = "black") +
  ylim(-1, 3) +
  labs(x = "WGI",
       y = "Median") +
  theme_minimal()



wgi_stdevs <- ppd_wgi_merge %>%
  summarize(corruption_sd = sd(ppd_wgi_merge$wgi_corruption_score, na.rm = T),
            goveff_sd = sd(ppd_wgi_merge$wgi_goveff_score, na.rm = T),
            polstab_sd = sd(ppd_wgi_merge$wgi_polstability_score, na.rm = T),
            regqual_sd = sd(ppd_wgi_merge$wgi_regquality_score, na.rm = T),
            rol_sd = sd(ppd_wgi_merge$wgi_ruleoflaw_score, na.rm = T),
            voacc_sd = sd(ppd_wgi_merge$wgi_voiceacct_score, na.rm = T))

wgistdevs <- as.numeric(wgi_stdevs[1, ])

wgi_stdevs <- data.frame(wgi_indices, wgistdevs)


ggplot(wgi_stdevs, aes(x = wgi_indices, y = wgistdevs)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = round(wgistdevs, 3)), vjust = -0.5, color = "black") +
  ylim(-1, 3) +
  labs(x = "WGI",
       y = "Standard Deviation") +
  theme_minimal()



wgi_corruption_by_year <- ppd_wgi_merge %>%
  group_by(start_year) %>%
  summarize(median_wgi_corruption = median(wgi_corruption_score, na.rm = T))
  
wgi_corruption_by_year <- wgi_corruption_by_year %>%
  filter(!is.na(wgi_corruption_by_year$median_wgi_corruption))

ggplot(wgi_corruption_by_year, aes(x = start_year, y = median_wgi_corruption)) +
  geom_line(stat = "identity", color = "darkblue") +
  geom_text(aes(label = round(median_wgi_corruption, 3)), vjust = -0.5, color = "black", size = 2) +
  theme_minimal() +
  ylim(-1,2) +
  labs(x = "Start Year",
       y = "Median WGI Corruption Score")



wgi_goveff_by_year <- ppd_wgi_merge %>%
  group_by(start_year) %>%
  summarize(median_wgi_goveff = median(wgi_goveff_score, na.rm = T))

wgi_goveff_by_year <- wgi_goveff_by_year %>%
  filter(!is.na(wgi_goveff_by_year$median_wgi_goveff))

ggplot(wgi_goveff_by_year, aes(x = start_year, y = median_wgi_goveff)) +
  geom_line(stat = "identity", color = "darkblue") +
  geom_text(aes(label = round(median_wgi_goveff, 3)), vjust = -0.5, color = "black", size = 2) +
  theme_minimal() +
  ylim(-1,2) +
  labs(x = "Start Year",
       y = "Median WGI Government Effectiveness Score")


wgi_polstab_by_year <- ppd_wgi_merge %>%
  group_by(start_year) %>%
  summarize(median_wgi_polstab = median(wgi_polstability_score, na.rm = T))

wgi_polstab_by_year <- wgi_polstab_by_year %>%
  filter(!is.na(wgi_polstab_by_year$median_wgi_polstab))


ggplot(wgi_polstab_by_year, aes(x = start_year, y = median_wgi_polstab)) +
  geom_line(stat = "identity", color = "darkblue") +
  geom_text(aes(label = round(median_wgi_polstab, 3)), vjust = -0.5, color = "black", size = 2) +
  theme_minimal() +
  ylim(-1,2) +
  labs(x = "Start Year",
       y = "Median WGI Political Stability Score")



wgi_regqual_by_year <- ppd_wgi_merge %>%
  group_by(start_year) %>%
  summarize(median_wgi_regqual = median(wgi_regquality_score, na.rm = T))

wgi_regqual_by_year <- wgi_regqual_by_year %>%
  filter(!is.na(wgi_regqual_by_year$median_wgi_regqual))


ggplot(wgi_regqual_by_year, aes(x = start_year, y = median_wgi_regqual)) +
  geom_line(stat = "identity", color = "darkblue") +
  geom_text(aes(label = round(median_wgi_regqual, 3)), vjust = -0.5, color = "black", size = 2) +
  theme_minimal() +
  ylim(-1,2) +
  labs(x = "Start Year",
       y = "Median WGI Regulatory Quality Score")



wgi_voacc_by_year <- ppd_wgi_merge %>%
  group_by(start_year) %>%
  summarize(median_wgi_voacc = median(wgi_voiceacct_score, na.rm = T))

wgi_voacc_by_year <- wgi_voacc_by_year %>%
  filter(!is.na(wgi_voacc_by_year$median_wgi_voacc))


ggplot(wgi_voacc_by_year, aes(x = start_year, y = median_wgi_voacc)) +
  geom_line(stat = "identity", color = "darkblue") +
  geom_text(aes(label = round(median_wgi_voacc, 3)), vjust = -0.5, color = "black", size = 2) +
  theme_minimal() +
  ylim(-1,2) +
  labs(x = "Start Year",
       y = "Median WGI Voice and Accountability Score")




wgi_rol_by_year <- ppd_wgi_merge %>%
  group_by(start_year) %>%
  summarize(median_wgi_rol = median(wgi_ruleoflaw_score, na.rm = T))

wgi_rol_by_year <- wgi_rol_by_year %>%
  filter(!is.na(wgi_rol_by_year$median_wgi_rol))


ggplot(wgi_rol_by_year, aes(x = start_year, y = median_wgi_rol)) +
  geom_line(stat = "identity", color = "darkblue") +
  geom_text(aes(label = round(median_wgi_rol, 3)), vjust = -0.5, color = "black", size = 2) +
  theme_minimal() +
  ylim(-1,2) +
  labs(x = "Start Year",
       y = "Median WGI Rule of Law Score")





wgi_region_medians <- ppd_wgi_merge%>%
  group_by(region) %>%
  summarize(median_wgi_rol = median(wgi_ruleoflaw_score, na.rm = T),
            median_wgi_cor = median(wgi_corruption_score, na.rm = T),
            median_wgi_polstab = median(wgi_polstability_score, na.rm = T),
            median_wgi_goveff = median(wgi_goveff_score, na.rm = T),
            median_wgi_regqual = median(wgi_regquality_score, na.rm = T),
            median_wgi_voacc = median(wgi_voiceacct_score, na.rm = T))

wgi_region_medians <- wgi_region_medians %>%
  filter(!is.na(wgi_region_medians$region))

wgi_region_medians <- wgi_region_medians %>%
  filter(!is.na(wgi_region_medians$median_wgi_polstab))


wgi_region_medians_long <- wgi_region_medians %>% 
  pivot_longer(cols = -region, names_to = "variable", values_to = "value")

# Plot
ggplot(wgi_region_medians_long, aes(x = region, y = value, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Region", fill = "Region")



new_names <- c("Rule of Law", "Control of Corruption", "Political Stability", "Government Effectiveness", "Regulatory Quality", "Voice and Accountability")

# Change the levels of the factor
wgi_region_medians_long$variable <- factor(wgi_region_medians_long$variable, labels = new_names)





regions_years_wgi <- ppd_wgi_merge %>%
  select(c(region, start_year, wgi_corruption_score, wgi_goveff_score, wgi_polstability_score, wgi_regquality_score, wgi_ruleoflaw_score, wgi_voiceacct_score))



wgi_median_long$index <- factor(wgi_median_long$index, labels = new_names)


wgi_median <- ppd_wgi_merge %>%
  group_by(start_year, region) %>%
  summarise(across(starts_with("wgi"), median, na.rm = TRUE), .groups = "drop")

# Reshape the data
wgi_median_long <- wgi_median %>% 
  pivot_longer(cols = starts_with("wgi"), names_to = "index", values_to = "median")

wgi_median_long <- wgi_median_long %>%
  filter(!is.na(region))



# Plot
ggplot(wgi_median_long, aes(x = start_year, y = median, color = region, group = interaction(region, index))) +
  geom_line() +
  facet_wrap(~ index, scales = "free_y") +
  labs(x = "Year", y = "Median", color = "Region") +
  theme_minimal()



# Change the levels of the factor
wgi_ido_medians_long$index <- factor(wgi_ido_medians_long$index, labels = new_names)

wgi_ido_medians <- ppd_wgi_merge %>%
  group_by(start_year, donor) %>%
  summarise(across(starts_with("wgi"), median, na.rm = TRUE), .groups = "drop")

wgi_ido_medians <- wgi_ido_medians %>%
  filter(!is.na(wgi_ido_medians$wgi_polstability_score))


wgi_ido_medians_long <- wgi_ido_medians %>% 
  pivot_longer(cols = starts_with("wgi"), names_to = "index", values_to = "median")

# Plot
ggplot(wgi_ido_medians_long, aes(x = start_year, y = median, color = donor, group = interaction(donor, index))) +
  geom_line() +
  facet_wrap(~ index, scales = "free_y") +
  labs(x = "Year", y = "Median", color = "Region") +
  theme_minimal()




wgi_ido_medians_long$variable <- factor(wgi_ido_medians_long$variable, labels = new_names)


wgi_ido_medians <- ppd_wgi_merge%>%
  group_by(donor) %>%
  summarize(median_wgi_rol = median(wgi_ruleoflaw_score, na.rm = T),
            median_wgi_cor = median(wgi_corruption_score, na.rm = T),
            median_wgi_polstab = median(wgi_polstability_score, na.rm = T),
            median_wgi_goveff = median(wgi_goveff_score, na.rm = T),
            median_wgi_regqual = median(wgi_regquality_score, na.rm = T),
            median_wgi_voacc = median(wgi_voiceacct_score, na.rm = T))

wgi_ido_medians <- wgi_ido_medians %>%
  filter(!is.na(wgi_ido_medians$median_wgi_polstab))


wgi_ido_medians_long <- wgi_ido_medians %>% 
  pivot_longer(cols = -donor, names_to = "variable", values_to = "value")




# Plot
ggplot(wgi_ido_medians_long, aes(x = donor, y = value, fill = donor)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "IDO", fill = "IDO") +
  ylim(-1,0.5)


# Trends of CPIA 

# Data preparation
cpia_medians <- ppd_cpia %>%
  summarize(
    building_hr_median = median(cpia_build_hr_score, na.rm = TRUE),
    bus_reg_env_median = median(cpia_bus_reg_env_score, na.rm = TRUE),
    debt_policy_median = median(cpia_debt_pol_score, na.rm = TRUE),
    rev_mob_eff_median = median(cpia_revmob_eff_score, na.rm = TRUE),
    gender_eq_median = median(cpia_gend_eq_score, na.rm = TRUE),
    econ_mgmt_cluster_median = median(cpia_econmgt_clust_score, na.rm = TRUE),
    pub_res_eqty_median = median(cpia_publres_eqty_score, na.rm = TRUE),
    financial_sector_median = median(cpia_fin_sector_score, na.rm = TRUE),
    fiscal_policy_median = median(cpia_fisc_pol_score, na.rm = TRUE),
    macro_eco_mgmt_median = median(cpia_macecon_mgt_score, na.rm = TRUE),
    social_policy_eqty_median = median(cpia_social_policy_eqty_score, na.rm = TRUE),
    env_policy_median = median(cpia_env_policy_score, na.rm = TRUE),
    prop_rights_gov_median = median(cpia_prop_rbg_score, na.rm = TRUE),
    pub_admin_median = median(cpia_public_admin_score, na.rm = TRUE),
    budget_fin_mgmt_median = median(cpia_finmgt_qual_score, na.rm = TRUE),
    social_prot_median = median(cpia_social_prot_score, na.rm = TRUE),
    struct_policy_cluster_median = median(cpia_struc_pol_score, na.rm = TRUE),
    trade_median = median(cpia_trade_score, na.rm = TRUE),
    pub_sector_corr_median = median(cpia_publ_corr_score, na.rm = TRUE),
    ida_resource_index_median = median(cpia_ida_res_score, na.rm = TRUE),
    pub_sector_mgmt_cluster_median = median(cpia_publ_sct_mgt_score, na.rm = TRUE)
  )

cpia_stdevs <- ppd_cpia %>%
  summarize(
    building_hr_sd = sd(cpia_build_hr_score, na.rm = TRUE),
    bus_reg_env_sd = sd(cpia_bus_reg_env_score, na.rm = TRUE),
    debt_policy_sd = sd(cpia_debt_pol_score, na.rm = TRUE),
    rev_mob_eff_sd = sd(cpia_revmob_eff_score, na.rm = TRUE),
    gender_eq_sd = sd(cpia_gend_eq_score, na.rm = TRUE),
    econ_mgmt_cluster_sd = sd(cpia_econmgt_clust_score, na.rm = TRUE),
    pub_res_eqty_sd = sd(cpia_publres_eqty_score, na.rm = TRUE),
    financial_sector_sd = sd(cpia_fin_sector_score, na.rm = TRUE),
    fiscal_policy_sd = sd(cpia_fisc_pol_score, na.rm = TRUE),
    macro_eco_mgmt_sd = sd(cpia_macecon_mgt_score, na.rm = TRUE),
    social_policy_eqty_sd = sd(cpia_social_policy_eqty_score, na.rm = TRUE),
    env_policy_sd = sd(cpia_env_policy_score, na.rm = TRUE),
    prop_rights_gov_sd = sd(cpia_prop_rbg_score, na.rm = TRUE),
    pub_admin_sd = sd(cpia_public_admin_score, na.rm = TRUE),
    budget_fin_mgmt_sd = sd(cpia_finmgt_qual_score, na.rm = TRUE),
    social_prot_sd = sd(cpia_social_prot_score, na.rm = TRUE),
    struct_policy_cluster_sd = sd(cpia_struc_pol_score, na.rm = TRUE),
    trade_sd = sd(cpia_trade_score, na.rm = TRUE),
    pub_sector_corr_sd = sd(cpia_publ_corr_score, na.rm = TRUE),
    ida_resource_index_sd = sd(cpia_ida_res_score, na.rm = TRUE),
    pub_sector_mgmt_cluster_sd = sd(cpia_publ_sct_mgt_score, na.rm = TRUE)
  )

# Plots
cpia_medians_long <- pivot_longer(cpia_medians, cols = everything(), names_to = "index", values_to = "median")
cpia_stdevs_long <- pivot_longer(cpia_stdevs, cols = everything(), names_to = "index", values_to = "stdev")

ggplot(cpia_medians_long, aes(x = index, y = median)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  labs(x = "CPIA Index", y = "Median") +
  ylim(0,5) + 
  ggtitle("CPIA Medians")

ggplot(cpia_stdevs_long, aes(x = index, y = stdev)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "CPIA Index", y = "Standard Deviation") +
  ylim(0,5) +
  ggtitle("CPIA Standard Deviations")

# Trends over time
cpia_trends <- ppd_cpia %>%
  group_by(start_year) %>%
  summarize(across(starts_with("cpia"), median, na.rm = TRUE))

cpia_trends_long <- pivot_longer(cpia_trends, cols = -start_year, names_to = "index", values_to = "median")

ggplot(cpia_trends_long, aes(x = start_year, y = median, color = index, group = index)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Year", y = "Median Score", color = "Index") +
  theme(legend.position = "right")


# By donor
cpia_ido_medians <- ppd_cpia %>%
  group_by(donor) %>%
  summarize(
    building_hr_median = median(cpia_build_hr_score, na.rm = TRUE),
    bus_reg_env_median = median(cpia_bus_reg_env_score, na.rm = TRUE),
    debt_policy_median = median(cpia_debt_pol_score, na.rm = TRUE),
    rev_mob_eff_median = median(cpia_revmob_eff_score, na.rm = TRUE),
    gender_eq_median = median(cpia_gend_eq_score, na.rm = TRUE),
    econ_mgmt_cluster_median = median(cpia_econmgt_clust_score, na.rm = TRUE),
    pub_res_eqty_median = median(cpia_publres_eqty_score, na.rm = TRUE),
    financial_sector_median = median(cpia_fin_sector_score, na.rm = TRUE),
    fiscal_policy_median = median(cpia_fisc_pol_score, na.rm = TRUE),
    macro_eco_mgmt_median = median(cpia_macecon_mgt_score, na.rm = TRUE),
    social_policy_eqty_median = median(cpia_social_policy_eqty_score, na.rm = TRUE),
    env_policy_median = median(cpia_env_policy_score, na.rm = TRUE),
    prop_rights_gov_median = median(cpia_prop_rbg_score, na.rm = TRUE),
    pub_admin_median = median(cpia_public_admin_score, na.rm = TRUE),
    budget_fin_mgmt_median = median(cpia_finmgt_qual_score, na.rm = TRUE),
    social_prot_median = median(cpia_social_prot_score, na.rm = TRUE),
    struct_policy_cluster_median = median(cpia_struc_pol_score, na.rm = TRUE),
    trade_median = median(cpia_trade_score, na.rm = TRUE),
    pub_sector_corr_median = median(cpia_publ_corr_score, na.rm = TRUE),
    ida_resource_index_median = median(cpia_ida_res_score, na.rm = TRUE),
    pub_sector_mgmt_cluster_median = median(cpia_publ_sct_mgt_score, na.rm = TRUE),
    .groups = 'drop'
  )

cpia_ido_medians_long <- pivot_longer(cpia_ido_medians, cols = -donor, names_to = "variable", values_to = "median")

# Plotting
ggplot(cpia_ido_medians_long, aes(x = variable, y = median, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ donor, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d(option = "ma", name = "CPIA Indices") +
  labs(x = "CPIA Indices", y = "Median") +
  theme(legend.position = "bottom")

# Trends over time by donor
cpia_ido_trends <- ppd_cpia %>%
  group_by(start_year, donor) %>%
  summarize(across(starts_with("cpia"), median, na.rm = TRUE), .groups = 'drop')

cpia_ido_trends_long <- pivot_longer(cpia_ido_trends, cols = -c(start_year, donor), names_to = "index", values_to = "median")

ggplot(cpia_ido_trends_long, aes(x = start_year, y = median, color = donor, group = interaction(donor, index))) +
  geom_line() +
  facet_wrap(~ index, scales = "free_y") +
  labs(x = "Year", y = "Median", color = "Donor (IDO)") +
  theme_minimal()


# Regional trends
cpia_region_medians <- ppd_cpia %>%
  group_by(region) %>%
  summarize(across(starts_with("cpia"), median, na.rm = TRUE), .groups = 'drop')

cpia_region_medians_long <- pivot_longer(cpia_region_medians, cols = -region, names_to = "CPIA Index", values_to = "Median")

# Plotting
ggplot(cpia_region_medians_long, aes(x = region, y = Median, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ `CPIA Index`, scales = "free_y") +  # Adding faceting by CPIA Index
  theme_minimal() +
  labs(x = "Region", y = "Median CPIA Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(palette = "Blues", name = "Region") +
  ggtitle("Median CPIA Scores by Region and CPIA Index")

cpia_trends_region <- ppd_cpia %>%
  group_by(region, start_year) %>%
  summarize(across(starts_with("cpia"), median, na.rm = TRUE), .groups = 'drop')

cpia_trends_region_long <- pivot_longer(cpia_trends_region, cols = -c(region, start_year), names_to = "CPIA Index", values_to = "Median")

# Plotting
ggplot(cpia_trends_region_long, aes(x = start_year, y = Median, color = region, group = interaction(region, `CPIA Index`))) +
  geom_line() +
  facet_wrap(~ `CPIA Index`, scales = "free_y") +
  theme_minimal() +
  labs(x = "Year", y = "Median CPIA Score", color = "Region") +
  ggtitle("CPIA Score Trends Over Time by Region") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# REGRESSIONS
# Regressions for WGI indices
reg_wgi_all_pdo <- lm(six_overall_rating ~ wgi_ruleoflaw_score + wgi_polstability_score + wgi_corruption_score + wgi_goveff_score + wgi_regquality_score + wgi_voiceacct_score, data = ppd_wgi_merge)
summary(reg_wgi_all_pdo)

reg_regqual_pdo <- lm(six_overall_rating ~ wgi_regquality_score, data = ppd_wgi_merge)
summary(reg_regqual_pdo)

library(ggplot2)
ggplot(data = ppd_wgi_merge, aes(x = wgi_regquality_score, y = six_overall_rating)) +
  geom_point(stat = "identity") +
  geom_smooth(method = "lm", se = FALSE)

plot(reg_wgi_all_pdo)

reg_corruption_pdo <- lm(six_overall_rating ~ wgi_corruption_score, data = ppd_wgi_merge)
corruption_summary <- summary(reg_corruption_pdo)
corruption_summary$r.squared

reg_rol_pdo <- lm(six_overall_rating ~ wgi_ruleoflaw_score, data = ppd_wgi_merge)
summary(reg_rol_pdo)

reg_goveff_pdo <- lm(six_overall_rating ~ wgi_goveff_score, data = ppd_wgi_merge)
summary(reg_goveff_pdo)

reg_polstab_pdo <- lm(six_overall_rating ~ wgi_polstability_score, data = ppd_wgi_merge)
summary(reg_polstab_pdo)

reg_voacc_pdo <- lm(six_overall_rating ~ wgi_voiceacct_score, data = ppd_wgi_merge)
summary(reg_voacc_pdo)

plot(ppd_wgi_merge$wgi_polstability_score, ppd_wgi_merge$six_overall_rating)
abline(reg = reg_polstab_pdo)

ggplot(ppd_wgi_merge, aes(x = wgi_ruleoflaw_score, y = six_overall_rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") + 
  labs(x = "WGI Rule of Law", y = "PDO Score") +
  theme_minimal()

ggplot(ppd_wgi_merge, aes(x = wgi_polstability_score, y = six_overall_rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") + 
  labs(x = "WGI Political Stability", y = "PDO Score") +
  theme_minimal()

ggplot(ppd_wgi_merge, aes(x = wgi_voiceacct_score, y = six_overall_rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") + 
  labs(x = "WGI Voice and Accountability", y = "PDO Score") +
  theme_minimal()

ggplot(ppd_wgi_merge, aes(x = wgi_goveff_score, y = six_overall_rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") + 
  labs(x = "WGI Government Effectiveness", y = "PDO Score") +
  theme_minimal()

ggplot(ppd_wgi_merge, aes(x = wgi_corruption_score, y = six_overall_rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") + 
  labs(x = "WGI Corruption", y = "PDO Score") +
  theme_minimal()

ggplot(ppd_wgi_merge, aes(x = wgi_regquality_score, y = six_overall_rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") + 
  labs(x = "WGI Regulatory Quality", y = "PDO Score") +
  theme_minimal()



# Correlation studies for score & WGI 
library(dplyr)
library(tidyverse)
ppd_wgi_binary <- ppd_wgi_merge %>%
  select(c(six_overall_rating, wgi_corruption_score, wgi_ruleoflaw_score, wgi_goveff_score, wgi_polstability_score, wgi_voiceacct_score, wgi_regquality_score))

ppd_wgi_binary <- ppd_wgi_binary %>%
  filter(!is.na(wgi_corruption_score))

pdo_binary <- rep(NA, 8101)


for (i in 1:8101) {
  if (ppd_wgi_binary$six_overall_rating[i] > 3) {
    pdo_binary[i] <- 1
  }
  else {
    pdo_binary[i] <- 0
  }
}

ppd_wgi_binary$six_overall_rating_binary <- pdo_binary

ppd_wgi_binary$six_overall_rating_binary <- factor(ppd_wgi_binary$six_overall_rating_binary, levels = c(1, 0))

logreg_pdo_cor <- glm(six_overall_rating_binary ~ wgi_corruption_score, data = ppd_wgi_binary, family = "binomial")
summary(logreg_pdo_cor)

plot(logreg_pdo_cor)

logreg_pdo_cor <- glm(six_overall_rating_binary ~ wgi_corruption_score, data = ppd_wgi_binary, family = "binomial")
summary(logreg_pdo_cor)

plot(logreg_pdo_cor)


log_pdo_wgiall <- glm(six_overall_rating_binary ~ wgi_corruption_score + wgi_regquality_score + wgi_ruleoflaw_score + wgi_voiceacct_score + wgi_goveff_score + wgi_polstability_score,
             data = ppd_wgi_binary,
             family = "binomial")

summary(mvlog)

avPlots(log_pdo_wgiall)


install.packages("ggeffects")
library(ggeffects)

ggplot(reg_wgi_all_pdo)


ggplot(ggpredict(reg_wgi_all_pdo, terms = c("wgi_ruleoflaw_score", "wgi_polstability_score", "wgi_corruption_score", "wgi_goveff_score", "wgi_regquality_score", "wgi_voiceacct_score")),
         aes(x, predicted, color = group)) + geom_line() + facet_wrap(~facet) +
  labs(x = "Combined WGI", y = "Predicted PDO")
      

library(car)
avPlots(reg_wgi_all_pdo)


plot.lm(reg_wgi_all_pdo)

install.packages("corrplot")
library(corrplot)

pdo_wgi <- ppd_wgi_binary %>%
  select(-c(six_overall_rating_binary))

corrplot(cor(pdo_wgi))


ggplot(ppd_wgi_binary, aes(x = wgi_corruption_score, wgi_polstability_score, y = six_overall_rating_binary)) +
  geom_point(alpha = .5) +
  stat_smooth(method = "glm", se = FALSE, method.args = list(family = binomial))




# Correlation studies for score & CPIA
# Correlation matrix for CPIA indicators and performance score
# Load necessary libraries
library(car)
library(ggplot2)


# Subset the data to include only CPIA indicators and the target variable
cpia_vars <- names(ppd_cpia)[grepl("cpia_", names(ppd_cpia))]
formula <- as.formula(paste("six_overall_rating ~", paste(cpia_vars, collapse = "+")))

# Multivariate Linear Regression
lm_all_cpia <- lm(formula, data = ppd_cpia)
summary(lm_all_cpia)

# Generate added-variable plots for the multivariate regression
avPlots(lm_all_cpia)

# Individual regressions

# Create an empty list to store results
results <- list()  

for (var in cpia_vars) {
  # Fit linear model
  formula <- as.formula(paste(var, "six_overall_rating", sep = " ~ "))
  model <- lm(formula, data = ppd_cpia)
  
  # Extract model statistics
  coefficient <- coef(model)[2]  # Extract slope (coefficient of the predictor)
  intercept <- coef(model)[1]  # Extract intercept
  summary_model <- summary(model)
  p_value <- summary_model$coefficients[2, 4]  # p-value for the predictor
  r_squared <- summary_model$r.squared  # R-squared value
  
  # Store results
  results[[var]] <- list(
    Coefficient = coefficient,
    Intercept = intercept,
    P_value = p_value,
    R_squared = r_squared
  )
  
  # Plot
  plot_data <- ggplot(ppd_cpia, aes_string(x = var, y = "six_overall_rating")) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = paste("Dot Plot of", var, "vs. Six Overall Rating"), x = var, y = "Six Overall Rating")
  
  print(plot_data)
}






