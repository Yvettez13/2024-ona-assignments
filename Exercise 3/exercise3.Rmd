---
title: "Exercise 3"
output: pdf_document
date: "2024-03-31"
---
```{r}
options(repos = c(CRAN = "https://cloud.r-project.org"))

```




```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
install.packages("arrow")
install.packages("readr")
install.packages("gender")
install.packages("tidyr")
install.packages("wru")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("igraph")
install.packages("ggraph")
install.packages("tidyverse")
library(tidyverse)
library(ggraph)
library(igraph)
library(ggplot2)
library(lubridate) 
library(wru)
library(tidyr)
library(readr)
library(dplyr)
library(arrow)
library(gender)
```

```{r}
applications <- read_parquet("C:/Users/xzhu71/Desktop/app_data_sample.parquet")
edges <- read_csv(paste0("C:/Users/xzhu71/Desktop/edges_sample.csv"))
```

add gender variables for examiners

```{r}
#install_genderdata_package()
# get a list of first names without repetitions
examiner_names <- applications %>% 
  distinct(examiner_name_first)
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )

examiner_names_gender

```



```{r}
# remove extra colums from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)

# joining gender back to the dataset
applications <- applications %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")

# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()
```



add race variable for the examiners
```{r}
examiner_surnames <- applications %>% 
  select(surname = examiner_name_last) %>% 
  distinct()
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```





```{r}
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))

examiner_race
```



```{r}
# removing extra columns
examiner_race <- examiner_race %>% 
  select(surname,race)

applications <- applications %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))

rm(examiner_race)
rm(examiner_surnames)
gc()
```


add tenure variable in examiners
```{r}
examiner_dates <- applications %>% 
  select(examiner_id, filing_date, appl_status_date) 
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))
```



```{r}
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    ) %>% 
  filter(year(latest_date)<2018)

examiner_dates
```


```{r}
applications <- applications %>% 
  left_join(examiner_dates, by = "examiner_id")

rm(examiner_dates)
gc()
```



Question 2


compare examiners'demographics

```{r}
applications$workgroup <- substr(as.character(applications$examiner_art_unit), 1, 3)
```


```{r}
filtered_data <- filter(applications, workgroup %in% c('176', '162'))
# Printing the summary of missing values for demographic variables
na_demographics <- filtered_data %>%
  select(gender, race, tenure_days) %>%
  summarise_all(~sum(is.na(.)))
na_demographics
```



```{r}
#drop the missing values since the portion is relatively low
filtered_data_no <- filtered_data %>%
  filter(!is.na(gender) & !is.na(tenure_days))
filtered_data_no
```


```{r}
# Summary statistics for tenure days by workgroup
tenure_stats <- filtered_data_no %>%
  group_by(workgroup) %>%
  summarise(
    Count = n(),
    Mean = mean(tenure_days, na.rm = TRUE),
    SD = sd(tenure_days, na.rm = TRUE),
    Min = min(tenure_days, na.rm = TRUE),
    Max = max(tenure_days, na.rm = TRUE)
  )
tenure_stats
```



```{r}
gender_distribution <- filtered_data_no %>%
  group_by(workgroup, gender) %>%
  summarise(Count = n(), .groups = "drop") %>%
  ggplot(aes(x = workgroup, y = Count, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  ggtitle("Gender Distribution by Workgroup")

# Display the plot
gender_distribution
```

Interpretation: The bar chart illustrates the gender distribution within the two workgroups, showing a higher count of male examiners compared to female examiners in both groups.Both groups exhibit a gender imbalance favoring male examiners. 







```{r}
# Race Distribution
ggplot(filtered_data_no, aes(x = workgroup, fill = race)) +
  geom_bar(position = "stack") +
  ggtitle("Race Distribution by Workgroup") +
  theme_minimal()
```
Both groups have a majority of white individuals, followed by a smaller proportion of Asian, Hispanic, and black individuals. The distribution appears relatively consistent between the two workgroups, suggesting that the racial makeup is not significantly different when comparing Workgroup 162 to Workgroup 176. 






```{r}
# Tenure Days Distribution
ggplot(filtered_data_no, aes(x = tenure_days, fill = workgroup)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
  facet_wrap(~workgroup) +
  ggtitle("Tenure Days Distribution by Workgroup") +
  theme_minimal()
# Boxplot for tenure_days across workgroups
ggplot(filtered_data_no, aes(x = workgroup, y = tenure_days)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Tenure Days by Workgroup")
```
This histogram shows the distribution of tenure days for two different workgroups, labeled 162 and 176. It appears that Workgroup 162 has a large number of individuals with a relatively short tenure, as indicated by the peak at the lower end of the tenure days axis. In contrast, Workgroup 176 shows a more uniform distribution across a range of tenure days, with a peak at the higher end. This suggests that Workgroup 176 may have more experienced examiners, or it could indicate a difference in hiring or retention patterns between the two groups.





```{r}
joined_data <- inner_join(filtered_data_no, edges, by = "application_number")
joined_data
```


```{r}
selected_data <- joined_data %>%
  select(application_number, examiner_name_last, examiner_name_first, examiner_id, 
         gender, race, tenure_days, workgroup, ego_examiner_id, alter_examiner_id)
selected_data
```



network visualization of workgroup 162



```{r}

selected_data_162 <- filter(selected_data, workgroup == "162")
g_162 <- graph_from_data_frame(selected_data_162[, c("ego_examiner_id", "alter_examiner_id")], directed = TRUE)

# Combine gender information from both roles
all_genders <- rbind(
  data.frame(id = selected_data$ego_examiner_id, gender = selected_data$gender),
  data.frame(id = selected_data$alter_examiner_id, gender = selected_data$gender)
)

# Remove potential duplicates to ensure each examiner ID has a single gender entry
all_genders_unique <- all_genders[!duplicated(all_genders$id), ]
#Since some examiners might appear in both roles, this line removes duplicates to ensure that each examiner ID is listed only once with its corresponding gender.

# Prepare a nodes data frame with ID from the graph
nodes_df <- data.frame(id = V(g_162)$name)
nodes_df <- merge(nodes_df, all_genders_unique, by = "id", all.x = TRUE)

# Exclude nodes with NA or "Unknown" gender
nodes_df <- nodes_df[!is.na(nodes_df$gender), ]

# Subset the graph to only include nodes with known gender information
nodes_to_keep <- as.character(nodes_df$id)  
g_162_filtered <- induced_subgraph(g_162, which(V(g_162)$name %in% nodes_to_keep))
V(g_162_filtered)$gender <- nodes_df$gender[match(V(g_162_filtered)$name, nodes_df$id)]

ggraph(g_162_filtered, layout = 'fr') + 
  geom_edge_link(color = 'gray', alpha = 0.5) +  
  geom_node_point(aes(color = gender), size = 5, alpha = 0.9) +  
  scale_color_manual(values = c('male' = 'blue', 'female' = 'red', 'Other' = 'grey')) + 
  theme_void() +  
  ggtitle("Network Visualization by Gender (Workgroup 162)") +  
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  labs(color = 'Gender')


```




network visualization of workgroup 176

```{r}
selected_data_176 <- filter(selected_data, workgroup == "176")
g_176 <- graph_from_data_frame(selected_data_176[, c("ego_examiner_id", "alter_examiner_id")], directed = TRUE)

# Combine gender information for both roles
all_genders_176 <- rbind(
  data.frame(id = selected_data_176$ego_examiner_id, gender = selected_data_176$gender),
  data.frame(id = selected_data_176$alter_examiner_id, gender = selected_data_176$gender)
)

# Remove potential duplicates to ensure a single gender entry per examiner ID
all_genders_unique_176 <- all_genders_176[!duplicated(all_genders_176$id), ]

# Prepare a nodes data frame with ID from the graph
nodes_df_176 <- data.frame(id = V(g_176)$name)
nodes_df_176 <- merge(nodes_df_176, all_genders_unique_176, by = "id", all.x = TRUE)

# Exclude nodes with NA gender
nodes_df_176 <- nodes_df_176[!is.na(nodes_df_176$gender), ]
nodes_to_keep_176 <- as.character(nodes_df_176$id)
g_176_filtered <- induced_subgraph(g_176, which(V(g_176)$name %in% nodes_to_keep_176))


V(g_176_filtered)$gender <- nodes_df_176$gender[match(V(g_176_filtered)$name, nodes_df_176$id)]

ggraph(g_176_filtered, layout = 'fr') + 
  geom_edge_link(color = 'gray', alpha = 0.5) +  
  geom_node_point(aes(color = gender), size = 5, alpha = 0.9) +  
  scale_color_manual(values = c('male' = 'blue', 'female' = 'red', 'Other' = 'grey')) + 
  theme_void() +  
  ggtitle("Network Visualization by Gender (Workgroup 176)") +  
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  labs(color = 'Gender')


```

network visualization of race

```{r}

all_races <- rbind(
  data.frame(id = selected_data$ego_examiner_id, race = selected_data$race),
  data.frame(id = selected_data$alter_examiner_id, race = selected_data$race)
)


# Remove potential duplicates to ensure a single race entry per examiner ID
all_races_unique <- all_races[!duplicated(all_races$id), ]

# Prepare a nodes data frame with ID from the graph
nodes_df <- data.frame(id = V(g_162)$name)

# Merge the nodes data frame with the consolidated race information
nodes_df <- merge(nodes_df, all_races_unique, by = "id", all.x = TRUE)

# Exclude nodes with NA race
nodes_df <- nodes_df[!is.na(nodes_df$race), ]
nodes_to_keep <- as.character(nodes_df$id)
g_162_filtered <- induced_subgraph(g_162, which(V(g_162)$name %in% nodes_to_keep))

# Update the race attribute for the filtered graph
V(g_162_filtered)$race <- nodes_df$race[match(V(g_162_filtered)$name, nodes_df$id)]

ggraph(g_162_filtered, layout = 'fr') + 
  geom_edge_link(color = 'gray', alpha = 0.5) +  
  geom_node_point(aes(color = race), size = 5, alpha = 0.9) +  
  scale_color_manual(values = c('White' = 'blue', 'Black' = 'black', 'Asian' = 'yellow', 'Hispanic' = 'red', 'Other' = 'grey')) +  
  theme_void() +  
  ggtitle("Network Visualization by Race (Workgroup 162)") +  
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  labs(color = 'Race')


```

```{r}

nodes_df_176 <- data.frame(id = V(g_176)$name)
# Merge the nodes data frame with the consolidated race information
nodes_df_176 <- merge(nodes_df_176, all_races_unique, by = "id", all.x = TRUE)

# Exclude nodes with NA race
nodes_df_176 <- nodes_df_176[!is.na(nodes_df_176$race), ]
nodes_to_keep_176 <- as.character(nodes_df_176$id)
g_176_filtered <- induced_subgraph(g_176, which(V(g_176)$name %in% nodes_to_keep_176))

V(g_176_filtered)$race <- nodes_df_176$race[match(V(g_176_filtered)$name, nodes_df_176$id)]

# Visualization for Workgroup 176
ggraph(g_176_filtered, layout = 'fr') + 
  geom_edge_link(color = 'gray', alpha = 0.5) +  
  geom_node_point(aes(color = race), size = 5, alpha = 0.9) +  
  scale_color_manual(values = c('White' = 'blue', 'Black' = 'black', 'Asian' = 'yellow', 'Hispanic' = 'red', 'Other' = 'grey')) + 
  theme_void() +  
  ggtitle("Network Visualization by Race (Workgroup 176)") +  
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  labs(color = 'Race')
```


Question 3


justification for choosing degree centrality:
Degree centrality was chosen because it provides a direct count of the number of connections an examiner has within the advice network, which in this context, represents the active engagement in advice-seeking (out-degree) and advice-giving (in-degree) behaviors. It is a measure of immediate influence and potential knowledge dissemination within the network, reflecting how integral an examiner is to the flow of information.

By integrating degree centrality with demographic variables such as tenure, race, and gender, we can explore the nuances of network dynamics. For instance, the code below merges centrality scores with tenure data, allowing us to analyze whether an examiner's centrality increases with their tenure.

```{r}

degree_centrality_162 <- degree(g_162, mode = "all")
degree_centrality_176 <- degree(g_176, mode = "all")
degree_centrality_df_162 <- data.frame(ego_examiner_id = names(degree_centrality_162), 
                                       degree_centrality = degree_centrality_162)

selected_data_162 <- merge(selected_data_162, degree_centrality_df_162, by = "ego_examiner_id")



ggplot(selected_data_162, aes(x = tenure_days, y = degree_centrality, color = race)) +
  geom_point() +
  facet_wrap(~gender) +  
  theme_minimal() +
  labs(title = "Tenures and Degree Centrality", x = "Tenure Days", y = "Degree Centrality")



```
Most of the points are clustered at a low degree centrality value for both males and females, suggesting that the majority of examiners do not have a large number of connections within the network.

There are a few points with notably higher centrality, particularly among male examiners and white race. These individuals might be key nodes within the network, possibly acting as hubs of information or advice.

white examiners (blue points) are the most prevalent group. There are fewer points for Asian (red) and Hispanic (green) examiners, which may reflect their distribution within the workforce or could suggest a potential area for further investigation regarding diversity within the organization.

```{r}

# Perform t-test
t_test_result <- t.test(degree_centrality ~ gender, data = selected_data_162)

# Print the results
print(t_test_result)



```
The t-values here indicating a significant difference in the mean degree centrality between female and male examiners.
The p-value is less than 2.2e-16.This means we reject the null hypothesis, which states there is no difference between the groups. Therefore, we have sufficient evidence to conclude that there is a statistically significant difference in the mean degree centrality between female and male examiners.




























































