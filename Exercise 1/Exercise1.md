---
title: "Exercise 1"
output:md_document: default
date: '2024-03-18'
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(dplyr)
library(tidygraph)
library(ggraph)
library(igraph)
```

##import dataset

```{r}
contacts_df<- read.csv("/Users/yvette/Desktop/ORGB/Connections.csv", skip = 3)
```

##get the count of contacts + total count

```{r pressure, echo=FALSE}
employer_counts <- contacts_df %>%
  group_by(Company) %>%
  summarise(Count = n())
print(employer_counts)
```


```{r}
total_count <- sum(employer_counts$Count)
print(total_count)
```
```{r}
# Create a unique identifier for each contact 
contacts_df <- contacts_df %>%
  mutate(Label = paste(First.Name, str_sub(Last.Name, 1, 1), sep = " ")) %>%
  mutate(Label = str_trim(Label))
print(head(contacts_df))
```
```{r}
# Creating the nodes dataframe
nodes <- contacts_df %>%
  select(Label, Company) %>%
  distinct()
# Creating the edges dataframe by finding pairs of contacts in the same company
edges <- contacts_df%>%
  select(Label, Company) %>%
  distinct() %>%
  inner_join(contacts_df %>% select(Label, Company) %>% distinct(), by = "Company", suffix = c("_x", "_y")) %>%
  filter(Label_x != Label_y) %>%
  select(Label_x, Label_y) %>%
  distinct()

print(head(nodes))
print(head(edges))
```


```{r}
g = tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
g <- g %>% 
  activate(nodes) %>%
  mutate(degree = centrality_degree(), pagerank = centrality_pagerank()) %>%
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness())


ggraph(g, layout = 'kk') + 
  geom_edge_link(aes(alpha = betweenness)) + 
  geom_node_point(aes(size = pagerank, colour = pagerank)) + 
  scale_color_continuous(guide = 'legend') + 
  theme_graph()

```

```{r}
g <- g %>% 
  activate(nodes) %>%
  mutate(community = as.factor(group_louvain()))

ggraph(g, layout = 'kk') + 
  geom_edge_link(aes(alpha = stat(index)), show.legend = FALSE) + 
  geom_node_point(aes(colour = community), size = 5) + 
  theme_graph()
```


