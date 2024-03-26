---
title: "Exercise2"
output: md_document
date: '2024-03-26'
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(igraph)
```

create a dataset where where edges are based
on seat adjacency
```{r cars}
final_edges <- matrix(c(
    '1', '2',
    '2', 'A',
    '3', '4', '3', 'D', '3', '5', '3', 'C', '3', 'B',
    '4', 'C',
    '5', 'D', '5', '6',
    '6', 'B', '6', 'D',
    'A', '2', 'A', 'B', 'A', 'C',
    'B', 'D', 'B', 'C', 'B', 'A', 'B', '6', 'B', '3',
    'C', 'B', 'C', '3', 'C', '4', 'C', 'A', 'C', 'D',
    'D', '5', 'D', '6', 'D', 'C', 'D', 'B', 'D', '3'
), byrow = TRUE, ncol = 2)
final_edges <- unique(t(apply(final_edges, 1, sort)))
final_edges
```

calculate degree, closeness,betweeness centrality
```{r}
g <- graph_from_edgelist(final_edges, directed = FALSE)
degree_centrality <- degree(g)
closeness_centrality <- closeness(g)
betweenness_centrality <- betweenness(g)
```


```{r}
centrality_measures <- data.frame(
  node = names(degree_centrality)[names(degree_centrality) %in% c("A", "B", "C", "D")],
  degree = degree_centrality[names(degree_centrality) %in% c("A", "B", "C", "D")],
  closeness = closeness_centrality[names(closeness_centrality) %in% c("A", "B", "C", "D")],
  betweenness = betweenness_centrality[names(betweenness_centrality) %in% c("A", "B", "C", "D")]
)

print(centrality_measures)
```

seat A with the least degree which represents the least direct connection it can made might more appropriate in connecting different groups.

seat B and C with same high degree and closeness indicate that they not only have a large number of direct connections, but also include the shortest average distance from them to other seats.At that time, seat B as relatively higher betweeness represents that it also plays an important tole on connecting other groups. Seat B and C are beneficial for those who wanna maximize their direct connection and communicate easily.

Seat D with the lowest closeness and betweenness might less important and beneficial to network. However, it also has the largest number of degree (direct connection).


```{r}

plot(g, vertex.size = degree_centrality * 5, main = "Fakebook Bus Network ")
```



