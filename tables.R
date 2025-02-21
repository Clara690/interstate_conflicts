library(gt)
library(tidyverse)
library(tibble)
library(igraph)
library(glue)
# load the data
df <- read.csv('./data/MIDB 5.0.csv')
# remove rows before second world war
df <- df %>%
  filter(styear >= 1945 & styear <= 1991)

# columns needed to be kept
keeps <- c('dispnum', 'stabb', 'sidea')
df_cleaned <- df %>%
  select(all_of(keeps))
# remove conflicts where only 2 states were involved
df_cleaned <- df_cleaned %>% 
  group_by(dispnum) %>% 
  filter(n() > 2) %>% 
  ungroup()
# remove states who had no alliance in a conflict
df_cleaned <- df_cleaned %>% 
  group_by(dispnum, sidea) %>% 
  filter(n() > 1) %>% 
  ungroup()

# initialize a matrix to store the data
alliance_pair <- matrix(ncol = 2)

# split states that were in the same conflict but on different side
for (num in unique(df_cleaned$dispnum)){
  # select all countries in the same conflicts
  df_sub <- df_cleaned[df_cleaned$dispnum == num, ]
  side_a <-  c(df_sub$stabb[df_sub$sidea == 1])
  side_b <-  c(df_sub$stabb[df_sub$sidea == 0])
  if (length(side_a) >= 2){
    results <- t(combn(side_a, 2))
    alliance_pair <- rbind(alliance_pair, results)
  }
  if (length(side_b) >= 2){
    results <- t(combn(side_b, 2))
    alliance_pair <- rbind(alliance_pair, results)
  }
}
# remove NA from the matrix
alliance_pair <- alliance_pair[rowSums(is.na(alliance_pair)) == 0, ]
# data handling and generate igraph object
G_alliance <- graph_from_edgelist(alliance_pair)
mtx_alliance <- as.matrix(alliance_pair)
undG_alliance <- as.undirected(G_alliance, mode = 'collapse')

# centrality stat of graphs
sta_stat <- centrality(undG_alliance, alpha = 1)
# node level attributes
n_nodes <- vcount(undG_alliance)
n_edges <- ecount(undG_alliance)

avg_degree <- as.character(glue(
  '{round(mean(sta_stat$OutDegree), 2)}±{round(sd(sta_stat$OutDegree), 2)}'))
# avg_degree <- mean(sta_stat$InDegree)
rng <- as.character(
  glue('{round(range(sta_stat$Betweenness), 2)[1]}~ {round(range(sta_stat$Betweenness), 2)[2]}'))

tbl_descip <- tibble(
  'term' = c('Node counts','Edge counts','Average degree', 'Betweenness'),
  'value' = as.character(c(n_nodes, n_edges, avg_degree, rng)))

#-------------------------------Visualization------------------------------
##########
# table 1 #
##########
gt(tbl_descip,
   rowname_col = 'term') %>% 
  tab_header(title = md('**Table 1: Node-level Statistics**'),) %>% 
  tab_options(source_notes.font.size = '10px') %>% 
  tab_source_note(md('*Source: Palmer, Glenn, Roseanne W. McManus,Vito D’Orazio, Michael R. Kenwick, Mikaela Karstens, Chase Bloch, Nick Dietrich, Kayla Kahn, Kellan Ritter, Michael J. Soules. 2020.*')) %>% 
  cols_align(
    align = 'center',
    columns = 'value'
  ) %>% 
  cols_width(
    term ~ px(200),
    everything() ~ px(100)
  )

##########
# table 2 #
##########
# top 5 states by btw c
top5 <- names(sort(sta_stat$Betweenness, decreasing = TRUE)[1:5])
# fetch the corresponding values for degree c
deg_c <- sta_stat$OutDegree[names(sta_stat$OutDegree) %in% top5]
tbl_sta <- tibble(
  'state' = top5,
  'Between C' = sort(sta_stat$Betweenness, decreasing = TRUE)[1: 5],
  'Degree C' = deg_c
)
gt(tbl_sta,
   rowname_col = 'state') %>% 
  fmt_number(decimals = 1) %>% 
  tab_header(md('**Table 2: Top 5 Countries by Betweenness Centrality**')) %>% 
  tab_source_note(md('*Source: Palmer, Glenn, Roseanne W. McManus,Vito D’Orazio, Michael R. Kenwick, Mikaela Karstens, Chase Bloch, Nick Dietrich, Kayla Kahn, Kellan Ritter, Michael J. Soules. 2020.*')) %>% 
  tab_options(heading.align = 'center',
              source_notes.font.size = 'small') %>% 
  cols_align(
    align = 'center',
    columns = c('Degree C', 'Between C')
  ) %>% 
  cols_width(
    state ~ px(100),
    everything() ~ px(100)
  )

# network level attributes
d <- round(igraph::edge_density(undG_alliance),2) # density
# transitivity
t <- round(igraph::transitivity(undG_alliance, type = 'undirected'), 2)  
# shortest path between node i and node j 
pair_shortest_path <- as.vector(sta_stat$ShortestPathLengths[
  sta_stat$ShortestPathLengths > 0])

############
# histogram 1 #
###########
ggplot(data.frame(pair_shortest_path), aes(x = pair_shortest_path)) +
  geom_histogram(binwidth = 0.5, fill = 'cadetblue1') +
  labs(title = 'Distribution of Shortest path',
       subtitle = 'Data from 1945 to 1991',
       tag = 'Figure 1',
       x = 'Path Lengths',
       y = 'Count',
       caption = 'Figures caculated by the authors') +
  theme_light()

#################################
# table 3 network level attribute # 
#################################

tbl_ntw <- tibble('term' = c('Density', 'Reciprocity','Transitivity'),
                  'value' = c(d, NA, t))
gt(tbl_ntw, rowname_col = 'term') %>% 
  tab_header(md('**Table 3: Network Statistics**')) %>% 
  tab_source_note(md('*Source: Palmer, Glenn, Roseanne W. McManus,Vito D’Orazio, Michael R. Kenwick, Mikaela Karstens, Chase Bloch, Nick Dietrich, Kayla Kahn, Kellan Ritter, Michael J. Soules. 2020.*')) %>% 
  tab_options(heading.align = 'center',
              source_notes.font.size = 'small') %>% 
  cols_align(
    align = 'center',
    columns = c('value')
  ) %>% 
  cols_width(
    term ~ px(100),
    everything() ~ px(100)
  )
############
# histogram 1 #
###########
ggplot(data.frame(pair_shortest_path), aes(x = pair_shortest_path)) +
  geom_histogram(binwidth = 0.5, fill = 'cadetblue1') +
  labs(title = 'Distribution of Shortest path',
       subtitle = 'Data from 1945 to 1991',
       tag = 'Figure 1',
       x = 'Path Lengths',
       y = 'Count',
       caption = 'Figures caculated by the authors') +
  theme_light()