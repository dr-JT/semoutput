## https://kateto.net/netscix2016.html

library(lavaan)

data <- HolzingerSwineford1939

factors <- c("visual", "textual", "speed")
model <- '
# latent factors
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9

# correlated errors

# constraints

'
fit <- cfa(model = model, data = data,
           mimic = "lavaan", missing = "ML",
           std.lv = TRUE, std.ov = FALSE,
           se = "standard", bootstrap = 1000)


## igraph
library(igraph)
library(dplyr)

loadings <- standardizedSolution(fit, level = 0.95) %>%
  filter(op == "=~")

latent_paths <- standardizedSolution(fit, level = 0.95) %>%
  filter(op == "~~",
         lhs %in% factors,
         !is.na(pvalue),
         lhs != rhs)

nodes_latent <- loadings %>%
  mutate(type = "latent",
         shape = "circle",
         size = 35,
         size2 = NA) %>%
  select(id = lhs, type, shape, size, size2) %>%
  distinct()

nodes_manifest <- loadings %>%
  mutate(type = "manifest",
         shape = "rectangle",
         size = 30,
         size2 = 10) %>%
  select(id = rhs, type, shape, size, size2)

nodes <- bind_rows(nodes_latent, nodes_manifest) %>%
  mutate(x = c(2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1),
         y = c(2, 5, 8, 1, 2, 3, 4, 5, 6, 7, 8, 9))

links_loadings <- loadings %>%
  mutate(type = "loading", value = round(est.std, 2), arrow_curved = 0, arrow_mode = 2) %>%
  select(from = rhs, to = lhs, type, value, arrow_curved, arrow_mode)

links_latent <- latent_paths %>%
  mutate(type = "correlation", value = round(est.std, 2), arrow_curved = c(0,1,0), arrow_mode = 3) %>%
  select(from = rhs, to = lhs, type, value, arrow_curved, arrow_mode)

links <- bind_rows(links_loadings, links_latent)

diagram <- graph_from_data_frame(links, vertices = nodes, directed = TRUE)

layout <- norm_coords(cbind(nodes$x, nodes$y), xmin = -1, xmax = 0, ymin = -.7, ymax = .7)

plot(diagram,
     frame = FALSE,
     rescale = FALSE,
     layout = layout,
     vertex.color = "white",
     vertex.weight = 10,
     vertex.shape = nodes$shape,
     vertex.size = nodes$size,
     vertex.size2 = nodes$size2,
     vertex.label.color = "black",
     edge.label = links$value,
     edge.color = "black",
     edge.label.color = "black",
     edge.arrow.size = .15,
     edge.arrow.width = .75,
     edge.curved = links$arrow_curved,
     edge.arrow.mode = links$arrow_mode)



