printer <- function (links, nodes){ 
  
  forceNetwork(Links = links, Nodes = nodes, Source = "source",
               Target = "target", Value = 1, NodeID = "name",
               Group = "group", opacity = 1, zoom = TRUE)

  
}

