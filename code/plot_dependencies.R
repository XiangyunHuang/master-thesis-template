#' Plot network of package dependencies
#'
#' @param pkg package description, can be path or package name. See \code{\link[devtools]{as.package}} for
#'    more information.
#'
#' @details The resulting plot visualizes the network of package dependencies. If you are trying to cut down
#'    on package dependencies look for big red dots that represent a lot of upstream but few downstream
#'    dependencies.
#' @import ggplot2
#' @export
#'
#' @examples


plot_dependencies <- function(pkg = ".") {
  library("devtools")
  library("miniCRAN")
  library("igraph")
  library("ggplot2")
  library("ggnetwork")

  pkg <- devtools::as.package(pkg)

  dependencies <- unlist(strsplit(pkg$imports, split = "\n"))[-1]
  dependencies <- gsub("\\n| \\(.+\\)|,", "", dependencies)
  dependency_graph <- miniCRAN::makeDepGraph(dependencies, suggests = FALSE, enhances = FALSE)
  class(dependency_graph) <- "igraph"
  dependency_graph <- dependency_graph + igraph::vertices(pkg$package) + igraph::edges(as.vector(rbind(dependencies, pkg$package)))
  dependency_graph <- igraph::simplify(dependency_graph)

  edge_list <- igraph::get.edgelist(dependency_graph)
  dependency_graph <- igraph::graph(rbind(edge_list[, 2], edge_list[, 1]))

  dependency_graph_df <- ggnetwork::ggnetwork(
    dependency_graph
    ,
    layout = "fruchtermanreingold"
    , arrow.gap = 0.015
    , layout.par = list(niter = 5000)
  )

  dependency_graph_df$package <- dependency_graph_df$vertex.names
  dependency_graph_df$face <- ifelse(dependency_graph_df$package == pkg$package, "bold", "plain")

  dependency_graph_df$n_dependencies <- as.vector(table(gsub("\\|.+", "", attr(igraph::E(dependency_graph), "vnames")))[as.character(dependency_graph_df$package)])
  dependency_graph_df$n_dependencies[is.na(dependency_graph_df$n_dependencies)] <- 0

  dependency_graph_df$importance <- as.vector(table(gsub(".+\\|", "", attr(E(dependency_graph), "vnames")))[as.character(dependency_graph_df$package)])
  dependency_graph_df$importance[is.na(dependency_graph_df$importance)] <- 0

  max_downstream_deps <- max(dependency_graph_df$importance)

  dependency_graph_df$importance <- dependency_graph_df$importance / max_downstream_deps
  dependency_graph_df$importance <- abs(1 - dependency_graph_df$importance)

  dependency_graph_df <- as.data.frame(lapply(dependency_graph_df, as.vector))

  ggplot(dependency_graph_df, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_nodes(aes(color = n_dependencies), size = 6.5, alpha = 0.4) +
    geom_edges(arrow = arrow(length = unit(4, "pt"), type = "closed"), color = grey(0.4)) +
    geom_nodelabel_repel(
      aes(label = package, fontface = face, color = n_dependencies)
      ,
      box.padding = unit(8, "pt")
    ) +
    geom_nodes(aes(color = n_dependencies, size = 7 * importance)) +
    scale_color_distiller(palette = "Spectral") +
    scale_size(labels = function(x) abs(max_downstream_deps - ceiling(x / 7 * max_downstream_deps))) +
    theme_blank(legend.position = "top") +
    guides(
      size = guide_legend(title = "Downstream dependencies", title.position = "top", title.hjust = 0.5, label.position = "bottom", label.hjust = 0.5)
      , color = guide_colorbar(title = "Upstream dependencies", title.position = "top", title.hjust = 0.5, barwidth = unit(130, "pt"), barheight = unit(4, "pt"))
    )
}