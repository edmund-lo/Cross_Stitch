process_image <- function(image_file_name, k_list){
  ## process_image(image, k_list) produces a list of information containing
  ## the image dataframe, the output of the kclust calls and the tidied clusters,
  ## for each k in k_list. This function uses kmeans to compute the clusterings.
  ## This function also finds the nearest DMC thread colour for each cluster.
  ##
  ## Input:
  ## - image: An image of class cimg.
  ## - k_list: A vector of the number of clusters in the clustering.
  ##
  ## Output:
  ## - A list of tibbles with the number of clusters, image dataframe, output of
  ##   kmeans clustering, and a tibble containing the tidied clusters, their associated 
  ##   RGB values and their nearest DMC thread colour information.
  ##
  ## Example:
  ##   library(tidyverse)
  ##   library(tidymodels)
  ##   library(imager)
  ##   library(dmc)
  ##   library(scales)
  ##   library(cowplot)
  ##   library(sp)
  ##   fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager')
  ##   image <- load.image(fpath)
  ##   cluster_info <- process_image(image, c(2,4,6,8))  
  
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  if(!require(imager)) {
    stop("The imager packages must be installed. Run install.packages(\"imager\") and then try again.")
  }
  if(!require(dmc)) {
    stop("The dmc packages must be installed. Run devtools::install_github(\"sharlagelfand/dmc\") and then try again.")
  }
  if(!require(scales)) {
    stop("The scales packages must be installed. Run install.packages(\"scales\") and then try again.")
  }
  if(!require(cowplot)) {
    stop("The cowplot packages must be installed. Run install.packages(\"cowplot\") and then try again.")
  }
  if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  
  set.seed(1000)
  
  tidy_dat <- as.data.frame(image, wide = "c") %>% rename(R = c.1, G = c.2, B = c.3)
  tidy_dat <- select(tidy_dat, c(-c.4))
  
  dat <- select(tidy_dat,c(-x,-y))
  
  cluster_info <- tibble(clusters = k_list, 
                         im_dat = vector("list", length(k_list)), 
                         kclust = vector("list", length(k_list)), 
                         info = vector("list", length(k_list)))
  
  for (i in 1:length(k_list)) {
    kclust <- kmeans(dat, centers = k_list[i], nstart = 4)
    
    centres <- tidy(kclust)
    
    centres <- centres %>% mutate(col = rgb(R,G,B))
    
    dmc_dat <- map(centres$col, ~ dmc(.x, visualize = FALSE))
    tidy_dmc <- bind_rows(dmc_dat)
    
    centres <- cbind(centres, 
                     dmc = as.character(tidy_dmc$hex), 
                     dmc_name = tidy_dmc$name, 
                     dmc_num = tidy_dmc$dmc)
    
    cluster_info$im_dat[[i]] <- tidy_dat
    cluster_info$kclust[[i]] <- kclust
    cluster_info$info[[i]] <- centres
  }
  
  return(cluster_info)
}


scree_plot <- function(cluster_info){
  ## scree_plot(cluster_info) produces and plots a scree plot using the number of clusters and 
  ## total within sum of squares from the cluster_info.
  ##
  ## Input:
  ## - cluster_info: A list of tibbles with the number of clusters, image dataframe, output of
  ##   kmeans clustering, and a tibble containing the tidied clusters, their associated 
  ##   RGB values and their nearest DMC thread colour information.
  ##
  ## Output:
  ## - A scree plot of the k number of clusters vs the total within sum of squares.
  ##
  ## Example:
  ##   library(tidyverse)
  ##   library(tidymodels)
  ##   library(imager)
  ##   library(dmc)
  ##   library(scales)
  ##   library(cowplot)
  ##   library(sp)
  ##   fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager')
  ##   image <- load.image(fpath)
  ##   cluster_info <- process_image(image, c(2,4,6,8))
  ##   scree_plot(cluster_info)
  
  kclusts <- tibble(k = cluster_info$clusters) %>%
    mutate(kclust = cluster_info$kclust, glanced = map(kclust, glance))
  
  clusterings <- kclusts %>% unnest(cols = c(glanced))
  
  scree <- ggplot(clusterings, aes(k, tot.withinss)) +
    geom_line() +
    geom_point() +
    xlab("k (Clusters)") +
    ylab("Total Within Sum of Squares") +
    ggtitle("Scree Plot")
  
  return(scree)
}


colour_strips <- function(cluster_info){
  ## colour_strips(cluster_info) produces and plots a colour strip for each k number of clusters.
  ## The colour strips contain the dmc colours for each cluster.
  ##
  ## Input:
  ## - cluster_info: A list of tibbles with the number of clusters, image dataframe, output of
  ##   kmeans clustering, and a tibble containing the tidied clusters, their associated 
  ##   RGB values and their nearest DMC thread colour information.
  ##
  ## Output:
  ## - A colour strip for each k number of clusters, containing the dmc colours for each cluster.
  ##
  ## Example:
  ##   library(tidyverse)
  ##   library(tidymodels)
  ##   library(imager)
  ##   library(dmc)
  ##   library(scales)
  ##   library(cowplot)
  ##   library(sp)
  ##   fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager')
  ##   image <- load.image(fpath)
  ##   cluster_info <- process_image(image, c(2,4,6,8))
  ##   colour_strips(cluster_info)
  
  strips = vector("list", length(cluster_info$clusters))
  
  square <- function(x, label_size) { 
    ggplot()  + 
      coord_fixed(xlim=c(0,1), ylim = c(0,1)) + theme_void() + 
      theme(plot.background = element_rect(fill = x)) + 
      geom_text(aes(0.5,0.5),label = x , size = label_size)
  }
  
  for (i in 1:length(cluster_info$info)) {
    t <- tibble(colours = as.character(cluster_info$info[[i]][,8]),
                squares = purrr::map(colours, ~ square(.x, 24/length(colours))))
    
    n_col = length(t$colours)
    rect_dat <- tibble(x1 = c(0:(n_col-1)), x2 = c(1:n_col), y1 = rep(0,n_col),
                       y2 =rep(1,n_col), colour = t$colours)
    
    strip <- rect_dat %>% ggplot() + coord_fixed() + 
      geom_rect(aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=colour), color="black") +
      geom_text(aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=colour), size=3) + 
      scale_fill_manual(values = rect_dat$colour, labels = rect_dat$colour)+ 
      theme_void() + theme(legend.position = "none")
    
    strips[[i]] <- strip
  }
  
  plot_strips <- plot_grid(plotlist = strips, labels = cluster_info$clusters, ncol = 1, label_size = 12)
  
  return(plot_strips)
}


make_pattern <- function(cluster_info, k, x_size, black_white = FALSE, background_colour = NULL){
  ## make_pattern(cluster_info, k, x_size, black_white = FALSE, background_colour = NULL) 
  ## produces a cross-stitch pattern that can be followed, with a legend that has thread colour,
  ## and a guide grid. This function can produce a coloured pattern or black-white pattern or
  ## a pattern without a specific background colour.
  ##
  ## Input:
  ## - cluster_info: A list of tibbles with the number of clusters, image dataframe, output of
  ##   kmeans clustering, and a tibble containing the tidied clusters, their associated 
  ##   RGB values and their nearest DMC thread colour information.
  ## - k: The chosen cluster size
  ## - x_size: The approximate total number of possible stitches in the horizontal direction
  ## - black_white: A logical to print the pattern in black and white if TRUE or colour 
  ##   if FALSE. By default, it is FALSE.
  ## - background_colour: The colour of the background, which should not be stitched in the
  ##   pattern. By default, it is NULL and so there is no colour.
  ##
  ## Output:
  ## - A plot of the cross-stitch pattern with a legend that has thread colour, and a guide grid.
  ##
  ## Example:
  ##   library(tidyverse)
  ##   library(tidymodels)
  ##   library(imager)
  ##   library(dmc)
  ##   library(scales)
  ##   library(cowplot)
  ##   library(sp)
  ##   fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager')
  ##   image <- load.image(fpath)
  ##   cluster_info <- process_image(image, c(2,4,6,8))
  ##   make_pattern(cluster_info, 6, 50, FALSE, NULL)
  
  index <- which(cluster_info$clusters == k)
  cluster <- cluster_info$kclust[[index]]$cluster
  
  res_data <- cbind(cluster_info$im_dat[[index]], cluster)
  res_data <- res_data %>% select(x,y,cluster)
  
  agg_image <- change_resolution(res_data, x_size)
  
  colour <- cluster_info$info[[index]] %>% select(cluster, dmc, dmc_name, dmc_num)
  colour$dmc <- as.character(colour$dmc)
  colour$dmc_num <- paste("(", as.character(colour$dmc_num), ")", sep = "")
  colour$dmc_name <- paste(as.character(colour$dmc_name), as.character(colour$dmc_num), sep = " ")
  
  shape <- c(1:k)
  
  colour <- cbind(colour, shape)
  
  final_dat <- left_join(agg_image, colour, by = "cluster")
  
  if(!is.null(background_colour)){
    final_dat <- subset(final_dat, final_dat$dmc != background_colour)
  }
  
  pattern <- final_dat %>% ggplot(aes(x, y)) + 
    geom_point(aes(colour = cluster, shape = cluster)) +
    scale_colour_manual(values = colour %>% select(cluster, dmc) %>% deframe,
                        label = colour %>% select(cluster, dmc_name) %>% deframe,
                        guide = guide_legend(ncol = 2)) +
    scale_shape_manual(values = colour %>% select(cluster, shape) %>% deframe,
                       label = colour %>% select(cluster, dmc_name) %>% deframe,
                       guide = guide_legend(ncol = 2)) +
    scale_y_reverse() +
    theme_minimal() +
    theme(legend.position="bottom") + 
    theme(legend.title = element_blank()) +
    ggtitle("Colour Cross-Stitch Pattern") +
    theme(panel.grid.major = element_line(colour="black", size = (1))) +
    coord_fixed(ratio = 1)
  
  bw_pattern <- final_dat %>% ggplot(aes(x, y)) + 
    geom_point(aes(shape = cluster)) +
    scale_shape_manual(values = colour %>% select(cluster, shape) %>% deframe,
                       label = colour %>% select(cluster, dmc_name) %>% deframe,
                       guide = guide_legend(ncol = 2)) +
    scale_y_reverse() +
    theme_minimal() +
    theme(legend.position="bottom") +
    theme(legend.title = element_blank()) +
    ggtitle("Black and White Cross-Stitch Pattern") +
    theme(panel.grid.major = element_line(colour="black", size = (1))) +
    coord_fixed(ratio = 1)
  
  if(black_white == TRUE) {
    return(bw_pattern)
  } else {
    return(pattern)
  }
}


change_resolution <- function(image_df, x_size) {
  ## change_resolution(image_df, x_size) subsamples an image to produce
  ## a lower resolution image. Any non-coordinate columns in the data
  ## frame are summarized with their most common value in the larger
  ## grid cell.
  ##
  ## Input:
  ## - image_df: A data frame in wide format. The x-coordinate column MUST
  ##             be named 'x' and the y-coordinate column MUST be named 'y'.
  ##             Further columns have no naming restrictions.
  ## - x_size:   The number of cells in the x-direction. The number of cells
  ##             in the vertical direction will be computed to maintain the 
  ##             perspective. There is no guarantee that the exact number
  ##             of cells in the x-direction is x_size
  ##
  ## Output:
  ## - A data frame with the same column names as image_df, but with fewer 
  ##   entries that corresponds to the reduced resolution image.
  ##
  ## Example:
  ##   library(imager)
  ##   library(dplyr)
  ##   fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager') 
  ##   im <- load.image(fpath)
  ##   im_dat<- as.data.frame(im,wide = "c") %>% rename(R = c.1, G = c.2, B = c.3) %>%
  ##            select(x,y,R,G,B)
  ##   agg_image <- change_resolution(im_dat, 50)
  
  if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  if(!require(dplyr)) {
    stop("The dplyr packages must be installed. Run install.packages(\"dplyr\") and then try again.")
  }
  
  sp_dat <- image_df 
  gridded(sp_dat) = ~x+y
  
  persp = (gridparameters(sp_dat)$cells.dim[2]/gridparameters(sp_dat)$cells.dim[1])
  y_size = floor(x_size*persp)
  orig_x_size = gridparameters(sp_dat)$cells.dim[1]
  orig_y_size = gridparameters(sp_dat)$cells.dim[2]
  
  x_res = ceiling(orig_x_size/x_size)
  y_res = ceiling(orig_y_size/y_size)
  
  gt = GridTopology(c(0.5,0.5), c(x_res, y_res),
                    c(floor(orig_x_size/x_res), floor(orig_y_size/y_res)))
  SG = SpatialGrid(gt)
  agg = aggregate(sp_dat, SG, function(x) names(which.max(table(x)))[1] )
  agg@grid@cellsize <- c(1,1)
  df <- agg %>% as.data.frame %>% rename(x = s1, y = s2)  %>% select(colnames(image_df))
  
  return(df)
  
}

