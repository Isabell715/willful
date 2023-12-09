#' Scatter Plot indicating the utility difference w/o inquiry
#'
#' The function only works when the definition of variables ends with "_list"
#'
#' The function plot the relationship between expected utility after inquiry and
#' expected utility without inquiry, given two variables x and y
#'
#' I first create 3 vectors of the same length, making x and y and the final
#' utility difference a one to one match
#'
#' Then I use ggplot to group z into 3, assigning different groups different colors
#' @examples
#' X_list <- seq(0.00, 10, by=.01);
#' alpha_list <- seq(0.5, 1, by = .001);
#' scatter(X_list, k_list)
#' @export
scatter <- function(x, y, z = matrixW){
  #Define variables and put into dataframe
  vecX <- c(rep(x, times = length(y))) #x_1, x_2, x_3...
  vecY <- c(rep(y, each = length(x))) #y_1, y_1, y_1...
  vecW<- as.data.frame(z) %>%
    unlist(use.names=FALSE) #(x_1, y_1), (x_2, y_1), (x_3, y_1)...
  df <- data.frame(x = vecX, y = vecY, z = vecW)

  #For labs and for figure title
  tit_x <- deparse(substitute(x)) %>%
    stri_sub(1, length(deparse(substitute(x)))-7)
  tit_y <- deparse(substitute(y)) %>%
    stri_sub(1, length(deparse(substitute(y)))-7)

  #draw scatter plot
  #dev.new(width=7, height=5, unit="in")
  plot1 <- ggplot(df, aes(x, y)) +
    geom_point(aes(colour = 'W^n < W^i'), data = df %>% filter(z > 0),
               show.legend = T, size = 0.1)+
    geom_point(aes(colour = 'W^n = W^i'),data = df %>% filter(z == 0),
               show.legend = T, size = 0.1)+
    geom_point(aes(colour = 'W^n > W^i'),data = df %>% filter(z < 0),
               show.legend = T,  size = 0.1)+
    scale_color_brewer(palette="Greens")+#greys
    theme_bw()+
    theme(legend.title = element_blank())+#remove legend title
    guides(colour = guide_legend(override.aes = list(size=5)))+#manualy set the size of the legend
    labs(x=tit_x, y = tit_y)
  ggsave(paste(tit_x,"-",tit_y,".pdf"), width = 7, height = 5, unit="in")
  #dev.off()
  return(plot1)
}
