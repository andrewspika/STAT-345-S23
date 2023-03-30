# Function to take a tibble and returns a new ggplot with the NBA court drawn in on the graph
# Inputs: tibble as variable 'data'
# Outputs: ggplot with NBA court drawn in
NBAcourtWrite <- function(data = tibble()) {
  court <- data %>% ggplot() +
    geom_segment(aes(22, -4, xend=22, yend=10)) +
    geom_segment(aes(-25, -4, xend=25, yend=-4)) + 
    geom_segment(aes(-22, -4, xend=-22, yend=10)) + 
    geom_segment(aes(-6, -4, xend=-6, yend=15)) + 
    geom_segment(aes(6, -4, xend=6, yend=15)) + 
    geom_segment(aes(-25, -4, xend=-25, yend=43)) + 
    geom_segment(aes(25, -4, xend=25, yend=43)) +
    geom_segment(aes(-8, 15, xend=8, yend=15)) + 
    geom_segment(aes(-8, -4, xend=-8, yend=15)) + 
    geom_segment(aes(8, -4, xend=8, yend=15)) + 
    geom_segment(aes(-25, 43, xend=25, yend=43)) + 
    geom_circle(aes(x0=0, y0=15, r=6), inherit.aes = FALSE) + 
    geom_curve(aes(22, 10, xend=0, yend=24.65), curvature=0.3) +
    geom_curve(aes(0, 24.65, xend=-22, yend=10), curvature=0.3) +
    coord_fixed(ratio=1, xlim=c(-25, 25), ylim=c(-4, 43))
  
  return(court)
}
