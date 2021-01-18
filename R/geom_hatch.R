draw_key_hatch <- function(data, params, size) {
  if (is.null(data$linetype)) {
    data$linetype <- 0
  } else {
    data$linetype[is.na(data$linetype)] <- 0
  }

  print(data)
  print(params)
  grid::segmentsGrob(0.1, 0.5, 0.9, 0.5,
               gp = grid::gpar(
                 col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
                 fill = alpha(params$arrow.fill %||% data$colour
                              %||% data$fill %||% "black", data$alpha),
                 lwd = (data$size %||% 0.5) * .pt,
                 lty = data$linetype %||% 1,
                 lineend = "butt"
               ),
               arrow = params$arrow
  )
}



GeomHatch <- ggplot2::ggproto("GeomHatch", GeomSegment,
                             required_aes = c("x", "y", "xend", "yend"),

                             draw_key = draw_key_hatch
)

hatch <- ggplot2::ggproto("hatch", ggplot2::Stat,

                          required_aes = c("x", "y"),

                          setup_params = function(data, params) {

                            plt = ggplot(data=data, ggplot2::aes(x=x, y=y))
                            params$xDiff = ggplot_build(plt)$layout$panel_params[[1]]$x.range # gets x range
                            params$yDiff = ggplot_build(plt)$layout$panel_params[[1]]$y.range # gets y range

                            if (is.null(params$patternList)){ # creating pattern list if none given
                              patterns = c("-","|","/","\\", "x", "+", "")
                              nGroups = length(unique(data$group))
                              patterns = rep(patterns, ceiling(nGroups/length(patterns)))
                              patternList = vector(mode = "list", length = nGroups) # initializing patternList
                              params$patternList = lapply(1:nGroups, function(i){patternList[[i]] =
                                list(pattern = patterns[i])})
                            }


                            params
                          },

                          compute_group = function(data, scales, xDiff, yDiff, pointSize) {
                            currentPatternAes = patternList[[data$group[1]]]
                            pattern = currentPatternAes$pattern
                            lineCoords = drawPattern(x=data$x, y=data$y, xDiff=xDiff, yDiff=yDiff,
                                                     patternAes = currentPatternAes, pointSize = pointSize)

                            # if (is.null(currentPatternAes$angle)){ # what angle each pattern translates to
                            #   if (pattern %in% c("horizontal", "-", "vertical", "|")){
                            #     if (pattern %in% c("horizontal", "-")){ lineCoords$angle = 0}
                            #     if (pattern %in% c("vertical", "|")){ lineCoords$angle = -90}
                            #   }else{
                            #     if (pattern %in% c("positiveDiagonal", "/")){ lineCoords$angle = -135}
                            #     if (pattern %in% c("negativeDiagonal", "\\")){ lineCoords$angle = -45}
                            #     if (pattern %in% c("cross", "x")){ lineCoords$angle = list(-45, -135)}
                            #     if (pattern %in% c("checkers", "+")){ lineCoords$angle = list(0, -90)}
                            #   }
                            # }else{
                            #   lineCoords$angle = lapply(currentPatternAes$angle, function(i) -i)
                            # }

                            lineCoords
                          },

                          finish_layer = function(data, params){
                            #print(data)
                            #print(params)
                            return(data)
                          }
)

stat_hatch <- function(mapping = NULL, data = NULL, geom = GeomHatch,
                       position = "identity", na.rm = FALSE, show.legend = TRUE,
                       inherit.aes = TRUE, patternList = NULL, pointSize = NULL, ...) {




  ggplot2::layer(
    stat = hatch, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(patternList = patternList, pointSize = pointSize, na.rm = na.rm, ...)
  )
}



ggplot(data = pdacData, aes(x=Xt, y=Yt, group = location)) +
  geom_point(aes(color = location), size = 1) +
  stat_hatch(pointSize = 1)
