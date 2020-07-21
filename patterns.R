library(ggplot2)

# creates custom patterns and saves in svg format

checkers <- function(target, width, height, thickness, n){
  svg(target, width=width, height=height, pointsize = 1)
  gg = ggplot(x=c(0,1), y=c(0,1)) 
  
  for (i in 0:n){
    gg = gg + geom_vline(xintercept=i*(1/n), size=thickness) + geom_hline(yintercept=i*(1/n), size=thickness) 
  }
  gg = gg + theme(plot.margin=unit(c(0, 0, 0 , 0), "cm")) + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme_void()
  plot(gg)
  dev.off()
}
gg = checkers("D:/umd/summer 2020/patterns/svgPatterns/cross.svg", 8, 5, 0.5, 50)


horizontal <- function(target, width, height, thickness, n){
  svg(target, width=width, height=height, pointsize=1)
  gg = ggplot(x=c(0,1), y=c(0,1)) 
  
  for (i in 0:n){
    gg = gg + geom_hline(yintercept=i*(1/n), size=thickness) 
  }
  gg = gg + theme(plot.margin=unit(c(0, 0, 0 , 0), "cm")) + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme_void()
  plot(gg)
  dev.off()
}
gg = horizontal("D:/umd/summer 2020/patterns/svgPatterns/horizontal.svg", 8, 5, 1, 50)


vertical <- function(target, width, height, thickness, n){
  svg(target, width=width, height=height, pointsize=1)
  gg = ggplot(x=c(0,1), y=c(0,1)) 
  for (i in 0:n){
    gg = gg + geom_vline(xintercept=i*(1/n), size=thickness) 
  }
  gg = gg + theme(plot.margin=unit(c(0, 0, 0 , 0), "cm")) + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme_void()
  plot(gg)
  dev.off()
}
gg = vertical("D:/umd/summer 2020/patterns/svgPatterns/vertical.svg", 8, 5, 1, 50)

positiveDiagnol <- function(target, width, height, slope, thickness, n){
  svg(target, width=width, height=height, pointsize = 1)
  gg = ggplot(x=c(0,1), y=c(0,1)) + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme_void()
  for (i in 0:n){
    gg = gg + geom_abline(slope=slope, intercept=i*(1/n), size=thickness)
    gg = gg + geom_abline(slope=slope, intercept=-i*(1/n), size=thickness) 
  }
  plot(gg)
  dev.off()
}
gg = positiveDiagnol("D:/umd/summer 2020/patterns/svgPatterns/diagnol.svg", 8, 5, 1, 1, 50)

negativeDiagnol <- function(target, width, height, slope, thickness, n){
  svg(target, width=width, height=height, pointsize=1)
  gg = ggplot(x=c(0,1), y=c(0,1)) + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme_void()
  for (i in 0:n){
    gg = gg + geom_abline(slope=-slope, intercept=i*(1/n), size=thickness)
    gg = gg + geom_abline(slope=-slope, intercept=1 + (i*(1/n)), size=thickness) 
  }
  plot(gg)
  dev.off()
}
gg = negativeDiagnol("D:/umd/summer 2020/patterns/svgPatterns/negativediagnol.svg", 8, 5, 1, 1, 50)
