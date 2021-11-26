gen.simple.gal.plot <- function() {
  steps <- 0:10*5
  
  map_dfr(steps, function(s) {
    d <- gen.party.by.grp.df(s)
    g <- calc.test.gall(d)
    
    c(
      step = s,
      gall = g
    )
  })
}