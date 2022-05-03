#' @title myDDT
#'
#' @param ddt the data set to be used
#' @param Species
#'
#' @return A plot
#' @export
#'
#' @examples
#' \dontrun{myDDT(ddt, Species = "CCATFISH") }
myDDT = function(ddt, Species){
  library(ggplot2)
  print(DavidsPackage::ddt)
  ddt1 = subset(DavidsPackage::ddt, SPECIES == Species)
  print(ddt1)
  rf = table(ddt1)/length(ddt1)
  rf
  write.table(ddt1, file = "LvsWforSPECIES.csv")
  quad.lm = lm(LENGTH~WEIGHT + I(WEIGHT ^ 2), data = ddt1)
  ggScatterPlot <- ggplot(ddt1, aes(x = WEIGHT, y = LENGTH, fill = SPECIES, color = RIVER))+
    geom_point()+ labs(title = "David Hadden", x = "WEIGHT", y = "LENGTH")
  ggScatterPlot + theme_classic()+geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)


}
