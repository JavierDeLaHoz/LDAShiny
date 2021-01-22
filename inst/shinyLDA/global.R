library(htmlwidgets)

################ exportation list options for graphic
export <- list(
    list(
        text = "png",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/png' }); }")
    ),
    list(
        text = "jpeg",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
    ),
    list(
        text = "svg",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/svg+xml' }); }")
    ),
    list(
        text = "pdf",
        onclick = JS("function () {
                   this.exportChart({ type: 'application/pdf' }); }")
    )
)




harmonicMean <- function(logLikelihoods, precision=2000L) {
    library("Rmpfr")
    llMed <- median(logLikelihoods)
    as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                         prec = precision) + llMed))))


}

###### Rescale numeric vector to have specified minimum "0" and maximum (1).
g4metric <- function (values){ columns <- base::subset(values, select = 2:ncol(values))
   values <- base::data.frame(values["topics"], base::apply(columns,
                                                             2, function(column) {
                                                                 scales::rescale(column, to = c(0, 1), from = range(column))
                                                             }))
    values <- reshape2::melt(values, id.vars = "topics", na.rm = TRUE)
    }

#######Avoid accidentally closing a bsModal
bsModalNoClose <-function(...) {
    b = bsModal(...)
    b[[2]]$`data-backdrop` = "static"
    b[[2]]$`data-keyboard` = "false"
    return(b)
}
