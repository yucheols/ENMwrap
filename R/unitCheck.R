unitCheck <- function(ref.env, proj.env, n) {
  require(dplyr)
  require(dismo)

  results <- list()

  name.ref.env <- sort(names(ref.env))
  name.proj.env <- sort(names(proj.env))

  pts <- randomPoints(mask = ref.env[[1]], n = n) %>% as.data.frame()

  check <- for (i in 1:length(name.ref.env)) {
    ref.ex <- raster::extract(ref.env, pts) %>% na.omit() %>% as.data.frame()
    proj.ex <- raster::extract(proj.env, pts) %>% na.omit() %>% as.data.frame()

    ref.max <- max(ref.ex[[name.ref.env[[i]]]])
    proj.max <- max(proj.ex[[name.proj.env[[i]]]])

    results[[i]] <- data.frame(ref.max, proj.max)
  }
  results <- dplyr::bind_rows(results)
  results$var.name = name.ref.env
  return(results)
}
