# # Written by Felicity Charles
# 28/10/2025
# Caveat emptor

# The following function code was adapted from https://github.com/nspope/radish/blob/master/R/radish_graph.R so conductance_surface() could be run in R version 4.5.1 and using more efficient spatial libraries

conductance_surface <- function(covariates, coords, directions = 4, saveStack = T) {
  
  # Produce warning if missing cells are not identical across layers
  spdat <- as.matrix(values(covariates))
  missing <- is.na(rowSums(spdat))
  if(!all(apply(spdat[missing,,drop=FALSE], 1, function(x) all(is.na(x)))))
    warning("Missing cells are not identical across rasters; be careful regarding model selection (see ?conductance_surface)")
  
  # share missing cells across layers and ensure graph is fully connected
  if (any(missing))
  {
    spdat[missing,] <- NA
    cr <- covariates[[1]]
    values(cr) <- ifelse(missing, NA, 1) #b/c of how patches handles zeros
    cr <- patches(cr, directions = directions, zeroAsNA = T)
    connected_component <- names(which.max(table(values(cr)))) # Using the largest connected subgraph
    disconnected <- values(cr) != as.integer(connected_component)
    disconnected[is.na(disconnected)] <- FALSE
    if(any(disconnected))
      warning(paste("Pruned", sum(disconnected), "disconnected cells across rasters"))
    missing <- disconnected | missing
    spdat[missing] <- NA
    for(i in 1:nlyr(covariates))
      values(covariates[[i]]) <- spdat[,i]
} # Close if any missing
  
  # get adjacency list
  adj <- adjacent(covariates[[1]], cells = which(!missing), target = which(!missing), directions = directions)
  
  # Find cells of demes (i.e., population subdivisions)
  cells <- unmapped_cells <- cellFromXY(covariates[[1]], coords)
  
  # Remove NAs and remap indices for adjacency list to be contiguous
  map <- cbind(1:length(which(!missing)), (1:ncell(covariates[[1]]))[which(!missing)])
  spdat <- spdat[!missing,, drop = F]
  adj[, 1] <- map[match(adj[,1], map[,2]), 1]
  adj[, 2] <- map[match(adj[,2], map[,2]), 1]
  cells <- map[match(cells, map[,2]), 1]
  
  # Check that cells lie on connected portion of raster
  if(any(is.na(values(covariates[[1]])[unmapped_cells])))
    stop("At least one population subdivision is located on a missing cell")
  
  # Figure out which raster layers are factors
  is_factor <- sapply(1:nlyr(covariates), function(i) terra::is.factor(covariates[[i]]))
  spdat <- as.data.frame(spdat)
  if (any(is_factor)){
    factors <- names(covariates)[is_factor]
    warning("Treating covariates \"", paste(factors, collapse = "\"\""), "\" as factors")
    for(i in factors){
      layer_idx <- which(names(covariates) == i)
      levels    <- terra::cats(covariates[[layer_idx]])[[1]]
      
      # Handle different possible column names in cats
      if ("ID" %in% names(levels)) {
        ids <- levels$ID
      } else if ("value" %in% names(levels)) {
        ids <- levels$value
      } else {
        ids <- levels[[1]]
      }
      
      if ("VALUE" %in% names(levels)) {
        level_names <- levels$VALUE
      } else if (ncol(levels) > 1) {
        level_names <- levels[[2]]
      } else {
        level_names <- ids
      }
      
      spdat[,i] <- factor(level_names[match(spdat[,i], ids)])
    } # Close for factors
  } # Close if any is factors
    
  # Form and factorise Laplacian
  N <- nrow(spdat)
  Q <- Matrix::sparseMatrix(i = adj[,1], j = adj[,2], dims = c(N,N),
                            x = -rep(1, nrow(adj)),
                            use.last.ij = T)
  Q <- Matrix::forceSymmetric(Q)
  Qd <- Matrix::Diagonal(N, x = -Matrix::rowSums(Q))
  In <- Matrix::Diagonal(N)[-N,]
  Qn <- Matrix::forceSymmetric(In %*% (Q + Qd) %*% Matrix::t(In))
  adj <- rbind(Q@i, rep(1:Q@Dim[2] - 1, diff(Q@p))) # Upper-triangular, 0-based
  LQn <- Matrix::Cholesky(Qn, LDL = T)
  
  out <- list("demes" = cells,
             "x" = spdat,
             "adj" = adj,
             "covariates" = colnames(spdat),
             "laplacian" = Q,
             "choleski" = LQn,
             "stack" = if(saveStack) covariates else NULL)
  out
    
}   # Close conductance_surface 
