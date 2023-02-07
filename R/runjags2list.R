#' @title Convert runjags posterior to list
#' @description Convert runjags posterior to named list of vectors or arrays.
#'
#' @param post list of class `runjags`. The output from a call to 
#'   \link[runjags]{run.jags}.
#' @param collapse.chains return array with dimension for each chain?
#'
#' @note If \code{collapse.chains = TRUE}, the last dimension of arrays will always 
#'   be samples from the posterior. If \code{collapse.chains = FALSE}, the last 
#'   dimension of arrays will be individual chains, and the one prior to that 
#'   will be samples from the posterior for each chain.
#' 
#' @seealso
#'   \link[base]{aperm} to transpose the array if necessary.   
#'   \link[base]{as.data.frame.table} to convert arrays to data.frames.
#'   
#' @export

runjags2list <- function(post, collapse.chains = TRUE) {
  if(!methods::is(post, "runjags")) stop("'post' must be of class 'runjags'")
  params <- colnames(runjags::combine.mcmc(post$mcmc))
  
  p <- sapply(post$monitor, function(p) {
    p1 <- paste0("^", p, "$")
    p2 <- paste0("^", p, "\\[")
    grep(paste0(p1, "|", p2), params, value = TRUE)
  }, simplify = FALSE)
  
  sapply(names(p), function(x) {
    dim.nums <- gsub(x, "", p[[x]])
    dim.nums <- gsub("\\[|\\]", "", dim.nums)
    dim.nums <- do.call(rbind, strsplit(dim.nums, ","))
    dim.nums <- apply(dim.nums, 2, as.numeric)
    
    post.p <- runjags::combine.mcmc(
      post$mcmc, 
      vars = x, 
      collapse.chains = collapse.chains
    )
    if(!is.list(post.p)) post.p <- list(post.p)
    
    mat <- lapply(post.p, function(chain) {
      if(length(dim.nums) == 0) {
        stats::setNames(as.vector(chain), rownames(chain))
      } else {
        dims <- apply(dim.nums, 2, max)
        arr <- array(as.vector(t(chain)), dim = c(dims, nrow(chain)))
        dimnames(arr)[length(dims) + 1] <- list(rownames(chain))
        arr
      }
    })
    
    if(length(dim.nums) == 0) {
      if(collapse.chains) mat[[1]] else do.call(cbind, mat) 
    } else {
      iter.dim <- ncol(dim.nums) + if(collapse.chains) 1 else 2
      do.call(abind::abind, c(mat, list(along = iter.dim)))
    }
  }, simplify = FALSE)
}