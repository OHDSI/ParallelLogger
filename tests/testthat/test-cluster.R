library("testthat")

test_that("Computation across 3 threads", {
  fun <- function(x) {
    return(x^2)
  }
  
  cluster <- makeCluster(numberOfThreads = 3)
  x <- clusterApply(cluster, 1:10, fun)
  stopCluster(cluster)
  
  expect_equal(length(x), 10)
  expect_equal(sum(unlist(x)), sum((1:10)^2))
})

test_that("Create a cluster of nodes for parallel computation", {
  f <- function() {
    "test"
  }
  ParallelLogger:::registerDefaultHandlers()
  
  summary(f())
  
  cluster <- ParallelLogger::makeCluster(2)
  res <- ParallelLogger::clusterApply(cluster, f(), summary)
  ParallelLogger::stopCluster(cluster)
  
  expect_equal(length(res), 1)
})

test_that("Test require package", {
  tryCatch(
    expr = {
      package <- "test invalid pkg for failing"
      cluster <- ParallelLogger::makeCluster(1)
      
      res <- capture.output(capture.output(out <- ParallelLogger::clusterRequire(cluster, package), type = "message"), type = "output")
      
      expectLoading <- sprintf("Loading required package: %s", package)
      expectWarning <- sprintf("Warning: there is no package called ‘%s’", package)
      
      expect_equal(out, FALSE)
      expect_true(grepl(expectLoading, res[1]))
      expect_true(grepl(expectWarning, res[2]))
    },
    error = function(e) {
    },
    warning = function(w) {
      
    },
    finally = {
      
    }
  )
})

test_that("Check andromedaTempFolder", {
  check <- "c:\\test"
  ParallelLogger:::doSetAndromedaTempFolder(check)
  expect_true(!is.null(getOption("andromedaTempFolder")))
  expect_equal(getOption("andromedaTempFolder"), check)
})


test_that("Test getThreadNumber", {
  expect_equal(getThreadNumber(), 0)
  
  fun <- function(x) {
    return(ParallelLogger::getThreadNumber())
  }
  
  cluster <- makeCluster(numberOfThreads = 3)
  x <- clusterApply(cluster, 1:3, fun)
  stopCluster(cluster)
  
  expect_equal(sort(unlist(x)), 1:3)
})


test_that("Test getPhysicalMemory", {
  expect_false(is.na(getPhysicalMemory()))
})
