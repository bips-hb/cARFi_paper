cluster.functions = makeClusterFunctionsSSH(
  list(Worker$new("localhost", 
                  ncpus = as.integer(parallel::detectCores() * .2), 
                  max.load = as.integer(parallel::detectCores() * .85))
  )
)