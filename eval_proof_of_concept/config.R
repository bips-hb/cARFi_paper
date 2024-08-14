cluster.functions = makeClusterFunctionsSSH(
  list(Worker$new("localhost", 
                  ncpus = as.integer(parallel::detectCores() * .8), 
                  max.load = as.integer(parallel::detectCores() * .85))
  )
)