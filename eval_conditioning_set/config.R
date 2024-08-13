cluster.functions = makeClusterFunctionsSSH(
  list(Worker$new("localhost", 
                  ncpus = as.integer(parallel::detectCores() * .75), 
                  max.load = as.integer(parallel::detectCores() * .85))
  )
)