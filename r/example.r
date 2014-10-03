
#load required libraries
library(plyr)
library(doSNOW)

#set number of clusters
n_clusters <- 4

#set write path for examples
write_path <- "~/"


#set up parallelization backend
cl<-makeCluster(n_clusters)
registerDoSNOW(cl)

#these should be equal
n_clusters
getDoParWorkers()

#give each worker an ID 
#this is optional, but occasionally is useful 
#in situations where you need workers to write data
#every worker writing to the same file will result in 
#uncaught errors and partially overwritten data
#worker ids enable each worker to write to its own set of 
#files
clusterApply(cl, seq(along=cl), function(id) WORKER.ID <<- paste0("worker_", id))


#example 1:
#each worker here writes to its own file
partime <- system.time({
  
  l_ply(c(1:50), .parallel=T, function(.item) {  
  cat(paste0("Rep: ", .item, " Worker ID: ", WORKER.ID, "\n"), file = paste0(write_path, WORKER.ID, ".txt"), append = T)
  print("You don't see this because parallel.")
})

})


sequentime <- system.time({
  
  l_ply(c(1:50), .parallel=F, function(.item) {  
    cat(paste0("Rep: ", .item, "\n"), file = paste0(write_path, "no_parallel.txt"), append = T)
    print("You do see this because not parallel.")
  })  
  
})


if ( (sequentime[3] - partime[3]) > 0) {print(paste0("Sweeeeeeet")  )}


#example 2:
#bootstrap t tests p values

partime <- system.time({
  
  l_ply(c(1:1000), .parallel=T, function(.item) { 
    p <- t.test(rnorm(1e5))$p.value
    cat(paste0(p, ", ", .item, ", ", WORKER.ID, "\n"), file = paste0(write_path, WORKER.ID, "_p.txt"), append = T)
  })
  
})

results <- list()
for (i in paste0(1:n_clusters) ) {
  results[[i]] <- read.table(file = paste0(write_path, "worker_", i, "_p.txt"), sep = ",", header = F, stringsAsFactors = F)
}

results <- ldply(results)[,-1]
names(results) <- c("p", "rep", "id")
hist(results$p)


sequentime <- system.time({
  
  l_ply(c(1:1000), .parallel=F, function(.item) {  
    p <- t.test(rnorm(1e5))$p.value
    cat(paste0(p, ", ", .item, "\n"), file = paste0(write_path, "no_parallel_p.txt"), append = T)
   })  
  
})


if ( (sequentime[3] - partime[3]) > 0) {print(paste0("Sweeeeeeet")  )}

