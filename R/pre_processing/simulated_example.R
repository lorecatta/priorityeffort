create_cons_feat_array(){
  
  n <- 9
  FaunalG <- 3
  EcologicalG <- 3  
  
  sp_names <- seq_len(n)
  area_of_occ <- 0
  target <- 0
  spf <- 0
  FaunalGroup <- seq_len(FaunalG)
  EcologicalGroup <- seq_len(EcologicalG)
    
  data.frame(sp_names = sp_names,
             area_of_occ = area_of_occ,
             target = target,
             spf = spf,
             FaunalGroup = FaunalGroup,
             EcologicalGroup = EcologicalGroup)  
  
  
}

create_site_species_array(){
  
  n_sp <- 9
  n_pu <- 100
  max_area <- 100
    
    
  out <- matrix(0, nrow = n_pu, ncol = n_sp)
  
  ids <- replicate(n_sp, sample(0:1, n_pu, replace = TRUE))
  
  area_vals <- rnorm(sum(ids), mean = 30, 1)
  
  out[ids==1] <- area_vals

  out
  
}

create_site_threat_array(){
  
  n_pu <- 100
  n_threats <- 2
  
  replicate(n_threats, runif(n_pu)) 
  
}

create_species_responses(){
  
  FaunalG <- 3
  n_threats <- 2
  n_response_types <- 3 
  n_intensity_cat <- 3
  
  x_vals <- c(1, 2, 3) * (1 / 3)  
  uncert <- 0.25
  
  fun_list <- vector("list", n_response_types)
  fun_list[[1]] <- function(x){out <- 1-exp(-6*x); return(out)} #convex
  fun_list[[2]] <- function(x){out <- 1/(1+exp(-15*(x-0.5))); return(out)} #sigmoid
  fun_list[[3]] <- function(x){out <- x; return(out)} #linear
  
  resp_type <- sample(seq_len(n_response_types), 1)  
  
  get_blu <- function(i){
    my_fun <- fun_list[[i]]
    b_gss <- my_fun(x_vals)
    low_bnds <- qnorm(0.1, mean = b_gss, sd = uncert)
    upp_bnds <- qnorm(0.9, mean = b_gss, sd = uncert)
  }
  
}


library(RandomFields)
n <- 100
x <- runif(n=n, min=-1, max=1)
y <- runif(n=n, min=-1, max=1)
data <- RFsimulate(model = RMgauss(), 
                   methods::as(x, "SpatialPoints")@coords,
                   n = 2,
                   spConform = FALSE)
plot(data)
