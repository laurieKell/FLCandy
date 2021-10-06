### ------------------------------------------------------------------------ ###
### stock assessment: SAM wrapper ####
### ------------------------------------------------------------------------ ###
### wrapper for calling SAM
### this makes used of the SAM wrapper in FLfse,
### which in turn calls SAM from the package stockassessment
SAM_wrapper <- function(stk, idx, tracking,
                        args, ### contains ay (assessment year)
                        forecast = FALSE,
                        fwd_trgt = "fsq", ### what to target in forecast
                        fwd_yrs = 1, ### number of years to add
                        fwd_yrs_average = -3:0, ### years used for averages
                        fwd_yrs_rec_start = NULL, ### recruitment 
                        fwd_yrs_sel = -3:-1, ### selectivity
                        fwd_yrs_lf_remove = -2:-1,
                        fwd_splitLD = TRUE,
                        parallel = FALSE,
                        conf = NULL, ### SAM configuration
                        par_ini = NULL, ### initial parameters
                        track_ini = FALSE, ### store ini for next year
                        ...){
  
  ### get additional arguments
  args <- c(args, list(...))
  
  ### get current (assessment) year
  ay <- args$ay
  
  ### check if initial parameter values for SAM exist from last year's fit
  ### and reuse if possible
  ### this overrides generic initial parameters, if they exist in par_ini
  ### (they are only used in first simulation year)
  if (isTRUE(track_ini) & !is.null(attr(tracking@units, "par_ini"))) {
    
    par_ini <- attr(tracking@units, "par_ini")
    
  }
  
  ### fit SAM to provided data
  fit <- FLR_SAM(stk = stk, idx = idx, conf = conf, par_ini = par_ini,
                 DoParallel = parallel, ...)
  
  ### store parameter values and provide them as initial values next year
  ### store in tracking object, this is the only object which does not get 
  ### overriden next year
  ### weird workaround: save as attribute of "unit" slot of tracking,
  ###                   otherwise the attributes will be lost later...
  if (isTRUE(track_ini)) {
    
    attr(tracking@units, "par_ini") <- sam_getpar(fit)
    
  }
  
  ### convert into FLStock
  stk0 <- SAM2FLStock(object = fit, stk = stk) 