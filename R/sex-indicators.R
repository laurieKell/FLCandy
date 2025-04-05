#' Operational Sex Ratio (OSR)
#'
#'  $ OSR = \frac{\sum_l (N_{1,f} \times Mat_{1,f})}{\sum_l (N_{1,f} \times Mat_{1,f}) + \sum_l (N_{1,m} \times Mat_{1,m})} $
#' 
#' @param object An FLStock object with 'F' and 'M' units
#' @param female Character specifying female unit name
#' @param male Character specifying male unit name
#' @return FLQuant with OSR values (proportion of females)

setGeneric("osr", function(object, ...) standardGeneric("osr"))

setMethod("osr", signature(object="FLStock"),
          function(object, female="F", male="M") {
            
            # Validate sex units exist
            if(!female %in% dimnames(object)$unit)
              stop("Female unit '", female, "' not found in object@unit")
            if(!male %in% dimnames(object)$unit)
              stop("Male unit '", male, "' not found in object@unit")
            
            # Calculate mature numbers-at-length/age
            mat=stock.n(object)%*%mat(object)
            
            # Sum mature numbers over quant (age/length)
            matF=quantSums(mat[,,female])
            matM=quantSums(mat[,,male])
            
            # Calculate
            osr=matF%/%(matF%+%matM)
            
            # Assign  units
            units(osr)="fraction"
            
            return(osr)})

#' Example
#' @examples
#' \dontrun{
#' 
#' data(ple4sex)
#'
#' ## Males are =0, so set to same as females
#' mat(ple4sex)[,,"M"]=mat(ple4sex)[,,"F"]
#' 
#' index=osr(ple4sex)
#'
#' ggplot(index) + 
#'   geom_line(aes(x = year, y = data)) +
#'   ylab("Operational Sex Ratio (Female Proportion)")
#'   
#'  }
#' Calculate Sex Ratio-at-Length for an FLStock object
#' 
#' $ Sex ratio-at-length = \frac{N_{1,f}}{N_{1,f} + N_{1,m}} \$
#'     Where:
#'     
#'     - \$ N_{1,f} \$: Female numbers at length \$ l \$
#'     - \$ N_{1,m} \$: Male numbers at length \$ l \$
#'     
#'     
#' 
#' @param object An FLStock object with 'female' and 'male' units
#' @param female Character specifying female unit name
#' @param male Character specifying male unit name
#' @return FLQuant with sex ratio values (female proportion) at length

setGeneric("srlen", 
           function(object, ...) standardGeneric("srlen"))

setMethod("srlen", signature(object="FLQuant"),
          function(object, female="F", male="M") {
            
            # Validate sex units exist
            if(!female %in% dimnames(object)$unit)
              stop("Female unit '", female, "' not found")
            if(!male %in% dimnames(object)$unit)
              stop("Male unit '", male, "' not found")
            
            # Extract numbers-at-length by sex
            nF=stock.n(object)[,,female]
            nM=stock.n(object)[,,male]
            
            # Calculate sex ratio-at-length
            ratio=nF%/%(nF%+%nM)
            
            # Handle potential divisions by zero
            ratio[is.nan(ratio)]=0
            
            # Add metadata
            units(ratio)="fraction"
            name(ratio)="Sex ratio-at-length"
            
            return(ratio)})
#' @examples
#' \dontrun{
#' 
#' 
#' }
