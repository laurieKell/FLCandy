<<<<<<< HEAD
myRegEquation <- function(data,mapping,...) {
  # Extract x and y variables
  x <- eval_data_col(data,mapping$x)
  y <- eval_data_col(data,mapping$y)
  
  # Get variable names for the equation
  x_name <- gsub("~","",deparse(mapping$x))
  y_name <- gsub("~","",deparse(mapping$y))
  
  x_name="x"
  y_name="y"
  
  # Get grouping variable (SRR in your case)
  group_var <- eval_data_col(data,mapping$colour)
  
  # Initialize equation text
  equation_text <- ""
  
  # Create a dataframe for model fitting
  df <- data.frame(x=x,y=y,group=group_var)
  
  # For each level of the grouping variable
  for(level in levels(group_var)) {
    # Subset data for this group
    group_data <- subset(df,group == level)
    
    # Fit linear model if enough data
    if(nrow(group_data) >= 3) {
      tryCatch({
        model <- lm(y ~ x,data=group_data)
        
        # Extract coefficients
        intercept <- round(coef(model)[1],2)
        slope <- round(coef(model)[2],2)
        
        # Format with proper sign
        sign <- ifelse(slope >= 0,"+","")
        
        # Create equation string
        eq <- paste0(level,"\n ",y_name,"=",
                     intercept,sign,slope,"*",x_name)
        
        # Add equation to text
        if(equation_text != "") equation_text <- paste0(equation_text,"\n\n")
        equation_text <- paste0(equation_text,eq)
      },error=function(e) {
        # Handle errors gracefully
        if(equation_text != "") equation_text <- paste0(equation_text,"\n\n")
        equation_text <<- paste0(equation_text,level,": Error fitting model")
      })}}
  
  # Return an empty plot if no equations generated
  if(equation_text == "") {
    equation_text <- "Insufficient data"}
  
  # Create and return the plot with equations
  ggplot(data=data,mapping=mapping) + 
    annotate("text",x=0.5,y=0.5,label=equation_text,
             size=2.5,hjust=0.5,vjust=0.5) + 
    theme_void() +
    theme(panel.border=element_rect(color="gray80",fill=NA,linewidth=0.5))}


# Custom correlation function with better formatting
myCor<-function(data,mapping,...) {
  x=eval_data_col(data,mapping$x)
  y=eval_data_col(data,mapping$y)
  corr=round(cor(x,y,use="pairwise.complete.obs"),3)
  
  # Get the mapped color variable
  colVar=eval_data_col(data,mapping$colour)
  
  # Calculate correlations for each group
  if(!is.null(colVar)) {
    cors=data %>% 
      group_by(!!mapping$colour) %>%
      summarize(cor=round(cor(!!mapping$x,!!mapping$y,use="pairwise.complete.obs"),3))
    
    corr_text=paste0("Corr: ",corr)
    for(i in 1:nrow(cors)) {
      corr_text=paste0(corr_text,"\n",
                       levels(colVar)[i],": ",cors$cor[i])
    }
  } else {
    corr_text=paste0("Corr: ",corr)
  }
  
  ggplot(data=data,mapping=mapping) + 
    annotate("text",x=0.5,y=0.5,label=corr_text,size=3.5) + 
    theme_minimal() +
    theme(panel.grid=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.border=element_rect(color="gray80",fill=NA,linewidth=0.5))
}

# Custom lower panel scatterplot with smoother
mySmooth=function(data,mapping,...) {
  ggplot(data=data,mapping=mapping) +
    geom_point(alpha=0.7,size=1) +
    geom_smooth(method="lm",se=TRUE,alpha=0.15,linewidth=1) +
    theme_minimal()}

# Custom density plot for diagonal
myDensity<-function(data,mapping,...) {
  ggplot(data=data,mapping=mapping) +
    geom_density(alpha=0.7,linewidth=0.8,...) +
    theme_minimal()}


myViolinDiag <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_violin(alpha = 0.7, scale = "width", ...) + # Add violin plot
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}
=======
myRegEquation <- function(data,mapping,...) {
  # Extract x and y variables
  x <- eval_data_col(data,mapping$x)
  y <- eval_data_col(data,mapping$y)
  
  # Get variable names for the equation
  x_name <- gsub("~","",deparse(mapping$x))
  y_name <- gsub("~","",deparse(mapping$y))
  
  x_name="x"
  y_name="y"
  
  # Get grouping variable (SRR in your case)
  group_var <- eval_data_col(data,mapping$colour)
  
  # Initialize equation text
  equation_text <- ""
  
  # Create a dataframe for model fitting
  df <- data.frame(x=x,y=y,group=group_var)
  
  # For each level of the grouping variable
  for(level in levels(group_var)) {
    # Subset data for this group
    group_data <- subset(df,group == level)
    
    # Fit linear model if enough data
    if(nrow(group_data) >= 3) {
      tryCatch({
        model <- lm(y ~ x,data=group_data)
        
        # Extract coefficients
        intercept <- round(coef(model)[1],2)
        slope <- round(coef(model)[2],2)
        
        # Format with proper sign
        sign <- ifelse(slope >= 0,"+","")
        
        # Create equation string
        eq <- paste0(level,"\n ",y_name,"=",
                     intercept,sign,slope,"*",x_name)
        
        # Add equation to text
        if(equation_text != "") equation_text <- paste0(equation_text,"\n\n")
        equation_text <- paste0(equation_text,eq)
      },error=function(e) {
        # Handle errors gracefully
        if(equation_text != "") equation_text <- paste0(equation_text,"\n\n")
        equation_text <<- paste0(equation_text,level,": Error fitting model")
      })}}
  
  # Return an empty plot if no equations generated
  if(equation_text == "") {
    equation_text <- "Insufficient data"}
  
  # Create and return the plot with equations
  ggplot(data=data,mapping=mapping) + 
    annotate("text",x=0.5,y=0.5,label=equation_text,
             size=2.5,hjust=0.5,vjust=0.5) + 
    theme_void() +
    theme(panel.border=element_rect(color="gray80",fill=NA,linewidth=0.5))}


# Custom correlation function with better formatting
myCor<-function(data,mapping,...) {
  x=eval_data_col(data,mapping$x)
  y=eval_data_col(data,mapping$y)
  corr=round(cor(x,y,use="pairwise.complete.obs"),3)
  
  # Get the mapped color variable
  colVar=eval_data_col(data,mapping$colour)
  
  # Calculate correlations for each group
  if(!is.null(colVar)) {
    cors=data %>% 
      group_by(!!mapping$colour) %>%
      summarize(cor=round(cor(!!mapping$x,!!mapping$y,use="pairwise.complete.obs"),3))
    
    corr_text=paste0("Corr: ",corr)
    for(i in 1:nrow(cors)) {
      corr_text=paste0(corr_text,"\n",
                       levels(colVar)[i],": ",cors$cor[i])
    }
  } else {
    corr_text=paste0("Corr: ",corr)
  }
  
  ggplot(data=data,mapping=mapping) + 
    annotate("text",x=0.5,y=0.5,label=corr_text,size=3.5) + 
    theme_minimal() +
    theme(panel.grid=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.border=element_rect(color="gray80",fill=NA,linewidth=0.5))
}

# Custom lower panel scatterplot with smoother
mySmooth=function(data,mapping,...) {
  ggplot(data=data,mapping=mapping) +
    geom_point(alpha=0.7,size=1) +
    geom_smooth(method="lm",se=TRUE,alpha=0.15,linewidth=1) +
    theme_minimal()}

# Custom density plot for diagonal
myDensity<-function(data,mapping,...) {
  ggplot(data=data,mapping=mapping) +
    geom_density(alpha=0.7,linewidth=0.8,...) +
    theme_minimal()}


myViolinDiag <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_violin(alpha = 0.7, scale = "width", ...) + # Add violin plot
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}
>>>>>>> d89e64d7a43a9799c7a6cd95c9073d428bfe2861
