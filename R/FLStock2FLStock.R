install.packages("abind")
install.packages("FLCore")  # Assuming you have already installed FLR and its dependencies


# Example list of FLStock objects (replace this with your actual list)
# flStockList <- list(flStock1, flStock2, ..., flStock1440)

# Initialize a new FLStock object based on the structure of the first FLStock in your list
#combinedFLStock <- flStockList[[1]]

# Loop through each slot in the FLStock object that needs to be combined
for(slotName in slotNames(combinedFLStock)) {
  # Check if the slot is an FLQuant since those are the ones we need to combine
  if(class(slot(combinedFLStock, slotName)) == "FLQuant") {
    
    # Use abind to combine the FLQuant objects across all iterations in the list
    # Assuming the third dimension (M) is where the iterations are stored
    combinedSlot <- abind(lapply(flStockList, function(x) slot(x, slotName)), along = 3)
    
    # Update the slot in the combined FLStock object
    slot(combinedFLStock, slotName) <- combinedSlot
  }
}

# Set the dimnames for iterations appropriately
dimnames(combinedFLStock) <- list(year = as.character(year(combinedFLStock)),
                                  unit = "unique",
                                  season = "all",
                                  area = "1",
                                  iter = as.character(1:1440),
                                  age = as.character(age(combinedFLStock)))

# Now, `combinedFLStock` is your FLStock object with 1440 iterations
