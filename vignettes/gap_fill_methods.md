# MetQC gap-fill methods

The MetQC app uses multiple gap-fill methods to impute data when the recorded data maybe incorrect or unreliable. This document will outline the methods undertaken, by the app, for each gap-fill method. 

### Original observation (raw)
This method simply reverts any previously imputed (either as part of the automated process or by a user) data back to the raw observed data. This can be used to undo any changes.  

### Missing
This method simply flags data as 'missing'. No further imputation is undertaken. 

### Time
Fits a smoothing model (a GAM) of the variable in question against time. New values are predicted from this model.

### Regression with covariate
Fits a model between the variable in question and another variable, which the user can pick from a drop down list. New values are predicted from this model. 

### Night-time zero
Sets values to zero during the night. Values measured during the day are unaffected. Night time is calculated using the position of measurement and the sun position.  

### No negative values
Sets all selected points that are negative to zero. 

### All zeros
Sets all selected points to zero.

### Regression with ERA5 covariate
Fits a model between the variable in question and the same variable from the ERA5 dataset. New values are predicted from this model. 
