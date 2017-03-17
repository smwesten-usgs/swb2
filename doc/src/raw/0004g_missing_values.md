### Treatment of Missing Values ###

Missing values in datasets are a big problem when they slip into calculations. SWB has a few new ways to deal with missing values when they are encountered.

  Suffix  | Argument  |  Description |
  Default value
  --------|-----------|--------------|-----------------
_MINIMUM_ALLOWED_VALUE	  | real value	| Sets a floor for data values; any values less than this minimum value will be replaced with the specified minimum value. |	largest negative value to fit in a Fortran real (kind=4)
_MAXIMUM_ALLOWED_VALUE    |	real value	| Sets a ceiling for data values; any values greater than this maximum value will be replaced with the specified maximum value.	| largest positive value to fit in a Fortran real (kind=4)
_MISSING_VALUES_CODE	    | numeric value (real or integer)	| Specifies a missing values code for the dataset. Older datasets often used a value of -99999 to indicate missing values. |	--
_MISSING_VALUES_OPERATOR	| “<”, “<=”, “>”, “>=”	| Specifies a comparison operator to be used in evaluating dataset values relative to the missing value code. |	--
_MISSING_VALUES_ACTION	  | “MEAN”, “ZERO” |"MEAN" will substitute the mean value calculated over the remaining valid cells; "zero" will substitute a value of 0.0 in place of missing values.  | 	--
