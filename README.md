# Yellow Fever Forecasting
Statistical forecasting models for Yellow Fever occurrence and incidence in Brazil.


Files included:

### Servadio_YFForec_Data_anon_00_05.Rdata
Anonymized municipality-week data used in modeling spanning from 2000-2005. 

### Servadio_YFForec_Data_anon_06_11.Rdata
Anonymized municipality-week data used in modeling spanning from 2006-2011. 

### Servadio_YFForec_Data_anon_12_18.Rdata
Anonymized municipality-week data used in modeling spanning from 2012-2018. The three .Rdata files combined comprise the full data set used in analyses.

### MakeLag.R
Produces specified lag periods for model fitting

### makeROC.R
Computes area under the ROC curve and mean absolute error for evaluating forecating abilities of the binomial and Gamma steps, respectively, of a Gamma hurdle model

### Min_Pre.R 
Fits models using minimum temperature, humidity, and precipitation at various lag periods during the endemic time period

### Max_Pre.R 
Fits models using maximum temperature, humidity, and precipitation at various lag periods during the endemmic time period

### Mean_Pre.R 
Fits models using mean temperature, humidity, and precipitation at various lag periods during the endemic time period

### Range_Pre.R 
Fits models using range of temperature, humidity, and precipitation at various lag periods during the endemic time period

### Min_Post.R 
Fits models using minimum temperature, humidity, and precipitation at various lag periods during the epidemic time period

### Max_Post.R 
Fits models using maximum temperature, humidity, and precipitation at various lag periods during the epidemic time period

### Mean_Post.R 
Fits models using mean temperature, humidity, and precipitation at various lag periods during the epidemic time period

### Range_Post.R 
Fits models using range of temperature, humidity, and precipitation at various lag periods during the epidemic time period

