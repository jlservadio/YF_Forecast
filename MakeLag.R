

# function for creating lags

make.lag = function(dat, varname, by.var, date.order, lag.length) {
	# dat = dataset
	# varname = variable to make a lag of
	# by.var = variable data are grouped by	
	# date.order = variable to specify time order
	# 	should be numeric for time steps
	# lag.length = number of time steps to lag

	varcol = which(names(dat) == varname)
	if (length(varcol) == 0) {
		stop('No variables in data matching variable to lag')
	} else if (length(varcol) > 1) {
		varcol = varcol[1]
		warning('Multiple variables match variable to lag')
	}

	bycol = which(names(dat) == by.var)
	if (length(bycol) == 0) {
		stop('No variables in data matching variable to group')
	} else if (length(bycol) > 1) {
		varcol = varcol[1]
		warning('Multiple variables match variable to group')
	}

	timecol = which(names(dat) == date.order)
	if (length(timecol) == 0) {
		stop('No variables in data matching time variable')
	} else if (length(timecol) > 1) {
		timecol = timecol[1]
		warning('Multiple variables match time variable')
	}

	dat = dat[order(dat[ , bycol], dat[ , timecol]), ]

	new.var = c(rep(NA, lag.length), dat[ , varcol][-c((nrow(dat)-lag.length+1):nrow(dat))])
	new.var[which(dat[ , timecol] < (lag.length + min(dat[ , timecol])))] = NA

	return(new.var)
	 
}

