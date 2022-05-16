make.roc = function(fitted.vals, y) {
	#fitted vals are fitted p's
	#y are actual outcome data
	
	fity_ypos <- fitted.vals[y == 1]
	fity_yneg <- fitted.vals[y == 0]

	sort_fity <- sort(fitted.vals)

	sens <- 0
	spec_c <- 0
	thresh <- 0

	for (i in length(sort_fity):1) {
		sens <- c(sens, mean(fity_ypos >= sort_fity[i]))
		spec_c <- c(spec_c, mean(fity_yneg >= sort_fity[i]))
    		thresh <- c(thresh, sort_fity[i])
	} 

	out1 = data.frame('spec_c' = spec_c, 'sens' = sens)

	xdif = spec_c[-1] - spec_c[-length(spec_c)]
	lower_y = apply(cbind(sens[-length(sens)], sens[-1]), 1, min)
	ydif = sens[-1] - sens[-length(sens)]

	auc = sum((abs(xdif) * lower_y) + (0.5 * abs(xdif) * abs(ydif)))

	out = list('curve' = out1, 'auc' = auc, 'thresh' = thresh)
	return(out)

}

dist.from.ideal = function(x, y) {
	dist = (x - 0)^2 + (y - 1)^2
	return(dist)
}


# Ad hoc check of fit for Gamma model

test.g = function(mod, dat) {
	# mod is the model object
	# dat is the data set to use for validation

	pred = predict(mod, dat[dat$Cases.bin == 1, ])
	y = dat$Inc[dat$Cases.bin == 1]

	SAE = sum(abs(y - pred), na.rm = TRUE)
	SSE = sum(abs((y - pred)^2), na.rm = TRUE)
	ME = mean(pred[!is.na(pred)]) / mean(y[!is.na(pred)])

	res = c(SAE, SSE, ME)
	names(res) = c('SAE', 'SSE', 'ME')
	return(res)

}
