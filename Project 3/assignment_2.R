
main_linear_regression= function(){

	tickers = c('SPY','VIX','TNX','OIL','GLD','GOLD','N225','FTSE','SPXS','SPXL');

	## Set the working directory to your own directory:
	setwd('/Users/chaoguo/Documents/data-science-projects/Project 3');

	# The file with the yahoo data (daily closing prices)
	fileName = '/Users/chaoguo/Documents/data-science-projects/Project 3/ADJCLOSE.data';
	load( file = fileName);
	colnames(ADJCLOSE) = tickers 

	nrDays = dim(ADJCLOSE)[1];

	## Comput the 1-day log returns:
	RET = log( ADJCLOSE[ 2: nrDays , ] / ADJCLOSE[ 1: (nrDays-1) , ] );		# print(head(RET))

	## Drop the rows (i.e., days) which have at least one NA ("Not Available" entry):
	nrNA_each_row = rowSums( is.na(RET) );		# print(nrNA_each_row);
	okRows = nrNA_each_row == 0;
	print(dim(RET));
	RET = RET[ okRows , ];	print(dim(RET))


	nrDays = dim(RET)[1];		days = rownames(RET);
	nrTickers = dim(RET)[2];	tickers = colnames(RET);
	print(dim(run_PCA(RET[1:nrDays-1,],5)));

	X_train = RET[1:nrDays-1,]%*%run_PCA(RET[1:nrDays-1,],5);
	X_test = RET[1:nrDays-1,]%*%run_PCA(RET[1:nrDays-1,],5);

	#Create a bunch of matricies recording different statistics
	daily_pnl = matrix(data=NA,nrow=nrDays-1,ncol=nrTickers);
	colnames(daily_pnl) = tickers;

	sharpe = matrix(data=NA,nrow=1,ncol=nrTickers);
	colnames(sharpe) = tickers;

	annual_return = matrix(data=NA,nrow=1,ncol=nrTickers);
	colnames(annual_return) = tickers;

	total_return = matrix(data=NA,nrow=1,ncol=nrTickers);
	colnames(total_return) = tickers;

	stats = matrix(data=NA,nrow=nrTickers,ncol=4);
	rownames(stats) = tickers;
	colnames(stats) = c('sharpe', 'average_pnl', 'annual_return', 'total_return');

	for(i in 1:nrTickers){  
		print('#############################################');
		print( paste('Stock = ',tickers[i] ) );
		y_train = RET[ 2 : nrDays , tickers[i], drop=FALSE];  # "drop=FALSE" ensurs it remains a 2-D array..
		print(dim(y_train))

		y_hat = compute_linear_regression(X_train, y_train, X_test);

		
		# compute the various performance statistics:
		for (j in 1:nrDays-1) {
			daily_pnl[j,i] = sign(y_hat[j])*y_train[j];
		}

		sharpe[1,i] = sqrt(252)*mean(daily_pnl[,i])/sd(daily_pnl[,i]);
		annual_return[1,i] = mean(daily_pnl[,i])*252;
		total_return[1,i] = sum(daily_pnl[,i]);


		stats[i,1] = sharpe[1,i];
		stats[i,2] = mean(daily_pnl[,i]);
		stats[i,3] = annual_return[1,i];
		stats[i,4] = total_return[1,i];

		#stats_this_stock = c( daily_pnl, yearly_pnl,  total_pnl, sharpe)
		#STATS = rbind(STATS,stats_this_stock);
		
		# Also record the cumulative pnl (Use the 'cumsum'command) = ?
		
		# readline('Press key to continue...');
	}
	print(annual_return);
	print(stats);

	# cum_daily_pnl = apply(daily_pnl,2,cumsum);
	# plot_cumsum(cum_daily_pnl);

	return('DONE!');
}


compute_Sharpe_Ratio = function(x){
	sh = mean(x, na.rm=TRUE) / sd(x, na.rm=TRUE) * sqrt(252);  return(sh);
}


plot_cumsum = function(CUM_SUM_MTX){
	# INPUT: CUM_SUM_MTX, a matrix of size nrDays  x nrTickers 
	# 	which plots the time series (one from each column) on the same plot.

	nrTickers = dim(CUM_SUM_MTX)[2];
	nrDays = dim(CUM_SUM_MTX)[1];

	myColors = rainbow(nrTickers);  # generated however many colors are needed - one for each column.
	# myColors = c('red','blue', 'green','black','magenta', 'cyan','yellow')

	# pdf(file = 'cumulative_pnl_across_time.pdf', height = 12,  width = 8);
	plot (c(0, nrDays),c(-6,6.),type='n', xlab ='days', ylab = 'pnl') # sets the x and y axes scales
	for ( i in 1 : nrTickers){
		# plots the time series on the same graph
		lines( 1: nrDays, CUM_SUM_MTX[ ,i], col = myColors[i], lwd=2);  # , ylim = c(-1,1)
	}

	legend( 'topleft', legend = colnames(CUM_SUM_MTX), lty = 1, lwd = 2, col = myColors);  # , fill=TRUE
	# dev.off()
}


compute_linear_regression = function(X_train, y_train, X_test){

	# Training X matrix: X_train  of size n by p
	# Training y vector: y_train  of size n by 1
	# Test X matrix: X_test  of size k by p

	# Performs multiple linear regression (without an intercept) on the training data, and applied the obtained coefficients to a test matrix.

	colnames(y_train) = 'y'
	both = data.frame(X_train, y  = y_train);
	regObj = lm(y ~ . , data = both)  # with intercept  beta_0
	# regObj = lm(y ~ . +0, data = both)  # without intercept  beta_0

	print(summary( regObj ) );   # see the summary of the regression

	rsq = summary( regObj )$r.squared;		# print( paste('R_square= ', rsq) );

	# fit_residuals = resid( regObj );  # if you ever need the residuals from the training fit

	# print(X_test)
	# print(data.frame(X_test) )

	# apply the beta's to X_test and compute your prediction:
	X_test = data.frame(X_test);
	y_hat_obj = predict(regObj, newdata = X_test, se.fit = TRUE)
	y_hat = y_hat_obj$fit
	coefficients = summary(regObj)$coefficients;  # if you ever need the actual coefficients (beta_i) i=0,1,...,p
	# print(coefficients)

	return(y_hat);
}

run_PCA = function(high_dim_train,num_of_cp) {
    pca_result = prcomp(high_dim_train, scale=TRUE, center=TRUE, rtex=TRUE);

    pc_vectors = pca_result$rotation[,1:num_of_cp];
    #print(paste("The first",num_of_cp,"principal components are:", sep=" "));
    return(pc_vectors);
}
