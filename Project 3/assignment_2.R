ADJCLOSE = read.table('ADJCLOSE.csv', sep=',', header=TRUE);
tickers = colnames(ADJCLOSE); 
print(paste("There are",dim(ADJCLOSE)[1],"records in total.", sep=" "));
ADJCLOSE = na.omit(ADJCLOSE);
nrTickers = dim(ADJCLOSE)[2];
print(paste("After deleting N/A, there are",nrDays,"records in total", sep=" "));

RET = log( ADJCLOSE[ 2: nrDays , ] / ADJCLOSE[ 1: (nrDays-1) , ] ); 
nrDays = dim(RET)[1];    



# X_train = RET[1:(nrDays-1),];
# X_test = RET[1:(nrDays-1),];

X_train = RET[1:(nrDays-1),] * run_PCA(RET[1:(nrDays-1),],5);
X_test = RET[1:(nrDays-1),] * run_PCA(RET[1:(nrDays-1),],5);

#in sample analysis begins
average_daily_pnl = matrix(data=NA, nrow=nrTickers, ncol=1);
yearly_pnl = matrix(data=NA, nrow=nrTickers, ncol=1);
total_pnl = matrix(data=NA, nrow=nrTickers, ncol=1);
sharpe_ratio = matrix(data=NA, nrow=nrTickers, ncol=1);

stats = matrix(data=NA, nrow=nrTickers, ncol=4);
rownames(stats) = tickers;
colnames(stats) = c("average_daily_pnl","yearly_pnl","total_pnl","sharpe_ratio");

daily_pnl = matrix(data=NA, nrow=nrDays-1, ncol=nrTickers);

for(i in 1:nrTickers){  
    print('#############################################');
    print(paste('Stock = ',tickers[i]));
    y_train = RET[ 2 : nrDays , tickers[i], drop=FALSE];  # "drop=FALSE" ensurs it remains a 2-D array..
    print(dim(y_train));
    y_hat = compute_linear_regression(X_train, y_train, X_test);
    y_hat = as.data.frame(y_hat);

    for (j in 1:(nrDays-1)) {
    　　　daily_pnl[j,i] = sign(y_hat[j,1])*y_train[j,1];
    }

    average_daily_pnl[i,1] = mean(daily_pnl[,i]);
    yearly_pnl[i,1] = mean(daily_pnl[,i])*252;
    total_pnl[i,1] = sum(daily_pnl[,i]);
    sharpe_ratio[i,1] = sqrt(252)*mean(daily_pnl[,i])/sd(daily_pnl[,i]);


}

stats[,1] = average_daily_pnl;
stats[,2] = yearly_pnl;
stats[,3] = total_pnl;
stats[,4] = sharpe_ratio;

print(stats);
#in sample analysis ends

run_PCA = function(high_dim_train,num_of_cp) {
    pca_result = prcomp(high_dim_train, scale=TRUE, center=TRUE, rtex=TRUE);
    #print(pca_result);

    pc_vectors = pca_result$rotation[,1:num_of_cp];
    #print(paste("The first",num_of_cp,"principal components are:", sep=" "));
    return(pc_vectors);
};


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

	rsq = summary( regObj )$r.squared;        # print( paste('R_square= ', rsq) );

	# fit_residuals = resid( regObj );  # if you ever need the residuals from the training fit

	# print(X_test)
	# print(data.frame(X_test) )

	# apply the beta's to X_test and compute your prediction:
	X_test = data.frame(X_test);
	y_hat_obj = predict(regObj, newdata = X_test, se.fit = TRUE)
	y_hat = y_hat_obj$fit
	coefficients = summary(regObj)$coefficients;  # if you ever need the actual coefficients (beta_i) i=0,1,...,p
	# print(coefficients)

	return(y_hat)
}