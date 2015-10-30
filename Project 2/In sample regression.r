pdf('nowindow.pdf');

X_train = RET[1:nrDays-1,];
X_test = RET[1:nrDays-1,];

#Create a bunch of matricies recording different statistics
daily_pnl = matrix(data=NA,nrow=nrDays-1,ncol=nrTickers);
colnames(daily_pnl) = tickers;
sharpe = matrix(data=NA,nrow=1,ncol=nrTickers);
colnames(sharpe) = tickers;
annual_return = matrix(data=NA,nrow=1,ncol=nrTickers);
colnames(annual_return) = tickers;
total_return = matrix(data=NA,nrow=1,ncol=nrTickers);
colnames(total_return) = tickers;
STATS = matrix(data=NA,nrow=nrTickers,ncol=4);
rownames(STATS) = tickers;
colnames(STATS) = c('daily_pnl', 'yearly_pnl', 'total_pnl', 'sharpe');

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


	STATS[i,1] = sharpe[1,i];
	STATS[i,2] = mean(daily_pnl[,i]);
	STATS[i,3] = annual_return[1,i];
	STATS[i,4] = total_return[1,i];
}

print(annual_return);
print(STATS);

cum_daily_pnl = apply(daily_pnl,2,cumsum);
plot_cumsum(cum_daily_pnl);

dev.off();
return('DONE!');
}
