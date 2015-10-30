print('Start of out-of-sample analysis');

pdf('window.pdf');

#Create a bunch of matricies recording different statistics
daily_pnl = matrix(data=NA,nrow=nrDays,ncol=nrTickers);
colnames(daily_pnl) = tickers;

STATS = matrix(data=NA,nrow=nrTickers,ncol=4);
rownames(STATS) = tickers;
colnames(STATS) = c('daily_pnl', 'yearly_pnl', 'total_pnl', 'sharpe');

for (k in 101:(nrDays-1)) {    
	print( paste('Day = ',k ) );

	X_train = RET[(k-100):(k-1),];
	X_test = RET[k,  , drop=FALSE ];

	for(i in 1:8){  
		y_train = RET[ (k-100+1) :k , tickers[i],drop=FALSE ];  #"drop=FALSE" ensurs it remains a 2-D array..
		y_hat = compute_linear_regression(X_train, y_train, X_test);
		daily_pnl[k-100,i] = sign(y_hat) * RET[k+1,i];
	}

}

cum_daily_pnl = apply(daily_pnl,2,cumsum);

AVG = apply( daily_pnl , 2 ,   mean, na.rm=TRUE ); 
SH = apply( daily_pnl , 2 ,   compute_Sharpe_Ratio );
annual_return = AVG*252; 
total_return = cum_daily_pnl[5,]; 

STATS[,4]=SH;
STATS[,3]=total_return;
STATS[,2]=annual_return;
STATS[,1]=AVG;

print(STATS);

plot_cumsum(cum_daily_pnl);

dev.off();
return('DONE!');