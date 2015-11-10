pdf('plot2.pdf',width=22,height=12);
par(mfrow=c(1,2));
par(mar = c(5, 5, 1, 1))

load(file = 'CorrMtx.data');
cov_matrix = cov(RETS); #This will give us the covariance cov_matrix

spec_decomp = eigen(cov_matrix,symmetric=TRUE);
eigenvalues = spec_decomp$values;

print(paste("Maximal eigenvalue before shuffle is",max(eigenvalues),sep=" "));
eigenvalues = eigenvalues[eigenvalues < max(eigenvalues)]; #Thank you, Stack Overflow!
hist(eigenvalues,breaks=50);
plot(quantile(eigenvalues,probs=seq(0,1,0.05),na.rm=TRUE));

#Then we randomly shuffle the entries of RETS
eigenvalues_matrix = matrix(data=NA,nrow=50,ncol=dim(RETS)[2]);

for (i in 1:50) {
	print(paste("Iteration",i,sep=" "));

	rets_rand = matrix(sample(c(RETS)),nrow=dim(RETS)[1],ncol=dim(RETS)[2]);
	cov_matrix_rand = cov(rets_rand);
	spec_decomp_rand = eigen(cov_matrix_rand,symmetric=TRUE);
	eigenvalues_rand = spec_decomp_rand$values;
	eigenvalues_matrix[i,]=eigenvalues_rand;
}
#print(dim(eigenvalues_matrix));

avg_eigenvalues = apply(eigenvalues_matrix,2,mean);
print(paste("Maximal eigenvalue after shuffle is",max(avg_eigenvalues),sep=" "));
hist(avg_eigenvalues,breaks=50);
plot(quantile(avg_eigenvalues,probs=seq(0,1,0.05),na.rm=TRUE));

plot(eigenvalues[1:20],avg_eigenvalues[1:20]);
lines(eigenvalues[1:20],eigenvalues[1:20],type='b');

dev.off();