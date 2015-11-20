C = read.csv('England_2009_2010.csv',sep=',');


serial_rank = function(C) {
	C = data.matrix(C);
	n = dim(C)[1];
	S = matrix(data=NA,ncol=n,nrow=n);
	# DO NOT USE THE FOLLOWING WAY SINCE WE HAVE A CLOSED FORM EXPRESSION
	# for (i in 1:n){
	# 	for (j in 1:n) {
	# 		S[i,j]=(n + C[i,,drop=FALSE] %*% C[j,,drop=FALSE])/2;
	# 	}
	OneMatrix = matrix(data=1,nrow=n,ncol=n);
	S = (1/2)*(n*(OneMatrix%*%t(OneMatrix))+C%*%t(C));

	temp = S %*% OneMatrix;
	diag_temp = diag(temp);
	D = matrix(data=0,nrow=n,ncol=n);
	for (i in 1:n) {
		D[i,i] = diag_temp[i];
	}

	L = D - S;

	#Compute the Fidler vector of S
	eigenL = eigen(L);
	print (eigenL$vectors);

	nonzero_eigenval = eigenL$values[eigenL$values != 0];
	#print (nonzero_eigenval);
	print(length(nonzero_eigenval));
	Fiedler = eigenL$vectors[,length(nonzero_eigenval),drop=FALSE];

	#This might be the correct one?
	#Fiedler = eigenL$vectors[,length(nonzero_eigenval)-1,drop=FALSE];

	return (Fiedler);
	
}