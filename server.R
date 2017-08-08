# server.R
library(e1071)

# load Initial Series
InitSeries <- read.csv("./test6.csv",header = FALSE)


D <- 8
N <- 1200


n <- 6   # event duration
E <- rep(NA, n)   # initialization of event

mx <- vector()
for (i in 1:(D+1)){
  mx <- cbind(mx, InitSeries$V1[i:(i+391)])
}


shinyServer(function(input, output, session) {
	#global varible on x-axis
	x <- 400
	df <- as.data.frame(mx)

	
	autoUpdate <- reactiveTimer(500,session)

	observe({
		autoUpdate()
	  
	  
		x <<- x + 1
		
		if (x > 500 && x < 520){
		  y <<- sin(40*pi*x/N) + rnorm(1,0,0.1) + rnorm(1,0,0.5)
		} else {  y <<- sin(40*pi*x/N) + rnorm(1,0,0.1) }
		
		
		model <<- svm(V9 ~ ., data = df)
		new.data <<- append(as.numeric(df[392,][-1]),y)
		df <<- rbind(df[-1,], new.data)
		pred <<- predict(model, df[392,])

		# matching value
		V <<- df[392,D+1]-pred
		
		# occurence
		O = as.numeric(abs(V) > 0.4)
		
		# event
		E <<- append(E[-1],O)
		
		# number of surprises within event
		S <<- sum(E)
		
		
		
		
		#pass data to client as object - x & y are passed to client from server on every second
		variableToPassClient = sprintf('{"X":"%s", 
                          "Y": "%s", "Z": "%s"                          
                          }', x, y, O)

		session$sendCustomMessage(type="SendObjectToClientDynamicCallbackHandler",variableToPassClient)
	})
	
 
})