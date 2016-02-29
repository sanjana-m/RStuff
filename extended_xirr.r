sppv <- function (i, n) {
  return((1 + i/100)^(-n))
}

# Net Present Value
npv <- function(x, i) {
  npv = c()
  # print(length(i))
   for (k in 1:length(i)) {
    pvs = x[,1] * sppv(i[1], x[,2])
    npv = c(npv, sum(pvs))
   }
  print(paste("NPV: ",npv))
  return(npv)
}

# Internal rate of return for non-periodic cash flow
# Input: cashflow - vector of numeric
#           dates - vector of strings containing dates with this format "%d-%m-%y"
# Output: irr - internal rate of return - range 0,1

xirr <- function (cashflow, dates) {
  if (length(cashflow) != length(dates)) {
    stop("length(cashflow) != length(dates)")
  }
  
  cashflow_adj <- matrix(,nrow=length(cashflow),ncol=2)
  print(paste("DIMENSIONS - ",dim(cashflow_adj)))
  for (i in 1:length(cashflow)) {
    d1 <- as.Date(dates[1], "%d-%m-%Y")
    d2 <- as.Date(dates[i], "%d-%m-%Y")
    
    interval <- as.integer(d2 - d1)
    print(paste("d2 ",d2," d1 ",d1," interval ",interval))

	cashflow_adj[i,1] <- cashflow[i]
	cashflow_adj[i,2] <- interval


    # if(interval > 0){
      # cashflow_adj <- c(cashflow_adj, rep(0, interval-1), cashflow[i+1])
    # }
    # else{
      # cashflow_adj <- c(cashflow_adj, cashflow[i+1])
    # }
	
  }

  print(cashflow_adj)

  # Bisection method finding the rate to zero npv
  left = -10
  right = 10
  epsilon = 1e-8
  count <- 0
  while (abs(right-left) > 2*epsilon) {
    midpoint = (right+left)/2
    print(paste("left", left ,"Right", right, "count = " , count))
    print(paste("Right-left", abs(right-left)))
    if (npv(cashflow_adj, left) * npv(cashflow_adj, midpoint) > 0) {
      left = midpoint
    } else {
      right = midpoint
    }
    count <- count + 1
  }
  
  # Irr for daily cashflow (not in percentage format)
  irr = (right+left) / 2 / 100  
  # Irr for daily cashflow multiplied by 365 to get yearly return
  irr <- irr * 365 
  # Annualized yield (return) reflecting compounding effect of daily returns
  irr <- (1 + irr / 365) ^ 365 - 1
  
  irr
}

# data has all the transaction history, of form - 
# -------------------------------------------------
# | FOLIO_ID | TYPE | DATE | AMOUNT | NAV | UNITS |
# -------------------------------------------------

# Returns Data sorted on Name, units, and descending date
sort_data <- function(data){
  data <-  data[with(data,order(Name,-Units,Date)),]
  data
}

# Finds unique fund names
fund_names <- function(data){
  a <- as.vector(unique(data[,1]))
}

# Returns a list of funds and their transactions
fund_data <- function(data){
  a <- fund_names(data)
  y <- list()
  for(i in a){
    x <- rbind(data[1,])
    for(j in 1:nrow(data)){
      if(data[j,'Name'] == i){
        x <- rbind(x,data[j,])
      }
    }
    x <- x[-1,]
    y[[i]] <- x
  }
  y
}

# Adds all redemptions of a set of transactions
add_redemptions <- function(a){
  i <- 1
  while((a[i,"Units"]>0)&&(i < nrow(a))){
    i <- i+1
  }
  while(i < nrow(a)){
    a[nrow(a),"Units"] <- a[nrow(a),"Units"] + a[i,"Units"]
    a <- a[-i,]
  }
  a
}

# Balances redemptions with purchases
knock_off <- function(a){
  i <- 1
  while((i < nrow(a)) && ((a[nrow(a),'Units'] + a[i,'Units']) < 0)){
    print(paste("HERE1: ",a[nrow(a),'Units']," ",a[i,'Units']))
    a[nrow(a),"Units"] <- a[nrow(a),'Units'] + a[i,'Units']
    a <- a[-i,]
    print(a)
    exp <- ((i < nrow(a)) && ((a[nrow(a),'Units'] + a[i,'Units']) < 0))
  }

  if(a[nrow(a),'Units']+a[i,'Units'] >= 0){
    print(paste("HELLO"))
    units <- a[i,'Units']
    amount <- as.numeric(gsub(",","",a[i,"Amount"]))
    a[i,'Units'] <- abs(a[nrow(a),'Units'] + a[i,'Units'])
    a[i,'Amount'] <- round((amount * (a[i,'Units']/units)),digits=2)
    a <- a[-nrow(a),]
    if(a[1,'Units'] == 0){
      a <- a[-1,]
    }
  }
  a
}

# Calculates extended XIRR
ext_xirr <- function(data){
  data <- sort_data(data)
  list_funds <- fund_data(data)
  amts <- c()
  dates <- c()

  for(i in 1:length(list_funds)){
    list_funds[[i]] <- add_redemptions(list_funds[[i]])
    list_funds[[i]] <- knock_off(list_funds[[i]])
    amts <- c(amts,as.vector(list_funds[[i]][,"Amount"]))
    dates <- c(dates,as.vector(list_funds[[i]][,"Date"]))
  }

  amts <- as.numeric(gsub(",","",amts))

  table <- matrix(c(amts,dates),nrow=(length(amts)),ncol=2)
  print(paste("TABLEEEEEEEE: "))
  table <- table[order(as.Date(table[,2],"%d-%m-%Y")),]
  print(table)

  amts <- as.numeric(gsub(",","",table[,1]))
  dates <- table[,2]

  number <- readline(prompt="Enter present value: ")
  number <- (as.numeric(number)) * (-1)

  amts <- c(amts,number)
  dates <- c(dates,format(Sys.Date(),"%d-%m-%y"))

  xirr1 <- xirr(amts,dates)
  print(paste("XIRR: ",xirr1))
}

#~ npv(rep(8792,12), 666.31/12) # 15755.01 - 100000 - 10.0088%

# the correct IRR is 10.06% - source: matlab example
#mycashflow <- c(-10000,2500, 2000, 3000,4000)
#mydates <- c("12-01-87", "14-02-88", "03-03-88", "14-06-88", "01-12-88")
#xirr(mycashflow, mydates)

# the correct IRR is 37.34% - source: microsoft example
#mycashflow <- c(-10000,2750,4250,3250,2750)
#mydates <- c("1-01-08","1-03-08","30-10-08","15-02-09","1-04-09")
#xirr(mycashflow, mydates)
