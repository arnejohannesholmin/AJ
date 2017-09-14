#*********************************************
#*********************************************
#' Returns a vector of time steps that balance load on each core given the compactly specified school information.
#'
#' @param tlist  is a list of the numbers of the pings to be simulated, where each element of the list corresponds to one compactlyu specified school.
#' @param school  is a list of compactly specified dynamic school information.
#' @param cores  is an integer specifying the number of cores to run the simulations over in parallel (should be lower than the number of cores in the computer).
#' @param runs  is n integer giving the number of runs used to generate the optimal order.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD zeros
#' @importFrom stats sd
#'
#' @export
#' @rdname echoIBM.loadbalance_compact_school
#'
echoIBM.loadbalance_compact_school<-function(tlist, school, cores, runs=1e4){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2015-05-04 - First version.
	########### DESCRIPTION: ###########
	# Returns a vector of time steps that balance load on each core given the compactly specified school information.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---tlist--- is a list of the numbers of the pings to be simulated, where each element of the list corresponds to one compactlyu specified school.
	# ---school--- is a list of compactly specified dynamic school information.
	# ---cores--- is an integer specifying the number of cores to run the simulations over in parallel (should be lower than the number of cores in the computer).
	# ---runs--- is n integer giving the number of runs used to generate the optimal order.
	
	
	##################################################
	##################################################
	########## Preparation ##########
	# 'sumfish' is the total number of fish in each cluster of schools. The function generates a lot of random orders of the schools, and tries to minimize the variation in 'sumfish':
	sumfish = zeros(runs, cores)
	# 'orders' holds the orders of the schools:
	orders = zeros(runs, length(tlist))
	# 's' is a simple sequence to be reordered at each time step:
	s = seq_along(school$nbfS)
	
	
	########## Execution and output ##########
	for(i in seq_len(nrow(orders))){
		# Reorder:
		orders[i,] = sample(s)
		# Generate a matrix of 'cores' columns, and sum along the columns:
		sumfish[i,] = colSums(matrix(school$nbfS[orders[i,]], ncol=cores))
		}
	# Extract the order with the lowest variation in number of fish (using standard deviation):
	atmindiffsumfish = which.min(apply(sumfish, 1, sd))
	optimalorder = orders[atmindiffsumfish,]
	# Return the new order:
	list(order=optimalorder, t=unlist(tlist[optimalorder]), sumfish=sumfish[atmindiffsumfish,])
	##################################################
	##################################################
	}
