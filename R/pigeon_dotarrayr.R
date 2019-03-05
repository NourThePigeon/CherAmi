## dotarrayr
##
##$ The point of pigeon_dotarray() is to quickly create classic dot arrays
##$     for numerosity discrimination experiments. Feeding into it the basic
##$     variables you want expressed by the dots and it'll create every
##$     combination that needs to occur for counterbalancing. Saves the image
##$     files and the dot locations for every array.
pigeon_dotarray <- function(x){

  # sets a seed so that there is some measure of reproducability
  set.seed(112211)

  # tidy_expand to get every permutation
  #! TODO above
  #! TODO make every variable representative

  # creates a loop for each dot array permutation
  for(i in seq(nrow(Dotamounts))){

    # creates a loop for the LargeArray and SmallArray of each permutation
    for(j in seq(2)){

      # LargeArray Creation
      if(j == 1){

        # SmallArray Creation
      } else {

      }

    }

  }

}
