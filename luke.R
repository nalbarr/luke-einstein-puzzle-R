# luke.R

install.packages("combinat")

library(combinat)

# i - house colors
# j - persons
# k - drinks
# l - travel
# m - pets

# Facts:
### 1. Nicola lives in the tan house.  
### 2. Ed has a guinea pig.
### 3. David drinks mochaccino.
### 4. The blue house is on the left of the green house. 
### 5. The owner of the green house drinks flat whites.
### 6. The person who travels by car has a pet squirrel.
### 7. The owner of the striped house travels by bike.
### 8. The person living in the middle house drinks double expressos.
### 9. Nick lives in the first house.
### 10. The person who travels by train was next to the person that had the pit bull.
### 11. The person who has a badger lives next to the person that travels by bike.
### 12. The person who travel by planes drink chai lattes.
### 13. Nigel goes everywhere by foot.
### 14. Nick lives next to the polka-dot house.
### 15. The person who travels by train has a neighbor that drinks decaf.

# Problem:
### Who owns the fish?  (Not Nigel)

# Solution
# 1. Inventory entities/attributes
# - Persons in (Nicola, Ed, David, Nick, Nigel)
# - Houses in (tan, blue green striped, polka-dot)
# - Pets in (guinea pig, squirrel, pit bull, badger, fish)
# - Coffee drinks in (mochaccino, flat whites, double expressos, chai latte, decaf)
# - Travel in (car, bike, train, foot, plane)
# 2. Fact #9.  
# - Connects specific person to specific house.  Assign inital house position #1
# - Also, qualifies other people for other houses.
# 3. 

col <- factor(c("Tan", "Blue", "Green", "Striped", "Polka-Dot"))
own <- factor(c("Nicola", "Ed", "David", "Nick", "Nigel"))
pet <- factor(c("Guinea Pig", "Squirrel", "Pit Bull", "Badger", "Fish"))
drink <- factor(c("Mochaccino", "Flat Whites", "Double Expresso", "Chai Latte", "Decaf"))
travel <- factor(c("Car", "Bike", "Train", "Walk", "Plane"))

col_p <- permn(levels(col))
own_p <- permn(levels(own))
pet_p <- permn(levels(pet))
drink_p <- permn(levels(drink))
travel_p <- permn(levels(travel))

# immediately left
# imright <- function(h1,h2){
#   return(h1-h2==1)
# }
imleft <- function(h1,h2){
  return(h1-h2==-1)
}

nextto <- function(h1,h2){
  return(abs(h1-h2)==1)
}

house_with <- function(f,val){
  return(which(levels(f)==val))
}

# i - house colors
# j - persons
# k - drinks
# l - travel
# m - pets
for (i in seq(length(col_p))){
  col <- factor(col, levels=col_p[[i]])
  
  if (imleft(house_with(col,"Blue"),house_with(col,"Green"))) { #4
    for (j in seq(length(own_p))){
      own <- factor(own, levels=own_p[[j]])
      
      if(house_with(own,"Nicola") == house_with(col,"Tan")){ #1
        if(house_with(own,"Nick") == 1){ #9
          if(nextto(house_with(own,"Nick"),house_with(col,"Polka-Dot"))){ #14
            for(k in seq(length(drink_p))){
              drink <- factor(drink, levels=drink_p[[k]])
              
              if(house_with(drink,"Flat Whites") == house_with(col,"Green")){ #5
                if(house_with(own,"David") == house_with(drink,"Mochaccino")){ #3
                  if(house_with(drink,"Double Expresso") == 3){ #8
                    for(l in seq(length(travel_p))){
                      travel <- factor(travel, levels=travel_p[[l]])
                      
                      if(house_with(travel,"Bike") == house_with(col,"Striped")){ #7
                        if(house_with(travel,"Plane") == house_with(drink,"Chai Latte")){ #12
                          if(house_with(own,"Nigel") == house_with(travel,"Walk")){ #13
                            if(nextto(house_with(travel,"Train"),house_with(drink,"Decaf"))){ #15
                              for(m in seq(length(pet_p))){
                                pet <- factor(pet, levels=pet_p[[m]])
                                
                                if(house_with(own,"Ed") == house_with(pet,"Guinea Pig")){ #2
                                  if(house_with(travel,"Car") == house_with(pet,"Squirrel")){ #6
                                    if(nextto(house_with(travel,"Train"),house_with(pet,"Pit Bull"))){ #10
                                      if(nextto(house_with(travel,"Bike"),house_with(pet,"Badger"))){ #11
                                        res <- sapply(list(own,col,pet,drink,travel),levels)
                                        colnames(res) <- c("Person","House Color","Pet","Drink","Travel")
                                        print(res)
                                      }
                                    }
                                  }
                                }
                              }
                            }  
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
}

