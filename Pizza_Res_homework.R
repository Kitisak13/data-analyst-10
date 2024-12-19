# Variable

piz_menu <- c("classic","meat","seafood","spicy","hawaiian")
price_piz <- c(200,300,500,400,350)
drink_menu <- c("water","soda","beers","wine","juice","coffee")
price_drink <- c(20,30,80,200,50,90)

list_menu <- list(piz_menu,price_piz,drink_menu,price_drink)
confirm_order <- "YES"

# create Function

Pizza_2 <- function() {
  print("Hello welcome to pizzeria restaurant")
  list1 <- data.frame(Menu = list_menu[[1]], Price = list_menu[[2]])
  print(list1)
  menu <- readline("Choose menu:")
  list2 <- data.frame(Menu = list_menu[[3]], Price = list_menu[[4]])
  print(list2)
  drink <- readline("choose drink:")
  
  get_menu_price <- function(menu, list1) {
    if (!menu %in% list1$Menu) {
    return(NA) 
    }
    row_index_menu <- which(list1$Menu == menu) 
    price_menu <- list1$Price[row_index_menu]
    return(price_menu)
  }
  
  get_drink_price <- function(drink, list2) {
    if (!drink %in% list2$Menu) { 
    return(NA) 
    } 
    row_index_drink <- which(list2$Menu == drink) 
    price_drink <- list2$Price[row_index_drink]
    return(price_drink)
  }
  
    if((menu %in% piz_menu) & (drink %in% drink_menu)) {
    print(toupper(glue("Your order = Pizza ",menu," and ",drink)))
    print(glue(menu," = ",get_menu_price(menu,list1)," THB"))
    print(glue(drink," = ",get_drink_price(drink,list2), " THB"))
    print(glue("Total Price = ",get_menu_price(menu,list1)+get_drink_price(drink,list2), " THB"))
  } else {
    print("Try to choose menu again")
  }
  
  confirm <- readline("Confirm Order YES or NO:")
    if(confirm == confirm_order) {
    print("Your order has been received, Wait aroud 10 min")
  } else {
    Pizza_2()
  }
}


# Run Function
Pizza_2()