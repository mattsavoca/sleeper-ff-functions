x_games_adj = function(player, x){
  val = case_when(
    player == "Chris Godwin" ~ x - (.05*4),
    player == "Robert Woods" ~ x,
    player == "Michael Thomas" ~ x,
    player == "Alvin Kamara" ~ x,
    player == "James Robinson" ~ x,
    player == "Deshaun Watson" ~ x - (.9*15),
    player == "DeAndre Hopkins" ~ x - 6,
    player == "Rob Gronkowski" ~ x - (.002*17),
    T ~ x
  )
  return(val)
}
