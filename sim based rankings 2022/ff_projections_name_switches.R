# To UNDERDOG -----


## FP to Underdog ----

fp_to_underdog_name_switch = function(name){
  nm = gsub("Patrick Mahomes II", "Patrick Mahomes", name)
  nm = gsub("Gardner Minshew II", "Gardner Minshew", nm)
  nm = gsub("Travis Etienne Jr.", "Travis Etienne", nm)
  nm = gsub("Melvin Gordon III", "Melvin Gordon", nm)
  nm = gsub("Ronald Jones II", "Ronald Jones", nm)
  nm = gsub("DK Metcalf", "D.K. Metcalf", nm)
  nm = gsub("Darrell Henderson Jr.", "Darrell Henderson", nm)
  nm = gsub("Brian Robinson Jr.", "Brian Robinson", nm)
  nm = gsub("Ken Walker III", "Ken Walker", nm)
  nm = gsub("Duke Johnson Jr.", "Duke Johnson", nm)
  nm = gsub("Wayne Gallman Jr.", "Wayne Gallman", nm)
  nm = gsub("Pierre Strong Jr.", "Pierre Strong", nm)
  nm = gsub("D.J. Chark Jr.", "D.J. Chark", nm)
  nm = gsub("DJ. Chark", "D.J. Chark", nm)
  nm = gsub("DJ Chark Jr.", "D.J. Chark", nm)
  nm = gsub("William Fuller V", "William Fuller", nm)
  nm = gsub("Odell Beckham Jr.", "Odell Beckham", nm)
  nm = gsub("Laviska Shenault Jr.", "Laviska Shenault", nm)
  nm = gsub("Marvin Jones Jr.", "Marvin Jones", nm)
  nm = gsub("Terrace Marshall Jr.", "Terrace Marshall", nm)
  nm = gsub("Michael Pittman Jr.", "Michael Pittman", nm)
  nm = gsub("Allen Robinson II", "Allen Robinson", nm)
  nm = gsub("Mark Ingram II", "Mark Ingram", nm)
  nm = gsub("Jeff Wilson Jr.", "Jeff Wilson", nm)
  nm = gsub("Cedrick Wilson Jr.", "Cedrick Wilson", nm)
  nm = gsub("Keelan Cole Sr.", "Keelan Cole", nm)
  nm = gsub("Calvin Austin III", "Calvin Austin", nm)
  nm = gsub("Cyril Grayson Jr.", "Cyril Grayson", nm)
  nm = gsub("Mohamed Sanu Sr.", "Mohamed Sanu", nm)
  nm = gsub("Irv Smith Jr.", "Irv Smith", nm)
  nm = gsub("Donald Parham Jr.", "Donald Parham", nm)
  nm = gsub("Chris Herndon IV", "Chris Herndon", nm)
  nm = gsub("Anthony McFarland Jr.", "Anthony McFarland", nm)
  nm = gsub("John Metchie III", "John Metchie", nm)
  nm = gsub("Velus Jones Jr.", "Velus Jones", nm)
  nm = gsub("James Proche II", "James Proche", nm)
  nm = gsub("Scotty Miller", "Scott Miller", nm)
  nm = gsub("Deonte Harty", "Deonte Harris", nm)
  nm = gsub("Eli Mitchell", "Elijah Mitchell", nm)
  nm = gsub("Tyquan Thorton", "Tyquan Thornton", nm)
  nm = gsub("Dee Eskridge", "D'Wayne Eskridge", nm)
  nm = gsub("Damoun Patterson", "Damon Hazelton", nm)
  nm = gsub("Mitch Trubisky", "Mitchell Trubisky", nm)
  nm = gsub("Robbie Anderson", "Robby Anderson", nm)
  nm = gsub("Jakeem Grant Sr.", "Jakeem Grant", nm)
  nm = gsub("Tony Jones Jr.", "Tony Jones", nm)
  nm = gsub("Larry Rountree III", "Larry Rountree", nm)
  nm = gsub("Demetric Felton Jr.", "Demetric Felton", nm)
  nm = gsub("P.J. Walker", "Phillip Walker", nm)
  nm = gsub("Isiah Pacheco", "Isaih Pacheco", nm)
  nm = gsub("Phillip Dorsett II", "Phillip Dorsett", nm)
  nm = gsub("Benny Snell Jr.", "Benny Snell", nm)
  return(nm)
}



## PFF to Underdog ------

pff_to_underdog_name_switch = function(name){
  nm = gsub("Patrick Mahomes II", "Patrick Mahomes", name)
  nm = gsub("D.J. Moore", "DJ Moore", nm)
  nm = gsub("AJ Dillon", "A.J. Dillon", nm)
  nm = gsub("Ronald Jones II", "Ronald Jones", nm)
  nm = gsub("Todd Gurley II", "Todd Gurley", nm)
  nm = gsub("Darrell Henderson", "Darrell Henderson", nm)
  nm = gsub("William Fuller V", "Will Fuller V", nm)
  nm = gsub("Joshua Palmer", "Josh Palmer", nm)
  nm = gsub("KJ Hamler", "K.J. Hamler", nm)
  nm = gsub("Cedrick Wilson Jr.", "Cedrick Wilson", nm)
  nm = gsub("Wayne Gallman Jr.", "Wayne Gallman", nm)
  nm = gsub("William Fuller V", "Will Fuller V", nm)
  nm = gsub("Keelan Cole Sr.", "Keelan Cole", nm)
  nm = gsub("Mohamed Sanu Sr.", "Mohamed Sanu", nm)
  nm = gsub("Chris Herndon IV", "Chris Herndon", nm)
  nm = gsub("Cyril Grayson Jr.", "Cyril Grayson", nm)
  nm = gsub("Donald Parham Jr.", "Donald Parham", nm)
  nm = gsub("Michael Pittman Jr.", "Michael Pittman", nm)
  nm = gsub("Jeff Wilson Jr.", "Jeff Wilson", nm)
  nm = gsub("Melvin Gordon III", "Melvin Gordon", nm)
  nm = gsub("D.J. Chark Jr.", "D.J. Chark", nm)
  nm = gsub("A.J. Dillon", "AJ Dillon", nm)
  nm = gsub("Marvin Jones Jr.", "Marvin Jones", nm)
  nm = gsub("Mark Ingram II", "Mark Ingram", nm)
  nm = gsub("Duke Johnson Jr.", "Duke Johnson", nm)
  nm = gsub("Allen Robinson II", "Allen Robinson", nm)
  nm = gsub("Odell Beckham Jr.", "Odell Beckham", nm)
  nm = gsub("Laviska Shenault Jr.", "Laviska Shenault", nm)
  nm = gsub("Victor Bolden Jr.", "Victor Bolden", nm)
  nm = gsub("Paul Richardson Jr.", "Paul Richardson", nm)
  nm = gsub("K.J. Hamler", "KJ Hamler", nm)
  nm = gsub("Ray-Ray McCloud", "Ray-Ray McCloud III", nm)
  nm = gsub("Irv Smith Jr.", "Irv Smith", nm)
  nm = gsub("Lynn Bowden Jr.", "Lynn Bowden", nm)
  nm = gsub("Richie James Jr.", "Richie James", nm)
  nm = gsub("Steven Mitchell Jr.", "Steven Mitchell", nm)
  nm = gsub("Willie Snead IV", "Willie Snead", nm)
  nm = gsub("Terrace Marshall Jr.", "Terrace Marshall", nm)
  nm = gsub("Joe Webb III", "Joe Webb", nm)
  nm = gsub("Tyquan Thorton", "Tyquan Thornton", nm)
  nm = gsub("Demetric Felton Jr.", "Demetric Felton", nm)
  nm = gsub("Jakeem Grant Sr.", "Jakeem Grant", nm)
  nm = gsub("Tony Jones Jr.", "Tony Jones", nm)
  nm = gsub("Larry Rountree III", "Larry Rountree", nm)
  nm = gsub("P.J. Walker", "Phillip Walker", nm)
  nm = gsub("Trestan Ebner", "Trestan Ebnar", nm)
  nm = gsub("Isaih Pacheco", "Isiah Pacheco", nm)
  return(nm)
}


# To FantasyDraftCoach -------

underdog_to_fdc_nameswitch = function(player){
  fdc_player = case_when(
    player == "Patrick Mahomes" ~ "Patrick Mahomes II",
    player == "D.K. Metcalf" ~ "DK Metcalf",
    player == "Travis Etienne" ~ "Travis Etienne Jr.",
    player == "Michael Pittman" ~ "Michael Pittman Jr.",
    player == "Melvin Gordon" ~ "Melvin Gordon III",
    player == "Kenneth Walker" ~ "Ken Walker III",
    player == "Allen Robinson" ~ "Allen Robinson II",
    player == "Darrell Henderson" ~ "Darrell Henderson Jr.",
    player == "Rob Gronkowski" ~ "Codey McElroy",
    player == "Ronald Jones" ~ "Ronald Jones II",
    player == "Robby Anderson" ~ "Robbie Anderson",
    player == "Ronald Jones" ~ "Ronald Jones II",
    player == "Odell Beckham" ~ "Deonte Harty",
    player == "Darrel Williams" ~ "Dwayne Washington",
    player == "Julio Jones" ~ "Stanley Morgan Jr.",
    player == "Mark Ingram" ~ "Mark Ingram II",
    player == "Marvin Jones" ~ "Marvin Jones Jr.",
    player == "D.J. Chark" ~ "DJ Chark Jr.",
    player == "Irv Smith" ~ "Irv Smith Jr.",
    player == "Laviska Shenault" ~ "Laviska Shenault Jr.",
    player == "John Metchie" ~ "John Metchie III",
    player == "Cedrick Wilson" ~ "Cedrick Wilson Jr.",
    player == "Donald Parham" ~ "Donald Parham Jr.",
    player == "Jeff Wilson" ~ "Jeff WIlson Jr.",
    player == "Duke Johnson" ~ "Duke Johnson Jr.",
    player == "Terrace Marshall" ~ "Terrace Marshall Jr.",
    player == "Velus Jones" ~ "Velus Jones Jr.",
    player == "Demetric Felton" ~ "Demetric Felton Jr.",
    player == "D'Wayne Eskridge" ~ "Dee Eskridge",
    player == "Justyn Ross" ~ "Josh Gordon",
    player == "Antonio Brown" ~ "Dezmon Patmon",
    player == "William Fuller" ~ "Bo Melton",
    T ~ player
  )
  return(fdc_player)
}


fdc_needed_names = c(
  "Jeff Smith",
  "Dwayne Washington",
  "Josh Gordon",
  "Bo Melton",
  "Codey McElroy",
  "Dezmon Patmon"
)

# To FantasyPros --------
## Awesemo to FP -----
awesemo_to_ff_nameadjust = function(player){
  nm = gsub("Eli Mitchell","Elijah Mitchell", player)
  nm = gsub("JJ Arcega-Whiteside", "J.J. Arcega-Whiteside", nm)
  nm = gsub("Benny Snell Jr.", "Benny Snell", nm)
  nm = gsub("DJ Chark Jr.", "D.J. Chark", nm)
  nm = gsub("Kenneth Walker", "Ken Walker III", nm)
  nm = gsub("Ray-Ray McCloud III", "Ray-Ray McCloud", nm)
  nm = gsub("Tyquan Thorton", "Tyquan Thornton", nm)
  nm = gsub("Brian Robinson Jr.", "Brian Robinson", nm)
  nm = gsub("Brian Robinson Jr. Jr.", "Brian Robinson", nm)
  nm = gsub("Chig Okonkwo", "Chigoziem Okonkwo", nm)
  return(nm)
}


## ETR to FP ----
etr_to_fp_nameadjust = function(player){
  nm = gsub("DK Metcalf", "D.K. Metcalf", player)
  nm = gsub("Kenneth Walker III", "Ken Walker IIr", nm)
  nm = gsub("Will Fuller", "William Fuller", nm)
  nm = gsub("Robbie Anderson", "Robby Anderson", nm)
  nm = gsub("Josh Palmer", "Joshua Palmer", nm)
  nm = gsub("K.J. Hamler", "KJ Hamler", nm)
}