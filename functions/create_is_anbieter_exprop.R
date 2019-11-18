
create_is_anbieter_exprop <- function(is_anbieter) {

  # List all companies that would get expropriated according to referendum
  # All others not mentioned here (Pears Global etc.) do not exist in the dataset
  # Hilfswerk is not profit oriented, thus excluded

  exprop_anbieter <- c(
    "Deutsche Wohnen Gruppe",
    "Vonovia SE",
    "ADO Immobilien Management GmbH",
    "AKELIUS GmbH",
    "Covivio Immobilien GmbH",
    "Grand City Property Ltd., ZNL Dtl.",
    "Grand City Property",
    "BGP Immobilienservice GmbH")

  is_anbieter_exprop <- fct_collapse(is_anbieter,
    `Deutsche Wohnen` = "Deutsche Wohnen Gruppe",
    `Vonovia` = "Vonovia SE",
    `ADO Immobilien` = "ADO Immobilien Management GmbH",
    Akelius = "AKELIUS GmbH",
    `Convivio` = "Covivio Immobilien GmbH",
    `Grand City Properties` = c(
      "Grand City Property Ltd., ZNL Dtl.",
      "Grand City Property"),
    `BGP Investment` = "BGP Immobilienservice GmbH",
    # group_other Option funktioniert für den Befehl fct_collapse hier nicht
    `andere Anbieter` = levels(is_anbieter)[which(!levels(is_anbieter) %in% c(
      exprop_anbieter,
      "keine Angaben"))],
    `keine Angaben` = "keine Angaben"
    )

  return(is_anbieter_exprop)
}
