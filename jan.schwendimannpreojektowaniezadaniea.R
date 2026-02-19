przelicz_walute = function(kwota, kurs) {
  if (kurs == 0) {
    kurs = 4.32
  }
  kwota_przeliczona=kwota/kurs
  
  return(kwota_przeliczona)
}
print(przelicz_walute(100,0))
print(przelicz_walute(100,2))
