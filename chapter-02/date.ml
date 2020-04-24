let valid_date mon date =
  if date > 31 then false
  else if mon = "Feb" && date > 29 then false
  else if (mon = "Apr" || mon = "Jun" || mon = "Sep" || mon = "Nov") && date > 30 then false
  else true