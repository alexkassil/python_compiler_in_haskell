lend amount balance = let reserve = 100
                          newBalance = balance - amount
                      in if newBalance < reserve
                            then Nothing
                            else Just newBalance
