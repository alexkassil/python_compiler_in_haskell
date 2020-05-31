myDrop n ls = if (n >= (length ls))
                 then []
              else if n == 0
                   then ls
                   else(myDrop (n - 1) (tail ls))


