import JSON
import PrettyJSON

main = do
  print example
  putStrLn ""
  render example
  putStrLn ""
  format example
  putStrLn ""
  putStrLn (pretty 15 (renderJValue example))

