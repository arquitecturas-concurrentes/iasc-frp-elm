import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (red, blue)
import Mouse
import Time exposing (every, second)
import Text exposing (fromString)
import Signal exposing (foldp)
import Maybe exposing (..)

type alias AccountingState = (Account, Maybe Transference, Account)
type alias Account = Int
type alias Transference = (Int, Bool)

main : Signal Element
main = every (2 * second)
          |> Signal.map newTransference
          |> runTransference
          |> Signal.map draw


draw : AccountingState -> Element
draw (src, tx, dst) = drawAccounts src dst ++ drawTransference tx 
                        |> collage 500 200

drawAccounts src dst = [
  filled red (rect 100 50),
  label "origen = " src, 
  moveX 200 (filled blue (rect 100 50)), 
  moveX 200 (label "destino = " dst)] 

drawTransference tx = case tx of 
                        (Nothing) -> []
                        (Just tx) -> [moveX 100 (transferenceLabel tx)]

label l a =  toString a
              |> (++) l
              |> fromString
              |> text

transferenceLabel (amount, left) = label (if left then "==>" else "<==") amount 

runTransference = foldp f (200, Nothing, 200)
f : Transference -> AccountingState -> AccountingState
f tx (src, _, dst) = let (amount, left) = tx in
                      if left 
                        then (src - amount, Just tx, dst + amount)
                        else (src + amount, Just tx, dst - amount) 
                   
newTransference = round >> newTransference'
newTransference' seed = if seed % 2 == 0   
                          then (100, True) 
                          else (20, False) 



