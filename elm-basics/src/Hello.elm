-- Elm Basics:
{-
   Elm is a purely functional strongly typed programming language designed for building stateful web uis
   It compiles to optimized js and offers strong compile time guarentees ensuring almost zero runtimes exceptions
   Elm Compiler can infer types on its own, although its preferred to specify them for documentation and better error messages
   Elm types and data structures are immutable
   Notes:
    * Elm is indentation sensitive and case sensitive
    * Modules begin with a Capital letter while as all functions and argument names must begin with lower case
   Fundamental Data types:
    * Char
    * Bool
    * String
    * Int
    * Float
    * List
    * Tuple
    * Record
   Constrained Type variables (imported by default):
    * number (Int or Float)
    * appendable  (List or String)
    * comparable (Int, Float, Char, String, Lists and Tuples of comparable values)
    * compappend (String and List comparable)
    Additional Types
    * Maybe
    * Result
    Elm Module System
    Module names must match their file name
    Each module can expose a set of functions
     * Qualified imports
     * Open imports

-}


module Hello exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


z =
    'h'


zi : Char
zi =
    'V'



-- String


ab =
    "short string" ++ " another string"


a : String
a =
    """
    Long string
    spread across multiple lines
    """



-- Bool


b : Bool
b =
    True && not (True || False)



-- Number


gb : number
gb =
    (2 + 4) * (4 ^ 2 - 9)



-- Int


c : Int
c =
    4 ^ 2



-- Float


d : Float
d =
    3 / 2



-- List of ints


e : List Int
e =
    [ 1, 2, 3 ]



-- Tuple (can store multiple data types)


f : ( Int, String, Bool )
f =
    ( 1, "helo", True )



-- Record


g : { name : String, place : String }
g =
    { name = "asrar", place = "bengaluru" }



-- Updating values in records (gives a new record with the updated value)


ff =
    { g | name = "aszen" }



-- Adding strings and lists


h =
    "hello" ++ " world"


h2 =
    [ 1, 2, 3 ] ++ [ 4, 5, 6 ]



-- Inserting to lists (inserts at the front)
-- Lists are based on linked lists


i =
    1 :: [ 2, 3 ]



-- Conditionals


ii : Int
ii =
    100


remarks : String
remarks =
    if ii >= 100 then
        "perfect"

    else if ii < 100 && ii >= 50 then
        "okay"

    else
        "disaster"



-- Case of


no =
    1000


noLargeness =
    case no of
        1 ->
            "single"

        100 ->
            "century"

        _ ->
            "unknown"



-- type variables
-- are kept in lower case and indicate that the type can have any value
-- here `v` is the type variable


duplicateValue : v -> ( v, v )
duplicateValue v =
    ( v, v )



-- Map


kk =
    { a = 1, b = 2 }


kkk =
    { a = 8, b = 4 }



-- gives [1, 8]


vv =
    List.map .a [ kk, kkk ]



-- Functions


square : Float -> Float
square n =
    n * n


hypotenuse : Float -> Float -> Float
hypotenuse aa bb =
    sqrt (square aa + square bb)



-- Type aliases give a short name to a type


type alias Point =
    ( Float, Float )


cartesianDistance : Point -> Point -> Float
cartesianDistance ( p, q ) ( x, y ) =
    hypotenuse (x - p) (y - q)



-- Record Constructors
-- shorthand for creating records for a record type


type alias User =
    { id : Int, name : String, age : Int }


fooUser =
    User 1 "foo" 22


barUser =
    User 2 "bar" 24



-- Custom types (creating types from other types)
-- Account is a custom type
-- Account type has two variants which can diverge in the data they contain


type Account
    = ActiveAccount String Int
    | InactiveAccount String


account1 : Account
account1 =
    ActiveAccount "account1" 5


account2 : Account
account2 =
    InactiveAccount "account2"



-- Using custom types with pattern matching
-- `_` represents a wild card (variable / data exists but its not used)


getAccountName : Account -> String
getAccountName acct =
    case acct of
        ActiveAccount name age ->
            name

        InactiveAccount name ->
            name


getAccountAge : Account -> Int
getAccountAge acct =
    case acct of
        ActiveAccount _ age ->
            age

        InactiveAccount _ ->
            0



-- Maybe Type
-- For describing optionals
-- toFloat may fail in that case a Nothing type is returned else Float hence the function returns a type Maybe Float


fff : Maybe Float
fff =
    String.toFloat "3.14"


zf : Float
zf =
    case fff of
        Just fu ->
            fu

        Nothing ->
            0



-- Error Reporting and Result Type
-- Result type is defined as either an error or the value


validateAge : String -> Result String Int
validateAge inputAge =
    case String.toInt inputAge of
        Nothing ->
            Err "This is not a number"

        Just age ->
            if age < 0 then
                Err "Not a valid age"

            else if age > 150 then
                Err "Are you even human"

            else
                Ok age



-- Anonymous Functions


squares =
    List.map (\n -> n ^ 2) (List.range 1 100)



-- Function application operators give output of one function as input to another


names =
    "John, Mike, Aadam"



-- gives [Aadam, Mike, John]


xxx =
    String.split "," names |> List.sort |> String.join ", "



-- recursion as replacement for loops


generateFibonacci : Int -> Int
generateFibonacci n =
    if n <= 0 then
        0

    else
        case n of
            1 ->
                1

            2 ->
                1

            _ ->
                generateFibonacci (n - 2) + generateFibonacci (n - 1)


generateFibSeq : Int -> List Int
generateFibSeq till =
    if till <= 0 then
        []

    else if till == 1 then
        [ 1 ]

    else
        generateFibSeq (till - 1) ++ [ generateFibonacci till ]



-- Multi statement functions


funCalc : Int -> Int
funCalc n =
    let
        zb =
            n + 10

        g =
            zb / 10

        k =
            g * n
    in
    k + 2



-- Main
{-
   The main is a special value and represents the the main function to run
-}


main : Program () Model Message
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- Model


type alias Model =
    { count : Int
    , noOfCountUpdates : Int
    , showNoOfCountUpdates : Bool
    }



-- Init


init : Model
init =
    { count = 0
    , noOfCountUpdates = 0
    , showNoOfCountUpdates = False
    }



-- Update


type Message
    = Incr
    | Dcr
    | Reset
    | IncrBy10
    | ShowCountUpdates


modelAdder : Model -> Int -> Model
modelAdder model x =
    { model | count = model.count + x, noOfCountUpdates = model.noOfCountUpdates + 1 }


update : Message -> Model -> Model
update msg model =
    case msg of
        Incr ->
            modelAdder model 1

        Dcr ->
            { model | count = model.count - 1, noOfCountUpdates = model.noOfCountUpdates + 1 }

        Reset ->
            { model | count = 0, noOfCountUpdates = model.noOfCountUpdates + 1 }

        IncrBy10 ->
            modelAdder model 10

        ShowCountUpdates ->
            { model | showNoOfCountUpdates = not model.showNoOfCountUpdates, noOfCountUpdates = model.noOfCountUpdates + 1 }



-- View


view : Model -> Html Message
view model =
    div []
        [ div [] [ text (String.fromInt model.count) ]
        , button [ onClick Dcr ]
            [ text "-" ]
        , button
            [ onClick Incr ]
            [ text "+" ]
        , button
            [ onClick Reset ]
            [ text "reset" ]
        , button
            [ onClick IncrBy10 ]
            [ text "incr by 10" ]
        , button
            [ onClick ShowCountUpdates ]
            [ text
                (if model.showNoOfCountUpdates then
                    "Hide count updates"

                 else
                    "Show count updates"
                )
            ]
        , if model.showNoOfCountUpdates then
            div [] [ text (String.fromInt model.noOfCountUpdates) ]

          else
            div [] []
        ]
