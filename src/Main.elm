module Main exposing (Model, Msg(..), Stock, cartProductView, cartView, main, stockProductView, stockView, update, view)

import Browser exposing (sandbox, element)
import Cart exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (..)
import Url 


type alias Stock =
    List Product


type alias Model =
    { cart : Cart
    , stock : Stock
    }


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


init : () -> (Model, Cmd Msg)
init _ =
    (Model []
        [ Product "prod1" 100.5
        , Product "prod2" 15.36
        , Product "prod3" 21.15
        ], Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

{-| Messages - Add, remove, and change number of items in basket
-}
type Msg
    = Add Product
    | Remove Product
    | Decrement Product


{-| Update function to handle all updates - Add, Remove, or Decrement amount of product
-}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Add product ->
            ({ model | cart = add model.cart product }, Cmd.none)

        Remove product ->
            ({ model | cart = remove model.cart product }, Cmd.none)

        Decrement product ->
            ({ model | cart = decrement model.cart product }, Cmd.none)


{-| View wrapper - Stock and cart
-}
view : Model -> Html Msg
view model =
    section []
        [ stockView model.stock
        , cartView model.cart
        ]


{-| Produces a table of stock based on stockProductView
-}
stockView : Stock -> Html Msg
stockView stock =
    table []
        [ caption [] [ h1 [] [ text "Stock" ] ]
        , thead []
            [ tr []
                [ th [ align "left", width 100 ] [ text "Name" ]
                , th [ align "right", width 100 ] [ text "Price" ]
                , th [ width 100 ] []
                ]
            ]
        , tbody [] (List.map stockProductView stock)
        ]


{-| Stock product view - Produces individual table rows for each item of stock
-}
stockProductView : Product -> Html Msg
stockProductView product =
    tr []
        [ td [] [ text product.name ]
        , td [ align "right" ] [ text ("\t£" ++ String.fromFloat product.price) ]
        , td [] [ button [ onClick (Add product) ] [ text "Add to Cart" ] ]
        ]


{-| Cart view - Produces a table showing what's in cart
-}
cartView : Cart -> Html Msg
cartView cart =
    if isEmpty cart then
        p [] [ text "Cart is empty" ]

    else
        table []
            [ caption [] [ h1 [] [ text "Cart" ] ]
            , thead []
                [ tr []
                    [ th [ align "left", width 100 ] [ text "Name" ]
                    , th [ align "right", width 100 ] [ text "Price" ]
                    , th [ align "center", width 50 ] [ text "Qty" ]
                    , th [ align "right", width 100 ] [ text "Subtotal" ]
                    ]
                ]
            , tbody [] (List.map cartProductView cart)
            , tfoot []
                [ tr []
                    [ td [ align "right", colspan 4 ] [ text ("£" ++ String.fromFloat (subtotal cart)) ] ]
                ]
            ]


{-| Individual row in the cart table
-}
cartProductView : Item -> Html Msg
cartProductView item =
    tr []
        [ td [] [ text item.product.name ]
        , td [ align "right" ] [ text ("£" ++ String.fromFloat item.product.price) ]
        , td [ align "center" ] [ text (String.fromInt item.qty) ]
        , td [ align "center" ] [ button [ onClick (Decrement item.product) ] [ text "-" ] ]
        , td [ align "right" ] [ text ("£" ++ String.fromFloat (itemSubtotal item)) ]
        , td [ align "center" ] [ button [ onClick (Add item.product) ] [ text "+" ] ]
        , td [ align "center" ] [ button [ onClick (Remove item.product) ] [ text "Remove" ] ]
        ]
