module Cart exposing
    ( Cart
    , Item
    , Product
    , add
    , decrement
    , itemSubtotal
    , remove
    , subtotal
    )

-- This is module and its API definition

import List exposing (..)



-- we need list manipulation functions


type alias Cart =
    List Item


type alias Item =
    { product : Product
    , qty : Int
    }


type alias Product =
    { name : String
    , price : Float
    }


{-| Add a product to card, or increase quantity
-}
add : Cart -> Product -> Cart
add cart product =
    if isEmpty (filter (\item -> item.product == product) cart) then
        append cart [ Item product 1 ]

    else
        List.map
            (\item ->
                if item.product == product then
                    { item | qty = item.qty + 1 }

                else
                    item
            )
            cart


remove : Cart -> Product -> Cart
remove cart product =
    if not (isEmpty (filter (\item -> item.product == product) cart)) then
        filter
            (\item ->
                item.product /= product
            )
            cart

    else
        cart


decrement : Cart -> Product -> Cart
decrement cart product =
    List.map
        (\item ->
            if item.product == product then
                { item | qty = item.qty - 1 }

            else
                item
        )
        cart


{-| Calculate rounded cart subtotal
-}
subtotal : Cart -> Float
subtotal cart =
    sum (map itemSubtotal cart)


{-| Item subtotal takes item and return the subtotal float.
-}
itemSubtotal : Item -> Float
itemSubtotal item =
    item.product.price * toFloat item.qty
