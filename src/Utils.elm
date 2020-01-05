module Utils exposing (find, kernelFilter1)


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


kernelFilter1 : (Maybe a -> a -> Maybe a -> Bool) -> List a -> List a
kernelFilter1 filter list =
    let
        kernelFilterHelp : List a -> Maybe a -> List a
        kernelFilterHelp innerList prev =
            case innerList of
                [] ->
                    []

                item :: rest ->
                    if filter prev item (List.head rest) then
                        item :: kernelFilterHelp rest (Just item)

                    else
                        kernelFilterHelp rest (Just item)
    in
    kernelFilterHelp list Nothing
