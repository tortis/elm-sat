module Utils exposing (find, kernelMap1, kernelFilter1)

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


kernelMap1 : (Maybe a -> a -> Maybe a -> b) -> List a -> List b
kernelMap1 mapper list =
    let
        kernelMapHelp : List a -> Maybe a -> List b
        kernelMapHelp innerList prev =
            case innerList of
                [] ->
                    []

                item :: rest ->
                    mapper prev item (List.head rest) :: kernelMapHelp rest (Just item)

    in
        kernelMapHelp list Nothing


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
