discard :: (a -> Bool) -> [a] -> [a]
discard filter collection = [element | element<-collection, not (filter element)]

keep :: (a -> Bool) -> [a] -> [a]
keep filter collection = [element | element<-collection, filter element]

