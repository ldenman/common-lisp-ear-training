;;;; DSL
(defmacro song (&rest data)
  `(quote ,data))

(song
 (I  . 1)
 (do 4 mi so mi)
 (IV . 1)
 (fa la so fa)
 (V  . 1)
 (so ti re fa)
 (I  . 1)
 (mi re do))

(song do 4 re mi fa so la)
