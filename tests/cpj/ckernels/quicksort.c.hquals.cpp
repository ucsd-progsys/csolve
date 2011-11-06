qualif VIDX(V:int)             : PINDEX(~A,4)
qualif GTMINUS1(V:int)         : 0 <= (V + 1)
qualif AINDEX(V: ptr, ~a: int) : V [ = ; != ; <= ; >= ] (@a + (4 * ~a))