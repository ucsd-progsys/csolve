qualif VIDX(V:int)       : PINDEX(~A,4)
qualif VSIZEGE(V:ptr)    : PSIZE_GE((4 * ~A))
qualif GTMINUS1(V:int)   : 0 <= (V + 1)
qualif AINDEX(V: ptr)    : V = (@a + (4 * @i))
qualif QSSTART(V: ptr)   : @a <= V
qualif QSEFFECT(V: ptr)  : V != (@a + (4 * ~A))
qualif QSEFFECT(V: ptr)  : V <= (@a + (4 * ~A))
qualif SORTRIGHT(V: ptr) : V >= (@a + (4 * @lt))
