main :: 
  forall  	[A0, C1]
  arg	  	(n: int(4,true,{v | v > 0}), m: int(4,true,{v | v > 100}))
  ret	  	ref(A0, 0, {v | true})
  store_in	[A0 |-> 0: int(4, 0[1], {v | true}), 4: ref(0, 0,{v | true}); 
                 C1 |-> 0: {v : int(4, true) | true} ]
  store_in	[A0 |-> 0: int(4, 0[1], {v | true}), 4: ref(0, 0,{v | true}); 
                 C1 |-> 0: {v : int(4, true) | true} ]

foo :: 
  forall  	[A0;C1]
  arg	  	(n: int(4, true, {v | v > 0}), m: int(4, true, {v | v > 100}))
  ret	  	ref(0, true, {v | true})
  store_in	[A0 |-> 0: int(4, 0[1],{v | true})]
  store_out	[C1 |-> 0: int(4, 0[1],{v | true})]


