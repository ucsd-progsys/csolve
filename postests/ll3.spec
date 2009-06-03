main :: 
  forall  	[0;1]
  arg	  	(n: {v : int(4, true) | v > 0}, m: {v: int(4, true) | v > 100})
  ret	  	{v: ref(0, 0) | true}
  store_in	[0 |-> 0: {v : int(4, 0[1]) | true}, 4: {v : ref(0, 0) | true}; 
                 1 |-> 0: {v : int(4, true) | true} ]
  store_out	[0 |-> 0: {v : int(4, 0[1]) | true}, 4: {v : ref(0, 0) | true}; 
                 1 |-> 0: {v : int(4, true) | true} ]
