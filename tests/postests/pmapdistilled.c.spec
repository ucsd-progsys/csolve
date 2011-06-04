page_getfree ::
  arg       (pages: ref (A0, 0{4}))
  ret       int (4, true)
  store_in  [A0 |-> 0{4}: int (4, true)]
  store_out [A0 |-> 0{4}: int (4, true)]

page_free ::
  arg       (page: int (4, true), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       int (4, true)
  store_in  [A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

is_page_protected ::
  arg       (ppno: int (4, true), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       int (4, true)
  store_in  [A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

page_decref ::
  arg       (ppno: int (4, true), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       int (0, true)
  store_in  [A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

env_check ::
  arg       (env: ref (A2, 0), envs: ref (A3, 0), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       int (0, true)
  store_in  [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

mem_check ::
  arg       (envs: ref (A3, 0), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       int (0, true)
  store_in  [A3 |-> 0: ref (A2, 0); 
             A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); 
	     A0 |-> 0{4}: int (4, true); 
	     A1 |-> 0{4}: int (4, true)]
  store_out [A3 |-> 0: ref (A2, 0); 
  	     A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); 
	     A0 |-> 0{4}: int (4, true); 
	     A1 |-> 0{4}: int (4, true)]

env_alloc ::
  arg       (envs: ref (A3, 0), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       ref (A2, 0)
  store_in  [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

env_free ::
  arg       (env: ref (A2, 0), envs: ref (A3, 0), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       int (0, true)
  store_in  [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

page_alloc ::
  arg (env: ref (A2, 0), vp: int (4, 0[1]), envs: ref (A3, 0), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret int (4, true)
  store_in  [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

page_unmap ::
  arg       (env: ref (A2, 0), vp: int (4, 0[1]), envs: ref (A3, 0), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       int (0, true)
  store_in  [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

page_map ::
  arg       (srcenv: ref (A2, 0), srcvp: int (4, 0[1]), dstenv: ref (A2, 0), dstvp: int (4, 0[1]), envs: ref (A3, 0), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       int (4, true)
  store_in  [A3 |-> 0: ref (A2, 0); 
             A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); 
	     A0 |-> 0{4}: int (4, true); 
	     A1 |-> 0{4}: int (4, true)]
  store_out [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

main ::
  arg       ()       
  ret       int (0, true)
  store_in  []
  store_out []


//main ::
//  arg       (envs: ref (A2, 0), pages: ref (A0, true), page_protected: ref (A1, true))
//  ret       int (0, true)
//  store_in  [A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
//  store_out [A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
