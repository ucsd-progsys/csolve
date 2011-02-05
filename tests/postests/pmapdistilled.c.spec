page_getfree ::
  forall    [A0]
  arg       (pages: ref (A0, 0{4}))
  ret       int (4, true)
  store_in  [A0 |-> 0{4}: int (4, true)]
  store_out [A0 |-> 0{4}: int (4, true)]

page_free ::
  forall    [A0; A1]
  arg       (page: int (4, true), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       int (4, true)
  store_in  [A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

is_page_protected ::
  forall    [A0; A1]
  arg       (ppno: int (4, true), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       int (4, true)
  store_in  [A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

page_decref ::
  forall    [A0; A1]
  arg       (ppno: int (4, true), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       int (0, true)
  store_in  [A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

env_check ::
  forall    [A0; A1; A2; A3]
  arg       (env: ref (A2, 0), envs: ref (A3, 0), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       int (0, true)
  store_in  [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

mem_check ::
  forall    [A0; A1; A2; A3]
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
  forall    [A0; A1; A2; A3]
  arg       (envs: ref (A3, 0), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       ref (A2, 0)
  store_in  [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

env_free ::
  forall    [A0; A1; A2; A3]
  arg       (env: ref (A2, 0), envs: ref (A3, 0), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       int (0, true)
  store_in  [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

page_alloc ::
  forall [A0; A1; A2; A3]
  arg (env: ref (A2, 0), vp: int (4, 0[1]), envs: ref (A3, 0), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret int (4, true)
  store_in  [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

page_unmap ::
  forall    [A0; A1; A2; A3]
  arg       (env: ref (A2, 0), vp: int (4, 0[1]), envs: ref (A3, 0), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       int (0, true)
  store_in  [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
  store_out [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

page_map ::
  forall    [A0; A1; A2; A3]
  arg       (srcenv: ref (A2, 0), srcvp: int (4, 0[1]), dstenv: ref (A2, 0), dstvp: int (4, 0[1]), envs: ref (A3, 0), pages: ref (A0, 0{4}), page_protected: ref (A1, 0{4}))
  ret       int (4, true)
  store_in  [A3 |-> 0: ref (A2, 0); 
             A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); 
	     A0 |-> 0{4}: int (4, true); 
	     A1 |-> 0{4}: int (4, true)]
  store_out [A3 |-> 0: ref (A2, 0); A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]

main ::
  forall    [A0; A1; A2]
  arg       ()       
  ret       int (0, true)
  store_in  []
  store_out []


//main ::
//  forall    [A0; A1; A2]
//  arg       (envs: ref (A2, 0), pages: ref (A0, true), page_protected: ref (A1, true))
//  ret       int (0, true)
//  store_in  [A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
//  store_out [A2 |-> 0: int (4, true), 4: ref (A2, 0), 8: ref (A2, 0), 12[4]: int (4, true); A0 |-> 0{4}: int (4, true); A1 |-> 0{4}: int (4, true)]
