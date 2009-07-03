page_getfree ::
  forall    [A0]
  arg       (pages: ref (A0, true, {v | true}))
  ret       int (4, true, {v | true})
  store_in  [A0 |-> true: int (4, true, {v | true})]
  store_out [A0 |-> true: int (4, true, {v | true})]

page_free ::
  forall    [A0; A1]
  arg       (page: int (4, true, {v | true}), pages: ref (A0, true, {v | true}), page_protected: ref (A1, true, {v | true}))
  ret       int (4, true, {v | true})
  store_in  [A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]
  store_out [A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]

is_page_protected ::
  forall    [A0; A1]
  arg       (ppno: int (4, true, {v | true}), pages: ref (A0, true, {v | true}), page_protected: ref (A1, true, {v | true}))
  ret       int (4, true, {v | true})
  store_in  [A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]
  store_out [A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]

page_decref ::
  forall    [A0; A1]
  arg       (ppno: int (4, true, {v | true}), pages: ref (A0, true, {v | true}), page_protected: ref (A1, true, {v | true}))
  ret       int (0, true, {v | true})
  store_in  [A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]
  store_out [A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]

env_check ::
  forall    [A0; A1; A2]
  arg       (env: ref (A2, 0, {v | true}), envs: ref (A2, 0, {v | true}), pages: ref (A0, true, {v | true}), page_protected: ref (A1, true, {v | true}))
  ret       int (0, true, {v | true})
  store_in  [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]
  store_out [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]

mem_check ::
  forall    [A0; A1; A2]
  arg       (envs: ref (A2, 0, {v | true}), pages: ref (A0, true, {v | true}), page_protected: ref (A1, true, {v | true}))
  ret       int (0, true, {v | true})
  store_in  [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]
  store_out [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]

env_alloc ::
  forall    [A0; A1; A2]
  arg       (envs: ref (A2, 0, {v | true}), pages: ref (A0, true, {v | true}), page_protected: ref (A1, true, {v | true}))
  ret       ref (A2, 0, {v | true})
  store_in  [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]
  store_out [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]

env_free ::
  forall    [A0; A1; A2]
  arg       (env: ref (A2, 0, {v | true}), envs: ref (A2, 0, {v | true}), pages: ref (A0, true, {v | true}), page_protected: ref (A1, true, {v | true}))
  ret       int (0, true, {v | true})
  store_in  [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]
  store_out [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]

page_alloc ::
  forall [A0; A1; A2]
  arg (env: ref (A2, 0, {v | true}), vp: int (4, true, {v | true}), envs: ref (A2, 0, {v | true}), pages: ref (A0, true, {v | true}), page_protected: ref (A1, true, {v | true}))
  ret int (4, true, {v | true})
  store_in [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]
  store_out [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]

page_unmap ::
  forall    [A0; A1; A2]
  arg       (env: ref (A2, 0, {v | true}), vp: int (4, 0[1], {v | true}), envs: ref (A2, 0, {v | true}), pages: ref (A0, true, {v | true}), page_protected: ref (A1, true, {v | true}))
  ret       int (0, true, {v | true})
  store_in  [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]
  store_out [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]

page_map ::
  forall    [A0; A1; A2]
  arg       (srcenv: ref (A2, 0, {v | true}), srcvp: int (4, 0[1], {v | true}), dstenv: ref (A2, 0, {v | true}), dstvp: int (4, 0[1], {v | true}), envs: ref (A2, 0, {v | true}), pages: ref (A0, true, {v | true}), page_protected: ref (A1, true, {v | true}))
  ret       int (4, true, {v | true})
  store_in  [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]
  store_out [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]

main ::
  forall    [A0; A1; A2]
  arg       ()       
  ret       int (0, true, {v | true})
  store_in  []
  store_out []


//main ::
//  forall    [A0; A1; A2]
//  arg       (envs: ref (A2, 0, {v | true}), pages: ref (A0, true, {v | true}), page_protected: ref (A1, true, {v | true}))
//  ret       int (0, true, {v | true})
//  store_in  [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]
//  store_out [A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: ref (A2, 0, {v | true}), 12[4]: int (4, true, {v | true}); A0 |-> true: int (4, true, {v | true}); A1 |-> true: int (4, true, {v | true})]
