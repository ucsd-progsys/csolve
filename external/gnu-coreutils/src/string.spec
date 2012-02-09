
// /usr/include/string.h:65: Error: No spec for extern function memset. Autogen spec is:

memset ::
  arg       (__s : ref(A225, 0),
             __c : int(4, 0{1}),
             __n : int(4, 0{1}))
  ret       ref(A225, 0)
  store     [A225 |-> 0: int(0, 0{1})]

// /usr/include/string.h:68: Error: No spec for extern function memcmp. Autogen spec is:

memcmp ::
  arg       (__s1 : ref(A226, 0),
             __s2 : ref(A226, 0),
             __n : int(4, 0{1}))
  ret       int(4, 0{1})
  store     [A226 |-> 0: int(0, 0{1})]

// /usr/include/string.h:235: Error: No spec for extern function strchr. Autogen spec is:

strchr ::
  arg       (__s : ref(A227, 0),
             __c : int(4, 0{1}))
  ret       ref(A228, 0)
  store_in  [A227 |-> 0: int(1, 0{1})]
  store_out [A227 |-> 0: int(1, 0{1});
             A228 |-> 0: int(1, 0{1})]


