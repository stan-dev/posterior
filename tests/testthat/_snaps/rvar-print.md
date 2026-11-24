# print(<rvar>) works

    Code
      print(rvar())
    Output
      rvar<1>[0] mean ± sd:
      rvar()
    Code
      print(x)
    Output
      rvar<2>[2,3] mean ± sd:
           [,1]         [,2]         [,3]        
      [1,]  1.5 ± 0.71   5.5 ± 0.71   9.5 ± 0.71 
      [2,]  3.5 ± 0.71   7.5 ± 0.71  11.5 ± 0.71 
    Code
      print(x_with_chains)
    Output
      rvar<1,2>[2,3] mean ± sd:
           [,1]         [,2]         [,3]        
      [1,]  1.5 ± 0.71   5.5 ± 0.71   9.5 ± 0.71 
      [2,]  3.5 ± 0.71   7.5 ± 0.71  11.5 ± 0.71 
    Code
      print(x, summary = "median_mad")
    Output
      rvar<2>[2,3] median ± mad:
           [,1]         [,2]         [,3]        
      [1,]  1.5 ± 0.74   5.5 ± 0.74   9.5 ± 0.74 
      [2,]  3.5 ± 0.74   7.5 ± 0.74  11.5 ± 0.74 

# print(<rvar_factor>) works

    Code
      print(x)
    Output
      rvar_factor<1,2>[2,3] mode <entropy>:
           [,1]      [,2]      [,3]     
      [1,] a <0.28>  e <0.28>  i <0.28> 
      [2,] c <0.28>  g <0.28>  k <0.28> 
      12 levels: a b c d e f g h i j k l

# print(<rvar_ordered>) works

    Code
      print(x)
    Output
      rvar_ordered<1,2>[2,3] mode <dissent>:
           [,1]       [,2]       [,3]      
      [1,] a <0.067>  e <0.067>  i <0.067> 
      [2,] c <0.067>  g <0.067>  k <0.067> 
      12 levels: a < b < c < d < e < f < g < h < i < j < k < l

# str(<rvar>) works

    Code
      str(rvar())
    Output
       rvar<1>[0]  
    Code
      str(rvar(1:3))
    Output
       rvar<3>[1]  2 ± 1
    Code
      str(x, vec.len = 5)
    Output
       rvar<2>[3,4]  1.5 ± 0.71  3.5 ± 0.71  5.5 ± 0.71  7.5 ± 0.71  9.5 ± 0.71 ...
    Code
      str(x_with_attrs, vec.len = 5)
    Output
       rvar<2>[3,4]  1.5 ± 0.71  3.5 ± 0.71  5.5 ± 0.71  7.5 ± 0.71  9.5 ± 0.71 ...
       - dimnames(*)=List of 2
        ..$ : chr [1:3] "a" "b" "c"
        ..$ : NULL
       - attr(draws_of(*), "foo")=List of 2
        ..$ : num 1
        ..$ : num 2
       - attr(*, "bar")=List of 2
        ..$ : num 1
        ..$ : num 2

# str(<rvar_factor>) works

    Code
      str(x, vec.len = 5)
    Output
       rvar_factor<1,2>[3,4]  a <0.00>  c <0.22>  e <0.22>  g <0.22>  i <0.22> ...
       - 24 levels: a b c d ... x
       - attr(*, "foo")= chr "bar"

# str(<rvar_ordered>) works

    Code
      str(x, vec.len = 5)
    Output
       rvar_ordered<1,2>[3,4]  a <0.000>  c <0.032>  e <0.032>  g <0.032>  i <0.032> ...
       - 24 levels: a < b < c < d < ... < x
       - attr(*, "foo")= chr "bar"

