# A simple Fortran hash table module


# Example:

```Fortran


program test
    use hashtable_lib
    implicit none
    real(nreal) :: x(5), a(5), b(7) ,y(5)
    type(hashtable) :: mytable 
    
    call mytable%init(3, 7) !! (n-keys, n-values)
    
    x = [1.0, 2.0, 3.0, 4.0, 5.0]
    call mytable%add('H2', x, 5) !! the final is the dimension of x
    
    a = [1.0, 1.0, 1.0, 1.0, 1.0]
    call mytable%add('CO', a, 5)
    call mytable%add('CO', a, 5)
    
    b = [2.0, 2.0, 2.0, 2.0, 2.0, 7.0, 7.0]
    call mytable%add('H2O', b, 7)
    
    call mytable%get('H2', y)
    print*,y
    
    call mytable%get('CO', y)
    print*,y
    
    call mytable%list()
    
    call mytable%del('H2')
    call mytable%list()
    
    call mytable%add('H2', x, 5)
    call mytable%list()
    
    call mytable%get('H2', y)
    print*,y
    
    call mytable%add('NH3', x, 5)
    call mytable%get('xxxx', y)
end
```

# Output:

```text
   1.0000000000000000        2.0000000000000000        3.0000000000000000        4.0000000000000000        5.0000000000000000     
   1.0000000000000000        1.0000000000000000        1.0000000000000000        1.0000000000000000        1.0000000000000000     
           1 H2
           2 CO
           3 H2O
           2 CO
           3 H2O
           1 H2
           2 CO
           3 H2O
   1.0000000000000000        2.0000000000000000        3.0000000000000000        4.0000000000000000        5.0000000000000000     
 Key is full. "NH3" cannot be added.
 Max. number=           3

```
