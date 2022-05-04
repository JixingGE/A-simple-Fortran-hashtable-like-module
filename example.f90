program test
    use hashtable_lib
    implicit none
    real(nreal) :: x(5), a(5), b(7) ,y(5)
    type(hashtable) :: mytable 
    
    call mytable%init(3, 7) !! (n-keys, n-values)
    
    x = [1.0, 2.0, 3.0, 4.0, 5.0]
    call mytable%add('H2', x) !! the final is the dimension of x
    
    a = [1.0, 1.0, 1.0, 1.0, 1.0]
    call mytable%add('CO', a)
    call mytable%add('CO', a)
    
    b = [2.0, 2.0, 2.0, 2.0, 2.0, 7.0, 7.0]
    call mytable%add('H2O', b)
    
    call mytable%get('H2', y)
    print*,y
    
    call mytable%get('CO', y)
    print*,y
    
    call mytable%list()
    
    call mytable%del('H2')
    call mytable%list()
    
    call mytable%add('H2', x)
    call mytable%list()
    
    call mytable%get('H2', y)
    print*,y
    
    call mytable%get('xxxx', y)
    call mytable%add('NH3', x)
    
end
