
program test
    use hashtable_lib
    implicit none
    real(nreal) :: x(5), a(5), b(7) ,y(5)
    type(hashtable) :: mytable 
    
    call mytable%init(3, 7) !! (n-keys, n-values)
    
    x = [1.0, 2.0, 3.0, 4.0, 5.0]
    call mytable%add('H2', x, 5)
    
    a = [1.0, 1.0, 1.0, 1.0, 1.0]
    call mytable%add('CO', a, 5)
    
    b = [2.0, 2.0, 2.0, 2.0, 2.0, 7.0, 7.0]
    call mytable%add('H2O', b, 7)
    
    call mytable%get('H2', y)
    print*,y
    call mytable%get('CO', y)
    print*,y
    print*, mytable%key(1), mytable%key(2), mytable%key(3)
    
    call mytable%add('NH3', x, 5)
    call mytable%get('xxxx', y)
end

