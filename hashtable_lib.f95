module hashtable_lib
implicit none
integer, parameter :: nchar=10, nreal=8

type hashtable
    integer :: nkeys, nvalues
    integer :: current_index, ifound, ifull
    character(len=nchar),allocatable :: key(:)
    real(nreal), allocatable :: value(:,:)
    
    CONTAINS
    PROCEDURE  :: init => init_hashtable
    PROCEDURE  :: add => add_key_and_value
    PROCEDURE  :: get => get_value
                
end type hashtable

    contains

    subroutine init_hashtable(mytable, nkeys, nvalues)
    implicit none
    integer, intent(in) :: nkeys, nvalues
    CLASS(hashtable), intent(inout) ::  mytable
    
    mytable%nkeys = nkeys
    mytable%nvalues = nvalues
    allocate(mytable%key(mytable%nkeys), mytable%value(mytable%nkeys, mytable%nvalues) )
    mytable%key=""
    mytable%value=0.0d0
    mytable%current_index=0
    end subroutine init_hashtable


    subroutine add_key_and_value(mytable, xkey, xval, nval)
    implicit none
    integer :: i
    character(len=*),intent(in) :: xkey
    integer, intent(in) :: nval
    real(nreal),intent(in) :: xval(nval)
    CLASS(hashtable) , intent(inout) ::  mytable
    
    mytable%ifound=0
    mytable%ifull=1
    do i=1,mytable%nkeys
        if ( trim(mytable%key(i))==trim(xkey)) then
            mytable%current_index = i
            mytable%ifound=1
            exit
        endif
        if ( trim(mytable%key(i))=="") then
            mytable%ifull=0
        endif
    enddo
    
    if (mytable%ifull==1) then
        print*,'Key is full. "', trim(xkey), '" cannot be added.' 
        print*,'Max. number=', mytable%nkeys
        stop
    endif
    
    if (mytable%ifound==0) then
       do i=1,mytable%nkeys
            if ( trim(mytable%key(i))=="") then
                mytable%current_index = i
                exit
           endif
       enddo
    endif
    mytable%key(mytable%current_index) = xkey
    mytable%value(mytable%current_index, 1:nval) = xval
    end subroutine add_key_and_value	

    subroutine get_value(mytable, xkey, xval)
    implicit none
    integer :: i, nval, ival
    character(len=*),intent(in) :: xkey
    real(nreal),intent(out) :: xval(*)
    CLASS(hashtable), intent(inout) ::   mytable
    
    mytable%ifound=0
    do i=1,mytable%nkeys
        if ( trim(mytable%key(i))==trim(xkey)) then
            mytable%current_index = i
            mytable%ifound=1
            exit
        endif
    enddo
    if (mytable%ifound==1) then
        do i=1,mytable%nvalues
            if (mytable%value(mytable%current_index, i)==0) then
                ival = i-1
                exit
            endif
        enddo
        xval(1:ival) = mytable%value(mytable%current_index, 1:ival)
    else
        print*,"'",xkey, "' not fund"
        stop
    endif
    
    end subroutine get_value


end module hashtable_lib

