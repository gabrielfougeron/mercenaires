module strategies
    
    use rules
    
contains

! Stupid strategy always pays 1
    function stupid() result(pay)
    
        integer :: pay
        
        pay = 1
        
    end function stupid

! Human input lowest verbose level
    function human_noverb() result(pay)
        
        integer :: pay
        
        read*,pay
        
    end function human_noverb
    
! Human input w/o clearing screen
    function human() result(pay)
        
        integer :: pay
        
        if (isplaying == leftisplaying) then
            print*, 'You are the player on the left. What do you do ?'            
        else if (isplaying == rightisplaying) then
            print*, 'You are the player on the right. What do you do ?'
        else
            print*, 'There was an error'
        end if
        
        read*,pay
        
    end function human

! Human input with screen clearing    
    function human_blind() result(pay)
    
        integer :: pay
        
        call system('clear')
        call print_cgstate()
        
        if (isplaying == leftisplaying) then
            print*, 'You are the player on the left. What do you do ?'            
        else if (isplaying == rightisplaying) then
            print*, 'You are the player on the right. What do you do ?'
        else
            print*, 'There was an error'
        end if
        
        read*,pay
    
    end function human_blind

! Play randomly according to a given distribution    
    subroutine play_distrib(pi,s,pay)
        
        real(kind=real_kind)    , dimension(s)          , intent(in)    :: pi       ! Discrete probability distribution
        integer                                         , intent(in)    :: s        ! Money in wallet
        integer                                         , intent(out)   :: pay      ! What you choose to pay
        
        real(kind=real_kind)                                            :: nran     ! Random number
        integer                                                         :: i,j,k
        
        call random_number(nran)
        
        i=1
        j=s

        do while (i .ne. j)
        
            k = (i+j)/2
            if (nran < pi(k)) then
                j = k
            else
                i = k+1
            end if

        end do
        
        pay = i
        
    end subroutine play_distrib

! Play according to uniform distribution
    function unif_distrib() result(pay)
    
        integer                                         :: pay
        integer , dimension(3)                          :: state
        real(kind=real_kind)    , dimension(moneyinit)  :: pi       ! Discrete probability distribution
        integer                                         :: i
        
        call copy_current_board_left(state)
        
        do i=1,state(1)
            pi(i) = real(i,real_kind)/state(1)
        end do
        
        call play_distrib(pi,state(1),pay)
    
    end function unif_distrib
    
end module strategies
