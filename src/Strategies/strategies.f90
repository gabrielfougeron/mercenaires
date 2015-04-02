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
    
        integer                                             :: pay
        integer , dimension(3)                              :: state
        real(kind=real_kind)    , allocatable, dimension(:) :: pi       ! Discrete probability distribution
        integer                                             :: i
        
        call copy_current_board_left(state)
        allocate(pi(state(1)))
        
        do i=1,state(1)
            pi(i) = real(i,real_kind)/state(1)
        end do
        
        call play_distrib(pi,state(1),pay)
    
    end function unif_distrib
    
! An example of deterministic play
    function itsatrap() result(pay)
    
        integer                                         :: pay
        integer , dimension(3)                          :: state
        
        call copy_current_board_left(state)
        
        select case(state(2))
        case(1-disttocen)
            pay = min(state(1),state(3))
        case default
            pay = 1
        end select
    
    end function itsatrap

! A strategy that takes another strategy and pays 1 more.
    function plusone() result(pay)
    
        integer                                         :: pay
        integer , dimension(3)                          :: state
        
        pay = unif_distrib()
        call copy_current_board_left(state)
        
        if (pay < state(1)) then
            pay = pay + 1
        end if
        
    
    end function plusone
    
! A strategy that takes another strategy and pays 1 less.
    function minusone() result(pay)
    
        integer                                         :: pay
        integer , dimension(3)                          :: state
        
        pay = unif_distrib()
        call copy_current_board_left(state)
        
        if ((pay > 1) .and. (state(1) > 1)) then
            pay = pay - 1
        end if
        
    
    end function minusone

! Finds a Nash equilibrium of a strategic form game with integer payoffs using the Lemke-Howson algorithm.
    subroutine find1nash_lemhow(A,BT,m,n,x,y)
        
        real(kind=real_kind)    , dimension(m,n)    , intent(inout) :: A    ! Payoff matrix : m lines, n columns
        real(kind=real_kind)    , dimension(n,m)    , intent(inout) :: BT   ! Payoff matrix : n lines, m columns
        integer                                     , intent(in)    :: m    ! Number of lines of A = number of pure strategies of player X
        integer                                     , intent(in)    :: n    ! Number of columns of A = number of pure strategies of player Y
        real(kind=real_kind)    , dimension(m)      , intent(out)   :: x    ! Non scaled probabilities to play pure strategies for player X
        real(kind=real_kind)    , dimension(n)      , intent(out)   :: y    ! Non scaled probabilities to play pure strategies for player Y
        
        integer                                                     :: k,l,i,j
        real(kind=real_kind)    , dimension(n)                      :: s,C1
        real(kind=real_kind)    , dimension(m)                      :: r,D1
        real(kind=real_kind)    , dimension(n,m)                    :: C 
        real(kind=real_kind)    , dimension(m,n)                    :: D 
        real(kind=real_kind)                                        :: aoffset,btoffset
        real(kind=real_kind)                                        :: p,q
        integer                                     , parameter     :: ko = 1   ! Missing label, chosen to be 1 here.
        
        aoffset=0
        do i=1,m
            do j=1,n
                p = A(i,j)
                if (aoffset < p) then
                    aoffset = p
                end if
            end do
        end do

        btoffset=0
        do i=1,n
            do j=1,m
                p = BT(i,j)
                if (btoffset < p) then
                    btoffset = p
                end if
            end do
        end do
        
        A = A + aoffset
        BT = BT + btoffset
        
        k = 0
        
        
        
    end subroutine find1nash_lemhow
    
end module strategies
