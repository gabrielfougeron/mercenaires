module strategies
    
    use rules

    real(kind=real_kind)    , dimension(:,:,:,:)    , allocatable   ::  behavstrat1
    real(kind=real_kind)    , dimension(:,:,:,:)    , allocatable   ::  behavstrat2
    
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
        
        pay = behav_distrib1()
        call copy_current_board_left(state)
        
        if (pay < state(1)) then
            pay = pay + 1
        end if
        
    
    end function plusone
    
! A strategy that takes another strategy and pays 1 less.
    function minusone() result(pay)
    
        integer                                         :: pay
        integer , dimension(3)                          :: state
        
        pay = behav_distrib1()
        call copy_current_board_left(state)
        
        if ((pay > 1) .and. (state(1) > 1)) then
            pay = pay - 1
        end if
        
    
    end function minusone

! Finds a Nash equilibrium of a strategic form game with real payoffs using the Lemke-Howson algorithm.
    subroutine find1nash_lemhow_real(B,A,m,n,x,v,y,u)
        
        real(kind=real_kind)    , dimension(m,n)    , intent(in)    :: A    ! Payoff matrix : m lines, n columns
        real(kind=real_kind)    , dimension(m,n)    , intent(in)    :: B    ! Payoff matrix : n lines, m columns
        integer                                     , intent(in)    :: m    ! Number of lines of A = number of pure strategies of player X
        integer                                     , intent(in)    :: n    ! Number of columns of A = number of pure strategies of player Y
        real(kind=real_kind)    , dimension(m)      , intent(out)   :: x    ! Scaled probabilities to play pure strategies for player X
        real(kind=real_kind)                        , intent(out)   :: u    ! Expected payoff for player X
        real(kind=real_kind)    , dimension(n)      , intent(out)   :: y    ! Scaled probabilities to play pure strategies for player Y
        real(kind=real_kind)                        , intent(out)   :: v    ! Expected payoff for player X
        
        real(kind=real_kind)    , dimension(n+m,n+m)                :: mat
        real(kind=real_kind)    , dimension(n+m)                    :: c
        integer                 , dimension(n+m)                    :: bas
        real(kind=real_kind)                                        :: moffset
        real(kind=real_kind)                                        :: p,q, piv
        real(kind=real_kind)                        , parameter     :: eps = 1d-11
        integer                                     , parameter     :: ko =  1  ! Missing label.
        integer                                                     :: k,l,i,j
        integer                                                     :: mpn,mp1,mp2
        
        ! Initialisation
        
        mpn = m + n
        mp1 = m + 1
        mp2 = m + 2

        moffset=1d25
        do i=1,m
            do j=1,n
                p = min(A(i,j),B(i,j))
                if (moffset > p) then
                    moffset = p
                end if
            end do
        end do
        
        moffset = moffset - 1
!~         moffset =  - 2
        
        do i=1,m
            do j=1,m
                if (i .ne. j) then
                    mat(i,j) = 0
                else
                    mat(i,j) = 1
                end if
            end do
            do j=mp1,mpn
                mat(i,j) = A(i,j-m) - moffset
            end do
        end do
        do i=mp1,mpn
            do j=1,m
                mat(i,j) = B(j,i-m) - moffset
            end do
            do j=mp1,mpn
                if (i .ne. j) then
                    mat(i,j) = 0
                else
                    mat(i,j) = 1
                end if
            end do
        end do
        
        do i=1,mpn
            bas(i) = i
        end do
        
        c = 1
        k = ko
        ! Treat picked up label until missing label is found
        mainloop:&
        do

!~             print*,'k=',k
!~             print*,'bas=',bas
!~             print*,'mat = '
!~             do i=1,mpn
!~                 print*,mat(i,:)
!~             end do
            


            ! Determines picked up label !
            l = mp1
            do while (mat(l,k) .le. eps)
                l = l + 1
            end do
            do i=(l+1),mpn
                if (mat(i,k) > eps) then
                    p = mat(i,k)*c(l) - mat(l,k)*c(i)
                    if (p > 0) then
                        if (p < eps) then
                            if (bas(i) < bas(l)) then
                                l=i
                            end if
                        else
                            l=i
                        end if
                    end if
                end if
            end do


            ! Pivoting arround l
            c(l) = c(l) / mat(l,k)
            p = mat(l,k)
            do i=1,k-1
                mat(l,i) = mat(l,i) / p
            end do
            mat(l,k) = 1
            do i=k+1,mpn
                mat(l,i) = mat(l,i) / p
            end do
            do i=mp1,l-1
                p = mat(i,k)
                do j=1,mpn
                    mat(i,j) = mat(i,j) - p * mat(l,j)
                end do 
                c(i) = c(i) - p*c(l)
            end do
            do i=l+1,mpn
                p = mat(i,k)
                do j=1,mpn
                    mat(i,j) = mat(i,j) - p * mat(l,j)
                end do
                c(i) = c(i) - p*c(l)                    
            end do

            i = bas(l)
            bas(l) = k  
            k = i
            if (k == ko) then
                exit mainloop
            end if         

            ! Determines picked up label
            l = 1
            do while (mat(l,k) .le. eps)
                l = l + 1
            end do
            do i=(l+1),m
                if (mat(i,k) > eps) then
                    p = mat(i,k)*c(l) - mat(l,k)*c(i)
                    if (p > 0) then
                        if (p < eps) then
                            if (bas(i) < bas(l)) then
                                l=i
                            end if
                        else
                            l=i
                        end if
                    end if
                end if
            end do
            
            ! Pivoting arround l
            c(l) = c(l) / mat(l,k)
            p = mat(l,k)
            do i=1,k-1
                mat(l,i) = mat(l,i) / p
            end do
            mat(l,k) = 1
            do i=k+1,mpn
                mat(l,i) = mat(l,i) / p
            end do
            do i=1,l-1
                p = mat(i,k)
                do j=1,mpn
                    mat(i,j) = mat(i,j) - p * mat(l,j)
                end do
                c(i) = c(i) - p*c(l)
            end do
            do i=l+1,m
                p = mat(i,k)
                do j=1,mpn
                    mat(i,j) = mat(i,j) - p * mat(l,j)
                end do
                c(i) = c(i) - p*c(l)
            end do


            ! k enters the basis, l leaves the basis
            i = bas(l)
            bas(l) = k  
            k = i
            if (k == ko) then
                exit mainloop
            end if         
        end do mainloop
        
        x = 0
        y = 0
        do i=mp1,mpn
            if (bas(i) < mp1) then
                x(bas(i)) = c(i)
            end if
        end do
        do i=1,m
            if (bas(i) > m) then
                y(bas(i)-m) = c(i)
            end if
        end do
        
        
        u = 0
        do i=1,m
            u = u + x(i)
        end do
        u = 1/u
        x = x*u

        v = 0
        do i=1,n
            v = v + y(i)
        end do
        v = 1/v
        y = y*v

        do i=1,m
            if (x(i) < eps) then
                x(i) = 0
            end if
        end do
        do i=1,n
            if (y(i) < eps) then
                y(i) = 0
            end if
        end do

        u = 0
        do i=1,m
            u = u + x(i)
        end do
        u = 1/u
        x = x*u

        v = 0
        do i=1,n
            v = v + y(i)
        end do
        v = 1/v
        y = y*v


        u=0
        do i=1,m
            do j=1,n
                u = u + x(i)*B(i,j)*y(j)
            end do
        end do
        
        v=0
        do i=1,m
            do j=1,n
                v = v + x(i)*A(i,j)*y(j)
            end do
        end do

    end subroutine find1nash_lemhow_real

! Finds a perfect subgame Nash equilibrium for the mercenaires game
    subroutine find1_perfect_subnash(moneymax,payofffun,behavstrat)
    
        integer                                                         , intent(in)    :: moneymax     ! Strategies are generated for all states of money < moneymax
        real(kind=real_kind)    , external                                              :: payofffun    ! payoff function for end states
        real(kind=real_kind)    , dimension(:,:,:,:)    , allocatable   , intent(out)   :: behavstrat   ! behavstrat(lmoney,state,rmoney,(payoff,probas))
        
        integer                                                 :: rmon,lmon,mposnow    ! Available money and position of mercenaries
        integer                                                 :: srmon,slmon          ! Spent money
        integer                                                 :: crmon,clmon,mpos     ! Leftover money and subsequent position of mercenaries
        integer                                                 :: i,j,k,l
        real(kind=real_kind)    , dimension(moneymax,moneymax)  :: lpayoff,rpayoff
        real(kind=real_kind)                                    :: p,q
        
        
        if (allocated(behavstrat)) then
            deallocate(behavstrat)
        end if
        
        allocate(behavstrat(1:moneymax,(-disttocen+1):(disttocen-1),1:moneymax,0:moneymax))
        
        do lmon=1,moneymax
            do rmon=1,lmon
                do mposnow=(1-disttocen),(disttocen-1)
                    ! (lmon,mposnow,rmon) is the current game state.
                    ! List available plays and perform payoff matrix assembly
                    do slmon=1,lmon
                        do srmon=1,rmon
                            clmon = lmon - slmon
                            crmon = rmon - srmon
                            if (slmon == srmon) then
                                mpos = mposnow
                            else if (slmon > srmon) then
                                mpos = mposnow + 1
                            else
                                mpos = mposnow - 1
                            end if
                            ! (clmon,mpos,crmon) is the game state if (slmon,srmon) is played
                            if ((clmon == 0) .or. (crmon == 0) .or. (abs(mpos) == disttocen) )then
                                p = payofffun(clmon,mpos ,crmon)
                                q = payofffun(crmon,-mpos,clmon)        ! The situation is reversed.
                            else
                                p = behavstrat(clmon,mpos ,crmon,0)
                                q = behavstrat(crmon,-mpos,clmon,0)     ! The situation is reversed.
                            end if
                            lpayoff(slmon,srmon) = p
                            rpayoff(slmon,srmon) = q
!~                             rpayoff(slmon,srmon) = -p
                        end do
                    end do
                    
!~                     print*,lmon,mposnow,rmon
!~                     print*,'B lpayoff = '
!~                     do i=1, lmon
!~                         print*,lpayoff(i,1:rmon)
!~                     end do
!~                     print*,'A rpayoff = '
!~                     do i=1, lmon
!~                         print*,rpayoff(i,1:rmon)
!~                     end do
                                    
                    call find1nash_lemhow_real(            &
                     rpayoff(1:lmon,1:rmon)                 &
                    ,lpayoff(1:lmon,1:rmon)                 &
                    ,lmon           ,rmon                   &
                    ,behavstrat(lmon,mposnow ,rmon,1:lmon)  &
                    ,behavstrat(lmon,mposnow ,rmon,0)       &
                    ,behavstrat(rmon,-mposnow,lmon,1:rmon)  &
                    ,behavstrat(rmon,-mposnow,lmon,0)       )
                    
!~                     print*,'stratl',behavstrat(lmon,mposnow ,rmon,0:lmon)
!~                     print*,'stratr',behavstrat(rmon,-mposnow,lmon,0:rmon)
!~                     read*,
                    
                end do
            end do
        end do
        
    
    end subroutine find1_perfect_subnash

! Play according to the first behavior distribution
    function behav_distrib1() result(pay)
    
        integer                                             :: pay
        integer , dimension(3)                              :: state
        
        call copy_current_board_left(state)     
        call play_distrib(behavstrat1(state(1),state(2),state(3),1:state(1))    &
                            ,state(1),pay)
    
    end function behav_distrib1
    
    function behav_distrib2() result(pay)
    
        integer                                             :: pay
        integer , dimension(3)                              :: state
        
        call copy_current_board_left(state)
        call play_distrib(behavstrat2(state(1),state(2),state(3),1:state(1))    &
                            ,state(1),pay)
    
    end function behav_distrib2

! Sums probabilities to buid cdf from pdf
    subroutine sumproba_behavestrat(moneymax,behavstrat)
    
        integer                 , intent(in)    :: moneymax
        real(kind=real_kind)            ,   &
        dimension(   1:moneymax         ,   &
        (-disttocen+1):(disttocen-1)    ,   &
        1:moneymax                      ,   &
        0:moneymax                         )&
                                , intent(inout) :: behavstrat
        
        integer                                 :: i,j,k,l
        
        do i=1,moneymax
            do j=1,moneymax
                do k=(1-disttocen),(disttocen-1)
                    do l=2,moneymax
                        behavstrat(i,k,j,l) = behavstrat(i,k,j,l) + behavstrat(i,k,j,l-1)
                    end do
                end do
            end do
        end do
    
    end subroutine sumproba_behavestrat

! Natural end payoff function
    function natendpayoff(lmon,mpos,rmon) result (p)
        
        integer , intent(in)    :: lmon     ! Left player money
        integer , intent(in)    :: mpos     ! Position of mercenaries as seen by left player
        integer , intent(in)    :: rmon     ! Right player money
        real(kind=real_kind)    :: p        ! Payoff
        
        if (mpos == disttocen) then
            p = 1
        else if (mpos == -disttocen) then
            p = -1
        else if (lmon == 0) then                                        ! Left player has no money left
            if ((rmon - (mpos + disttocen)) .ge. 0) then               ! Right payer can invade left player
                p = -1
            else
                p = 0
            end if
        else if (rmon == 0) then                                        ! Left player has no money left        
            if ((lmon + mpos - disttocen) .ge. 0) then                 ! Right payer can invade left player
                p = 1
            else
                p = 0
            end if
        else
            print*,'Game is not over yet, I cannot define an end payoff'        
            p = 0
        end if
        
    end function natendpayoff
    
! Augmented end payoff function
    function augendpayoff(lmon,mpos,rmon) result (p)
        
        integer , intent(in)    :: lmon     ! Left player money
        integer , intent(in)    :: mpos     ! Position of mercenaries as seen by left player
        integer , intent(in)    :: rmon     ! Right player money
        real(kind=real_kind)    :: p        ! Payoff
        
        integer                 :: monend
        
        if (mpos == disttocen) then
            p = 1 + real(lmon, kind = real_kind) / real(moneyinit, kind = real_kind)
        else if (mpos == -disttocen) then
            p = - ( 1 + real(rmon, kind = real_kind) / real(moneyinit, kind = real_kind))
        else if (lmon == 0) then                                        ! Left player has no money left
            monend = (rmon - (mpos + disttocen))
            if (monend .ge. 0) then                                     ! Right payer can invade left player
                p = - ( 1 + real(monend, kind = real_kind) / real(moneyinit, kind = real_kind))
            else
                p = 0
            end if
        else if (rmon == 0) then                                        ! Left player has no money left 
            monend = (lmon + mpos - disttocen)
            if (monend .ge. 0) then                                     ! Right payer can invade left player
                p = 1 + real(monend, kind = real_kind) / real(moneyinit, kind = real_kind)
            else
                p = 0
            end if
        else
            print*,'Game is not over yet, I cannot define an end payoff'        
            p = 0
        end if
        
    end function augendpayoff
    
end module strategies

