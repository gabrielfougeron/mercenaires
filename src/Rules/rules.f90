module rules

    integer                 , parameter :: real_kind = 8                ! Precision du calcul
    integer                 , parameter :: moneyinit = 50               ! Initial money
    integer                 , parameter :: moneyinitl = moneyinit       ! Initial money for left player
    integer                 , parameter :: moneyinitr = moneyinit       ! Initial money for right player
    integer                 , parameter :: disttocen = 3                ! Distance to center <=> number of displacement from center to invasion
    integer                 , parameter :: initmpos = 0                 ! Initial position of mercenaries
    
    integer                 , parameter :: leftwon = 1                  ! Game is over, left won
    integer                 , parameter :: rightwon = 2                 ! Game is over, right won
    integer                 , parameter :: gametied = 3                 ! Game is over. Nobody won.
    integer                 , parameter :: gamenotover = 4              ! Game is not over yet.
    
    integer                 , parameter :: moveislegal = 0              ! The move is legal
    integer                 , parameter :: leftcheat = 1                ! Left player is trying to cheat.
    integer                 , parameter :: rightcheat = 2               ! Right player is trying to cheat.
    integer                 , parameter :: bothcheat = 3                ! Both players are trying to cheat.
    
    integer                 , parameter :: verbstfu = 0                 ! verbosity shut the fuck up
    integer                 , parameter :: verbwinner = 1               ! verbosity print only winner
    integer                 , parameter :: verbeachstate = 2            ! verbosity print each move
    integer                 , parameter :: verball = 3                  ! verbosity print ALL
    
    integer                 , parameter :: leftisplaying = 1            ! Left player is deciding
    integer                 , parameter :: rightisplaying = 2           ! Right player is deciding
    integer                             :: isplaying                    ! Who is playing ?
    
    integer , dimension(3)              :: cgstate                      ! Current game state
                                                                        ! Game state format :   state(1) = Left player money
                                                                        !                       state(2) = Mercenaries position
                                                                        !                       state(3) = Right player money

    private                             :: cgstate
    private                             :: init_board
    private                             :: exec_board_move

    
contains

! Initializes board.
    subroutine init_board(state)
    
        integer , dimension(3)  , intent(out)   :: state
    
        state(1) = moneyinitl
        state(2) = initmpos
        state(3) = moneyinitr
    
    end subroutine init_board

! Copies board
    subroutine copy_board(state,cstate)
    
        integer , dimension(3)  , intent(in)    ::  state
        integer , dimension(3)  , intent(out)   ::  cstate
    
        cstate = state
    
    end subroutine copy_board

! Copies and reverses board view
    subroutine copy_reverse_board(state,rstate)
    
        integer , dimension(3)  , intent(in)    ::  state
        integer , dimension(3)  , intent(out)   ::  rstate
        
        rstate(1) = state(3)
        rstate(2) = - state(2)
        rstate(3) = state(1)

    end subroutine copy_reverse_board

! Copies current board so that the player who is playing is on the left
    subroutine copy_current_board_left(state)

        integer , dimension(3)  , intent(out)   :: state
        
        if (isplaying == leftisplaying) then
            call copy_board(cgstate,state)
        else 
            call copy_reverse_board(cgstate,state)
        end if

end subroutine copy_current_board_left

! Checks if somebody has won.
    subroutine checkwin(state,win)
    
        integer , dimension(3)  , intent(in)    :: state
        integer                 , intent(out)   :: win
        
        if (abs(state(2)) .ge. disttocen) then
            if (state(2) > 0) then 
                win = leftwon
            else
                win = rightwon
            end if
        else if (state(1) .le. 0) then                                  ! Left player has no money left
            if ((state(3) - (state(2) + disttocen)) .ge. 0) then        ! Right player can invade left player
                win = rightwon
            else
                win = gametied
            end if
        else if (state(3) .le. 0) then                                  ! Right player has no money left
            if ((state(1) + state(2) - disttocen) .ge. 0) then          ! Left player can invade right player
                win = leftwon
            else
                win = gametied
            end if
        else                                                            ! Game goes on bitch !
            win = gamenotover
        end if
    
    end subroutine checkwin

! Checks if move is legal.
    subroutine checklegal(state,money,legal)

        integer , dimension(3)  , intent(in)    :: state
        integer , dimension(2)  , intent(in)    :: money                ! Left money, Right money
        integer                 , intent(out)   :: legal
        
        if ((money(1) > state(1)) .or. (money(1) < 1)) then             ! Left player cannot pay this
            if ((money(2) > state(3)) .or. (money(2) < 1)) then         ! Right player cannot pay this
                legal = bothcheat
            else
                legal = leftcheat
            end if 
        else if ((money(2) > state(3)) .or. (money(2) < 1)) then        ! Right player cannot pay this
            legal = rightcheat
        else
            legal = moveislegal
        end if

    end subroutine checklegal

! Executes the move.
    subroutine exec_board_move(state,money)

        integer , dimension(3)  , intent(inout)    :: state
        integer , dimension(2)  , intent(inout)    :: money             ! Left money, Right money
        
        state(1) = state(1) - money(1)
        state(3) = state(3) - money(2)
        if (money(1) > money(2)) then
            state(2) = state(2) + 1
        else if (money(1) < money(2)) then
            state(2) = state(2) - 1        
        end if
        
    end subroutine exec_board_move

! Print current game state
    subroutine print_cgstate()
    
        character(len=11+2*disttocen)   :: message
        integer                         :: i
        
        write(message(1:6),'(I4,A)') cgstate(1),' |'
        
        do i = (1 - disttocen),(cgstate(2) - 1)
            write(message(6+disttocen+i:7+disttocen+i),'(A)') 'o'
        end do
        
        write(message(6+disttocen+cgstate(2):7+disttocen+cgstate(2)),'(A)') 'M'
        
        do i = (cgstate(2) + 1),(disttocen-1)
            write(message(6+disttocen+i:7+disttocen+i),'(A)') 'o'
        end do
        
        write(message((6+2*disttocen):(11+2*disttocen)),'(A,I4)') '| ',cgstate(3)
        
        print*, message
        
    end subroutine print_cgstate

! Play game
    subroutine play_game(lstrategy,rstrategy,winner,verblvlin)
    
        integer , external                  :: lstrategy                ! Left player strategy
        integer , external                  :: rstrategy                ! Right player strategy
        integer , intent(out)               :: winner                   ! Who wins.
        integer , intent(in)    , optional  :: verblvlin                ! Input verbosity level.
        
        integer                             :: verblvl                  ! Effective verbosity level.
        integer , dimension(2)              :: money                    ! Collect money
        integer                             :: legal                    ! Is move legal ?
        
        if (present(verblvlin)) then                                    ! Default verbose is shut the fuck up
            verblvl = verblvlin
        else
            verblvl = verbstfu
        end if
        
        print*,cgstate
        
        call init_board(cgstate)

        print*,cgstate
        
        call checkwin(cgstate,winner)
        if (winner .ne. gamenotover) then
            print*, 'Wrong board initialisation'
            return
        end if
    
        if (verblvl > verbstfu) then
            print*, 'New game initiated'
        end if

        do while (winner == gamenotover)
            
            if (verblvl .ge. verbeachstate) then
                call print_cgstate()
            end if  
            
            isplaying = leftisplaying
            money(1) = lstrategy()

            isplaying = rightisplaying
            money(2) = rstrategy()
            
            call checklegal(cgstate,money,legal)
            
            if (legal .ne. moveislegal) then
                select case (legal)
                case(leftcheat)
                    if (verblvl .ge. verbwinner) then
                        print*,'Left player cheated'
                    end if
                    winner = rightwon
                case(rightcheat)
                    if (verblvl .ge. verbwinner) then
                        print*,'Right player cheated'
                    end if
                    winner = leftwon
                case default
                    winner = gametied
                end select
            else
                
                call exec_board_move(cgstate,money)
                
                call checkwin(cgstate,winner)
                
            end if
            
        end do
        
        if (verblvl .ge. verbwinner) then
            if (verblvl .ge. verbeachstate) then
                call print_cgstate()
            end if  
            select case(winner)
            case(leftwon)
                print*, 'Left player won'
            case(rightwon)
                print*, 'Right player won'
            case(gametied)
                print*, 'Game is over, nobody won'
            end select
        end if
        
        
    end subroutine play_game

! Subroutine to generate random seed based on clock timing
    subroutine init_random_seed()

        integer :: ii, nn, clock
        integer, dimension(:), allocatable :: seed
        
        call random_seed(size = nn)
        allocate(seed(nn))
          
        call system_clock(count=clock)
          
        seed = clock + 37 * (/ (ii - 1, ii = 1, nn) /)
        call random_seed(put = seed)
          
        deallocate(seed)
    end subroutine init_random_seed
    
end module rules
