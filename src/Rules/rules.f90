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
    
    
    
    integer , dimension(3)              :: cgstate                      ! Current game state
                                                                        ! Game state format :   state(1) = Left player money
                                                                        !                       state(2) = Mercenaries position
                                                                        !                       state(3) = Right player money

contains

! Initializes board.
    subroutine init_board(state)
    
        integer , dimension(3)  , intent(out)   :: state
    
        state(1) = moneyinitl
        state(2) = initmpos
        state(3) = moneyinitr
    
    end subroutine init_board

! Reverses board view
    subroutine reverse_board(state)
    
        integer , dimension(3)  , intent(inout) ::  state
        integer                                 ::  tmp
        
        tmp = state(1)
        state(1) = state(3)
        state(3) = tmp
        state(2) = -state(2)

    end subroutine reverse_board

! Checks if somebody has won.
    subroutine checkwin(state,win)
    
        integer , dimension(3)  , intent(in)    :: state
        integer                 , intent(out)   :: win
        
        if (abs(state(2)) .ge. disttocen) then
            if (state(2) > 0) then 
                win = rightwon
            else
                win = leftwon
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

end module rules
