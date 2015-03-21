program game

    !$ use omp_lib
    use rules
    use strategies

    include 'def.f90'
    
    include 'prog.f90'
    
    contains
    
    ! Repeats a game with the same opponents multiple times
    subroutine play_many_games(lstrategy,rstrategy,ngames,winnertable)
    
        integer , external                      :: lstrategy            ! Left player strategy
        integer , external                      :: rstrategy            ! Right player strategy
        integer                 , intent(in)    :: ngames               ! Number of games played
        integer , dimension(3)  , intent(out)   :: winnertable          ! Remembers wins
        
        integer                                 :: winner
        integer                                 :: i
        
        winnertable = 0
        
        do i=1,ngames
        
            call play_game(lstrategy,rstrategy,winner)
        
            select case(winner)
            case(leftwon)
                winnertable(1) = winnertable(1) + 1
            case(rightwon)
                winnertable(3) = winnertable(3) + 1
            case(gametied)
                winnertable(2) = winnertable(2) + 1
            end select    
            
        end do
    
    end subroutine play_many_games
    

end program game
