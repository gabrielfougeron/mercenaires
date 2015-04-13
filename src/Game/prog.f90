call init_random_seed()

call find1_perfect_subnash(moneyinit,natendpayoff,behavstrat1)
call find1_perfect_subnash(moneyinit,augendpayoff,behavstrat2)

!~ state(1) = 2
!~ state(2) = 2
!~ state(3) = 7
!~ 
!~ print*, behavstrat1(state(1),state(2),state(3),0)
!~ print*, 100*behavstrat1(state(1),state(2),state(3),1:state(1))
!~ 
!~ print*,'  '
!~ 
!~ print*, behavstrat1(state(3),-state(2),state(1),0)
!~ print*, 100*behavstrat1(state(3),-state(2),state(1),1:state(3))
!~ 
!~ read*,

call sumproba_behavestrat(moneyinit,behavstrat1)
call sumproba_behavestrat(moneyinit,behavstrat2)


call play_game(human,behav_distrib1,winner,verball)

!~ ngames = 10**6

!~ call play_many_games(behav_distrib1,human,ngames,winnertable)

!~ rwinnertable = real(winnertable,real_kind)
!~ rwinnertable = rwinnertable / ngames 
!~ 
!~ print *,rwinnertable*100


