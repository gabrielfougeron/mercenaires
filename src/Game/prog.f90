call init_random_seed()

call find1_perfect_subnash(moneyinit,natendpayoff,behavstrat1)
call find1_perfect_subnash(moneyinit,augendpayoff,behavstrat2)

call sumproba_behavestrat(moneyinit,behavstrat1)
call sumproba_behavestrat(moneyinit,behavstrat2)

call play_game(human,behav_distrib1,winner,verball)

!~ ngames = 10**7

!~ call play_many_games(behav_distrib1,behav_distrib2,ngames,winnertable)

!~ rwinnertable = real(winnertable,real_kind)
!~ rwinnertable = rwinnertable / ngames 

!~ print *,rwinnertable*100


