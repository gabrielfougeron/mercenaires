call init_random_seed()

!~ call play_game(itsatrap,unif_distrib,winner,verball)
!~ 
ngames = 10000000
!~ 
call play_many_games(itsatrap,unif_distrib,ngames,winnertable)
!~ 
rwinnertable = real(winnertable,real_kind)
rwinnertable = rwinnertable / ngames 

print *,rwinnertable*100
