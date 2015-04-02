call init_random_seed()

!~ call play_game(human,unif_distrib,winner,verball)
!~ 
ngames = 10**7

call play_many_games(minusone,plusone,ngames,winnertable)

rwinnertable = real(winnertable,real_kind)
rwinnertable = rwinnertable / ngames 

print *,rwinnertable*100
