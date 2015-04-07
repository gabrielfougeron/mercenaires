call init_random_seed()

!~ call play_game(human,unif_distrib,winner,verball)
!~ 
!~ ngames = 10**7
!~ 
!~ call play_many_games(minusone,plusone,ngames,winnertable)
!~ 
!~ rwinnertable = real(winnertable,real_kind)
!~ rwinnertable = rwinnertable / ngames 
!~ 
!~ print *,rwinnertable*100


m = 3
n = 2

allocate(A(m,n))
allocate(B(m,n))
allocate(x(m))
allocate(y(n))

A(1,1) = 3
A(2,1) = 2
A(3,1) = 0
A(1,2) = 3
A(2,2) = 5
A(3,2) = 6

B(1,1) = 3
B(2,1) = 2
B(3,1) = 3
B(1,2) = 2
B(2,2) = 6
B(3,2) = 1




call find1nash_lemhow_real(A,B,m,n,x,u,y,v)
