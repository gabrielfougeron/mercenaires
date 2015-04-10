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

!~ 
m = 3
n = 2
!~ 
!~ m = 3
!~ n = 3

!~ m=3
!~ n=4

!~ m=2
!~ n=2

allocate(A(m,n))
allocate(B(m,n))
allocate(x(m))
allocate(y(n))

!~ A(1,1) = 3
!~ A(2,1) = 2
!~ A(3,1) = 0
!~ A(1,2) = 3
!~ A(2,2) = 5
!~ A(3,2) = 6
!~ 
!~ B(1,1) = 3
!~ B(2,1) = 2
!~ B(3,1) = 3
!~ B(1,2) = 2
!~ B(2,2) = 6
!~ B(3,2) = 1

!~ A(1,1) = 3
!~ A(2,1) = 4
!~ A(3,1) = 0
!~ A(1,2) = 3
!~ A(2,2) = 0
!~ A(3,2) = 4
!~ A(1,3) = 0
!~ A(2,3) = 1
!~ A(3,3) = 5
!~ 
!~ B = transpose(A)

!~ A(1,1) = 0
!~ A(2,2) = 0
!~ A(3,3) = 0
!~ A(1,2) = 1
!~ A(1,3) = - 1
!~ A(2,1) = - 1
!~ A(2,3) = 1
!~ A(3,1) = 1
!~ A(3,2) = -1
!~ 
!~ B = -A

!~ A(1,1) = 4
!~ A(2,1) = 16
!~ A(3,1) = 10
!~ A(1,2) = 12
!~ A(2,2) = 8
!~ A(3,2) = 8
!~ A(1,3) = 8
!~ A(2,3) = 12
!~ A(3,3) = 10
!~ A(1,4) = 6
!~ A(2,4) = 8
!~ A(3,4) = 9
!~ 
!~ B(1,1) = 25
!~ B(2,1) = 1
!~ B(3,1) = 17
!~ B(1,2) = 5
!~ B(2,2) = 15
!~ B(3,2) = 10
!~ B(1,3) = 5
!~ B(2,3) = 8
!~ B(3,3) = 10
!~ B(1,4) = 8
!~ B(2,4) = 4
!~ B(3,4) = 9

!~ A(1,1) = 1
!~ A(2,1) = -1
!~ A(1,2) = -1
!~ A(2,2) = 1
!~ 
!~ B = -transpose(A)


!~ A(1,1) = 0
!~ A(2,1) = 2
!~ A(3,1) = 3
!~ A(1,2) = 6
!~ A(2,2) = 5
!~ A(3,2) = 3
!~ 
!~ B(1,1) = 1
!~ B(2,1) = 0
!~ B(3,1) = 4
!~ B(1,2) = 0
!~ B(2,2) = 2
!~ B(3,2) = 3

!~ A(1,1) = 3
!~ A(2,1) = 2
!~ A(3,1) = 0
!~ A(1,2) = 3
!~ A(2,2) = 5
!~ A(3,2) = 6
!~ 
!~ B(1,1) = 3
!~ B(2,1) = 2
!~ B(3,1) = 3
!~ B(1,2) = 3
!~ B(2,2) = 6
!~ B(3,2) = 1

A(1,1) = 1
A(2,1) = 1
A(3,1) = 1
A(1,2) = 1
A(2,2) = 1
A(3,2) = 1

B(1,1) = 1
B(2,1) = 1
B(3,1) = 1
B(1,2) = 1
B(2,2) = 1
B(3,2) = 1


call find1nash_lemhow_real(A,B,m,n,x,u,y,v)

print*,' '
print*,'x=',x
print*,'u=',u
print*,'y=',y
print*,'v=',v



call find1_perfect_subnash(moneyinit,natendpayoff,behavstrat1)
