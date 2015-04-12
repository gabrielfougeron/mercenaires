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
m = 2
n = 2

allocate(A(m,n))
allocate(B(m,n))
allocate(x(m))
allocate(y(n))


A(1,1) = 0
A(2,1) = 0
A(1,2) = 1
A(2,2) = 0

B(1,1) = 0
B(2,1) = 0
B(1,2) = -1
B(2,2) = 0

!~ B = -A
!~ 
!~ B = A
!~ A = -B

print*,'A'
do i=1,m
    print*,A(i,:)
end do
print*,'B'
do i=1,m
    print*,B(i,:)
end do
call find1nash_lemhow_real2(A,B,m,n,x,u,y,v)

print*,' '
print*,'x=',x
print*,'u=',u
print*,'y=',y
print*,'v=',v


!~ 
call find1_perfect_subnash(moneyinit,natendpayoff,behavstrat1)

print*,behavstrat1(moneyinit,0,moneyinit,:)
