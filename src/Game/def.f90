integer                 , parameter                         :: nproc = 2


integer                                                     :: winner
integer                                                     :: ngames
integer                 , dimension(3)                      :: winnertable
real(kind=real_kind)    , dimension(3)                      :: rwinnertable
real(kind=real_kind)    , dimension(:,:)    , allocatable   :: A,B
real(kind=real_kind)    , dimension(:)      , allocatable   :: x,y
integer                                                     :: m,n,i,j
integer                 , dimension(3)                      :: state
real(kind=real_kind)                                        :: u,v
