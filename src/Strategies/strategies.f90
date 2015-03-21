module strategies
    
    use rules
    
contains

! Stupid strategy always pays 1
    function stupid() result(pay)
    
        integer :: pay
        
        pay = 1
        
    end function stupid



end module strategies
