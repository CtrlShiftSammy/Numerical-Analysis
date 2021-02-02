program Program
    implicit none
        integer i, j, n
        real x, F, term
        real, dimension(:,:), allocatable :: Points
        F = 0.0
        write (*, *) "Enter number of points."
        read (*,*) n
        write (*, *) "Enter value of x."
        read (*,*) x
        allocate(Points(n, 2))
        open (unit = 1, file = "Input.txt", status = 'old' )
            do i = 1, n
                read (1,*,end = 10) (Points(i, j) ,  j = 1, 2)
            end do
        10 close (unit = 1)
        open(unit = 2 , file="Steps.txt")
        do i = 1, n
            term = Points(i, 2)
            do j = 1, n
                if ( j /= i ) then
                    term = term * (x - Points(j, 1))
                    term = term / (Points(i, 1) - Points(j, 1))
                end if
            end do
            F = F + term
        end do
        write (*, *) "The value of f(", x, ") is ", F
end program Program