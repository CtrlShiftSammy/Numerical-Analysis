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
        open(unit = 2 , file="Steps.txt")
        F = 0.0
        write(2, *) "f(", x, ")"
        write(2, *) ""
        write(2, 12, advance = 'no') "= "
        do i = 1, n
            term = Points(i, 2)
            do j = 1, n
                if ( j /= i ) then
                    write(2, 13, advance = 'no') "{ (", x, ") - (", Points(j, 1), ") } "
                    if ( i /= n ) then
                        if ( j == n ) then
                            write(2, 12, advance = 'no') " / "
                        else
                            write(2, 12, advance = 'no') " X "
                        end if
                    else
                        if ( j == n - 1 ) then
                            write(2, 12, advance = 'no') " / "
                        else
                            write(2, 12, advance = 'no') " X "
                        end if    
                    end if
                end if
            end do
            do j = 1, n
                if ( j /= i ) then
                    write(2, 13, advance = 'no') "{ (", Points(i, 1), ") - (", Points(j, 1), ") } "
                    if ( i /= n ) then
                        if ( j == n ) then
                            write(2, 12, advance = 'no') " + "
                        else
                            write(2, 12, advance = 'no') " X "
                        end if
                    else
                        if ( j == n - 1 ) then
                            write(2, 12, advance = 'yes') ""
                        else
                            write(2, 12, advance = 'no') " X "
                        end if    
                    end if
                end if
            end do
            F = F + term
        end do
        F = 0.0
        write(2, *) ""
        write(2, 12, advance = 'no') "= "
        do i = 1, n
            term = Points(i, 2)
            do j = 1, n
                if ( j /= i ) then
                    write(2, 11, advance = 'no') "(", (x - Points(j, 1)), ") "
                    if ( i /= n ) then
                        if ( j == n ) then
                            write(2, 12, advance = 'no') " / "
                        else
                            write(2, 12, advance = 'no') " X "
                        end if
                    else
                        if ( j == n - 1 ) then
                            write(2, 12, advance = 'no') " / "
                        else
                            write(2, 12, advance = 'no') " X "
                        end if    
                    end if
                end if
            end do
            do j = 1, n
                if ( j /= i ) then
                    write(2, 11, advance = 'no') "(", (Points(i, 1) - Points(j, 1)), ") "
                    if ( i /= n ) then
                        if ( j == n ) then
                            write(2, 12, advance = 'no') " + "
                        else
                            write(2, 12, advance = 'no') " X "
                        end if
                    else
                        if ( j == n - 1 ) then
                            write(2, 12, advance = 'yes') ""
                        else
                            write(2, 12, advance = 'no') " X "
                        end if    
                    end if
                end if
            end do
            F = F + term
        end do
        F = 0.0
        write(2, *) ""
        write(2, 12, advance = 'no') "= "
        do i = 1, n
            term = Points(i, 2)
            do j = 1, n
                if ( j /= i ) then
                    term = term * (x - Points(j, 1))
                    term = term / (Points(i, 1) - Points(j, 1))
                end if
            end do
            F = F + term
            if ( i == n ) then
                write(2, 11, advance = 'yes') " ( ", term ," ) "
            else
                write(2, 11, advance = 'no') " ( ", term ," ) + "
            end if
        end do
        F = 0.0
        write(2, *) ""
        write(2, 12, advance = 'no') "= "
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
        write(2, 11) " ( ", F ," ) "
        11 format (a, f10.5, a)
        12 format (a)
        13 format (a, f10.5, a, f10.5, a)
        close ( unit = 2)
end program Program