program Program
    implicit none
        integer i, j, k, n
        real coeff, sum
        real, dimension(:,:), allocatable :: Matrix, L, U
        real, dimension(:), allocatable :: C, X, Z
        999 write (*,*) "Enter number of variables."
        read (*,*) n
        if ( n < 1 ) then
            write (*,*) "Invalid number of variables."
            goto 999
        end if
        allocate(Matrix(n, n))
        allocate(L(n, n))
        allocate(U(n, n))
        allocate(C(n))
        allocate(X(n))
        allocate(Z(n))
        open (unit = 1, file = "Input.txt", status = 'old' )
        do i = 1, n
            read (1,*,end = 10) (Matrix(i, j),  j = 1, n), C(i)
        end do
        10 close (unit = 1) 
        open(unit = 3 , file="Steps.txt")
        do i = 1, n
            do j = 1, n
                L(i, j) = Matrix(i, j)
                U(i, j) = Matrix(i, j)
            end do
        end do   
        write (3, *) "CALCULATING UPPER MATRIX (U):"
        write (3, *) ""
        do i = 1, n
            do j = 1, n
                do k = 1, n
                    write (3, '(f9.3)', advance = 'no') U(j, k)
                end do
                write (3, *) ""
            end do   
            write (3, *) ""     

            do j = (i + 1), n
                coeff = (U(j, i)) / (U(i, i))
                do k = i, n
                    U(j, k) = U(j, k) -  coeff * (U(i, k)) 
                end do
                if ( coeff > 0.0 ) then
                    write (3, '(a1, i1, a9, i1, a4, f10.5, a5, i1)') "R", j, " -----> R", j, " - (", coeff, ") x R", i
                else
                    write (3, '(a1, i1, a9, i1, a4, f10.5, a5, i1)') "R", j, " -----> R", j, " + (", abs(coeff), ") x R", i
                end if
                write (3, *) ""        
            end do
        end do
        write (3, *) "Final U Matrix is:"
        write (3, *) ""
        do j = 1, n
            do k = 1, n
                write (3, '(f9.3)', advance = 'no') U(j, k)
            end do
            write (3, *) ""
        end do   
        write (3, *) ""     
        write (3, *) "CALCULATING LOWER MATRIX (L):"
        write (3, *) ""
        do i = 1, n
            if ( i < n ) then
                do j = 1, n
                    do k = 1, n
                        write (3, '(f9.3)', advance = 'no') Matrix(j, k)
                    end do
                    write (3, *) ""
                end do   
                write (3, *) ""
                write (3, '(a23, i1, a1)') "For elements in column ", i, ":"
                write (3, *) ""
            end if
            do j = (i + 1), n
                L(j, i) = Matrix(j, i) / Matrix(i, i)
                write (3, 5) "L (",j ,", ", i, ") = (", Matrix(j, i),") / (", Matrix(i, i),") = ", L(j, i)
                write (3, *) ""
                L(i, j) = 0.0
                5 format (a3, i1, a2, i1, a5, f10.5, a5, f10.5, a5, f10.5, a5,f10.5)
            end do
            do j = (i + 1), n
                coeff = (Matrix(j, i)) / (Matrix(i, i))
                do k = i, (n + 1)
                    Matrix(j, k) = Matrix(j, k) -  coeff * (Matrix(i, k)) 
                end do
                if ( i < (n - 1) ) then
                    if ( coeff > 0.0 ) then
                        write (3, '(a1, i1, a9, i1, a4, f10.5, a5, i1)') "R", j, " -----> R", j, " - (", coeff, ") x R", i
                    else
                        write (3, '(a1, i1, a9, i1, a4, f10.5, a5, i1)') "R", j, " -----> R", j, " + (", abs(coeff), ") x R", i
                    end if
                    write (3, *) ""            
                end if
            end do
            L(i, i) = 1.0
        end do
        write (3, *) "Final L Matrix is:"
        write (3, *) ""
        do j = 1, n
            do k = 1, n
                write (3, '(f9.3)', advance = 'no') L(j, k)
            end do
            write (3, *) ""
        end do   
        write (3, *) ""     
        do i = 1, n
            sum = 0.0
            do j = (i - 1), 1, (-1)
                sum = sum + Z(j) * L(i, j)
            end do
            Z(i) = (C(i) - sum)
        end do
        do i = n, 1, (-1)
            sum = 0.0
            do j = (i + 1), n
                sum = sum + X(j) * U(i, j)
            end do
            X(i) = (Z(i) - sum) / U(i, i)
        end do
        open(unit = 2 , file="Output.txt")
        write (2, *) " L = "
        do i = 1, n
            do j = 1, n
                if ( j == n ) then
                    write (2, 4, advance = 'yes') L(i, j) !Printing in next line
                else
                    write (2, 4, advance = 'no') L(i, j) !Printing in same line
                end if
            end do
        end do
        write (2, *) ""
        write (2, *) " U = "
        do i = 1, n
            do j = 1, n
                if ( j == n ) then
                    write (2, 4, advance = 'yes') U(i, j) !Printing in next line
                else
                    write (2, 4, advance = 'no') U(i, j) !Printing in same line
                end if
            end do
        end do
        4 format (f10.2)
        write (2, *) ""
        write (2, '(a)', advance = 'no') " Z =   ["
        do i = 1, n
            write (2, '(f9.3)', advance = 'no') Z(i)
            write (*, '(f9.3)', advance = 'no') Z(i)
        end do
        write (2, *) "]"
        write (2, *) ""
        write (*, *) ""
        write (2, '(a)', advance = 'no') " X =   ["
        do i = 1, n
            write (2, '(f9.3)', advance = 'no') X(i)
            write (*, '(f9.3)', advance = 'no') X(i)
        end do
        write (2, *) "]"
close(unit = 2)
end program Program