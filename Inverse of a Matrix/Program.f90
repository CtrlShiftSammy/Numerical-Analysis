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
        do i = 1, n
            do j = 1, n
                L(i, j) = Matrix(i, j)
                U(i, j) = Matrix(i, j)
            end do
        end do   
        do i = 1, n
            do j = (i + 1), n
                coeff = (U(j, i)) / (U(i, i))
                do k = i, n
                    U(j, k) = U(j, k) -  coeff * (U(i, k)) 
                end do
            end do
        end do
        do i = 1, n
            do j = (i + 1), n
                L(j, i) = Matrix(j, i) / Matrix(i, i)
                L(i, j) = 0.0
            end do
            do j = (i + 1), n
                coeff = (Matrix(j, i)) / (Matrix(i, i))
                do k = i, (n + 1)
                    Matrix(j, k) = Matrix(j, k) -  coeff * (Matrix(i, k)) 
                end do
            end do
            L(i, i) = 1.0
        end do
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
        do i = 1, n
            write (2, '(f9.3)', advance = 'no') Z(i)
            write (*, '(f9.3)', advance = 'no') Z(i)
        end do
        write (2, *) ""
        write (*, *) ""
        do i = 1, n
            write (2, '(f9.3)', advance = 'no') X(i)
            write (*, '(f9.3)', advance = 'no') X(i)
        end do
close(unit = 2)
end program Program