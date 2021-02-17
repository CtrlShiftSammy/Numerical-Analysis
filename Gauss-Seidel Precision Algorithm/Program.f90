program Program
    implicit none
        integer i, j, k, l, m, n
        real minimum_error, sum
        real, dimension(:,:), allocatable :: Matrix, Table
        real, dimension(:), allocatable :: old_values
        999 write (*,*) "Enter number of variables."
        read (*,*) n
        if ( n < 1 ) then
            write (*,*) "Invalid number of variables."
            goto 999
        end if
        allocate(Matrix(n, (n + 1)))
        allocate(Table(10, (2 * n)))
        allocate(old_values(n))
        open (unit = 1, file = "Input.txt", status = 'old' )
        do i = 1, n
            read (1,*,end = 10) (Matrix(i, j),  j = 1, (n + 1))
        end do
        10 close (unit = 1) 
        open (unit = 2, file = "Guess.txt", status = 'old' )
        do i = 1, n
            read (2,*,end = 20) old_values(i)
        end do
        20 close (unit = 1) 
        open(unit = 3 , file="Output.txt")
        do i = 1, 10 !for steps, i.e. ith iteration
            do j = 1, n !for variables in each step, i.e. jth variable
                sum = 0.0
                do k = 1, n !for each variable
                    if ( j /= k ) then
                        sum = sum + old_values(k) * Matrix(j, k)
                        write(*,*) old_values(k), "X", Matrix(j, k)
                    end if
                end do
                write(*,*) sum
                Table(i, (2 * j - 1)) = (Matrix(j, (n + 1)) - sum) / (Matrix(j, j))
                Table(i, (2 * j)) = (Table(i, (2 * j - 1)) - old_values(j)) * 100.0 / Table(i, (2 * j - 1))
                do l = 1, 10
                    do m = 1, (2 * n)
                        if ( m == (2 * n) ) then
                            write (*, 4, advance = 'yes') Table(l, m) !Printing in next line
                        else
                            write (*, 4, advance = 'no') Table(l, m) !Printing in same line
                        end if
                    end do
                end do
                write(*,*)
                do k = 1, j
                    old_values(k) = Table(i, (2 * k - 1))
                end do
            end do
        end do

        do i = 1, 10
            do j = 1, (2 * n)
                if ( j == (2 * n) ) then
                    write (3, 4, advance = 'yes') Table(i, j) !Printing in next line
                else
                    write (3, 4, advance = 'no') Table(i, j) !Printing in same line
                end if
                4 format (f15.2)
            end do
        end do
        close(unit = 3)

end program Program