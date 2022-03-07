program Program
    implicit none
        integer i, j, m, N, n
        real, dimension(:,:), allocatable :: Points, Matrix, Table
        real, dimension(:), allocatable :: old_values
        real :: x, y, z, sum, a0, a1, a2
        open (unit = 1, file = "Input.txt", status = 'old' )
        read (*,*,end = 40) N
        read (*,*,end = 40) m
        allocate(Points(N, 2))
            do i = 1, N
                read (1,*,end = 40) (Points(i, j) ,  j = 1, 2)
            end do
        40 close (unit = 1)
        n = 3
        allocate(Matrix(n, (n + 1)))
        allocate(Table(10, (2 * n)))
        allocate(old_values(n))
        do i = 1, n
            Matrix(i, 1) = 1
            Matrix(i, 2) = Points(i, 1)
            Matrix(i, 3) = Points(i, 1) * Points(i, 1)
            Matrix(i, 4) = Points(i, 2)
        end do
        do i = 1, n
            old_values(i) = 0
        end do

        do i = 1, m
            do j = 1, n
                sum = 0.0
                do k = 1, n !for each variable
                    if ( j /= k ) then
                        sum = sum + old_values(k) * Matrix(j, k)
                    end if
                end do
                Table(i, (2 * j - 1)) = (Matrix(j, (n + 1)) - sum) / (Matrix(j, j))
                Table(i, (2 * j)) = (Table(i, (2 * j - 1)) - old_values(j)) * 100.0 / Table(i, (2 * j - 1))
                do k = 1, j
                    old_values(k) = Table(i, (2 * k - 1))
                end do
            end do
        end do

        open(unit = 2 , file="Output.txt")
        write (2, *) "a0 = ", Table(m, 1)
        write (2, *) "a1 = ", Table(m, 2)
        write (2, *) "a2 = ", Table(m, 3)
        close(unit = 2)
end program Program