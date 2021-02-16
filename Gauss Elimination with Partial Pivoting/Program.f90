program Program
    implicit none
        integer i, j, k, n, p
        real sum, coeff, max
        real, dimension(:,:), allocatable :: Matrix
        real, dimension(:), allocatable :: Solution, Pivot
        999 write (*,*) "Enter number of variables."
        read (*,*) n
        if ( n < 1 ) then
            write (*,*) "Invalid number of variables."
            goto 999
        end if
        allocate(Matrix(n, (n + 1)))
        allocate(Solution(n))
        allocate(Pivot(n + 1))
        open (unit = 1, file = "Input.txt", status = 'old' )
        do i = 1, n
            read (1,*,end = 10) (Matrix(i, j),  j = 1, (n + 1))
        end do
        10 close (unit = 1)    
        do i = 1, n
            max = abs(Matrix(i, i))
            p = i
            if ( i >= 1 ) then
                do j = i + 1, n
                    if ( Matrix(j, i) > max ) then
                        p = j
                        max = abs(Matrix(j, i))
                    end if
                end do
                do j = 1, (n + 1)
                    Pivot(j) = Matrix(p, j)
                    Matrix(p, j) = Matrix(i, j)
                    Matrix(i, j) = Pivot(j)
                end do    
            end if
            do j = (i + 1), n
                coeff = (Matrix(j, i)) / (Matrix(i, i))
                do k = i, (n + 1)
                    Matrix(j, k) = Matrix(j, k) -  coeff * (Matrix(i, k)) 
                end do
            end do
        end do
        open(unit = 2 , file="Output.txt")
            do i = n, 1, (-1)
                sum = 0.0
                do j = (i + 1), n
                    sum = sum + Solution(j) * Matrix(i, j)
                end do
                Solution(i) = (Matrix(i, (n + 1)) - sum) / Matrix(i, i)
            end do
            write (2, '(a)', advance = 'no') " Solution =   ["
            do i = 1, n
                write (2, '(f9.3)', advance = 'no') Solution(i)
                write (*, '(f9.3)', advance = 'no') Solution(i)
            end do
            write (2, *) "]"
        close(unit = 2)
end program Program