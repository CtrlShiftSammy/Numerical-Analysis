program Program
    implicit none
        integer i, j, m
        real, dimension(:,:), allocatable :: Points
        real :: choice, slope, SumX, SumY, SumZ, SumXX, SumXY, SumXXX, SumXXXX, SumXXY, SumYY, SumZX, SumYZ
        real ::  e, S, Sy, CC, a, b, c, d, Det, l, m1, n, k, p, q, r, s1, x, y, z 
        999 write (*, *) "Make your choice."
        write (*, *) "1: y = m x + c"
        write (*, *) "2: x yᵃ = b"
        write (*, *) "3: y = a eᵇˣ"
        write (*, *) "4: y = a x² + b x + c"
        write (*, *) "5: z = a + b x + c y"
        read (*,*) choice
        if ( choice == 1 ) then
            write (*, *) "Enter number of points."
            read (*,*,end = 10) m
            allocate(Points(m, 2))
            open (unit = 1, file = "Input.txt", status = 'old' )
                do i = 1, m
                    read (1,*,end = 10) (Points(i, j) ,  j = 1, 2)
                end do
            10 close (unit = 1)
            do i = 1, m
                SumX = SumX + Points(i, 1)
                SumY = SumY + Points(i, 2)
                SumXY = SumXY + Points(i, 1) * Points(i, 2)
                SumXX = SumXX + Points(i, 1) ** 2
            end do
            slope = ((real(m) * SumXY) - (SumX * SumY)) / ((real(m) * SumXX) - (SumX**2))
            c = (SumY / real(m)) - (slope * SumX / real(m))
            do i = 1, m
                Sy = Sy + ( Points(i, 2) - (SumY / real(m))) ** 2
                e = Points(i, 2) - (slope * Points(i, 1) - c)
                S = S + e ** 2
            end do
            CC = sqrt(( Sy - S)/ Sy)
            write (*, *) "y = ", slope, " x + ", c
            open(unit = 2 , file="Output.txt")
            if ( c >= 0.0 ) then
                write (2, *) "y = ", slope, " x + ", c
                write (2, *) "Correlation Coefficient =", CC
            else
                write (2, *) "y = ", slope, " x - ", abs(c)
                write (2, *) "Correlation Coefficient =", CC
            end if
            close(unit = 2)
        else if ( choice == 2 ) then
            write (*, *) "Enter number of points."
            read (*,*,end = 20) m
            allocate(Points(m, 2))
            open (unit = 1, file = "Input.txt", status = 'old' )
                do i = 1, m
                    read (1,*,end = 20) (Points(i, j) ,  j = 1, 2)
                end do
            20 close (unit = 1)
            do i = 1, m
                Points(i, 1) = log10(Points(i, 1))
                Points(i, 2) = log10(Points(i, 2))
            end do
            do i = 1, m
                SumX = SumX + Points(i, 1)
                SumY = SumY + Points(i, 2)
                SumXY = SumXY + Points(i, 1) * Points(i, 2)
                SumXX = SumXX + Points(i, 1) ** 2
            end do
            slope = ((real(m) * SumXY) - (SumX * SumY)) / ((real(m) * SumXX) - (SumX**2))
            c = (SumY / real(m)) - (slope * SumX / real(m))
            a = (-1.0) / slope
            b = 10.0 ** (c * a)
            write (*, *) " x (y ^ (", a, ")) = ", b
            open(unit = 2 , file="Output.txt")
            write (2, *) " x (y ^ (", a, ")) = ", b
            close(unit = 2)
        else if ( choice == 3 ) then
            write (*, *) "Enter number of points."
            read (*,*,end = 30) m
            allocate(Points(m, 2))
            open (unit = 1, file = "Input.txt", status = 'old' )
                do i = 1, m
                    read (1,*,end = 30) (Points(i, j) ,  j = 1, 2)
                end do
            30 close (unit = 1)
            do i = 1, m
                Points(i, 2) = log(Points(i, 2))
            end do
            do i = 1, m
                SumX = SumX + Points(i, 1)
                SumY = SumY + Points(i, 2)
                SumXY = SumXY + Points(i, 1) * Points(i, 2)
                SumXX = SumXX + Points(i, 1) ** 2
            end do
            slope = ((real(m) * SumXY) - (SumX * SumY)) / ((real(m) * SumXX) - (SumX**2))
            c = (SumY / real(m)) - (slope * SumX / real(m))
            a = exp(c)
            b = slope
            write (*, *) "y = ", a, " ✕ ( e ^ (", b," x ) )"
            open(unit = 2 , file="Output.txt")
            write (2, *) "y = ", a, " ✕ ( e ^ (", b," x ) )"
            close(unit = 2)
        else if ( choice == 4 ) then
            write (*, *) "Enter number of points."
            read (*,*,end = 40) m
            allocate(Points(m, 2))
            open (unit = 1, file = "Input.txt", status = 'old' )
                do i = 1, m
                    read (1,*,end = 40) (Points(i, j) ,  j = 1, 2)
                end do
            40 close (unit = 1)
            do i = 1, m
                SumX = SumX + Points(i, 1)
                SumY = SumY + Points(i, 2)
                SumXY = SumXY + Points(i, 1) * Points(i, 2)
                SumXX = SumXX + Points(i, 1) ** 2
                SumXXX = SumXXX + Points(i, 1) ** 3
                SumXXXX = SumXXXX + Points(i, 1) ** 4
                SumXXY = SumXXY + (Points(i, 1) ** 2) * (Points(i, 2))
            end do
            a = m
            b = SumX
            c = SumXX
            d = (-1.0) * SumY
            l = SumX
            m1 = SumXX
            n = SumXXX
            k = (-1.0) * SumXY
            p = SumXX
            q = SumXXX
            r = SumXXXX
            s1 = (-1.0) * SumXXY
            Det = ( a * m1 * r + b * p * n + c * l * q ) - ( a * n * q + b * l * r + c * m1 * p );
            x = (( b * r * k + c * m1 * s1 + d * n * q ) - ( b * n * s1 + c * q * k + d * m1 * r )) / Det
            y = (( a * n * s1 + c * p * k + d * l * r ) - ( a * r * k + c * l * s1 + d * n * p )) / Det
            z = (( a * q * k + b * l * s1 + d * m1 * p )-( a * m1 * s1 + b *p * k + d * l * q )) / Det
            write (*, *) " y = (", z, ") x² + (", y,") x + (", x,")"
            open(unit = 2 , file="Output.txt")
            write (2, *) " y = (", z, ") x² + (", y,") x + (", x,")"
            close(unit = 2)
        else if ( choice == 5 ) then
            write (*, *) "Enter number of points."
            read (*,*,end = 50) m
            allocate(Points(m, 3))
            open (unit = 1, file = "Input.txt", status = 'old' )
                do i = 1, m
                    read (1,*,end = 50) (Points(i, j) ,  j = 1, 3)
                end do
            50 close (unit = 1)
            do i = 1, m
                SumX = SumX + Points(i, 1)
                SumY = SumY + Points(i, 2)
                SumZ = SumZ + Points(i, 3)
                SumXX = SumXX + Points(i, 1) ** 2
                SumYY = SumYY + Points(i, 2) ** 2
                SumXY = SumXY + Points(i, 1) * Points(i, 2)
                SumYZ = SumYZ + Points(i, 2) * Points(i, 3)
                SumZX = SumZX + Points(i, 3) * Points(i, 1)
            end do
            a = m
            b = SumX
            c = SumY
            d = (-1.0) * SumZ
            l = SumX
            m1 = SumXX
            n = SumXY
            k = (-1.0) * SumZX
            p = SumY
            q = SumXY
            r = SumYY
            s1 = (-1.0) * SumYZ
            Det = ( a * m1 * r + b * p * n + c * l * q ) - ( a * n * q + b * l * r + c * m1 * p );
            x = (( b * r * k + c * m1 * s1 + d * n * q ) - ( b * n * s1 + c * q * k + d * m1 * r )) / Det
            y = (( a * n * s1 + c * p * k + d * l * r ) - ( a * r * k + c * l * s1 + d * n * p )) / Det
            z = (( a * q * k + b * l * s1 + d * m1 * p )-( a * m1 * s1 + b *p * k + d * l * q )) / Det
            write (*, *) " z = (", x, ") + (", y,") x + (", z,") y"
            open(unit = 2 , file="Output.txt")
            write (2, *) " z = (", x, ") + (", y,") x + (", z,") y"
            close(unit = 2)
        else
            write (*, *) "You chose... poorly. Try again."
            goto 999
        end if
end program Program