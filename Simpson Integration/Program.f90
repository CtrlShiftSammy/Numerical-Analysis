module FunctionModule
    implicit none
    
    contains
    function f(x)
        implicit none
        real f, x
        f = 2000.0 * log( 140000.0 / ( 140000.0 - 2100.0 * x)) - 9.8 * x
    end function f
end module FunctionModule

program Program
    use FunctionModule
    implicit none
        integer i
        real a, b, choice, n, h, sum1, sum2, Integral
        logical :: calculated = .false.
        write (*, *) "Enter lower limit."
        read (*, *) a
        write (*, *) "Enter upper limit."
        read (*, *) b
        999 write (*, *) "Make your choice."
        write (*, *) "1: Calculate integral using number of segments."
        write (*, *) "2: Calculate integral using length of each segment."
        read (*, *) choice
        if ( choice == 1 ) then
            998 write (*, *) "Enter an even number of segments."
            read (*, *) n
            if ((mod(n, 1.0) == 0.0) .and. (mod(n, 2.0) == 0.0)) then
                h = (b - a)/ n
                sum1 = 0.0
                sum2 = 0.0
                do i = 1, int(n - 1), 2
                    sum1 = sum1 + f( a + i * h )
                end do
                do i = 2, int(n - 2), 2
                    sum2 = sum2 + f( a + i * h )
                end do
                Integral = h * ( f (a) + f(b) + 4 * sum1 + 2 * sum2 ) / 3.0
                calculated = .true.
            else
                write(*, *) "Invalid n, try again."
                goto 998
            end if
        else if ( choice == 2 ) then
            997 write (*, *) "Enter an appropriate value of h."
            read (*, *) h
            n = (b - a)/ h
            if ((mod(n, 1.0) == 0.0) .and. (mod(n, 2.0) == 0.0)) then
                sum1 = 0.0
                sum2 = 0.0
                do i = 1, int(n - 1), 2
                    sum1 = sum1 + f( a + i * h )
                end do
                do i = 2, int(n - 2), 2
                    sum2 = sum2 + f( a + i * h )
                end do
                Integral = h * ( f (a) + f(b) + 4 * sum1 + 2 * sum2 ) / 3.0
                calculated = .true.
            else
                write(*, *) "Invalid h, try again."
                goto 997
            end if
        else
            write(*, *) "Invalid choice, try again."
            goto 999
        end if
        if ( calculated ) then
            open(unit = 1 , file="Output.txt")
                write (1, *) "Integral = ", Integral
                write (*, *) "Integral = ", Integral
            close(unit = 1)
        end if
end program Program