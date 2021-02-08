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
        real a, b, choice, n, h, sum, Integral
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
            998 write (*, *) "Enter number of segments."
            read (*, *) n
            if ( mod(n, 1.0) == 0.0 ) then
                h = (b - a)/ n
                sum = 0.0
                do i = 1, int(n - 1)
                    sum = sum + f( a + i * h )
                end do
                Integral = h * ( f (a) + f(b) + 2 * sum ) / 2.0
                calculated = .true.
            else
                write(*, *) "Invalid n, try again."
                goto 998
            end if
        else if ( choice == 2 ) then
            997 write (*, *) "Enter value of h."
            read (*, *) h
            n = (b - a)/ h
            if ( mod(n, 1.0) == 0.0 ) then
                sum = 0.0
                do i = 1, int(n - 1)
                    sum = sum + f( a + i * h )
                end do
                Integral = h * ( f (a) + f(b) + 2 * sum ) / 2.0
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