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
        open(unit = 1 , file="Steps.txt")
        if ( choice == 1 ) then
            998 write (*, *) "Enter number of segments."
            read (*, *) n
            if ( mod(n, 1.0) == 0.0 ) then
                h = (b - a)/ n
                write (1, *) "n = ", n
                write (1, *) ""
                write (1, *) "h = ", h
                write (1, *) ""
                write (1, *) "Integral ="
                write (1, *) ""
                write (1, *) "= ∫ f(x) dx <from a to b>"
                write (1, *) ""
                write (1, *) "= ∫ f(x) dx <from ", a," to ", b,">"
                write (1, *) ""
                write (1, *) "= ∫ f(x) dx <from ", a," to ", b,">"
                write (1, *) ""

                write (1, '(a)', advance = 'no') "= { ( b - a ) / ( 2 n ) } X { f ( a ) + "
                do i = 1, int(n - 1)
                    write (1, '(a, i4, a)', advance = 'no') "2 X f ( a + ", i, " X h ) + "
                end do
                write (1, *) " f ( b ) }"
                write (1, *) ""

                write (1, '(a, f10.4, a)', advance = 'no') "= (", h," / 2 ) X { f ( a ) + "
                do i = 1, int(n - 1)
                    write (1, '(a, i4, a)', advance = 'no') "2 X f ( a + ", i, " X h ) + "
                end do
                write (1, *) " f ( b ) }"
                write (1, *) ""

                write (1, '(a, f10.4, a, f10.4, a)', advance = 'no') "= (", h," / 2 ) X { f ( ", a," ) + "
                do i = 1, int(n - 1)
                    write (1, '(a, f10.4, a)', advance = 'no') "2 X f ( ", a + i * h, " ) + "
                end do
                write (1, *) " f ( ", b, " ) }"
                write (1, *) ""

                write (1, '(a, f10.4, a, f10.4, a)', advance = 'no') "= (", h / 2," ) X [ ( ", f(a)," ) + "
                do i = 1, int(n - 1)
                    write (1, '(a, f10.4, a)', advance = 'no') " { 2 X ( ", f(a + i * h), " ) } + "
                end do
                write (1, *) "  ( ", f(b), " ) ]"
                write (1, *) ""

                write (1, '(a, f10.4, a, f10.4, a)', advance = 'no') "= (", h / 2," ) X { ( ", f(a)," ) + "
                do i = 1, int(n - 1)
                    write (1, '(a, f10.4, a)', advance = 'no') " ( ",  2 * f(a + i * h), " ) + "
                end do
                write (1, *) "  ( ", f(b), " ) }"
                write (1, *) ""

                sum = 0.0
                do i = 1, int(n - 1)
                    sum = sum + f( a + i * h )
                end do

                write (1, '(a, f10.4, a, f13.4, a)') "= (", h / 2," ) X ( ", ( f (a) + f(b) + 2 * sum )," ) "
                write (1, *) ""

                Integral = h * ( f (a) + f(b) + 2 * sum ) / 2.0

                write (1, '(a, f13.4)') "= ", Integral
                write (1, *) ""

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
                write (1, *) "n = ", n
                write (1, *) ""
                write (1, *) "h = ", h
                write (1, *) ""
                write (1, *) "Integral ="
                write (1, *) ""
                write (1, *) "= ∫ f(x) dx <from a to b>"
                write (1, *) ""
                write (1, *) "= ∫ f(x) dx <from ", a," to ", b,">"
                write (1, *) ""
                write (1, *) "= ∫ f(x) dx <from ", a," to ", b,">"
                write (1, *) ""

                write (1, '(a)', advance = 'no') "= { ( b - a ) / ( 2 n ) } X { f ( a ) + "
                do i = 1, int(n - 1)
                    write (1, '(a, i4, a)', advance = 'no') "2 X f ( a + ", i, " X h ) + "
                end do
                write (1, *) " f ( b ) }"
                write (1, *) ""

                write (1, '(a, f10.4, a)', advance = 'no') "= (", h," / 2 ) X { f ( a ) + "
                do i = 1, int(n - 1)
                    write (1, '(a, i4, a)', advance = 'no') "2 X f ( a + ", i, " X h ) + "
                end do
                write (1, *) " f ( b ) }"
                write (1, *) ""

                write (1, '(a, f10.4, a, f10.4, a)', advance = 'no') "= (", h," / 2 ) X { f ( ", a," ) + "
                do i = 1, int(n - 1)
                    write (1, '(a, f10.4, a)', advance = 'no') "2 X f ( ", a + i * h, " ) + "
                end do
                write (1, *) " f ( ", b, " ) }"
                write (1, *) ""

                write (1, '(a, f10.4, a, f10.4, a)', advance = 'no') "= (", h / 2," ) X [ ( ", f(a)," ) + "
                do i = 1, int(n - 1)
                    write (1, '(a, f10.4, a)', advance = 'no') " { 2 X ( ", f(a + i * h), " ) } + "
                end do
                write (1, *) "  ( ", f(b), " ) ]"
                write (1, *) ""

                write (1, '(a, f10.4, a, f10.4, a)', advance = 'no') "= (", h / 2," ) X { ( ", f(a)," ) + "
                do i = 1, int(n - 1)
                    write (1, '(a, f10.4, a)', advance = 'no') " ( ",  2 * f(a + i * h), " ) + "
                end do
                write (1, *) "  ( ", f(b), " ) }"
                write (1, *) ""

                sum = 0.0
                do i = 1, int(n - 1)
                    sum = sum + f( a + i * h )
                end do

                write (1, '(a, f10.4, a, f13.4, a)') "= (", h / 2," ) X ( ", ( f (a) + f(b) + 2 * sum )," ) "
                write (1, *) ""

                Integral = h * ( f (a) + f(b) + 2 * sum ) / 2.0

                write (1, '(a, f13.4)') "= ", Integral
                write (1, *) ""

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
            write (1, *) "∴ Integral = ", Integral
            write (*, *) "Integral = ", Integral
        end if
        close(unit = 1)
end program Program