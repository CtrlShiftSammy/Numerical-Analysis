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
        open(unit = 1 , file="Steps.txt")
        if ( choice == 1 ) then
            998 write (*, *) "Enter an even number of segments."
            read (*, *) n
            if ((mod(n, 1.0) == 0.0) .and. (mod(n, 2.0) == 0.0)) then
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

                write (1, '(a)', advance = 'no') "= { ( b - a ) / ( 3 n ) } X { f ( a ) + "
                do i = 1, int(n - 1)
                    if ( mod(real(i), 2.0) == 1.0 ) then
                        write (1, '(a, i4, a)', advance = 'no') "4 X f ( a + ", i, " X h ) + "
                    else
                        write (1, '(a, i4, a)', advance = 'no') "2 X f ( a + ", i, " X h ) + "
                    end if
                end do
                write (1, *) " f ( b ) }"
                write (1, *) ""

                write (1, '(a, f10.4, a)', advance = 'no') "= (", h," / 3 ) X { f ( a ) + "
                do i = 1, int(n - 1)
                    if ( mod(real(i), 2.0) == 1.0 ) then
                        write (1, '(a, i4, a)', advance = 'no') "4 X f ( a + ", i, " X h ) + "
                    else
                        write (1, '(a, i4, a)', advance = 'no') "2 X f ( a + ", i, " X h ) + "
                    end if
                end do
                write (1, *) " f ( b ) }"
                write (1, *) ""

                write (1, '(a, f10.4, a, f10.4, a)', advance = 'no') "= (", h," / 3 ) X { f ( ", a," ) + "
                do i = 1, int(n - 1)
                    if ( mod(real(i), 2.0) == 1.0 ) then
                        write (1, '(a, f10.4, a)', advance = 'no') "4 X f ( ", a + i * h, " ) + "
                    else
                        write (1, '(a, f10.4, a)', advance = 'no') "2 X f ( ", a + i * h, " ) + "
                    end if
                end do
                write (1, *) " f ( ", b, " ) }"
                write (1, *) ""

                write (1, '(a, f10.4, a, f10.4, a)', advance = 'no') "= (", h / 3," ) X [ ( ", f(a)," ) + "
                do i = 1, int(n - 1)
                    if ( mod(real(i), 2.0) == 1.0 ) then
                        write (1, '(a, f10.4, a)', advance = 'no') " { 4 X ( ", f(a + i * h), " ) } + "
                    else
                        write (1, '(a, f10.4, a)', advance = 'no') " { 2 X ( ", f(a + i * h), " ) } + "
                    end if
                end do
                write (1, *) "  ( ", f(b), " ) ]"
                write (1, *) ""

                write (1, '(a, f10.4, a, f10.4, a)', advance = 'no') "= (", h / 3," ) X { ( ", f(a)," ) + "
                do i = 1, int(n - 1)
                    if ( mod(real(i), 2.0) == 1.0 ) then
                        write (1, '(a, f10.4, a)', advance = 'no') " ( ",  4 * f(a + i * h), " ) + "
                    else
                        write (1, '(a, f10.4, a)', advance = 'no') " ( ",  2 * f(a + i * h), " ) + "
                    end if
                end do
                write (1, *) "  ( ", f(b), " ) }"
                write (1, *) ""

                sum1 = 0.0
                sum2 = 0.0
                do i = 1, int(n - 1), 2
                    sum1 = sum1 + f( a + i * h )
                end do
                do i = 2, int(n - 2), 2
                    sum2 = sum2 + f( a + i * h )
                end do

                write (1, '(a, f10.4, a, f13.4, a)') "= (", h / 3," ) X ( ", ( f (a) + f(b) + 4 * sum1 + 2 * sum2 )," ) "
                write (1, *) ""

                Integral = h * ( f (a) + f(b) + 4 * sum1 + 2 * sum2 ) / 3.0

                write (1, '(a, f13.4)') "= ", Integral
                write (1, *) ""

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

                write (1, '(a)', advance = 'no') "= { ( b - a ) / ( 3 n ) } X { f ( a ) + "
                do i = 1, int(n - 1)
                    if ( mod(real(i), 2.0) == 1.0 ) then
                        write (1, '(a, i4, a)', advance = 'no') "4 X f ( a + ", i, " X h ) + "
                    else
                        write (1, '(a, i4, a)', advance = 'no') "2 X f ( a + ", i, " X h ) + "
                    end if
                end do
                write (1, *) " f ( b ) }"
                write (1, *) ""

                write (1, '(a, f10.4, a)', advance = 'no') "= (", h," / 3 ) X { f ( a ) + "
                do i = 1, int(n - 1)
                    if ( mod(real(i), 2.0) == 1.0 ) then
                        write (1, '(a, i4, a)', advance = 'no') "4 X f ( a + ", i, " X h ) + "
                    else
                        write (1, '(a, i4, a)', advance = 'no') "2 X f ( a + ", i, " X h ) + "
                    end if
                end do
                write (1, *) " f ( b ) }"
                write (1, *) ""

                write (1, '(a, f10.4, a, f10.4, a)', advance = 'no') "= (", h," / 3 ) X { f ( ", a," ) + "
                do i = 1, int(n - 1)
                    if ( mod(real(i), 2.0) == 1.0 ) then
                        write (1, '(a, f10.4, a)', advance = 'no') "4 X f ( ", a + i * h, " ) + "
                    else
                        write (1, '(a, f10.4, a)', advance = 'no') "2 X f ( ", a + i * h, " ) + "
                    end if
                end do
                write (1, *) " f ( ", b, " ) }"
                write (1, *) ""

                write (1, '(a, f10.4, a, f10.4, a)', advance = 'no') "= (", h / 3," ) X [ ( ", f(a)," ) + "
                do i = 1, int(n - 1)
                    if ( mod(real(i), 2.0) == 1.0 ) then
                        write (1, '(a, f10.4, a)', advance = 'no') " { 4 X ( ", f(a + i * h), " ) } + "
                    else
                        write (1, '(a, f10.4, a)', advance = 'no') " { 2 X ( ", f(a + i * h), " ) } + "
                    end if
                end do
                write (1, *) "  ( ", f(b), " ) ]"
                write (1, *) ""

                write (1, '(a, f10.4, a, f10.4, a)', advance = 'no') "= (", h / 3," ) X { ( ", f(a)," ) + "
                do i = 1, int(n - 1)
                    if ( mod(real(i), 2.0) == 1.0 ) then
                        write (1, '(a, f10.4, a)', advance = 'no') " ( ",  4 * f(a + i * h), " ) + "
                    else
                        write (1, '(a, f10.4, a)', advance = 'no') " ( ",  2 * f(a + i * h), " ) + "
                    end if
                end do
                write (1, *) "  ( ", f(b), " ) }"
                write (1, *) ""

                sum1 = 0.0
                sum2 = 0.0
                do i = 1, int(n - 1), 2
                    sum1 = sum1 + f( a + i * h )
                end do
                do i = 2, int(n - 2), 2
                    sum2 = sum2 + f( a + i * h )
                end do

                write (1, '(a, f10.4, a, f13.4, a)') "= (", h / 3," ) X ( ", ( f (a) + f(b) + 4 * sum1 + 2 * sum2 )," ) "
                write (1, *) ""

                Integral = h * ( f (a) + f(b) + 4 * sum1 + 2 * sum2 ) / 3.0

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
                write (1, *) "Integral = ", Integral
                write (*, *) "Integral = ", Integral
            close(unit = 1)
        end if
end program Program