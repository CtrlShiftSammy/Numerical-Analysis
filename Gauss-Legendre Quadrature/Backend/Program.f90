module FunctionModule
    implicit none
    
    contains
    function f(x)
        implicit none
        real f, x
        f = -0.18 * x**2 + 4.11 * x -9.79
    end function f
end module FunctionModule

program Program
    use FunctionModule
    implicit none
    real x1, x2, x3, x4, x5, x6, c1, c2, c3, c4, c5, c6
    real a, b, m, c, n, I
    999 write (*, *) "Enter number of points for Gauss–Legendre Quadrature."
    read (*, *) n
    write (*, *) "Enter lower limit."
    read (*, *) a
    write (*, *) "Enter upper limit."
    read (*, *) b
    m = (b - a) / 2
    c = (a + b) / 2
    open(unit = 2 , file="Steps.txt")
    write (2, *) "Integral = "
    if ( n == 2 ) then
        c1 = 1.0
        c2 = 1.0
        x1 = - 1.0 / sqrt (3.0)
        x2 = 1.0  / sqrt (3.0)
        I = m * (c1 * f( m * x1 + c) + c2 * f( m * x2 + c))
        write (2, *) "= ∫ f(x) dx <from a to b>"
        write (2, *) ""
        write (2, *) "= ∫ f(x) dx <from", a," to ", b, ">"
        write (2, *) ""
        write (2, *) "= { ( b - a ) / 2} X ∫ f(", m , " x + ", c ,") dx <from -1 to 1>"
        write (2, *) ""
        write (2, *) "= { ( ",b," - ",a," ) / 2} X ∫ f(", m , " x + ", c ,") dx <from -1 to 1>"
        write (2, *) ""
        write (2, *) "= (",m,") X ∫ f(", m , " x + ", c ,") dx <from -1 to 1>"        
        write (2, *) ""
        write (2, *) "= {(",m,") X c1 X f(", m , " x1 + ", c ,")} + {(",m,") X c2 X f(", m , " x2 + ", c ,")}"      
        write (2, *) ""
        write (2, *) "= {(",m,") X ",c1," X f(", m , "X ",x1," + ", c ,")} + {(",m,") X ",c2," X f(", m , "X ",x2," + ", c ,")}"      
        write (2, *) ""
        write (2, *) "= {(",m,") X ",c1," X f(",  m * x1 + c,")} + {(",m,") X ",c2," X f(",m * x2 + c,")}"      
        write (2, *) ""
        write (2, *) "= {(",m,") X ",c1," X (",f( m * x1 + c),")} + {(",m,") X ",c2," X (",f( m * x2 + c),")}"      
        write (2, *) ""
        write (2, *) "= (", m * c1 * f( m * x1 + c),") + (",m * c2 * f( m * x2 + c),")"
        write (2, *) ""
        write (2, *) "= ", I
        write (2, *) ""
    else if ( n == 3 ) then
        c1 = (5.0 / 9.0 )
        c2 = ( 8.0 / 9.0 )
        c3 = (5.0 / 9.0 )
        x1 = - 1.0 * sqrt (0.6)
        x2 = 0.0
        x3 = sqrt (0.6)
        I = m * (c1 * f( m * x1 + c) + c2 * f( m * x2 + c) + c3 * f( m * x3 + c))
        write (2, *) "= ∫ f(x) dx <from a to b>"
        write (2, *) ""
        write (2, *) "= ∫ f(x) dx <from", a," to ", b, ">"
        write (2, *) ""
        write (2, *) "= { ( b - a ) / 2} X ∫ f(", m , " x + ", c ,") dx <from -1 to 1>"
        write (2, *) ""
        write (2, *) "= { ( ",b," - ",a," ) / 2} X ∫ f(", m , " x + ", c ,") dx <from -1 to 1>"
        write (2, *) ""
        write (2, *) "= (",m,") X ∫ f(", m , " x + ", c ,") dx <from -1 to 1>"        
        write (2, *) ""
        write (2, *) "= {(",m,") X c1 X f(", m , " x1 + ", c ,")} + {(",m,") X c2 X f(", m , " x2 + ", c ,")}"
        write (2, *) "  + {(",m,") X c3 X f(", m , " x3 + ", c ,")}"
        write (2, *) ""
        write (2, *) "= {(",m,") X ",c1," X f(", m , "X ",x1," + ", c ,")} + {(",m,") X ",c2," X f(", m , "X ",x2," + ", c ,")}"
        write (2, *) "   + {(",m,") X ",c3," X f(", m , "X ",x3," + ", c ,")}"
        write (2, *) ""
        write (2, *) "= {(",m,") X ",c1," X f(",  m * x1 + c,")} + {(",m,") X ",c2," X f(",m * x2 + c,")}"
        write (2, *) "  + {(",m,") X ",c3," X f(",m * x3 + c,")}"
        write (2, *) ""
        write (2, *) "= {(",m,") X ",c1," X (",f( m * x1 + c),")} + {(",m,") X ",c2," X (",f( m * x2 + c),")}"
        write (2, *) "  + {(",m,") X ",c3," X (",f( m * x3 + c),")}"
        write (2, *) ""
        write (2, *) "= (", m * c1 * f( m * x1 + c),") + (",m * c2 * f( m * x2 + c),") + (",m * c3 * f( m * x3 + c),")"
        write (2, *) ""
        write (2, *) "= ", I
        write (2, *) ""
    else if ( n == 4 ) then
        c1 = ( 18.0 - sqrt(30.0)) / 36.0
        c2 = ( 18.0 + sqrt(30.0)) / 36.0
        c3 = ( 18.0 + sqrt(30.0)) / 36.0
        c4 = ( 18.0 - sqrt(30.0)) / 36.0
        x1 = -1 * sqrt ((3.0/7.0) + ((2.0/7.0) * (sqrt(6.0 / 5.0))))
        x2 = -1 * sqrt ((3.0/7.0) - ((2.0/7.0) * (sqrt(6.0 / 5.0))))
        x3 = sqrt ((3.0/7.0) - ((2.0/7.0) * (sqrt(6.0 / 5.0))))
        x4 = sqrt ((3.0/7.0) + ((2.0/7.0) * (sqrt(6.0 / 5.0))))
        I = m * (c1 * f( m * x1 + c) + c2 * f( m * x2 + c) + c3 * f( m * x3 + c) + c4 * f( m * x4 + c))
        write (2, *) "= ∫ f(x) dx <from a to b>"
        write (2, *) ""
        write (2, *) "= ∫ f(x) dx <from", a," to ", b, ">"
        write (2, *) ""
        write (2, *) "= { ( b - a ) / 2} X ∫ f(", m , " x + ", c ,") dx <from -1 to 1>"
        write (2, *) ""
        write (2, *) "= { ( ",b," - ",a," ) / 2} X ∫ f(", m , " x + ", c ,") dx <from -1 to 1>"
        write (2, *) ""
        write (2, *) "= (",m,") X ∫ f(", m , " x + ", c ,") dx <from -1 to 1>"        
        write (2, *) ""
        write (2, *) "= {(",m,") X c1 X f(", m , " x1 + ", c ,")} + {(",m,") X c2 X f(", m , " x2 + ", c ,")}"
        write (2, *) "  + {(",m,") X c3 X f(", m , " x3 + ", c ,")} + {(",m,") X c4 X f(", m , " x4 + ", c ,")}"
        write (2, *) ""
        write (2, *) "= {(",m,") X ",c1," X f(", m , "X ",x1," + ", c ,")} + {(",m,") X ",c2," X f(", m , "X ",x2," + ", c ,")}"
        write (2, *) "   + {(",m,") X ",c3," X f(", m , "X ",x3," + ", c ,")} + {(",m,") X ",c4," X f(", m , "X ",x4," + ", c ,")}"
        write (2, *) ""
        write (2, *) "= {(",m,") X ",c1," X f(",  m * x1 + c,")} + {(",m,") X ",c2," X f(",m * x2 + c,")}"
        write (2, *) "  + {(",m,") X ",c3," X f(",m * x3 + c,")} + {(",m,") X ",c4," X f(",m * x4 + c,")}"
        write (2, *) ""
        write (2, *) "= {(",m,") X ",c1," X (",f( m * x1 + c),")} + {(",m,") X ",c2," X (",f( m * x2 + c),")}"
        write (2, *) "  + {(",m,") X ",c3," X (",f( m * x3 + c),")} + {(",m,") X ",c4," X (",f( m * x4 + c),")}"
        write (2, *) ""
        write (2, *) "= (", m * c1 * f( m * x1 + c),") + (",m * c2 * f( m * x2 + c),") + (",m * c3 * f( m * x3 + c),")"
        write (2, *) "  + (",m * c4 * f( m * x4 + c),")"      
        write (2, *) ""
        write (2, *) "= ", I
        write (2, *) ""
    else if ( n == 5 ) then
        c1 = ( 322.0 + 13.0 * sqrt(70.0)) / 900.0
        c2 = ( 322.0 - 13.0 * sqrt(70.0)) / 900.0
        c3 = 128.0 / 225.0
        c4 = ( 322.0 - 13.0 * sqrt(70.0)) / 900.0
        c5 = ( 322.0 + 13.0 * sqrt(70.0)) / 900.0
        x1 = -1 * (sqrt (5.0 - (2.0 * (sqrt(10.0 / 7.0))))) / 3.0
        x2 = -1 * (sqrt (5.0 + (2.0 * (sqrt(10.0 / 7.0))))) / 3.0
        x3 = 0.0
        x4 = (sqrt (5.0 + (2.0 * (sqrt(10.0 / 7.0))))) / 3.0
        x5 = (sqrt (5.0 - (2.0 * (sqrt(10.0 / 7.0))))) / 3.0
        I =m *(c1* f(m* x1 + c) + c2* f(m* x2 + c) + c3* f(m* x3 + c) + c4* f(m* x4 + c) + c5* f(m* x5 + c))
        write (2, *) "= ∫ f(x) dx <from a to b>"
        write (2, *) ""
        write (2, *) "= ∫ f(x) dx <from", a," to ", b, ">"
        write (2, *) ""
        write (2, *) "= { ( b - a ) / 2} X ∫ f(", m , " x + ", c ,") dx <from -1 to 1>"
        write (2, *) ""
        write (2, *) "= { ( ",b," - ",a," ) / 2} X ∫ f(", m , " x + ", c ,") dx <from -1 to 1>"
        write (2, *) ""
        write (2, *) "= (",m,") X ∫ f(", m , " x + ", c ,") dx <from -1 to 1>"        
        write (2, *) ""
        write (2, *) "= {(",m,") X c1 X f(", m , " x1 + ", c ,")} + {(",m,") X c2 X f(", m , " x2 + ", c ,")}"
        write (2, *) "  + {(",m,") X c3 X f(", m , " x3 + ", c ,")} + {(",m,") X c4 X f(", m , " x4 + ", c ,")}"
        write (2, *) "  + {(",m,") X c5 X f(", m , " x5 + ", c ,")}"      
        write (2, *) ""
        write (2, *) "= {(",m,") X ",c1," X f(", m , "X ",x1," + ", c ,")} + {(",m,") X ",c2," X f(", m , "X ",x2," + ", c ,")}"
        write (2, *) "   + {(",m,") X ",c3," X f(", m , "X ",x3," + ", c ,")} + {(",m,") X ",c4," X f(", m , "X ",x4," + ", c ,")}"
        write (2, *) "  + {(",m,") X ",c5," X f(", m , "X ",x5," + ", c ,")}"   
        write (2, *) ""
        write (2, *) "= {(",m,") X ",c1," X f(",  m * x1 + c,")} + {(",m,") X ",c2," X f(",m * x2 + c,")}"
        write (2, *) "  + {(",m,") X ",c3," X f(",m * x3 + c,")} + {(",m,") X ",c4," X f(",m * x4 + c,")}"
        write (2, *) "  + {(",m,") X ",c5," X f(",m * x5 + c,")}"      
        write (2, *) ""
        write (2, *) "= {(",m,") X ",c1," X (",f( m * x1 + c),")} + {(",m,") X ",c2," X (",f( m * x2 + c),")}"
        write (2, *) "  + {(",m,") X ",c3," X (",f( m * x3 + c),")} + {(",m,") X ",c4," X (",f( m * x4 + c),")}"
        write (2, *) "  + {(",m,") X ",c5," X (",f( m * x5 + c),")}"      
        write (2, *) ""
        write (2, *) "= (", m * c1 * f( m * x1 + c),") + (",m * c2 * f( m * x2 + c),") + (",m * c3 * f( m * x3 + c),")"
        write (2, *) "  + (",m * c4 * f( m * x4 + c),") + (",m * c5 * f( m * x5 + c),")"      
        write (2, *) ""
        write (2, *) "= ", I
        write (2, *) ""
    else if ( n == 6 ) then
        c1 = 0.171324492
        c2 = 0.360761573
        c3 = 0.467913935
        c4 = 0.467913935
        c5 = 0.360761573
        c6 = 0.171324492
        x1 = -0.932469514
        x2 = -0.661209386
        x3 = -0.2386191860
        x4 = 0.2386191860
        x5 = 0.661209386
        x6 = 0.932469514
        I = m *(c1* f(m* x1 + c) + c2* f(m* x2 + c) + c3* f(m* x3 + c) + c4* f(m* x4 + c) + c5* f(m* x5 + c) + c6* f(m* x6 + c))
        write (2, *) "= ∫ f(x) dx <from a to b>"
        write (2, *) ""
        write (2, *) "= ∫ f(x) dx <from", a," to ", b, ">"
        write (2, *) ""
        write (2, *) "= { ( b - a ) / 2} X ∫ f(", m , " x + ", c ,") dx <from -1 to 1>"
        write (2, *) ""
        write (2, *) "= { ( ",b," - ",a," ) / 2} X ∫ f(", m , " x + ", c ,") dx <from -1 to 1>"
        write (2, *) ""
        write (2, *) "= (",m,") X ∫ f(", m , " x + ", c ,") dx <from -1 to 1>"        
        write (2, *) ""
        write (2, *) "= {(",m,") X c1 X f(", m , " x1 + ", c ,")} + {(",m,") X c2 X f(", m , " x2 + ", c ,")}"
        write (2, *) "  + {(",m,") X c3 X f(", m , " x3 + ", c ,")} + {(",m,") X c4 X f(", m , " x4 + ", c ,")}"
        write (2, *) "  + {(",m,") X c5 X f(", m , " x5 + ", c ,")} + {(",m,") X c6 X f(", m , " x6 + ", c ,")}"      
        write (2, *) ""
        write (2, *) "= {(",m,") X ",c1," X f(", m , "X ",x1," + ", c ,")} + {(",m,") X ",c2," X f(", m , "X ",x2," + ", c ,")}"
        write (2, *) "   + {(",m,") X ",c3," X f(", m , "X ",x3," + ", c ,")} + {(",m,") X ",c4," X f(", m , "X ",x4," + ", c ,")}"
        write (2, *) "  + {(",m,") X ",c5," X f(", m , "X ",x5," + ", c ,")} + {(",m,") X ",c6," X f(", m , "X ",x6," + ", c ,")}"   
        write (2, *) ""
        write (2, *) "= {(",m,") X ",c1," X f(",  m * x1 + c,")} + {(",m,") X ",c2," X f(",m * x2 + c,")}"
        write (2, *) "  + {(",m,") X ",c3," X f(",m * x3 + c,")} + {(",m,") X ",c4," X f(",m * x4 + c,")}"
        write (2, *) "  + {(",m,") X ",c5," X f(",m * x5 + c,")} + {(",m,") X ",c6," X f(",m * x6 + c,")}"      
        write (2, *) ""
        write (2, *) "= {(",m,") X ",c1," X (",f( m * x1 + c),")} + {(",m,") X ",c2," X (",f( m * x2 + c),")}"
        write (2, *) "  + {(",m,") X ",c3," X (",f( m * x3 + c),")} + {(",m,") X ",c4," X (",f( m * x4 + c),")}"
        write (2, *) "  + {(",m,") X ",c5," X (",f( m * x5 + c),")} + {(",m,") X ",c6," X (",f( m * x6 + c),")}"      
        write (2, *) ""
        write (2, *) "= (", m * c1 * f( m * x1 + c),") + (",m * c2 * f( m * x2 + c),") + (",m * c3 * f( m * x3 + c),")"
        write (2, *) "  + (",m * c4 * f( m * x4 + c),") + (",m * c5 * f( m * x5 + c),") + (",m * c6 * f( m * x6 + c),")"      
        write (2, *) ""
        write (2, *) "= ", I
        write (2, *) ""
    else 
        write(*, *) "Invalid choice, try again."
        goto 999
    end if
    write(*, *) "Integral = ", I
    write (2, *) "∴ Integral = ", I 
    close(unit = 2)
end program Program