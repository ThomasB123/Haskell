data Calculator = Const Int
                | Sum (Calculator,Calculator)
                | Product (Calculator,Calculator)

ceval (Const i) = i
ceval (Sum (c1,c2)) = (ceval c1) + (ceval c2)
ceval (Product (c1,c2)) = (ceval c1) * (ceval c2)