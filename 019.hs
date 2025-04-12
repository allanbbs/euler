isLeapYear year
    | mod year 100 == 0 && mod year 400 == 0 = True
    | mod year 4 == 0 = True
    | otherwise = False

numberOfDaysFeb year
    | isLeapYear year = 29
    | otherwise = 28

numberOfDays year month
    | month == 2 = numberOfDaysFeb year
    | month `elem` [9,4,6,11] = 30
    | otherwise = 31


data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq,Show)

next :: Weekday -> Weekday
next x =
    case x of
        Monday -> Tuesday
        Tuesday -> Wednesday
        Wednesday -> Thursday
        Thursday -> Friday
        Friday -> Saturday
        Saturday -> Sunday
        Sunday -> Monday

data Date = Date { day::Int, month::Int,year::Int, weekday:: Weekday} deriving (Show)


incr :: Date -> Date
incr date =
    let currentDay = day date
        currentMonth = month date
        currentYear = year date
        nextWeekday = next (weekday date)
        totalMonthDays = numberOfDays currentYear currentMonth
        newYear = currentMonth == 12 && currentDay == totalMonthDays
        newMonth = currentDay == totalMonthDays
        newDate
            | newYear = Date 1 1 (currentYear + 1) nextWeekday
            | newMonth = Date 1 (currentMonth + 1) currentYear nextWeekday
            | otherwise = Date (currentDay+1) currentMonth currentYear nextWeekday
    in newDate

eqDate :: Date -> Date -> Bool
eqDate a b = day a == day b && month a == month b && year a == year b

countSundaysAtFst :: Date -> Date -> Int
countSundaysAtFst a b = aux a b 0
    where
        aux current end acc
            | eqDate current end = acc
            | otherwise = if day current == 1 && weekday current == Sunday then aux (incr current) end (acc+1) else aux (incr current) end acc


origin = Date 1 1 1900 Monday
getInitialDate = aux origin 366
    where
        aux date 0 = date
        aux date n = aux (incr date) (n-1)

main :: IO()
main = print (countSundaysAtFst getInitialDate (Date 31 12 2000 Sunday))