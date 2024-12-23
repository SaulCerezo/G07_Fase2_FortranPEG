
module parser
    implicit none

contains

subroutine parse(input)
    character(len=*), intent(in) :: input
    integer :: cursor
    character(len=:), allocatable :: lexeme
    
    cursor = 1
    do while (lexeme /= "EOF" .and. lexeme /= "ERROR")
        lexeme = nextSym(input, cursor)
        print *, lexeme
    end do
end subroutine parse

function toLower(str) result(lowerStr)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: lowerStr
    integer :: i

    lowerStr = str
    do i = 1, len(str)
        if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
            lowerStr(i:i) = char(iachar(str(i:i)) + 32)
        end if
    end do
end function toLower

function nextSym(input, cursor) result(lexeme)
    character(len=*), intent(in) :: input
    integer, intent(inout) :: cursor
    character(len=:), allocatable :: lexeme
    integer :: i

    if (cursor > len(input)) then
        allocate( character(len=3) :: lexeme )
        lexeme = "EOF"
        return
    end if

    
        i = cursor
        do while (i <= len(input) .and. toLower(input(i:i)) >= "a" .and. toLower(input(i:i)) <= "z")
            i = i + 1
        end do
        if (i > cursor) then
            allocate( character(len=i-cursor) :: lexeme )
            lexeme = input(cursor:i-1)
            cursor = i
            return
        end if
            

    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    lexeme = "ERROR"
end function nextSym
end module parser
        