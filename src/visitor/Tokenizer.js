import Visitor from './Visitor.js';
import { Rango } from './CST.js';

export default class Tokenizer extends Visitor {
    generateTokenizer(grammar) {
        return `
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

    do while (cursor <= len(input))
        select case (input(cursor:cursor))
        case (" ", char(9), char(10), char(13))  
            cursor = cursor + 1
        case default
            exit
        end select
    end do

    ! Aquí se procesan todas las producciones generadas
    ${grammar.map((produccion) => produccion.accept(this)).join('\n')}

    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    lexeme = "ERROR"
end function nextSym
end module parser
    `;
    }
    
    visitProducciones(node) {
        console.log("Visita Producciones:", node.id, node.alias);  // Agrega log para verificar los datos
    
        // Asegurarse de que node.alias sea una cadena y eliminar las comas
        const aliasPrint = node.alias ? `print *, "--${String(node.alias).replace(/,/g, '')}--"` : '';
    
        // Generar la producción y el procesamiento correspondiente en Fortran
        return `
        ! Producción: ${node.id} 
        ${aliasPrint}  ! Imprimir alias en consola
        ${node.expr.accept(this)}  ! Procesando expresión de la producción
        `;
    }
    
    visitOpciones(node) {
        return node.exprs.map(node => node.accept(this)).join('\n');
    }
    visitUnion(node) {
        return node.exprs.map(node => node.accept(this)).join('\n');
    }
    visitId(node) {
        return `
        if (toLower(input(cursor:)) == "${node.id.toLowerCase()}") then
            allocate( character(len=len("${node.id}")) :: lexeme )
            lexeme = "${node.id}"
            cursor = cursor + len("${node.id}")
            return
        end if
        `;
    }
    visitParentesis(node) {
        return node.expr.accept(this);
    }
    visitExpresion(node) {
        const baseExpr = node.expr.accept(this);
        const label = node.label ? `! Etiqueta: ${node.label}\n` : '';
        const qty = node.qty || '';
    
        switch (qty) {
            case '*': // Cero o más veces
    return `
    ${label}
    i = cursor
    do while (cursor <= len(input))
        ${baseExpr}
        if (cursor == i) exit ! Rompe si no hay avance
        i = cursor
    end do
    `;
            case '+': // Una o más veces
                return `
    ${label}
    i = cursor
    do while (cursor <= len(input))
        ${baseExpr}  ! Procesando expresión
        if (cursor == i) exit ! Rompe si no hay avance
        i = cursor
    end do
    `;
            case '?': // Cero o uno
                return `
    ${label}
    i = cursor
    ${baseExpr}  ! Procesando expresión
    if (cursor == i) then
        ! No encontró ninguna coincidencia
        cursor = i  ! No avanza
    end if
    `;
            default: 
                return `${label}${baseExpr}`; // Comportamiento estándar para otras expresiones
        }
        
    }
    visitString(node) {
        const lowerCaseComparison = node.isCase 
            ? `
            if (toLower(input(cursor:cursor + ${node.val.length - 1})) == "${node.val.toLowerCase()}") then ! Foo
                allocate( character(len=${node.val.length}) :: lexeme)
                lexeme = input(cursor:cursor + ${node.val.length - 1})
                cursor = cursor + ${node.val.length}
                return
            end if
            ` 
            : `
            if ("${node.val}" == input(cursor:cursor + ${node.val.length - 1})) then ! Foo
                allocate( character(len=${node.val.length}) :: lexeme)
                lexeme = input(cursor:cursor + ${node.val.length - 1})
                cursor = cursor + ${node.val.length}
                return
            end if
            `;
    
        return lowerCaseComparison;
    }
    generateCaracteres(chars) {
        if (chars.length === 0) return '';
        return `
    if (findloc([${chars.map((char) => `"${char}"`).join(', ')}], input(i:i), 1) > 0) then
        lexeme = input(cursor:i)
        cursor = i + 1
        return
    end if
        `;
    }
    visitClase(node) {
        return `
    i = cursor
    ${this.generateCaracteres(node.chars.filter((node) => typeof node === 'string'))}
    ${node.chars.filter((node) => node instanceof Rango).map((range) => range.accept(this)).join('\n')}
        `;
    }
    visitRango(node) {
        return `
    i = cursor
    do while (i <= len(input) .and. input(i:i) >= "${node.bottom}" .and. input(i:i) <= "${node.top}")
        i = i + 1
    end do
    if (i > cursor) then
        allocate( character(len=i-cursor) :: lexeme )
        lexeme = input(cursor:i-1)
        cursor = i
        return
    end if
    `;
}
}