import Visitor from './Visitor.js';
import { Rango } from './CST.js';

export default class Tokenizer extends Visitor {
    generateTokenizer(grammar) {
        return `
module tokenizer
implicit none

contains
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

    ${grammar.map((produccion) => produccion.accept(this)).join('\n')}

    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    lexeme = "ERROR"
end function nextSym
end module tokenizer 
        `;
    }
    visitProducciones(node) {
        return node.expr.accept(this);
    }
    visitOpciones(node) {
        return node.exprs.map(node => node.accept(this)).join('\n');
    }
    visitUnion(node) {
        return node.exprs.map(node => node.accept(this)).join('\n');
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
        default: 
        return `${label}${baseExpr}`;
        }
    }
    visitString(node) {
        //aqui dentro hacemos la traduccion a fortran 
        return `
        if ("${node.val}" == input(cursor:cursor + ${
            node.val.length - 1
        })) then !Foo
            allocate( character(len=${node.val.length}) :: lexeme)
            lexeme = input(cursor:cursor + ${node.val.length - 1})
            cursor = cursor + ${node.val.length}
            return
        end if
        `
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
