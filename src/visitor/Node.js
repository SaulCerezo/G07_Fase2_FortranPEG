export default class Node {
    accept(visitor) {
        throw new Error(`${this.constructor.name}.accept no está implementado`);
    }
}