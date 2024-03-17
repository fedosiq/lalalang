# Lalalang

This is a λ-calculus inspired language interpreter.

Inspired by Artem Pianykh [video series](https://youtu.be/qRHJ4qcFbNE?si=tvEnHI8P20w-ZjH6).

## Lambda-calculus basics

### Types of expressions

1. variable:
   ```x, y```
2. lambda abstraction:
  ```λx.x + x```
3. application:
  ```(λx.x + x) y```
  
### Grammar

$$
M ::= x|λx.M |M \hspace{1mm} N
$$
