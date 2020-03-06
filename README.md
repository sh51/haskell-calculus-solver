# haskell-calculus-solver

Authors: Sihao Huang, Spencer Mitchell

A calculus solver written in Haskell.

To build the project run `stack build`. To test the project run `stack test`.

To run the project run `stack exec haskell-calculus-solver-exe`.

The calculus solver takes an expression as input and outputs the steps taken
to derive the expression.

The following syntax is allowed:  
* Addition:	a + b  
* Subtraction:	a - b  
* Multiplication:	a * b  
* Division:	a / b  
* Exponentiation:	a ^ b  
* Sub-expressions in parenthesis:	a + (b + c)  
* Function calls:  
* Sine:	sin(a)  
		Cosine:	cos(a)  
		Tangent:	tan(a)  
		Natural log:	ln(a)  
		Anonymous functions:	f(a)  
	Derivation:	deriv(x, x ^ 2)  
		The derivation function takes the variable to derive on as it's first  
		argument, and the expression to derive as the second argument.

The following laws and rules can be solved:  
	Exponential rule: deriv(x, a ^ b) = a ^ b * deriv(x, ln(a) * b)  
	Sum of derivatives: deriv(x, a + b) = deriv(x, a) + deriv(x, b)  
	Difference of derivatives: deriv(x, a - b) = deriv(x, a) - deriv(x, b)  
	Product of derivatives: deriv(x, a * b) = (a * deriv(x, b)) + (deriv(x, a) * b)  
	Division of derivatives: deriv(x, a / b) = ((deriv(x, a) * b) - (a * (deriv(x, b)))) / (b ^ 2)  
	Sine rule: deriv(x, sin(a)) = cos(a) * deriv(x, a)  
	Cosine rule: deriv(x, cos(a)) = -sin(a) * deriv(x, a)  
	Tangent rule: deriv(x, tan(a)) = deriv(x,a) * 1 / (cos(a) ^ 2))  
	Natural log rule: deriv(x, ln(a)) = deriv(x, a) * 1 / a  
	Linear rule: deriv(x, x) = 1  
	Constant rule: deriv(x, c) = 0  
	Identity: x + 0 = x  
	Identity: 0 + x = x  
	Identity: x * 1 = x  
	Identity: 1 * x = x  
	Multiplication by 0: x * 0 = 0  
	Multiplication by 0: 0 * x = 0  
	Multiplication Associativity: a * (b * c) = (a * b) * c  
	Reduction: a^b * (1/a) = a^(b-1)  
	Reduction: (1/a) * (a^b) = a^(b-1)  
	Reduction: (c * (a ^ b)) * (1 / a) = c * (a ^ (b - 1))  
	Reduction: (c * (1 / a)) * (a ^ b) = c * (a ^ (b - 1))  
	Trigonometric Identity: sin(x)^2 + cos(x)^2 = 1  
	Tangent definition: sin(x) / cos(x) = tan(x)  
	Subtraction by self: x - x = 0  
	Zero divided: 0 / x = 0  

## Special Features

* The solution will be saved in a markdown file called `your-answer.md`.

* To add new rules, update derivationLaws' (in DerivationLaws.hs) and simplificationLaws' (in SimplificationLaws.hs).

## Example

```bash
$ stack exec haskell-calculus-solver-exe
Enter your calculus homework problem here and I'll solve it for you.

> deriv(x, x ^ 2 + 3 * x + 2)
d/dx (((x^2) + (3 * x)) + 2)
= {Sum of derivatives}
(d/dx ((x^2) + (3 * x)) + d/dx 2)
= {Sum of derivatives}
((d/dx (x^2) + d/dx (3 * x)) + d/dx 2)
= {Exponential rule}
((((x^2) * d/dx (ln(x) * 2)) + d/dx (3 * x)) + d/dx 2)
= {Product of derivatives}
((((x^2) * ((ln(x) * d/dx 2) + (d/dx ln(x) * 2))) + d/dx (3 * x)) + d/dx 2)
= {Product of derivatives}
((((x^2) * ((ln(x) * d/dx 2) + (d/dx ln(x) * 2))) + ((3 * d/dx x) + (d/dx 3 * x))) + d/dx 2)
= {Natural log rule}
((((x^2) * ((ln(x) * d/dx 2) + (((d/dx x * 1) / x) * 2))) + ((3 * d/dx x) + (d/dx 3 * x))) + d/dx 2)
= {Linear rule}
((((x^2) * ((ln(x) * d/dx 2) + (((1 * 1) / x) * 2))) + ((3 * d/dx x) + (d/dx 3 * x))) + d/dx 2)
= {Linear rule}
((((x^2) * ((ln(x) * d/dx 2) + (((1 * 1) / x) * 2))) + ((3 * 1) + (d/dx 3 * x))) + d/dx 2)
= {Constant rule}
((((x^2) * ((ln(x) * 0) + (((1 * 1) / x) * 2))) + ((3 * 1) + (d/dx 3 * x))) + d/dx 2)
= {Constant rule}
((((x^2) * ((ln(x) * 0) + (((1 * 1) / x) * 2))) + ((3 * 1) + (0 * x))) + d/dx 2)
= {Constant rule}
((((x^2) * ((ln(x) * 0) + (((1 * 1) / x) * 2))) + ((3 * 1) + (0 * x))) + 0)
= {Simplification}
(((x^(2 - 1)) * 2) + 3)
Your solution is saved in your-answer.md
$ 
```
