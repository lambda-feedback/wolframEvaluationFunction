# wolframEvaluate

This evaluation function, currently experimental, utilises the [Wolfram Language](https://www.wolfram.com/language/) to compare a student's response to the correct answer. By default the comparison is *semantic*: the two strings are parsed, and compared as Wolfram expressions to determine whether they are mathematically equivalent. 

Another mode of comparison called *structure* is available, in which the response is compared to a *template* to check that it has the same structure. 

A third option is to use both semantic and structure comparison simultaneously; this checks that the answer and response are both mathematically and structurally equivalent, and is useful if one wants to insist that a response be, for example, fully factorised, or expressed in terms of real and imaginary parts.

Both modes of comparison handle equations and inequalities, including chains of arbitrary length; neither, however, is capable of answering the very general question "Are these two equations or inequalities equivalent?" Structure comparison works exactly as it does for expressions, comparing the response to a template. Semantic comparison takes, for both answer and response, the difference between the left-hand side and the right-hand side, and checks, for these two differences, whether one is a simple multiple of the other. More detail on this below.

Both modes of comparison will also handle answers and responses containing the strings `plus_minus` or `minus_plus`, with the precise way this works for semantic comparison being dependent on certain parameters; again, more detail below. 

The function will also perform simple comparison of *numbers*, to a given tolerance (by default zero). 

## Inputs

When you first set up a response area, or click on the Configure button next to an existing one, one of the tabs you'll see is labelled **EVALUATION**. On this tab, there is a field that has the same label: **EVALUATION**. It is here that you can select `wolframEvaluate` as your evaluation function of choice. 

The **EVALUATION** tab is also the place where you can set the values of certain *parameters* that govern the way comparison between the official *answer* and the student's *response* is carried out. Most of these parameters have hidden default values, meaning you need only set them if you want to override those defaults. There is one parameter, `type`, that is never hidden and must always be set explicitly, though it too has a default setting.

### The mandatory `type` parameter

#### `type`

Every comparison carried out by `wolframEvaluate` must have an explicit `type`. In the teacher/author user interface, this is set by default to `semantic`. You can easily override this default, but the parameter must not be deleted entirely.

The main available settings for `type` are `semantic`, `structure`,  `semantic_and_structure` and `numerical`. The settings `strict_structure` and `semantic_and_strict_structure` also exist, but are in development and for the moment undocumented.

##### Semantic comparison

If `type` is set to `semantic`, which is the default on the **EVALUATION** tab, then `wolframEvaluate` tries to test whether the teacher's answer and the student's response mean the same thing: that is, represent the same mathematical object. It is impossible to make this fully general (or mathematics would be in a sense trivial!) but `wolframEvaluate` uses the power of the Wolfram Language, which is quite substantial and covers a lot of cases. 

The simplest kind of semantic comparison is that between two strings that represent algebraic expressions, such as "(x - 1) (x + 3)" and "x^2 + 2 x - 3". In broad terms, `wolframEvaluate` converts these into Wolfram Language expressions and checks whether their difference simplifies to zero.

The evaluation function, with `type` set to `semantic`, can also handle *equations* and *inequalities*. When comparing simple two-sided relations such as "x = y" and "2 (x - y) = 0", the function computes, for each, the difference between the left-hand side and the right-hand side. If one of these differences is a simple multiple of the other, the test is passed. 

(**Technical note**: what "simple" means here depends on whether the relation is an inequality or an equality. In the former case, `wolframEvaluate` tests whether the ratio is a positive `Integer` or `Rational`. In the latter case, it tests whether it is an `Integer` or `Rational` of either sign, or a symbolic expression with numerator or denominator unity.)

<!--This should be refined somewhat to allow Real ratios and, in the case of inequalities, positive symbolic numerical quantities like pi.-->

Inequalities with opposing directions are treated as one would expect, with, for example, "x <= y" and "y >= x" being treated as semantically equivalent.

Chained relations such as "x = y = z^2" and "x - y = 0 = z^2 - y" are also handled. In the case of inequalities, "mixed strictness", as in "x <= y < z" and "z > y >= x", is permitted.

(**Technical note**: to avoid combinatorial explosions arising from permuting long chained equalities, the ordering of the terms in such chains must be the same in order for the test to be passed.)

<!--This should be reviewed: permutations are presumably OK for fairly short chained equalities.-->

##### Structural comparison

If `type` is set to `structure`, something quite different takes place. The teacher's answer is treated not as a single expression but as a *template*, which is matched with the student's response if and only if that response is of the right form. For example, the template "a * b" is matched by any response of the form "one thing times another", so that "(x - 1) (x + 3)" would pass, but "x^2 + 2 x - 3" would fail.

In the template "a * b", the symbols a and b are treated as *wildcards*: *any* student response of the form "one thing times another" will structurally match "a * b". By default, that always happens; however, this is not always what you may want. For example, you may wish to set up a template that matches "2 x" or "7 x" or "c x", but not "2 y" or "7 y" or "c y": that is, to define the template "a x", where "x" is to be treated not as a wildcard but as a literal symbol. To do that, you should use the `named_variables` parameter; see below.

##### Mixed comparison

More often than not, when using structural comparison, you will want, as a teacher-author, to combine it with semantic comparison: that is, you will want to test whether the student's response is mathematically equivalent to your answer *and* in the right form. For example, if the question is "Factorise $x^2 + 2 x - 3$", the response "(x - 1) (x + 3)" should be treated as correct, but "(x + 1) (x - 3)" should fail on the grounds that it is a different mathematical object, and "x^2 + 2 x - 3" should fail on the grounds that it is not of the form "one thing times another".

For this, `type` should be set to `semantic_and_structure`. 

Note that you must now specify both an expression (for semantic comparison) and a template (for structural comparison). The contents of the `answer` field form the expression; to specify the template you will need to set the `answer_template` parameter (see below).

##### Numerical comparison

If `type` is set to `numerical` (or, in fact, anything `wolframEvaluate` does not recognise), the answer and response are compared as numbers. By default, this comparison is exact, but you may set a *tolerance* that may be either absolute or relative. To do that, you will need to set the parameters `tolerance` and, potentially, `tolerance_is_absolute`. 

### The conditional `answer_template` parameter

If the `type` parameter is set up for mixed comparison (that is, if it has the value `semantic_and_structure` or the currently undocumented `semantic_and_strict_structure`), then you will need to set the  `answer_template` parameter. The value of `answer_template` should be a string representing your desired template, such as "a * b"; your answer itself should go in the answer field as with semantic comparison.

You may also wish, when performing mixed comparison, to set the optional `named_variables` parameter; see below.

### Optional parameters

There are eight optional parameters whose values you may set if you want to fine-tune the behaviour of `wolframEvaluate` by overriding a default setting. These are: `correct_response_feedback`, `incorrect_response_feedback`, `multiple_answers_interpretation`, `named_variables`, `plus_minus_split_preview`, `suppress_independent_variable`, `tolerance` and `tolerance_is_absolute`.

#### `correct_response_feedback` 
By default, `wolframEvaluate` feeds back the string "Correct!" when the response matches the answer. You may override this by setting the `correct_response_feedback` parameter, for example, you may wish the feedback to be "That's right, and well done for including the constant of integration."

#### `incorrect_response_feedback` 
By default, `wolframEvaluate` feeds back the string "Incorrect!" when the response does not match the answer. You may override this by setting the `incorrect_response_feedback` parameter.

#### `multiple_answers_interpretation`
For the question "Find both solutions in $x$ of the equation $x^2 + 2 x - 3 = 0$", the answer consists of the values 1 and -3. You can set that up in essentially two ways: either your given answer can be "{1, -3}" or it can be "-1 plus_minus 2"; these are entirely equivalent.

In the case of that particular question, you want the student's response to contain both solutions, but you do not mind in what order they list them. This is the default behaviour of `wolframEvaluate`, and corresponds to the default setting of the `multiple_answers_interpretation` parameter, namely `match_all`. Student responses that match your multiple answer, with this default setting, include "{1, -3}", "{-3, 1}", "-1 plus_minus 2" or "-1 minus_plus 2".

<!-- Desirable functionality: allow the student response "1, -3" etc -->

Conceivably, however, you may want instead to set the question "Find either of the two solutions in $x$ of the equation $x^2 + 2 x - 3 = 0$." In this case, either 1 or -3 would be an acceptable response. To achieve this, you should again set the answer to be "{1, -3}" or "-1 plus_minus 2", but you should explicitly give the `multiple_answers_interpretation` parameter the value `match_any`. 

(This feature is useful when you have a variable that may be positive or negative, such as in the question "Find, in terms of an integer variable $n$, all solutions of the equation $tan(x) = 1/\sqrt{3}$". The responses "pi/6 + n pi" and "pi/6 - n pi" are both correct, and if you set the answer to "pi/6 plus_minus n pi" and give `multiple_answers_interpretation` the value `match_any`, you will have exactly the behaviour you need.)

Finally, you may wish to set the question "Find, in ascending numerical order, the two solutions in $x$ of the equation $x^2 + 2 x - 3 = 0$." For this, you would deem acceptable the responses "{-3, 1}" or "-1 minus_plus 2", but not "{1, -3}" or "-1 plus_minus 2". To achieve this, set the answer to be "{-3, 1}" or "-1 minus_plus 2", and give `multiple_answers_interpretation` the value `match_order`.

The above examples are all cases of semantic comparison, but multiple answers also work in structural comparison, in broadly the same way.

#### `named_variables`

The `named_variables` parameter is only used in structural (and therefore, in mixed) comparison; if your comparison is semantic or numerical, the value of this parameter will simply be ignored. By default it consists of an empty Wolfram list.

By default, any symbol in a *template* is treated as a *wildcard*, so that
for example "a * b" matches structurally with any response in the form of a product of two factors, such as "2 x" or "(x - 1) (x + 3)".

But quite a lot of the time, you may have one or more *named variables* that you do not want to be "wildcarded" in this way. For example, suppose you want to check that a response is of the form "something times x" rather than "one thing times another", so that "2 x" is acceptable but "2 y" is not. If you simply set up the template "a x" and do nothing else, then "2 y" will indeed match. 

To prevent this, and insist on an explicit "x" in the student's response, you will need to give the `named_variables` parameter the value "{x}". A list of named variables may be as long as you like.

#### `plus_minus_split_preview`

This parameter is only used when the student's response contains a "plus_minus" or a "minus_plus"; otherwise, it is ignored. By default it has the value `True`, meaning that if a student types "-1 plus_minus 2" this will appear in the preview window as $\{1, -3\}$. If you give it the value `False` then "-1 plus_minus 2" appears in the preview window as $-1 \pm 2$.

#### `suppress_independent_variable`

This parameter is used only when the answer, or the student's response, contains a *derivative* of the form "y'[x]" or "dy/dx". By default it has the value `True`, meaning that either of these subexpressions is converted to "y'". If instead you give it the value `False`, then "y'[x]" stays as it is, and "dy/dx" becomes "y'[x]". 

<!-- Make it understand y'(x) -->

#### `tolerance`

This is a parameter used only in numerical comparison. By default, two numbers are compared by testing whether they are exactly equal. This works well for integers and rationals, but poorly for reals. For that reason, numerical quantities are compared using a `tolerance`, which is by default zero but which can be overridden by setting the `tolerance` parameter.
/
#### `tolerance_is_absolute`

By default, tolerance is *relative*: a response matches the answer "5",with tolerance 0.01, if it lies between $0.99 \times 5$ and $1.01 \times 5$.

If, instead, you want the tolerance to be *absolute*, so that the interval of acceptable values is between $4.99$ and $5.01$, you can set the `tolerance_is_absolute` parameter to `True`.

### Examples

TO BE DONE.