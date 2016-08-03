# mw-parser

A rule parser for MicroWorld

## Part of the overall MicroWorld system 

While this code works and is interesting on its own, you also need at least
[mw-engine](https://github.com/simon-brooke/mw-engine) and 
[mw-ui](https://github.com/simon-brooke/mw-ui). There will be other 
modules in due course.

You can see MicroWorld in action [here](http://www.journeyman.cc/microworld/) -
but please don't be mean to my poor little server. If you want to run big maps
or complex rule-sets, please run it on your own machines.

## Usage

Main entry point is (parse-rule _string_), where string takes a form detailed
in __[grammar](#grammar)__, below. If the rule is interpretted correctly the result will
be the source code of a Clojure anonymous function; if the rule cannot be interpretted,
an error 'I did not understand...' will be shown.

The function (compile-rule _string_) is like parse-rule, except that it returns
a compiled Clojure anonymous function.

### Generated function and evaluation environment

The generated function is a function of two arguments

+ __cell__ a cell in a world as defined in mw-engine.world, q.v.;
+ __world__ the world of which that cell forms part.

It returns a new cell, based on the cell passed.

Actions of the rule will (can only) modify properties of the cell; there are two
properties which are special and SHOULD NOT be modified, namely the properties
__x__ and __y__.

### Execution

Each time the world is transformed, exactly the same set of rules is applied to every
cell. The rules are applied to the cell in turn, in the order in which they are
written in the rule text, until the conditions of one of them match the cell.
The actions of that rule are then used to transform the cell, and the rest of
the rules are not applied.

So, for example, if your first rule is

    if x is more than -1 then state should be new

then no matter what your other rules are, your world will never change, because
all cells have x more than -1.

If you are having problems because one of your rules isn't working, look to
see whether there is another rule above it which is 'blocking' it.

### <a name="grammar"></a>Grammar

#### Comments

+ Any line which starts with the hash character (#) is ignored;
+ Any line which starts with a semi-colon (;) is ignored.

#### Rules

A rule comprises:

+ if _conditions_ then _actions_

Each rule must be on a single line. There should be nothing else on that line.

#### Conditions

In rules, _conditions_ is one of:

+ _condition_
+ _condition_ and _conditions_
+ _condition_ or _conditions_

Note that 'and' takes precedence over or, so

    conditionA and conditionB or conditionC and conditionD

is interpreted as

	(conditionA and (conditionB or (conditionC and conditionD)))

A _condition_ is one of:

+ _property_ is _value_
+ _property_ is not _value_
+ _property_ is in _values_
+ _property_ is not in _values_
+ _property_ is more than _numeric-value_
+ _property_ is less than _numeric-value_
+ _number_ neighbours have _property_ equal to _value_
+ _number_ neighbours have _property_ more than _numeric-value_
+ _number_ neighbours have _property_ less than _numeric-value_
+ more than _number_ neighbours have _property_ equal to _value_
+ fewer than _number_ neighbours have _property_ equal to _value_
+ some neighbours have _property_ equal to _value_
+ more than _number_ neighbours have _property_ more than _numeric-value_
+ fewer than _number_ neighbours have _property_ more than _numeric-value_
+ some neighbours have _property_ more than _numeric-value_
+ more than _number_ neighbours have _property_ less than _numeric-value_
+ fewer than _number_ neighbours have _property_ less than _numeric-value_
+ some neighbours have _property_ less than _numeric-value_

#### About neighbours

Note that everywhere above I've used 'neighbours', you can use 'neighbours
within _distance_', where _distance_ is a (small) positive integer.

A cell has eight immediate neighbours - cells which actually touch it (except
for cells on the edge of the map, which have fewer). If the cell we're
interested in is the cell marked 'X' in the table below, its immediate neighbours
are the ones marked '1'. But outside the ones marked '1', it has more distant
neighbours - those marked '2' and '3' in the table, and still more outside those.

<table style="padding-left: 20%;">
<tr><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: chartreuse;">3</td></tr>
<tr><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: chartreuse;">3</td></tr>
<tr><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: green;">1</td><td style="width: 1.5em; background-color: green;">1</td><td style="width: 1.5em; background-color: green;">1</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: chartreuse;">3</td></tr>
<tr><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: green;">1</td><td style="color:white; width: 1.5em; background-color: red;">X</td><td style="width: 1.5em; background-color: green;">1</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: chartreuse;">3</td></tr>
<tr><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: green;">1</td><td style="width: 1.5em; background-color: green;">1</td><td style="width: 1.5em; background-color: green;">1</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: chartreuse;">3</td></tr>
<tr><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: lime;">2</td><td style="width: 1.5em; background-color: chartreuse;">3</td></tr>
<tr><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: chartreuse;">3</td><td style="width: 1.5em; background-color: chartreuse;">3</td></tr>
</table>

If a rule just says 'neighbours', and not 'neighbours within', it means
'neighbours within 1'; so

    if some neighbours are scrub then state should be scrub

has exactly the same meaning as

    if some neighbours within 1 are scrub then state should be scrub

#### Actions

In these rules, _actions_ is one of:

+ _action_
+ _action_ and _actions_

and _action_ is:

+ _property_ should be _value_
+ _number_ chance in _number_ _property_ should be _value_

#### Properties

In the above, _property_ is the name of any property of a cell. Any alpha-numeric
string of characters can form the name of a property. Actions should __NOT__
try to change the reserved properties __x__ and __y__.

#### Values in Conditions

Values in conditions and actions are considered slightly differently. In a
condition, a value is one of:

+ _symbolic-value_
+ _numeric-value_

The '...more than...' and '...less than...' conditions imply a _numeric-value_.
Thus "if altitude is more than fertility..." is interpreted as meaning "if the value
of the property of the current cell called 'altitude' is greater than the value
of the property of the current cell called 'fertility'", whereas the apparently
similar condition 'if altitude is fertility...' is interpreted as meaning
"if the value of the property of the current cell called 'altitude' is the symbol
'fertility'".

Thus _symbolic-value_ is any sequence of alphanumeric characters, whereas
_numeric-value_ is one of:

+ _number_
+ _property_

and _number_ is any sequence of the decimal digits 0...9, the minus character
'-' and the period character '.', provided that the minus character can only be
in the first position, and  the period character can only appear once.

#### Values in Actions

A _value_ in an action is one of

+ _symbolic-value_
+ _arithmetic-value_
+ _number_

where _arithmetic-value_ is:

+ _property_ _operator_ _numeric-value_

and _operator_ is one of the simple arithmetic operators '+', '-', '*' and '/'.

### Shorthand

Note that '...neighbours are...' is equivalent to '...neighbours have state equal to...',
and 'some neighbours...' is equivalent to 'more than 0 neighbours...'

### Roadmap

The existing parser, *mw-parser.core*, works but is not well written. A much
better parser which does not yet completely work, *mw-parser.insta*, is also
included for the adventurous.

I intend to replace *mw-parser.core* with *mw-parser.insta* as soon as 
*mw-parser.insta* correctly parses all the test rules.

## License

Copyright © 2014 [Simon Brooke](mailto:simon@journeyman.cc)

Distributed under the terms of the
[GNU General Public License v2](http://www.gnu.org/licenses/gpl-2.0.html)
