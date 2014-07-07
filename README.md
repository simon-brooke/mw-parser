# mw-parser

A rule parser for MicroWorld

## Usage

Main entry point is (parse-rule _string_), where string takes a form detailed
in __grammar__, below. If the rule is interpretted correctly the result will
be a Clojure anonymous function; if the rule is not interpretted, currently nil
is returned and there's no helpful error message.

### Generated function and evaluation environment

The generated function is a function of two arguments

_ __cell__ a cell in a world as defined in mw-engine.world, q.v.;
_ __world__ the world of which that cell forms part.

It returns a new cell, based on the cell passed.

Actions of the rule will (can only) modify properties of the cell; there are two
properties which are special and SHOULD NOT be modified, namely the properties
__x__ and __y__. Currently there is no policing that these properties are not
modified.

### Grammar

A rule comprises:

+ if _conditions_ then _actions_

#### Conditions

where _conditions_ is:

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
+ more than _number_ neighbours have _property_ equal to _value_
+ fewer than _number_ neighbours have _property_ equal to _value_

#### Actions

Where _actions_ is:

+ _action_ 
+ _action_ and _actions_

and _action_ is:

+ _property_ should be _value_

#### Properties

In the above, _property_ is the name of any property of a cell. Any alpha-numeric
string of characters can form the name of a property. Actions should NOT refer
to the reserved properties __x__ and __y__.

#### Values in Conditions

Values in conditions and actions are considered slightly differently. In a 
condition, a value is one of

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

Note that '...members are...' is equivalent to '...members have state equal to...', 
and 'some neighbours...' is equivalent to 'more than 0 neighbours...'

## License

Copyright © 2014 Simon Brooke

Distributed under the GNU General Public License either version 2.0 or (at
your option) any later version.
