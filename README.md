# mw-parser

A rule parser for MicroWorld

## Usage

Main entry point is (parse-rule <string>), where string takes a form detailed
in **grammar**, below. If the rule is interpretted correctly the result will
be a Clojure anonymous function; if the rule is not interpretted, currently nil
is returned and there's no helpful error message.

### Generated function and evaluation environment

The generated function is a function of two arguments

* **cell** a cell in a world as defined in mw-engine.world, q.v.;
* **world** the world of which that cell forms part.

It returns a new cell, based on the cell passed.

Actions of the rule will (can only) modify properties of the cell; there are two
properties which are special and SHOULD NOT be modified, namely the properties
**x** and **y**. Currently there is no policing that these properties are not
modified.

### Grammar

A rule comprises:

*    if *conditions* then *actions*

#### Conditions

where *conditions* is:

*    *condition* 

or
*	*condition* and *conditions*

A *condition* is one of:

*	*property* is *value*
*	*property* is not *value*
*	*property* is in *values*
*	*property* is not in *values*
*	*property* is more than *numeric-value*
*	*property* is less than *numeric-value*
*	*number* neighbours have *property* equal to *value*
*	more than *number* neighbours have *property* equal to *value*
*	fewer than *number* neighbours have *property* equal to *value*

#### Actions

Where *actions* is:

*	*action*

or
*	*action* and *actions*

and *action* is:

*	*property* should be *value*

#### Properties

In the above, *property* is the name of any property of a cell. Any alpha-numeric
string of characters can form the name of a property. Actions should NOT refer
to the reserved properties **x** and **y**.

#### Values in Conditions

Values in conditions and actions are considered slightly differently. In a 
condition, a value is one of

*   *symbolic-value*
*   *numeric-value*

The '...more than...' and '...less than...' conditions imply a *numeric-value*.
Thus "if altitude is more than fertility..." is interpreted as meaning "if the value 
of the property of the current cell called 'altitude' is greater than the value
of the property of the current cell called 'fertility'", whereas the apparently
similar condition 'if altitude is fertility...' is interpreted as meaning
"if the value of the property of the current cell called 'altitude' is the symbol
'fertility'".

Thus *symbolic-value* is any sequence of alphanumeric characters, whereas 
*numeric-value* is one of:

*    *number*
*    *property*

and *number* is any sequence of the decimal digits 0...9, the minus character 
'-' and the period character '.', provided that the minus character can only be 
in the first position, and  the period character can only appear once.

#### Values in Actions

A *value* in an action is one of

*    *symbolic-value*
*    *arithmetic-value*
*    *number*

where *arithmetic-value* is:

*    *property* *operator* *numeric-value*

and *operator* is one of the simple arithmetic operators '+', '-', '*' and '/'.

### Shorthand

Note that '...members are...' is equivalent to '...members have state equal to...', 
and 'some neighbours...' is equivalent to 'more than 0 neighbours...'

## License

Copyright © 2014 Simon Brooke

Distributed under the GNU General Public License either version 2.0 or (at
your option) any later version.
