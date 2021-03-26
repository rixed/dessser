# Code generator tailored at data manipulation

Can generate (de)serialiers, converters, filters...

Supports several back-ends and various external data formats.

As of today:

Backends:
- OCaml
- C++

External formats:
- Clickhouse's row-binary
- Ramen's ringbuffers
- CSV
- S-Expressions

# Suggested reading order

1. DessserTypes.ml defines the supported data types

Dessser supports most compound data types, up to and including sum and product types (aka. tagged unions and tuples).
There is also limited support for user defined types.
There is no support for type parameters, though. In other words, users cannot define polymorphic types.

Roughly, types are organized in three classes:

- the types that can store user manipulable values, belonging to the
  `value_type` class. Those types can be (de)serialized and manipulated in many
  ways.

- often times, values (of some value type) are optional (aka null or unknown). So the class `maybe_nullable` extends the value types with a boolean indicating whether these values can be null.

- Finally, some extra types are added to support low level operations (pointers, machine sized words...)

2. DessserExpressions.ml defines the supported operations

The level of abstraction tries to maintain a good balance between simplicity for the user and for the back-end.

3. DessserBackEndOCaml.ml implements the OCaml back-end (the simplest)

4. Dessser.ml implements the (de)serializers and converters (parameterized with encodings)

Given a data type and an encoding, Dessser.ml can generate a serializer, a deserializer, a converter between two encoding, etc.

5. DessserSExpr.ml implements the simplest encoding: s-expressions

6. DessserHeapValue.ml implement the special encoding for memory hold values

Deserializing a value consists of converting from serial buffer into a memory "reified" value. DessserHeapValue construct an expression that will build that value (in any chosen back-end).

Likewise, serializing a value consists of building an expression that iterate through a memory value and write it in a buffer.

Additionally, DessserHeapValue can also build an expression that computes the size of the serialized value, without serializing it (come handy if preallocating the buffer is necessary).

7. DessserStdLib.ml implements various helper functions

As some expressions are a bit low-level, some commonly used more complex expressions are implemented as a library.
