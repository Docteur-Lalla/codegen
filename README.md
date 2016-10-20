CodeGen: a simple multi-language code-skeleton generator
========================================================

CodeGen is a program that read a configuration written in CodeGen's mini-language and compile it into the language specified by the user. Its main purpose is to write the skeleton of a code as fast as possible.

Example:

```
package test:
  class public final TestMain:
    + private myData : String
    - public static main : String[] -> void
```

This test package can be compiled into Java:

```java
package test;

public final class TestMain {
  private String myData;

  public static void main(String[]) {
    
  }
}
```

Installation
============

You can compile the sources using the Makefile. This project uses OCaml, so you need it installed.

Supported languages
===================

* Java
