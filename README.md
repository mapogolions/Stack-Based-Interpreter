## [Deprecated] Language Interpreter Design (CSE 305 Buffalo)

### How to use

```sh
$ git clone ...
$ cd root of project
$ sbt
sbt:cs305> test
sbt:cs305> console
scala> import io.github.mapogolions.cs305.buffalo.Main
scala> Main.channel("./resources/src/factorial.txt", "./resources/bin/factorial.txt")
scala> Main.channel("./resources/src/your-file", "./resources/bin/your-file")
```

To get out of `sbt:cs305`, type `exit`

To get out of `scala`, type `:quit`

### Examples
- [factorial](./resources/src/factorial.txt)
- [high order function](./resources/src/hoc.txt)
- [variable shadowing](./resources/src/shadowing.txt)
- [static scope](./resources/src/static-scope.txt)

## sbt project compiled with Dotty

### Usage

This is a normal sbt project, you can compile code with `sbt compile` and run it
with `sbt run`, `sbt console` will start a Dotty REPL.

For more information on the sbt-dotty plugin, see the
[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).
