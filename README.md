## Language Interpreter Design (CSE 305 Buffalo)

### How to use

```sh
$ git clone ...
$ cd root of project
$ sbt
stb:cs305> test
stb:cs305> console
scala> import io.github.mapogolion.cs305.buffalo.Main
scala> Main.channel("./resources/input.txt", "./resources/output.txt")
```
Enter `:quit` for terminated session

TODO: add description
## sbt project compiled with Dotty

### Usage

This is a normal sbt project, you can compile code with `sbt compile` and run it
with `sbt run`, `sbt console` will start a Dotty REPL.

For more information on the sbt-dotty plugin, see the
[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).
