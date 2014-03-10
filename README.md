**This project is in its infancy and the infant mortality of dev projects is very high. Use at your own risks...**

# Yet Another Template Engine

A Simple template engine initially created to speed-up generation of new projects in various languages, first of all Haskell.

# Rationale

It has become commonplace in various languages to provide *bootstrappers*, usually in the form of command-line utilities, that
create new instances of a project in a the target language given some basic project description, usually sorting out dependencies.
In the java world, one can use
[maven archetypes](https://maven.apache.org/guides/introduction/introduction-to-archetypes.html), in clojure we use
[lein new](https://github.com/technomancy/leiningen), in javascript there is [bower](https://github.com/bower/bower), in scala
[conscript](https://github.com/n8han/conscript/)...

There is nothing similar in Haskell and each and every time I start a new development I have to copy/paste existing project to get
a walking skeleton. So I started working on a small template-based generator that would allow me to maintain various projects
skeletons and instantiate them at will.

# Usage

> yate <template id> <output directory> <template descriptor>

## Template Id

One of:

* A directory inside the directory referenced by `YATE_TEMPLATES` environment variable, defaulting to current directory,
* A github-hosted project under the current user id,
* A full github URI to a project.

## Template Descriptor

A Haskell data-structure providing objects to instantiate variables, for example:

```
"project" :>: L [
    "name" :>: S "myproj",
    "version" :>: S "1.0",
    "synopsis"  :>: S "This is a sample project",
    "authors" :>: L [
        "author" :>: L [
            "name":>: S "Arnaud Bailly <arnaud.oqube@gmail.com>",
            "year" :>: S "2014"]]]
```

# Templates

* Use [mustache](http:://mustache.github.io) syntax,
* May contain a `.mapping` file that is itself a template defining how to map source files to target files.

# TODO

* Use [hastache](https://github.com/lymar/hastache)
* Use pure JSON as project descriptor
