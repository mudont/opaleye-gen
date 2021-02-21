# Opaleye-gen

This is intended a small little tool to generate all the intial boiler plate
needed to use Opaleye. While it could in some cases be run continously as the
database updates, it's not going to be possible in all cases because you might
have some custom database values that you want to parse into proper haskell
types, thus needing to change the generate boilerplate.

To use, just run it with

```
opaleye-gen -d postgresql://localhost/your_database -o Database.hs
```

That will read the specification of `your_database` from the database and
generate a file named `Database.hs` with all of the necessary Opaleye types to
start writing queries from.

# Changes in this fork
- Update to newer stack version, use hpack
- use optparse-generic just for the heck of it
- Replace a few deprecated things in generated code
- Generate Show instances for Database types