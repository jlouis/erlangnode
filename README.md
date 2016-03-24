# Introduction

*Work in Progress.*

The goal is to provide a mapping between Erlang Terms and OCaml. This
allows a programmer to write computations in OCaml and marshal them
easily between Erlang and OCaml.

The current implementation is based on a port-program and is only
tested for a subset of all possible Erlang Terms. But the work done
means we can eventually let OCaml handle all of the epmd communication
and thus write hidden nodes in OCaml.
