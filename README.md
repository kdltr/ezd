Introduction
============

This repository contains a port of the EasyDraw graphics server, written by Joel Bartlett in the 90's, to CHICKEN Scheme.

It contains two CHICKEN Scheme modules, `ezd` and `ezd-external` as well as a standalone graphics server executable: `ezd`.

The ezd module is the main piece of this project, it defines all the basic ezd commands as well as the `ezd` procedure to run them.
It also exposes the `read-eval-draw` procedure which starts up an interactive drawing prompt.

The ezd executable is just a regular program that runs the read-eval-draw loop when started.

Finally, the ezd-external module is just a simple helper module that exposes the `ezd` and `read-event` procedures to interact with an automatically started external ezd process.


Installation
============

To compile and install everything, simply run `chicken-install` in the repository’s directory.


Examples
========

You can find a few examples on how to use this program in the _examples/_ directory.

The clock example has three versions:

- clock.scm that uses the ezd module directly.
- clock-external.scm that uses the ezd-external module to communicate to an external ezd process.
- clock-c.c, a C version of the clock-external program, to show that ezd is not strictly tied to Scheme, but can in fact be used from any language.


Bugs
====

There are a number of differencies between Scheme->C (the original Scheme implementation EZD was running on) and CHICKEN Scheme, so bugs might be lurking around.

Some bugs I already know about:

- window redrawing doesn’t seem to occur correctly, sometimes the window stays blank until the next focus change.
- the textdrawing module doesn’t accept utf-8 characters outside of ASCII, and behaves strangely with new lines and backspace.

If you find some, send them my way by email to [kooda@upyum.com](mailto:kooda@upyum.com).


See also
========

Here you can find the original report about this software: [Don’t Fidget with Widgets, Draw!](http://www.hpl.hp.com/techreports/Compaq-DEC/WRL-91-6.pdf).

Have fun!