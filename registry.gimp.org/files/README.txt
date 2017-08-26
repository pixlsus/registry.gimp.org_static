Introduction
============

This is a script for The GIMP, a image manipulation program.  Basically it
consists in drawing circles with sequencial numbers inside, so some can
"label" parts of a image to reference in another document.


Installing
==========

To install it, just move the script to your local ``scripts`` directory, in
Unix systems it is located at ``~/.gimp-2.x/scripts``.  I don't know about
Windows.

If GIMP is opened, just select Filter -> Script-Fu -> Refresh menu.  If Gimp
is not open, just open it.  The menu associated with the script will appear
on Script-Fu -> Mark Number Circles.


Using
=====

Before starting, select your foreground and background color.  Foreground
color will be text color and background color will be circle color.

First you need to draw, on your image, a path with the Path/Vector tool.
Don't worry about it being a Bezier curve or whatever, but place the nodes
precisely where you want the labels to appear.

After drawing the path, select the Script-Fu -> Mark Number Circles and watch
a circle and a number appear in every node of your path.
