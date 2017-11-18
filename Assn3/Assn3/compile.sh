#!/usr/bin/env bash
scalac -Xlint -cp ".:spc.jar:scalatest.jar" *.scala
scalac -Xlint -cp ".:spc.jar:scalatest.jar" tests/*.scala
