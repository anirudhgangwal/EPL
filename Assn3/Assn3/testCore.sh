#!/usr/bin/env bash
scala -cp ".:spc.jar:scalatest.jar" org.scalatest.run Assignment3Tests.TestCore.CoreSubstSpec
scala -cp ".:spc.jar:scalatest.jar" org.scalatest.run Assignment3Tests.TestCore.CoreEvalSpec
