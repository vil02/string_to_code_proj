#!/usr/bin/env bash

set -euo pipefail

fun_0 ()
{
    true
}


fun_1 ()
{
    fun_0
    printf "C"
}


fun_1
