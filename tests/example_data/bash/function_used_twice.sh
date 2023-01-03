#!/usr/bin/env bash

set -euo pipefail

fun_0 ()
{
    printf "{"
    printf "%%"
}


fun_1 ()
{
    fun_0
    printf "\n"
    fun_0
}


fun_1
