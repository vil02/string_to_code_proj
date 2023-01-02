#!/usr/bin/env bash

set -euo pipefail

fun_b ()
{
    printf "{"
    printf "%%"
}


fun_a ()
{
    fun_b
    printf "\n"
    fun_b
}


fun_a
