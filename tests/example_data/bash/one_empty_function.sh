#!/usr/bin/env bash

set -euo pipefail

fun_b ()
{
    true
}


fun_a ()
{
    fun_b
    printf "C"
}


fun_a
