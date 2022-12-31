#!/usr/bin/env bash

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
