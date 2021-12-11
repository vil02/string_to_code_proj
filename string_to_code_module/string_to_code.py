# -*- coding: utf-8 -*-
"""
Created on Fri Dec 10 09:13:59 2021

@author: idzipio
"""
import random


def str_pieces(in_str, in_pieces_len):
    assert all(_ > 0 for _ in in_pieces_len)
    assert sum(in_pieces_len) == len(in_str)
    cur_str = in_str
    res = []
    for _ in in_pieces_len:
        res.append(cur_str[:_])
        cur_str = cur_str[_:]
    assert ''.join(res) == in_str
    return res


# def random_pieces_len(in_total_len):
#     small_len = 6
#     if in_total_len == 1:
#         res = [1]
#     elif in_total_len < small_len and random.randint(0, 1) == 0:
#         res = [in_total_len]
#     else:
#         max_tmp_len = min(small_len, in_total_len-1)
#         tmp_len = random.randint(1, max_tmp_len)
#         res = [tmp_len]+random_pieces_len(in_total_len-tmp_len)

#     random.shuffle(res)
#     assert sum(res) == in_total_len
#     return res

# def random_pieces_len(in_total_len, in_max_piece_num):
#     piece_num = random.randint(1, min(in_total_len, in_max_piece_num))
#     if in_total_len > 1 and piece_num == 1:
#         piece_num = 2
#     cur_num = in_total_len
#     res = []

#     for _ in range(piece_num-1):
#         if cur_num == 0:
#             break
#         tmp_len = random.randint(1, cur_num)
#         res.append(tmp_len)
#         cur_num -= tmp_len
#     if cur_num > 0:
#         res.append(cur_num)
#     random.shuffle(res)
#     assert sum(res) == in_total_len
#     assert len(res) <= in_max_piece_num
#     return res

def random_pieces_len(in_total_len):

    cur_num = in_total_len
    res = []

    while cur_num > 0:
        tmp_len = random.randint(1, max(cur_num-1, 1))
        res.append(tmp_len)
        cur_num -= tmp_len
    # if cur_num > 0:
    #     res.append(cur_num)
    random.shuffle(res)
    assert sum(res) == in_total_len
    return res

def random_split(in_str):
    return str_pieces(in_str, random_pieces_len(len(in_str)))

def str_to_function_stack(in_str):

    cur_fun_num = 0
    def get_function_name():
        return f"f_{cur_fun_num}"

    function_stack = []
    known_codes = {}
    def get_function_code(in_fun_name, in_needed_fun_list):
        res = '\n'.join(
            [f"inline void {in_fun_name}()",
             '{',
             *['    '+_ for _ in in_needed_fun_list],
             '}'])
        return res

    def generate_code(in_str):
        nonlocal cur_fun_num
        if in_str in known_codes:
            res = known_codes[in_str]
        elif len(in_str) == 1:
            # res = 'print_char<\''+str(in_str)+'\'>();'
            res = 'std::putchar(\''+str(in_str)+'\');'
        else:
            cur_function_name = get_function_name()
            cur_fun_num += 1
            res = f"{cur_function_name}();"
            needed_functions = [generate_code(_) for _ in random_split(in_str)]
            if len(needed_functions) == 1:
                print(in_str)
            function_stack.append(
                get_function_code(cur_function_name, needed_functions))
            known_codes[in_str] = res
        return res
    code = generate_code(in_str)
    return code, function_stack, known_codes



def str_to_cpp(in_str):
    initial_fun, function_list, codes = str_to_function_stack(in_str)
    function_list = '\n\n'.join(function_list)
    # print_fun_str = '\n'.join(
    #     ['template <char in_char>',
    #      'inline void print_char()',
    #      '{',
    #      '    std::cout<<in_char;',
    #      '}'])
    main_str = '\n'.join(
        ['int main()',
         '{',
         f'    {initial_fun}',
         '    return 0;',
         '}',
         ''])
    res = '\n\n'.join(
        ['#include <cstdio>',
         function_list,
         main_str])
    res = res.replace("\'\n\'", "\'\\n\'")
    res = res.replace("\'\t\'", "\'\\t\'")
    res = res.replace("\'\'\'", "\'\\\'\'")
    res = res.replace("(\'\\')", "(\'\\\\')")
    return res


# for _ in range(40):
#     print(random_pieces_len(random.randint(100, 300), 5))


# STR = 'Hello world mother fucker!'
# for _ in range(2):
#     print(len(STR))
#     STR, codes = str_to_cpp(STR)
#
# with open('res.cpp', 'w') as f:
#     f.write(STR)
# X = ''
