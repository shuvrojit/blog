+++
title = "Effecient Keybindings"
author = ["Shuvrojit Biswas"]
date = 2023-03-06
lastmod = 2023-04-20T23:31:24+06:00
tags = ["emacs"]
categories = ["productivity"]
draft = false
weight = 1001
+++

## Movement &amp; Keybindings {#movement-and-keybindings}

I have used interception tools to make my keybindings easier and more simpler. I have remapped my `crtrl` key to my left hand side key `d`, to my right hand side key `k` and also the `Capslock` key. And again I have remapped my `alt` key to my left hand side key `f`, to my right hand side key `j`. I have remapped my `shift` key to `space` key. All this remap only works when I'm holding down these keys. You can set keypress time for holding down and single button press timing. I have also remapped `shift` key as `esc`, which I need for my vim style bindings. ESC cancels all or single press shift cancels all.

`M-'` puts a - between letters.


## Text Editing {#text-editing}

Text Editing should be as easy and very flexible. People have their own weaknesses and therefore it should be different for each people. We are going to discuss Text editing in a few moments.

Emacs's default keybinding for editing text is very diffcult. You have to move your fingers a lot. You have to always press the CTRL key. But it was designed in the 70's for that times keyboard.

Modern World needs modern keybindings for editing text fast and smoothly. Vim comes to play in this times. Vim is also a lot old like the I don't know maybe 80-90's. It was first Vi, then it was vim.

Basically Vim has these things called modes. Which is like a different keybindings at a different state. There are a lot of modes. But we will be focusing on NORMAL, INSERT and VISUAL mode today.

When you open vim for the first time you're gonna be in NORMAL mode. Press i, it will take you to INSERT mode which is where you can type and edit files.

These are the basic keybindings of vim and emacs together. And it really is the best editor of the century.

i is for inside and a is for outside
relative jumps.
c is just like d but instead of going to the normal mode, you go into insert mode.
C is like c-k `kill-line`
s and S
{} up and down a paragraph.
jump list with ctrl-o.
Shift+V visually select full line.
di{ select inside curly braces
vi{
yi{
press o when in visual mode. end of visual mode, begining of visual mode
viw - yank
viW - :nonwhitespace yank

{kkd}
**VIM keybindings**:

| yy  | copy line           |
|-----|---------------------|
| yiw | copy current word   |
| diw | delete current word |

CTRL always operates on characters and Meta always operates on words.

| C-t | transpose char with previous char |
|-----|-----------------------------------|
| M-t | transpose word with previous word |
| M-u | upcase word                       |
| M-l | downcase word                     |
| M-c | capitalize word                   |
| C-k | kill-line                         |
| M-d | kill-word(forward)                |
| M-v | move screen window down           |
