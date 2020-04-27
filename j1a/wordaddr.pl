#! /usr/bin/perl -lan

($addr, $data) = split(" ", $_, 2);
$a = eval("0x".$addr);
printf ("%04x %04x: %s\n", $a, $a/2, $data);

