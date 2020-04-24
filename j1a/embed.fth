\ # embed.fth
\
\	| Project    | A Small Forth VM/Implementation   |
\	| ---------- | --------------------------------- |
\	| Author     | Richard James Howe                |
\	| Copyright  | 2017-2018 Richard James Howe      |
\	| License    | MIT                               |
\	| Email      | howe.r.j.89@gmail.com             |
\	| Repository | <https://github.com/howerj/embed> |
\
\ ## A Meta-compiler, an implementation of eForth, and a tutorial on both.
\
\ ## Introduction
\
\ In this file a meta-compiler (or a cross compiler written in [Forth][]) is
\ described and implemented, and after that a working Forth interpreter
\ is both described and implemented. This new interpreter can be used in turn
\ to meta-compile the original program, ad infinitum. The design decisions
\ and the philosophy behind Forth and the system will also be elucidated.
\
\ This files source is a Forth program which the meta-compiler can read, it
\ is also used as source for a simple document generation system using [AWK][]
\ which feeds into either [Pandoc][] for [PDF][] or the original [Markdown][]
\ script for [HTML][] output. The AWK script is crude and requires that the
\ Forth source file, [embed.fth][] be formatted in a specific way.
\
\ Lines beginning with a back-slash are turned into normal Markdown text, with
\ some characters needed to be escaped. Other lines are assumed to be Forth
\ source code and are turned into Markdown code/literal-text blocks with
\ a number and '|' symbol preceding them, numbering starts from line '0001'.
\
\ ### What is Forth?
\
\ Forth is a stack based procedural language, which uses Reverse Polish
\ Notation (RPN) to enter expressions. It is a minimal language with no type
\ checking and little to no error handling depending on the implementation.
\ Despite its small size and simplicity it has various features usually found
\ in higher level languages, such as reflection, incremental compilation and
\ an interactive read-evaluate-print loop.
\
\ It is still at heart a language that is close to the machine with low
\ level capabilities and direct access to memory. Memory manage itself is
\ mostly manual, with preallocation of all needed memory the preferred method
\ of program writing.
\
\ Forth has mostly fallen out of favor in recent years, it performed admirably
\ on the microcomputer systems available in the 1980s and is still suitable
\ for very memory constrained embed systems (having on a few kilobytes of
\ memory available to them), but lacks a lot of features modern languages
\ provide.
\
\ A catalogue of deficiencies hamper Forth adoption; poor string handling,
\ lack of libraries and poor integration with the operating system (on hosted
\ platforms), mutually incompatible and wildly different Forth implementations
\ and as mentioned - little error detection and handling.
\
\ Despite this fact it has a core of adherents who find uses for the language,
\ in fact some of its deficiencies are actually trade-offs. Having no type
\ checking means there is no type checking to do, having very little in the way
\ of error detection means errors do not have to be detected. This off loads
\ the complexity of the problem to the programmer and means a Forth
\ implementation can be minimal and terse.
\
\ The saying "Once you have seen one Forth implementation, you have seen
\ one Forth implementation" comes about because of how easy it is to implement
\ a Forth, which is a double edged sword. It is possible to completely
\ understand a Forth system, the software, the hardware and the problems you
\ are trying to solve and optimize everything towards this goal. This is oft
\ not possible with modern systems, a single person cannot totally understand
\ even subcomponents of modern systems in its entirety (such as compilers
\ or the operating system kernels we use).
\
\ Another saying from the creator of Forth, Charles Moore,
\ "Forth is Sudoku for programmers". The reason the author uses Forth is
\ because it is fun, no more justification is needed.
\
\ ### Project Origins
\
\ This project derives from a simulator for a [CPU written in VHDL][], designed
\ to execute Forth primitives directly. The CPU and Forth interpreter
\ themselves have their own sources, which all makes for a confusing pedigree.
\ The CPU, called the H2, was derived from a well known Forth CPU written
\ in [Verilog][], called the [J1 CPU][], and the Forth running on the H2 comes
\ from an adaption of [eForth written for the J1][]  and
\ from 'The Zen of eForth' by C. H. Ting.
\
\ Instead of a metacompiler written in Forth a cross compiler for a Forth like
\ language was made, which could create an image readable by both the
\ simulator, and the FPGA development tools. The simulator was cut down and
\ modified for use on a computer, with new instructions for input and output.
\
\ This system, with a cross compiler and virtual machine written in C, was
\ used to develop the present system which consists of only the virtual
\ machine, a binary image containing a Forth interpreter, and this metacompiler
\ with the meta-compiled Forth. These changes and the discarding of the cross
\ compiler written in C can be seen in the Git repository this project comes
\ in (<https://github.com/howerj/embed>). The VM here is no longer compatible
\ with the [H2 CPU][] it was derived from.
\
\ The project, documentation and Forth images are under an [MIT license][].
\
\ ### The Virtual Machine
\
\ The virtual machine is incredibly simple and cut down at around 200 lines of
\ C code, with most of the code being not being the virtual machine itself,
\ but code to get data in and out of the system correctly, or setting the
\ machine up. It is described in the appendix (at the end of this file), which
\ also contains an example implementation of the virtual machine.
\
\ The virtual machine is 16-bit dual stack machine with an instruction set
\ encoding which allows for many Forth words to be implemented in a single
\ instruction. As the CPU is designed to execute Forth, Subroutine Threaded
\ Code (STC) is the most efficient method of running Forth upon it.
\
\ What you are reading is itself a Forth program, all the explanatory text is
\ are Forth comments. The file is fed through a preprocessor to turn it into
\ a [Markdown][] file for further processing.
\
\ Many Forths are written in an assembly language, especially the ones geared
\ towards microcontrollers, although it is more common for new Forth
\ interpreters to be written in C. A metacompiler is a [Cross Compiler][]
\  written in Forth.
\
\ ### References
\
\ * 'The Zen of eForth' by C. H. Ting
\ * <https://github.com/howerj/embed> (This project)
\ * <https://github.com/howerj/libforth>
\ * <https://github.com/howerj/forth-cpu>
\
\ #### Jones Forth:
\
\ * <https://rwmj.wordpress.com/2010/08/07/jonesforth-git-repository/>
\ * <https://github.com/AlexandreAbreu/jonesforth>
\
\ #### J1 CPU:
\
\ * <http://excamera.com/files/j1.pdf>
\ * <http://excamera.com/sphinx/fpga-j1.html>
\ * <https://github.com/jamesbowman/j1>
\ * <https://github.com/samawati/j1eforth>
\
\ #### Meta-compilation/Cross-Compilation:
\
\ * <http://www.ultratechnology.com/meta1.html>
\ * <https://en.wikipedia.org/wiki/Compiler-compiler#FORTH_metacompiler>
\
\ The Virtual Machine is specifically designed to execute Forth, it is a stack
\ machine that allows many Forth words to be encoded in one instruction but
\ does not contain any high level Forth words, just words like *@*, 'r>' and
\ a few basic words for I/O. A full description of the virtual machine is
\ in the appendix.
\
\ ## Metacompilation wordset
\
\ This section defines the metacompilation wordset as well as the
\ assembler. The metacompiler, or cross compiler, requires some assembly
\ instructions to be defined so the two word sets are interlinked.
\
\ A clear understanding of how Forth vocabularies work is needed before
\ proceeding with the tutorial. Vocabularies are the way Forth manages
\ namespaces and are generally talked about that much, they are especially
\ useful (in fact pretty much required) for writing a metacompiler.

.( FORTH META COMPILATION START ) cr

only forth definitions hex
system +order
variable meta          ( Metacompilation vocabulary )
meta +order definitions

variable assembler.1         ( Target assembler vocabulary )
variable target.1            ( Target dictionary )
variable tcp                 ( Target dictionary pointer )
variable tlast               ( Last defined word in target )
variable tdoVar              ( Location of doVar in target )
variable tdoConst            ( Location of doConst in target )
variable tdoNext             ( Location of doNext in target )
variable tdoPrintString      ( Location of .string in target )
variable tdoStringLit        ( Location of string-literal in target )
variable fence               ( Do not peephole optimize before this point )
1984 constant #version       ( Version number )
5000 constant #target        ( Location where target image will be built )
2000 constant #max           ( Max number of cells in generated image )
2    constant =cell          ( Target cell size )
-1   constant optimize       ( Turn optimizations on [-1] or off [0] )
0    constant swap-endianess ( if true, swap the endianess )
$4100 constant pad-area      ( area for pad storage )
$7FFF constant (rp0)         ( start of return stack in *cells* )
$2400 constant (sp0)         ( start of variable stack in *cells* )
variable header -1 header !  ( if true target headers generated )

( 1   constant verbose ( verbosity level, higher is more verbose )
#target #max 0 fill    ( Erase the target memory location )

: ]asm assembler.1 +order ; immediate    ( -- )
: a: current @ assembler.1 current ! : ; ( "name" -- wid link )
: a; [compile] ; current ! ; immediate   ( wid link -- )

: ( [char] ) parse 2drop ; immediate ( "comment" -- discard until parenthesis )
: \ source drop @ >in ! ; immediate  ( "comment" -- discard until end of line )
: there tcp @ ;                      ( -- a : target dictionary pointer value )
: tc! #target + c! ;                 ( u a -- : store character in target )
: tc@ #target + c@ ;                 ( a -- u : retrieve character in target )
: [last] tlast @ ;                   ( -- a : last defined word in target )
: low  swap-endianess 0= if 1+ then ; ( b -- b : low byte at address )
: high swap-endianess    if 1+ then ; ( b -- b : high byte at address)
: t! over $FF and over high tc! swap 8 rshift swap low tc! ; ( u a -- )
: t@ dup high tc@ swap low tc@ 8 lshift or ; ( a -- u )
: 2/ 1 rshift ;                ( u -- u : non-standard definition divide by 2 )
: 2* 1 lshift ;                ( u -- u : multiple by two, non-standard )
: talign there 1 and tcp +! ;  ( -- : align target dictionary pointer value )
: tc, there tc! 1 tcp +! ;     ( c -- : write byte into target dictionary )
: t,  there t!  =cell tcp +! ; ( u -- : write cell into target dictionary )
: tallot tcp +! ;              ( n -- : allocate memory in target dictionary )
: update-fence there fence ! ; ( -- : update optimizer fence location )
: $literal                     ( <string>, -- )
  [char] " word count dup tc, 1- for count tc, next drop talign update-fence ;
: tcells =cell * ;             ( u -- a )
: tbody 1 tcells + ;           ( a -- a )
: tcfa cfa ;                   ( PWD -- CFA )
: tnfa nfa ;                   ( PWD -- NFA )
: meta! ! ;                    ( u a --  )
: dump-hex #target there $10 + dump ; ( -- )
: locations ( -- : list all words and locations in target dictionary )
 target.1 @
 begin
   ?dup
 while
   dup
   nfa count type space dup
   cfa >body @ u. cr
   $3FFF and @
 repeat ;
: display ( -- : display metacompilation and target information )
  hex
  ." COMPILATION COMPLETE" cr
(   verbose 1 u> if )
(     dump-hex cr )
(     ." TARGET DICTIONARY: " cr )
(     locations )
(   then )
  ." HOST:   " here        . cr
  ." TARGET: " there       . cr
  ." HEADER: " #target $30 dump cr ;

$26 constant (header-options)

: checksum #target there crc ; ( -- u : calculate CRC of target image )

: save-hex ( -- : save target binary to file )
   #target #target there + (save) throw ;

: finished ( -- : save target image and display statistics )
   display
   only forth definitions hex
   ." SAVING..." ." save-hex" ." DONE" cr
   ." STACK>" .s cr ;

\ ### The Assembler

: [a] ( "name" -- : find word and compile an assembler word )
  bl word assembler.1 search-wordlist 0= abort" [a]? "
  cfa compile, ; immediate

: asm[ assembler.1 -order ; immediate ( -- )

\ There are five types of instructions, which are differentiated from each
\ other by the top bits of the instruction.

a: #literal $8000 a; ( literal instruction - top bit set )
a: #alu     $6000 a; ( ALU instruction, further encoding below... )
a: #call    $4000 a; ( function call instruction )
a: #?branch $2000 a; ( branch if zero instruction )
a: #branch  $0000 a; ( unconditional branch )

\ An ALU instruction has a more complex encoding which can be seen in the table
\ in the appendix, it consists of a few flags for moving values to different
\ registers before and after the ALU operation to perform, an ALU operation,
\ and a return and variable stack increment/decrement.
\
\ Some of these operations are more complex than they first appear, either
\ because they do more than a single line explanation allows for, or because
\ they are not typical instructions that you would find in an actual processors
\ ALU and are only possible within the context of a virtual machine. Operations
\ like '#um/mod' are an example of the former, '#save' is an example of the
\ later.
\
\ The most succinct description of these operations, and the virtual machine,
\ is the source code for it which weighs in at under two hundred lines of
\ C code. Unfortunately this would not include that rationale that led to
\ the virtual machine being the way it is.
\ 
\ ALU Operations - only 4 bits
a: #t      $0000 a; ( T = t )
a: #n      $0100 a; ( T = n )
a: #t+n    $0200 a; ( T = n+t  )
a: #t&n    $0300 a; ( T = T and N )
a: #t|n    $0400 a; ( T = T  or N )
a: #t^n    $0500 a; ( T = T xor N )
a: #~t     $0600 a; ( Invert T )
a: #t==n   $0700 a; ( T = n == t? )
a: #n<t    $0800 a; ( T = n < t, signed version )
a: #t>>1   $0900 a; ( T = t right shift by 1 )
a: #t-1    $0A00 a; ( T == t - 1 )
a: #r      $0B00 a; ( T = Top of Return Stack )
a: #[t]    $0C00 a; ( T = memory[t] )
a: #t<<1   $0D00 a; ( T = t left shift by 1 )
a: #sp@    $0E00 a; ( T = variable stack depth )
a: #nu<t   $0F00 a; ( T = n < t, unsigned )
\ RESERVED: Enable Interrupts
\ RESERVED: Interrupts Enabled?
\ a: #rp@    $1200 a; ( T = return stack depth )
\ a: #t==0   $1300 a; ( T == 0? )
\ RESERVED: CPU-ID
\ RESERVED: Literal
\ UNUSED
\ UNUSED
\ a: #save   $1900 a; ( Save memory disk: n = start, T = end, T' = error )
\ a: #tx     $1A00 a; ( Transmit Byte: t = byte, T' = error )
\ a: #rx     $1B00 a; ( Block until byte received, T = byte/error )
\ a: #bye    $1C00 a; ( Exit Interpreter )
\ a: #cpu    $1D00 a; ( CPU information )

.( ALU_OP ) cr
\ The Stack Delta Operations occur after the ALU operations have been executed.
\ They affect either the Return or the Variable Stack.
\ {rpop, rpsh, dpop, dpsh} = insn[3:0]

a: dpop    $0002 or a; ( decrement variable stack by one )
a: dpsh    $0001 or a; ( increment variable stack by one )
a: dboth   $0003 or a; ( stack remains, but top element changed )
a: rpop    $0008 or a; ( decrement variable stack by one )
a: rpsh    $0004 or a; ( increment variable stack by one )

\ All of these instructions execute after the ALU and stack delta operations
\ have been performed except r->pc, which occurs before. They form part of
\ an ALU operation.

a: r->pc   $0010 or a; ( Set Program Counter to Top of Return Stack )
a: n->a    $0020 or a; ( Set [Top of Variable Stack] to Next on Variable Stack )
a: t->r    $0040 or a; ( Set Top of Return Stack to Top on Variable Stack )
a: t->n    $0080 or a; ( Set Next on Variable Stack to Top on Variable Stack )

\ There are five types of instructions; ALU operations, branches,
\ conditional branches, function calls and literals. ALU instructions
\ comprise of an ALU operation, stack effects and register move bits. Function
\ returns are part of the ALU operation instruction set.

.( STACK ) cr
: ?set dup $F000 and abort" argument too large " ; ( u -- )
a: branch  2/ ?set [a] #branch  or t, a; ( a -- : an Unconditional branch )
a: ?branch 2/ ?set [a] #?branch or t, a; ( a -- : Conditional branch )
a: call    2/ ?set [a] #call    or t, a; ( a -- : Function call )
a: ALU        ?set [a] #alu     or    a; ( u -- : Make ALU instruction )
a: alu                    [a] ALU  t, a; ( u -- : ALU operation )
a: literal ( n -- : compile a number into target )
  dup [a] #literal and if   ( numbers above $7FFF take up two instructions )
    invert recurse  ( the number is inverted, and 'literal' is called again )
    [a] #~t [a] alu ( then an invert instruction is compiled into the target )
  else
    [a] #literal or t, ( numbers below $8000 are single instructions )
  then a;
a: return ( -- : Compile a return into the target )
   [a] #t [a] r->pc [a] rpop [a] alu a;

.( BRANCH ) cr
\ ### Peep hole Optimizer
\
\ The following words implement a primitive peephole optimizer, which is not
\ the only optimization done, but is the major one. It performs tail call
\ optimizations and merges the return instruction with the previous instruction
\ if possible. These simple optimizations really make a lot of difference
\ in the size of meta-compiled program. It means proper tail recursive
\ procedures can be constructed.
\
\ The optimizer is wrapped up in the *exit,* word, it checks a fence variable
\ first, then the previously compiled cell to see if it can replace the last
\ compiled cell.
\
\ The fence variable is an address below which the peephole optimizer should
\ not look, this is to prevent the optimizer looking at data and merging with
\ it, or breaking control structures.
\
\ An exit can be merged into an ALU instruction if it does not contain
\ any return stack manipulation, or information from the return stack. This
\ includes operations such as *r->pc*, or *r+1*.
\
\ A call then an exit can be replaced with an unconditional branch to the
\ call.
\
\ If no optimization can be performed an *exit* instruction is written into
\ the target.
\
\ The optimizer can be held off manually be inserting a *nop*, which is a call
\ or instruction which does nothing, before the *exit*.
\
\ Other optimizations performed by the metacompiler, but not this optimizer,
\ include; inlining constant values and addresses, allowing the creation of
\ headerless words which are named only in the metacompiler and not in the
\ target, and the 'fallthrough;' word which allows for greater code sharing.
\ Some of these optimizations have a manual element to them, such as
\ 'fallthrough;'.
\

: previous there =cell - ;                      ( -- a )
: lookback previous t@ ;                        ( -- u )

\ mask top bits to locate instruction class
: call? lookback $F000 and [a] #call = ;        ( -- t )
\ depends on the jump class having top bits 0000
: call>goto previous dup t@ $0FFF and swap t! ; ( -- )
: fence? fence @  previous u> ;                 ( -- t )
\ does previous alu instruction fiddle with return stack [3:2] or flag r->pc [4]
: safe? lookback $F000 and [a] #alu = lookback $001C and 0= and ; ( -- t )
: alu>return previous dup t@ [a] r->pc [a] rpop swap t! ; ( -- )
: exit-optimize                                 ( -- )
  fence? if [a] return exit then
  call?  if call>goto  exit then
  safe?  if alu>return exit then
  [a] return ;
: exit, exit-optimize update-fence ;            ( -- )

\ ### Meta-Compiler Defining Words and Control Structures
\
\ *compile-only* and *immediate* set bits in the latest defined word for
\ making a word a "compile only" word (one which can only be executed from
\ within a word definition) and "immediate" respectively. None of these
\ are relevant to the execution of the metacompiler so are not checked by it,
\ but are needed when the target Forth is up and running.
\
\ These words affect the target dictionaries word definitions and not the
\ meta-compilers definitions.
\

: compile-only tlast @ tnfa t@ $20 or tlast @ tnfa t! ; ( -- )
: immediate    tlast @ tnfa t@ $40 or tlast @ tnfa t! ; ( -- )

\ *mcreate* creates a word in the metacompilers dictionary, not the targets.
\ For each word we create in the meta-compiled Forth we will need to create
\ at least one word in the meta-compilers dictionary which contains an address
\ of the Forth in the target.
\

: mcreate current @ >r target.1 current ! create r> current ! ;

\ *thead* compiles a word header into the target dictionary with a name
\ given a string. It is used by *t:*.
\

: thead ( b u -- : compile word header into target dictionary )
  header @ 0= if 2drop exit then
  talign
  there [last] t, tlast !
  there #target + pack$ c@ 1+ aligned tcp +! talign ;

\ *lookahead* parses the next word but leaves it in the input stream, pushing
\ a string to the parsed word. This is needed as we will be creating two
\ words with the same name with a word defined later on called *t:*, it
\ creates a word in the meta-compilers dictionary and compiles a word with
\ a header into the target dictionary.
\

: lookahead ( -- b u : parse a word, but leave it in the input stream )
  >in @ >r bl parse r> >in ! ;

\ The word *h:* creates a headerless word in the target dictionary for
\ space saving reasons and to declutter the target search order. Ideally
\ it would instead add the word to a different vocabulary, so it is still
\ accessible to the programmer, but there is already very little room on the
\ target.
\
\ *h:* does not actually affect the target dictionary, it can be used by
\ itself and is called by *t:*. *h:* is used in conjunction with either
\ *fallthrough;* or *t;* (*t;* calls *fallthrough;*). *h:* but does several
\ things:
\
\ 1. It sets *<literal>* to the meta-compilers version of *literal* so that
\ when we are compiling words within a meta-compiled word definition it does
\ the right thing, which is compile a number literal into the target
\ dictionary.
\ 2. It pushes a magic number *$F00D* onto the stack, this popped off and
\ checked for by *fallthrough;*, if it is not present we have messed up a
\ word definition some how.
\ 3. It creates a meta-compiler word in *target.1*, this word-list consists
\ of pointers into the target word definitions. The created word when called
\ compiles a pointer to the word it represents into the target image.
\ 4. It updates the *fence* variable to hold off the optimizer.
\
\ *fallthrough;* allows words to be created which instead of exiting just
\ carry on into the next word, which is a space saving measure and provides
\ a minor speed boost. Due to the way this Forth stores word headers
\ *fallthrough;* cannot be used to fall through to a word defined with a
\ word header.
\
\ *t;* does everything *fallthrough;* does except it also compiles an exit
\ into the dictionary, which is how a normal word definition is terminated.
\
\ The words *[* and *]* could be said to change the state of the meta-compiler,
\ even though the meta-compiler is not effected by the *state* variable. A
\ possible improvement to the meta-compiler and to the eForth image it
\ generates would be to vector words like *quit* so the [Outer Interpreter][]
\ could be replaced by the meta-compiler, or at least words like *interpret*
\ could be vectored. Another improvement would be to load and unload the
\ meta-compilers vocabularies when the new definitions of *:* and *;* are
\ encountered. There are man possibilities, however the improvements given
\ by them would be minor, using the meta-compiler is mostly like writing normal
\ Forth.
\
: literal [a] literal ;       ( u --  )
: [ ' literal   <literal> ! ; ( -- )
: ] ' (literal) <literal> ! ; ( -- )

: h: ( -- : create a word with no name in the target dictionary )
 [compile] [
 $F00D mcreate there , update-fence does> @ [a] call ;

\ *t:* does everything *h:* does but also compiles a header for that word
\ into the dictionary using *thead*. It does affect the target dictionary
\ directly.
: t: ( "name", -- : creates a word in the target dictionary )
  lookahead thead h: ;

: ?unstructured $F00D <> if source type cr 1 abort" unstructured! " then ;
\ @warning: Only use *fallthrough* to fall-through to words defined with *h:*.

: fallthrough; [compile] ] ?unstructured ; ( u -- )
: t; fallthrough; optimize if exit, else [a] return then ;

\ *;;* is used to do the same thing as 'fallthrough; h: <name>' in slightly
\ different way.
\
\	: a b h: cd c d ;; ( define 'a', a normal word, and 'cd' )
\	: cde cd e ;       ( use 'cd', and also do 'e' )
\
\ This is still used for code sharing allowing the tail end of a word to be
\ used within a definition.
\

: ;; t; ?unstructured ;

\ *fetch-xt* is used to check that a variable contains a valid execution token,
\ to implement certain functionality we will need to refer to functions yet
\ to be defined in the target dictionary. We will not be able to use these
\ features until we have defined these functions. For example we cannot use
\ *tconstant*, which defines a constant in the target dictionary, until we
\ have defined the target versions of *doConst*. A reference to this function
\ will be stored in *tdoConst* once it has been defined in the target
\ dictionary.
: fetch-xt @ dup 0= abort" (null) " ; ( a -- xt )

\ *tconstant* as mentioned defines a constant in the target dictionary which
\ is visible in that target dictionary (that is, it has a header and when
\ *words* is run in the target it will be in that list).
\
\ *tconstant* behaves like *constant* does, it parses out a name and pops
\ a variable off of the stack. As mentioned, it cannot be used until *tdoConst*
\ has been filled with a reference to the targets *doConst*. *tconstant* makes
\ a word in the meta-compiler which points to a word it makes in the target.
\ This words purpose when run in the target is to push a constant onto the
\ stack. When the constant is referenced when compiling words with the
\ meta-compiler it does not compile references to the constant, but instead
\ it finds out what the constant was and compiles it in as a literal - which
\ is a small optimization.
: tconstant ( "name", n --, Run Time: -- )
  >r
  lookahead
  thead
  there tdoConst fetch-xt [a] call r> t, >r
  mcreate r> ,
  does> @ tbody t@ [a] literal ;

\ *tvariable* is like *tconstant* expect for variables. It requires *tdoVar*
\ is set to a reference to targets version of *doVar* which pushes a pointer
\ to the targets variable location when run in the target. It does a similar
\ optimization as *tconstant*, it does not actually compile a call to the
\ created variables *doVar* field but instead compiles the address as a literal
\ in the target when the word is called by the meta-compiler.
: tvariable ( "name", n -- , Run Time: -- a )
  >r
  lookahead
  thead
  there tdoVar fetch-xt [a] call r> t, >r
  mcreate r> ,
  does> @ tbody [a] literal ;

\ *tlocation* just reserves space in the target.
: tlocation ( "name", n -- : Reserve space in target for a memory location )
  there swap t, mcreate , does> @ [a] literal ;
: [t] ( "name", -- a : get the address of a target word )
  bl word target.1 search-wordlist 0= abort" [t]?"
  cfa >body @ ;
: [f] ( "name", -- execute word in host Forth vocabulary )
  bl word forth-wordlist search-wordlist 0= abort" [f]?"
  cfa execute ;

\ @warning only use *[v]* on variables, not *tlocations*
: [v] [t] =cell + ; ( "name", -- a )

\ *xchange* takes two vocabularies defined in the target by their variable
\ names, "name1" and "name2", and updates "name1" so it contains the previously
\ defined words, and makes "name2" the vocabulary which subsequent definitions
\ are added to.
: xchange ( "name1", "name2", -- : exchange target vocabularies )
  [last] [t] t! [t] t@ tlast meta! ;

\ These words implement the basic control structures needed to make
\ applications in the meta-compiled program, there are no immediate words
\ and they do not need to be, *t:* and *t;* do not change the interpreter
\ state, once the actual metacompilation begins everything is command mode.
\
\ 'postpone' is useful to make sure that a reference to the target definition
\ is definitely called.
\

: begin  there update-fence ;                ( -- a )
: until  [a] ?branch ;                       ( a -- )
: if     there update-fence 0 [a] ?branch  ; ( -- a )
: skip   there update-fence 0 [a] branch ;   ( -- a )
: then   begin 2/ over t@ or swap t! ;       ( a -- )
: else   skip swap then ;                    ( a -- a )
: while  if swap ;                           ( a -- a a )
: repeat [a] branch then update-fence ;      ( a -- )
: again  [a] branch update-fence ;           ( a -- )
: aft    drop skip begin swap ;              ( a -- a )
: constant mcreate , does> @ literal ;       ( "name", a -- )
: [char] char literal ;                      ( "name" )
: postpone [t] [a] call ;                    ( "name", -- )
: next tdoNext fetch-xt [a] call t, update-fence ; ( a -- )
: exit exit, ;                               ( -- )
: ' [t] literal ;                            ( "name", -- )
: recurse tlast @ tcfa [a] call ;            ( -- )

\ @bug maximum string length is 64 bytes, not 255 as it should be.
: ." tdoPrintString fetch-xt [a] call $literal ; ( "string", -- )
: $" tdoStringLit   fetch-xt [a] call $literal ; ( "string", -- )

\ The following section adds the words implementable in assembly to the
\ metacompiler, when one of these words is used in the meta-compiled program
\ it will be implemented in assembly.

(               ALU     t->n t->r n->a rp  sp   NB. 'r->pc' in 'exit'  )
: nop      ]asm #t                             alu asm[ ;
: dup      ]asm #t      t->n              dpsh alu asm[ ;
: over     ]asm #n      t->n              dpsh alu asm[ ;
: invert   ]asm #~t                            alu asm[ ;
: +        ]asm #t+n                      dpop alu asm[ ;
: swap     ]asm #n      t->n                   alu asm[ ;
: nip      ]asm #t                        dpop alu asm[ ;
: drop     ]asm #n                        dpop alu asm[ ;
: >r       ]asm #n           t->r    rpsh dpop alu asm[ ;
: r>       ]asm #r      t->n         rpop dpsh alu asm[ ;
: r@       ]asm #r      t->n              dpsh alu asm[ ;
: @        ]asm #[t]                           alu asm[ ;
: t>>1     ]asm #t>>1                          alu asm[ ;
: t<<1     ]asm #t<<1                          alu asm[ ;
: =        ]asm #t==n                     dpop alu asm[ ;
: u<       ]asm #nu<t                     dpop alu asm[ ;
: <        ]asm #n<t                      dpop alu asm[ ;
: and      ]asm #t&n                      dpop alu asm[ ;
: xor      ]asm #t^n                      dpop alu asm[ ;
: or       ]asm #t|n                      dpop alu asm[ ;
: sp@      ]asm #sp@    t->n              dpsh alu asm[ ;
: 1-       ]asm #t-1                           alu asm[ ;
\ : rp@      ]asm #rp@    t->n              dpsh alu asm[ ;
: rp@      ]asm #t                             alu asm[ ;

\ : 0=       ]asm #t==0                          alu asm[ ;

\ : yield?   ]asm #bye                           alu asm[ ;
: yield?      ]asm #t                             alu asm[ ;
\ : rx?      ]asm #rx     t->n              dpsh alu asm[ ;
: rx?      ]asm #t                             alu asm[ ;
\ : tx!      ]asm #tx                            alu asm[ ;
: tx?      ]asm #t                             alu asm[ ;
\ : (save)   ]asm #save                     dpop alu asm[ ;
: (save)      ]asm #t                             alu asm[ ;
: rdrop    ]asm #t                   rpop      alu asm[ ;
: dup@     ]asm #[t]    t->n              dpsh alu asm[ ;
\ : dup0=    ]asm #t==0   t->n              dpsh alu asm[ ;
: dup>r    ]asm #t           t->r    rpsh      alu asm[ ;
: 2dup=    ]asm #t==n   t->n              dpsh alu asm[ ;
: 2dupxor  ]asm #t^n    t->n              dpsh alu asm[ ;
: store    ]asm #n      n->a              dpop alu asm[ ;
: 2dup<    ]asm #n<t    t->n              dpsh alu asm[ ;
: rxchg    ]asm #r             t->r            alu asm[ ;
: over-and ]asm #t&n                           alu asm[ ;
: over-xor ]asm #t^n                           alu asm[ ;

\ *for* needs the new definition of *>r* to work correctly.
: for >r begin ;
: meta: : ;
( : :noname h: ; )
: : t: ;
meta: ; t; ;
hide meta:
hide t:
hide t;
]asm #~t              ALU asm[ constant =invert ( invert instruction )
]asm #t  r->pc  rpop  ALU asm[ constant =exit   ( return/exit instruction )
]asm #n  t->r dpop rpsh ALU asm[ constant =>r     ( to r. stk. instruction )

$20   constant =bl         ( blank, or space )
$D    constant =cr         ( carriage return )
$A    constant =lf         ( line feed )
$8    constant =bs         ( back space )
$7F   constant =del        ( delete key )
$1B   constant =escape     ( escape character )

$10   constant dump-width  ( number of columns for *dump* )
$50   constant tib-length  ( size of terminal input buffer )
$40   constant word-length ( maximum length of a word )

$40   constant c/l         ( characters per line in a block )
$10   constant l/b         ( lines in a block )
$F    constant l/b-1       ( lines in a block, less one )
(rp0) 2* constant rp0      ( start of return stack )
(sp0) 2* constant sp0      ( start of variable stack )
$2BAD constant magic       ( magic number for compiler security )
$F    constant #highest    ( highest bit in cell )

\ @todo Move <key>, <emit> to system vocabulary
( Volatile variables )
\ $4000 Unused
$4002 constant last-def    ( last, possibly unlinked, word definition )
$4006 constant id          ( used for source id )
$4008 constant seed        ( seed used for the PRNG )
$400A constant handler     ( current handler for throw/catch )
$400C constant block-dirty ( -1 if loaded block buffer is modified )
$4010 constant <key>       ( -- c : new character, blocking input )
$4012 constant <emit>      ( c -- : emit character )
$4014 constant <expect>    ( "accept" vector )
$4016 constant <tap>       ( "tap" vector, for terminal handling )
$4018 constant <echo>      ( c -- : emit character )
$401A constant context     ( holds current context for search order )
  ( area for context is #vocs large )
$402A constant #tib        ( Current count of terminal input buffer )
$402C constant tib-buf     ( ... and address )
$402E constant tib-start   ( backup tib-buf value )
\ $4100 == pad-area

\ $C  constant vm-options    ( Virtual machine options register )
$1E   constant header-length ( location of length in header )
$20   constant header-crc    ( location of CRC in header )
(header-options) constant header-options ( location of options bits in header )

target.1         +order ( Add target word dictionary to search order )
meta -order meta +order ( Reorder so *meta* has a higher priority )
system           -order ( Remove system vocabulary to previously accidents )
forth-wordlist   -order ( Remove normal Forth words to prevent accidents )

\ # The Target Forth
\ With the assembler and meta compiler complete, we can now make our target
\ application, a Forth interpreter which will be able to read in this file
\ and create new, possibly modified, images for the Forth virtual machine
\ to run.
\
\ ## The Image Header
\
\ The following *t,* sequence reserves space and partially populates the
\ image header with file format information, based upon the PNG specification.
\ See <http://www.fadden.com/tech/file-formats.html> and
\ <https://stackoverflow.com/questions/323604> for more information about
\ how to design binary formats.
\
\ The header contains enough information to identify the format, the
\ version of the format, and to detect corruption of data, as well as
\ having a few other nice properties - some having to do with how other
\ systems and programs may deal with the binary (such as have a string literal
\ *FTH* to help identify the binary format, and the first byte being outside
\ the ASCII range of characters so it is obvious that the file is meant to
\ be treated as a binary and not as text).
\

0        t, \  $0: PC: program counter, jump to start / reset vector
0        t, \  $2: T, top of stack
(rp0)    t, \  $4: RP0, return stack pointer
(sp0)    t, \  $6: SP0, variable stack pointer
0        t, \  $8: Instruction exception vector
$8000    t, \  $A: VM Memory Size in cells
$0000    t, \  $C: VM Options
0        t, \  $E: Shadow PC
0        t, \ $10: Shadow T
(rp0)    t, \ $12: Shadow RP0
(sp0)    t, \ $14: Shadow SP0
$4689    t, \ $16: 0x89 'F'
$4854    t, \ $18: 'T'  'H'
$0A0D    t, \ $1A: '\r' '\n'
$0A1A    t, \ $1C: ^Z   '\n'
0        t, \ $1E: For Length of Forth image, different from VM size
0        t, \ $20: For CRC of Forth image, not entire VM memory
$0001    t, \ $22: Endianess check
#version t, \ $24: Version information
$0001    t, \ $26: Header options

\ ## First Word Definitions
\ The very first definitions are for the first stage boot loader, which can
\ be used to manipulate the image before we do anything else
\

0 tlocation    <cold> ( location of 'cold' )
[t] <cold> 2/ 0  t!   ( set starting word )
[t] <cold> 2/ $E t!   ( set shadow register starting location )

\ After the header and boot-loader words, two short words are defined,
\ visible only to the meta compiler and used by its internal machinery. The
\ words are needed by *tvariable* and *tconstant*, and these constructs cannot
\ be used without them. This is an example of the metacompiler and the
\ meta-compiled program being intermingled, which should be kept to a minimum.

h: doVar   r> ;    ( -- a : push return address and exit to caller )
h: doConst r> @ ;  ( -- u : push value at return address and exit to caller )

\ Here the address of *doVar* and *doConst* in the target is stored in
\ variables accessible by the metacompiler, so *tconstant* and *tvariable* can
\ compile references to them in the target.

[t] doVar   tdoVar   meta!
[t] doConst tdoConst meta!

\ Next some space is reserved for variables which will have no name in the
\ target and are not on the target Forths search order. We do this with
\ *tlocation*. These variables are needed for the internal working of the
\ interpreter but the application programmer using the target Forth can make
\ do without them, so they do not have names within the target.
\
\ A short description of the variables and their uses:
\
\ *cp* is the dictionary pointer, which usually is only incremented in order
\ to reserve space in this dictionary. Words like *,* and *:* advance this
\ variable.
\
\ *root-voc*, *editor-voc*, *assembler-voc*, and *_forth-wordlist* are
\ variables which point to word lists, they can be used with *set-order*
\ and pointers to them may be returned by *get-order*. By default the only
\ vocabularies loaded are the root vocabulary (which contains only a few
\ vocabulary manipulation words) and the forth vocabulary are loaded (which
\ contains most of the words in a standard Forth).
\

\ None of these variables are set to any meaningful values here and will be
\ updated during the metacompilation process.
\

0 tlocation root-voc          ( root vocabulary )
0 tlocation editor-voc        ( editor vocabulary )

\ System Variables
#version constant  ver   ( eForth version )
pad-area tconstant pad   ( pad variable - offset into temporary storage )
$8       constant  #vocs ( number of vocabularies in allowed )
$2       tconstant cell  ( size of a cell in bytes )
$400     tconstant b/buf ( size of a block )

0        tlocation cp    ( Dictionary Pointer: Set at end of file )
0        tlocation _forth-wordlist ( set at the end near the end of the file )
0        tlocation _system ( system specific vocabulary )
$0       tvariable >in   ( Hold character pointer when parsing input )
$0       tvariable state ( compiler state variable )
$0       tvariable hld   ( Pointer into hold area for numeric output )
$A       tvariable base  ( Current output radix )
$0       tvariable span  ( Hold character count received by expect   )
0        tvariable blk   ( current blk loaded, set in *cold* )
$FFFF    tvariable dpl   ( number of places after fraction )
0        tvariable current   ( WID to add definitions to )
xchange _forth-wordlist _system
0        tvariable <literal> ( holds execution vector for literal )
0        tvariable <boot>    ( execute program at startup )
0        tvariable <ok>      ( prompt execution vector )
xchange _system _forth-wordlist

\
\ ## Target Assembly Words
\
\ The first words added to the target Forths dictionary are all based on
\ assembly instructions. The definitions may seem like nonsense, how does the
\ definition of *+* work? It appears that the definition calls itself, which
\ obviously would not work. The answer is in the order new words are added
\ into the dictionary. In Forth, a word definition is not placed in the
\ search order until the definition of that word is complete, this allows
\ the previous definition of a word to be use within that definition, and
\ requires a separate word (*recurse*) to implement recursion.
\
\ However, the words *:* and *;* are not the normal Forth define and end
\ definitions words, they are the meta-compilers and they behave differently,
\ *:* is implemented with *t:* and *;* with *t;*.
\
\ *t:* uses *create* to make a new variable in the meta-compilers
\ dictionary that points to a word definition in the target, it also creates
\ the words header in the target (*h:* is the same, but without a header
\ being made in the target). The word is compilable into the target as soon
\ as it is defined, yet the definition of *+* is not recursive because the
\ meta-compilers search order, *meta*, is higher that the search order for
\ the words containing the meta-compiled target addresses, *target.1*, so the
\ assembly for *+* gets compiled into the definition of *+*.
\
\ Manipulation of the word search order is key in understanding how the
\ metacompiler works.
\
\ The following words will be part of the main search order, in
\ *forth-wordlist* and in the assembly search order.
\

( : nop    nop      ; ( -- : do nothing )
: dup      dup      ; ( n -- n n : duplicate value on top of stack )
: over     over     ; ( n1 n2 -- n1 n2 n1 : duplicate second value on stack )
: invert   invert   ; ( u -- u : bitwise invert of value on top of stack )
: +        +        ; ( u u -- u : addition without carry )
: swap     swap     ; ( n1 n2 -- n2 n1 : swap two values on stack )
: nip      nip      ; ( n1 n2 -- n2 : remove second item on stack )
: drop     drop     ; ( n -- : remove item on stack )
: @        @        ; ( a -- u : load value at address )
: !     store drop  ; ( u a -- : store *u* at address *a* )
: =        =        ; ( u1 u2 -- t : does u2 equal u1? )
: u<       u<       ; ( u1 u2 -- t : is u2 less than u1 )
: <        <        ; ( u1 u2 -- t : is u2 less than u1, signed version )
: and      and      ; ( u u -- u : bitwise and )
: xor      xor      ; ( u u -- u : bitwise exclusive or )
: or       or       ; ( u u -- u : bitwise or )
: 1-       1-       ; ( u -- u : decrement top of stack )
: 0=       0 =      ; ( u -- t : if top of stack equal to zero )
: dup0= dup 0= ;      ( u -- t u : if tos == 0 )
xchange _forth-wordlist _system
: rx?     rx?  0    ; ( -- c t | -1 t : fetch a single character, or EOF )
: tx!     tx! drop  ; ( c -- : transmit single character )
: (save)   drop drop 99 ; ( u1 u2 -- u : save memory from u1 to u2 inclusive )
: vm       vm       ; ( ??? -- ??? : perform arbitrary VM call )
xchange _system _forth-wordlist
there constant inline-start
: exit  exit  fallthrough; compile-only ( -- )
: >r    >r    fallthrough; compile-only ( u --, R: -- u )
: r>    r>    fallthrough; compile-only ( -- u, R: u -- )
: r@    r@    fallthrough; compile-only ( -- u )
: rdrop rdrop fallthrough; compile-only ( --, R: u -- )
there constant inline-end
h: [-1] -1 ;                 ( -- -1 : space saving measure, push -1 )
h: 0x8000 $8000 ;            ( -- $8000 : space saving measure, push $8000 )
h: 2drop-0 drop fallthrough; ( n n -- 0 )
h: drop-0 drop fallthrough;  ( n -- 0 )
h: 0x0000 $0000 ;            ( -- $0000 : space/optimization, push $0000 )
h: state@ state @ ;          ( -- u )
h: first-bit 1 and ;         ( u -- u )
h: in@ >in @ ;               ( -- u )
h: base@ base @ ;            ( -- u )
h: yield!? >r yield? rdrop ; ( u u -- : )
h: ?exit if rdrop exit then ; ( u --, R: xt -- xt| : conditional return )
: 2drop drop drop ;         ( n n -- )
: 1+ 1 + ;                  ( n -- n : increment a value  )
: negate 1- invert ;        ( n -- n : negate a number )
: - negate + ;              ( n1 n2 -- n : subtract n1 from n2 )
h: over- over - ;           ( u u -- u u )
h: over+ over + ;           ( u1 u2 -- u1 u1+2 )
: aligned dup first-bit + ; ( b -- a )
: bye 0 [-1] yield!? ( $38 -throw ) ; ( -- : leave the interpreter )
h: cell- cell - ;           ( a -- a : adjust address to previous cell )
: cell+  cell + ;           ( a -- a : move address forward to next cell )
: cells t<<1 ;              ( n -- n : convert cells count to address count )
: chars 1 t>>1   ;          ( n -- n : convert bytes to number of cells )
: ?dup dup if dup exit then ; ( n -- 0 | n n : duplicate non zero value )
: >  swap  < ;              ( n1 n2 -- t : signed greater than, n1 > n2 )
: u> swap u< ;              ( u1 u2 -- t : unsigned greater than, u1 > u2 )
:  <>  = invert ;           ( n n -- t : not equal )
: 0<> 0= invert ;           ( n n -- t : not equal  to zero )
: 0> 0 > ;                  ( n -- t : greater than zero? )
: 0< 0 < ;                  ( n -- t : less than zero? )
: 2dup over over ;          ( n1 n2 -- n1 n2 n1 n2 )
: tuck swap over ;          ( n1 n2 -- n2 n1 n2 )
: +! tuck @ +  fallthrough; ( n a -- : increment value at *a* by *n* )
h: swap! swap ! ;           ( a u -- )
h: zero 0 swap! ;           ( a -- : zero value at address )
: 1+!   1  h: s+! swap +! ;; ( a -- : increment value at address by 1 )
: 1-! [-1] s+! ;            ( a -- : decrement value at address by 1 )
: 2! ( d a -- ) tuck ! cell+ ! ;      ( n n a -- )
: 2@ ( a -- d ) dup cell+ @ swap @ ;  ( a -- n n )
h: get-current current @ ;            ( -- wid )
h: set-current current ! ;            ( wid -- )
: bl =bl ;                            ( -- c )
: within over- >r - r> u< ;           ( u lo hi -- t )
h: s>d dup 0< ;                       ( n -- d )
: abs s>d if negate exit then ;       ( n -- u )
: source #tib 2@ ;                    ( -- a u )
h: tib source drop ;                  ( -- a )
: source-id id @ ;                    ( -- 0 | -1 )
: rot >r swap r> swap ;               ( n1 n2 n3 -- n2 n3 n1 )
: -rot rot rot ;                      ( n1 n2 n3 -- n3 n1 n2 )
: um+ 2dup u< 0= if swap then over+ swap over swap u< 1 and ; ( u u -- u w )
h: rot-drop rot drop ;                ( n1 n2 n3 -- n2 n3 )
h: d0= or 0= ;                        ( d -- t )
h: dnegate invert >r invert 1 um+ r> + ; ( d -- d )
h: d+ >r swap >r um+ r> + r> + ;         ( d d -- d )
: execute >r ;                   ( cfa -- : execute a function )
h: @execute @ ?dup if execute exit then ;  ( cfa -- )
: lshift begin dup while swap t<<1 swap 1- repeat drop ;
: rshift begin dup while swap t>>1 $7FFF and swap 1- repeat drop ;
: c@ dup@ swap first-bit 3 lshift rshift h: lsb $FF and ;; ( b--c: char load )
: c! ( c b -- : store character at address )
  tuck first-bit 3 lshift dup>r swap lsb swap
  lshift over @
  $FF r> 8 xor lshift and or swap! ;
: here cp @ ;                         ( -- a )
: align here fallthrough;             ( -- )
h: cp! aligned cp ! ;                 ( n -- )
: allot cp +! ;                        ( n -- )
h: 2>r rxchg swap >r >r ;              ( u1 u2 --, R: -- u1 u2 )
h: 2r> r> r> swap rxchg nop ;          ( -- u1 u2, R: u1 u2 -- )
h: doNext 2r> ?dup if 1- >r @ >r exit then cell+ >r ;
[t] doNext tdoNext meta!
: um* ( u u -- ud )
  0 swap ( u1 0 u2 ) $F
  for dup um+ >r >r dup um+ r> + r>
  if >r over um+ r> + then
  next rot drop ;
: *    um* drop ;                     ( n n -- n )
: rp@ 99 ;
h: rp! ( n -- , R: ??? -- ??? : set the return stack pointer )
  r> swap begin dup rp@ = 0= while rdrop repeat drop >r ;
: min 2dup< fallthrough;              ( n n -- n )
h: mux if drop exit then nip ;        ( n1 n2 b -- n : multiplex operation )
: max 2dup > mux ;                    ( n n -- n )
: key ( -- c : return a character )
    begin <key> @execute dup if nip 1 [-1] yield!? then 0= until
    dup [-1] <> ?exit drop bye recurse ;
: /string over min rot over+ -rot - ;  ( b u1 u2 -- b u : advance string u2 )
h: +string 1 /string ;                 ( b u -- b u : )
: count dup 1+ swap c@ ;               ( b -- b u )
h: string@ over c@ ;                   ( b u -- b u c )
h: last get-current @ ;                ( -- pwd )
h: echo <echo> @execute ;              ( c -- )
: emit <emit> @execute ;               ( c -- : write out a char )
: cr =cr emit =lf emit ;               ( -- : emit a newline )
: space     1 fallthrough;             ( -- : emit a space )
h: spaces =bl fallthrough;             ( +n -- )
h: nchars                              ( +n c -- : emit c n times )
   swap 0 max for aft dup emit then next drop ;
h: colon-space [char] : emit space ;   ( -- )
: pick ?dup if swap >r 1- pick r> swap exit then dup ; 
h: ndrop for aft drop then next ; ( 0u...nu n -- : drop n cells )
h: >char dup $7F =bl within if drop [char] _ then ; ( c -- c )
: type 0 fallthrough;                  ( b u -- )
h: typist                              ( b u f -- : print a string )
  >r begin dup while
    swap count r@
    if
      >char
    then
    emit
    swap 1-
  repeat
  rdrop 2drop ;
h: print count type ;                    ( b -- )
h: $type [-1] typist ;                   ( b u --  )
: cmove for aft >r dup c@ r@ c! 1+ r> 1+ then next 2drop ; ( b b u -- )
: fill  swap for swap aft 2dup c! 1+ then next 2drop ;     ( b u c -- )
: catch ( i*x xt -- j*x 0 | i*x n )
  sp@       >r
  handler @ >r
  rp@ handler !
  execute
  r> handler !
  r> drop-0 ;
: throw ( k*x n -- k*x | i*x n )
  ?dup if
    handler @ rp!
    r> handler !
    rxchg ( *rxchg* is equivalent to 'r> swap >r' )
    sp@ swap - ndrop r>
  then ;
h: -throw negate throw ;  ( u -- : negate and throw )
[t] -throw 2/ 4 tcells t!
h: 1depth 1 fallthrough; ( ??? -- : check depth is at least one )
h: ?depth dup 0= if drop exit then sp@ 1- u> if 4 -throw exit then ; ( u -- )
: um/mod ( ud u -- ur uq )
  ?dup 0= if $A -throw exit then
  2dup u<
  if negate $F
    for >r dup um+ >r >r dup um+ r> + dup
      r> r@ swap >r um+ r> or
      if >r drop 1 + r> else drop then r>
    next
    drop swap exit
  then 2drop drop [-1] dup ;
: m/mod ( d n -- r q ) \ floored division
  dup 0< dup >r
  if
    negate >r dnegate r>
  then
  >r dup 0< if r@ + then r> um/mod r>
  if swap negate swap exit then ;
\ : */ >r um* r> m/mod nip ; ( n n n -- )
: /mod  over 0< swap m/mod ; ( n n -- r q )
: mod  /mod drop ;           ( n n -- r )
: /    /mod nip ;            ( n n -- q )
: decimal  $A fallthrough;  ( -- : set base to decimal )
h: base! base ! ;           ( u -- : set base )
: hex     $10 base! ;                      ( -- )
h: radix base@ dup 2 - $23 u< ?exit decimal $28 -throw ; ( -- u )
: hold   hld @ 1- dup hld ! c! fallthrough;     ( c -- )
h: ?hold hld @ pad $80 - u> ?exit $11 -throw ; ( -- )
h: extract dup>r um/mod rxchg um/mod r> rot ;  ( ud ud -- ud u )
h: digit 9 over < 7 and + [char] 0 + ;         ( u -- c )
: #> 2drop hld @ pad over- ;                ( w -- b u )
: #  2 ?depth 0 base@ extract digit hold ;  ( d -- d )
: #s begin # 2dup d0= until ;               ( d -- 0 )
: <# pad hld ! ;                            ( -- )
: sign 0< 0= ?exit [char] - hold ; ( n -- )
h: (.) ( n -- b u : convert a signed integer to a numeric string )
  dup>r abs 0 <# #s r> sign #> ;
h: (u.) 0 <# #s #> ;             ( u -- b u : turn *u* into number string )
: u.r >r (u.) r> fallthrough;    ( u +n -- : print u right justified by +n)
h: adjust over- spaces type ;    ( b n n -- )
h: d5u.r dup fallthrough;        ( u -- u )
h: 5u.r space 5 u.r ;            ( u -- )
( :  .r >r (.)( r> adjust ;      ( n n -- : print n, right justified by +n )
: u.  (u.) h: blt space type ;;  ( u -- : print unsigned number )
:  .  (.) blt ;                  ( n -- print number )
( : >base swap base @ >r base ! execute r> base ! ; )
( : d. $a  '  . >base ; )
( : h. $10 ' u. >base ; )
h: down cell negate and ; ( a -- a : align down )
xchange _forth-wordlist _system
h: lshift-xor lshift xor ;
h: rshift-xor rshift xor ;
: crc ( b u -- u : calculate ccitt-ffff CRC )
  [-1] ( -1 = 0xffff ) >r
  begin
    ?dup
  while
   string@ r> swap 
     ( CCITT polynomial $1021, or "x16 + x12 + x5 + 1" )
     over $8 rshift-xor ( crc x )
     dup  $4 rshift-xor ( crc x )
     dup  $5 lshift-xor ( crc x )
     dup  $C lshift-xor ( crc x )
     swap $8 lshift-xor ( crc )
   >r +string
  repeat r> nip ;
xchange _system _forth-wordlist
xchange _forth-wordlist _system
: pack$ ( b u a -- a ) \ null fill
  aligned dup>r over
  dup down
  - over+ zero 2dup c! 1+ swap ( 2dup 0 fill ) cmove r> ;
xchange _system _forth-wordlist
: compare ( a1 u1 a2 u2 -- n : string equality )
  rot
  over- ?dup if >r 2drop r> nip exit then
  for ( a1 a2 )
    aft
      count rot count rot - ?dup
      if rdrop nip nip exit then
    then
  next 2drop-0 ;
h: ^h ( bot eot cur -- bot eot cur )
  >r over r@ < dup
  if
    =bs dup echo =bl echo echo
  then r> + ;
h: tap dup echo over c! 1+ ; ( bot eot cur c -- bot eot cur )
h: delete? dup =bs = swap =del = or 0= ; ( c -- t : delete key pressed? )
h: ktap                                  ( bot eot cur c -- bot eot cur )
 dup =cr xor
 if delete? \ =bs xor
   if =bl tap exit then ^h exit
 then fallthrough;
h: drop-nip-dup drop nip dup ;
: accept ( b u -- b u )
  over+ over
  begin
    2dupxor
  while
    \ we need to be wary of 'key', because it calls 'bye' when input
    \ is exhausted it is an exit and potential entry point into the program,
    \ so we should try to keep the stack elements from 'accept' hidden
    >r 2>r key 2r> rot r> swap dup
    =lf xor if tap else drop-nip-dup then
  repeat drop over- ;
: expect <expect> @execute span ! drop ;   ( b u -- )
: query tib tib-length <expect> @execute #tib ! drop-0 fallthrough;
h: in! >in ! ;                             ( u -- )
h: word.count count fallthrough; ( nfa -- u : get a words length )
h: word.length $1F and ;
xchange _forth-wordlist _system
: nfa cell+ ; ( pwd -- nfa : move to name field address)
: cfa nfa dup c@ word.length + cell+ down ; ( pwd -- cfa )
xchange _system _forth-wordlist
h: .id nfa word.count type space ; ( pwd -- : print out a word )
h: immediate? nfa $40 fallthrough; ( pwd -- t : is word immediate? )
h: set? swap @ and 0<> ;           ( a u -- t : it any of 'u' set? )
h: compile-only? nfa $20 set? ;    ( pwd -- t : is word compile only? )
h: inline? inline-start inline-end within ; ( pwd -- t : is word inline? )
h: (search-wordlist) ( a wid -- PWD PWD 1|PWD PWD -1|0 a 0: find word in WID )
  swap >r dup
  begin
    dup
  while
    dup nfa count $9F ( $1F:word-length + $80:hidden ) and r@ count compare 0=
    if ( found! )
      rdrop
      dup immediate? 1 or negate exit
    then
    nip dup@
  repeat
  rdrop 2drop-0 ;
h: (find) ( a -- pwd pwd 1 | pwd pwd -1 | 0 a 0 : find a word dictionary )
  >r
  context
  begin
    dup@
  while
    dup@ @ r@ swap (search-wordlist) ?dup
    if
      >r rot-drop r> rdrop exit
    then
    cell+
  repeat drop-0 r> 0x0000 ;
: search-wordlist (search-wordlist) rot-drop ; ( a wid -- PWD 1|PWD -1|a 0 )
: find ( a -- pwd 1 | pwd -1 | a 0 : find a word in the dictionary )
  (find) rot-drop ;
h: digit? ( c base -- u f )
  >r [char] 0 - 9 over <
  if
    7 -
    dup $A < or
  then dup r> u< ;
: >number ( ud b u -- ud b u : convert string to number )
  begin
    ( get next character )
    2dup 2>r drop c@ base@ digit?
    0= if                                 ( d char )
      drop                                ( d char -- d )
      2r>                                 ( restore string )
      nop exit                            ( ..exit )
    then                                  ( d char )
    swap base@ um* drop rot base@ um* d+  ( accumulate digit )
    2r>                                   ( restore string )
    +string dup0=                         ( advance string and test for end )
  until ;
h: number? ( a u -- d -1 | a u 0 )
  [-1] dpl !
  base@ >r
  string@ [char] - = dup>r if     +string then
  string@ [char] $ =       if hex +string then
  2>r 0 dup 2r>
  begin
    >number dup
  while string@ [char] .  ( fsp @ ) xor
    if rot-drop rot r> 2drop-0 r> base! exit then
    1- dpl ! 1+ dpl @
  repeat 2drop r> if dnegate then r> base! [-1] ;
h: -trailing ( b u -- b u : remove trailing spaces )
  for
    aft =bl over r@ + c@ <
      if r> 1+ exit then
    then
  next 0x0000 ;
h: lookfor ( b u c xt -- b u : skip until *xt* test succeeds )
  swap >r -rot
  begin
    dup
  while
    string@ r@ - r@ =bl = 4 pick execute
    if rdrop rot-drop exit then
    +string
  repeat rdrop rot-drop ;
h: no-match if 0> exit then 0<> ; ( n f -- t )
h: match no-match invert ;        ( n f -- t )
h: parser ( b u c -- b u delta )
  >r over r> swap 2>r
  r@ ' no-match lookfor 2dup
  r> ' match    lookfor swap r> - >r - r> 1+ ;
: parse ( c -- b u ; <string> )
   >r tib in@ + #tib @ in@ - r@ parser >in +!
   r> =bl = if -trailing then 0 max ;
: ) ; immediate ( -- : do nothing )
:  ( [char] ) parse 2drop ; immediate \ ) ( parse until matching paren )
: .( [char] ) parse type ; ( print out text until matching parenthesis )
: \ #tib @ in! ; immediate ( comment until new line )
h: ?length dup word-length u< ?exit $13 -throw ;
: word 1depth parse ?length here pack$ ; ( c -- a ; <string> )
h: token =bl word ;                      ( -- a )
: char token count drop c@ ;             ( -- c; <string> )
h: ?dictionary dup $3FFF u< ?exit 8 -throw ;
: , here dup cell+ ?dictionary cp! ! ; ( u -- : store *u* in dictionary )
: c, here ?dictionary c! cp 1+! ;      ( c -- : store *c* in the dictionary )
h: doLit 0x8000 or , ;                 ( n+ -- : compile literal )
: literal ( n -- : write a literal into the dictionary )
  dup 0x8000 and ( n > $7FFF ? )
  if
    invert doLit =invert , exit ( store inversion of n the invert it )
  then
  doLit ; compile-only immediate ( turn into literal, write into dictionary )
h: make-callable chars $4000 or ;    ( cfa -- instruction )
: compile, make-callable , ;         ( cfa -- : compile a code field address )
h: $compile dup inline? if cfa @ , exit then cfa compile, ; ( pwd -- )
h: not-found source type $D -throw ; ( -- : throw 'word not found' )
h: ?compile dup compile-only? 0= ?exit source type $E -throw ;
xchange _forth-wordlist _system
: (literal) state@ 0= ?exit postpone literal ; ( u -- u | )
xchange _system _forth-wordlist
: interpret ( ??? a -- ??? : The command/compiler loop )
  \ dup count type space ( <- for tracing the parser )
  find ?dup if
    state@
    if
      0> if cfa execute exit then \ <- immediate word are executed
      $compile exit               \ <- compiling word are...compiled.
    then
    drop ?compile     \ <- check it's not a compile only word word
    cfa execute exit  \ <- if its not, execute it, then exit *interpreter*
  then
  \ not a word
  dup>r count number? if rdrop \ it's a number!
    dpl @ 0< if \ <- dpl will -1 if its a single cell number
       drop     \ drop high cell from 'number?' for single cell output
    else        \ <- dpl is not -1, it's a double cell number
       state@ if swap then
       <literal> @execute \ <literal> is executed twice if it's a double
    then
    <literal> @execute exit
  then
  r> not-found ; \ not a word or number, it's an error!
: compile  r> dup@ , cell+ >r ; compile-only ( --:Compile next compiled word )
: immediate $40 last nfa fallthrough; ( -- : previous word immediate )
h: toggle tuck @ xor swap! ;        ( u a -- : xor value at addr with u )
( : compile-only 0x8000 last toggle ; )
( : smudge last fallthrough; )
h: (smudge) nfa $80 swap toggle ; ( pwd -- )
h: count+ count + ;         ( b -- b : advance address over counted string )
h: do$ 2r> dup count+ aligned >r swap >r ; ( -- a )
h: string-literal do$ nop ; ( -- a : do string NB. nop to fool optimizer )
h: .string do$ print ;      ( -- : print string  )
[t] .string        tdoPrintString meta!
[t] string-literal tdoStringLit   meta!
( <string>, --, Run: -- b )
: $" compile string-literal fallthrough; immediate compile-only
h: parse-string [char] " word count+ cp! ; ( ccc" -- )
: ." compile .string parse-string ; immediate compile-only ( <string>, -- )
: abort [-1] [-1] yield!? ;                                    ( -- )
h: ?abort swap if print cr abort exit then drop ;              ( u a -- )
h: (abort) do$ ?abort ;                                        ( -- )
: abort" compile (abort) parse-string ; immediate compile-only ( u -- )
xchange _forth-wordlist _system
h: (ok) state@ ?exit ."  ok" cr ;  ( -- : default state aware prompt )
( : ok <ok> @execute ; )
h: preset tib-start #tib cell+ ! 0 in! id zero ;  ( -- : reset input )
: io! ( preset ) fallthrough;  ( -- : initialize I/O )
h: console ' rx? <key> ! ' tx! <emit> ! fallthrough;
h: hand
   ' (ok)
   ' drop ' tap
   fallthrough;
h: xio  ' accept <expect> ! <tap> ! <echo> ! <ok> ! ;
xchange _system _forth-wordlist
: ] [-1] state !    ;                                ( -- : compile mode )
: [      state zero ; immediate                      ( -- : command mode )
h: empty sp@ ndrop ; ( 0..n -- : empty variable stack )
h: ?error ( n -- : perform actions on error )
  ?dup 0= ?exit
  .             ( print error number )
  [char] ? emit ( print '?' )
  cr            ( and terminate the line )
  empty         ( empty the variable stack )
  fallthrough;
h: prequit      ( perform actions needed to start 'quit' off )
  preset        ( reset I/O streams )
  postpone [ ;  ( back into interpret mode )
h: eval ( -- : evaluation loop, get token, evaluate, loop, prompt )
  begin
    token dup c@
  while
    interpret 0 ?depth
  repeat drop <ok> @execute ;
( : @echo source type cr ; ( -- : can be used to monitor input )
: quit prequit begin query ( @echo ) ' eval catch ?error again ;
h: get-input source in@ source-id <ok> @ ; ( -- n1...n5 )
h: set-input <ok> ! id ! in! #tib 2! ;     ( n1...n5 -- )
: evaluate ( a u -- )
  get-input 2>r 2>r >r
  0 [-1] 0 set-input
  ' eval catch
  r> 2r> 2r> set-input
  throw ;
h: ?check ( magic-number -- : check for magic number on the stack )
   magic = ?exit $16 -throw ;
h: ?unique ( a -- a : print a message if a word definition is not unique )
  dup get-current (search-wordlist) 0= ?exit
    ( source type )
  space
  2drop last-def @ .id ." redefined" cr ;
h: ?nul ( b -- : check for zero length strings )
   dup c@ ?exit $A -throw ;
h: find-token token find fallthrough; ( -- pwd,  <string> )
h: ?not-found ?exit not-found ; ( t -- )
h: find-cfa find-token cfa ;                         ( -- xt, <string> )
: ' find-cfa state@ if postpone literal exit then ; immediate
: [compile] find-cfa compile, ; immediate compile-only  ( --, <string> )
: [char] char postpone literal ; immediate compile-only ( --, <string> )
( h: ?quit state@ 0= if $38 -throw exit then ; )
: ; ( ?quit ) ?check =exit , postpone [ fallthrough; immediate compile-only
h: get-current! ?dup if get-current ! exit then ; ( -- wid )
: : align here dup last-def ! ( "name", -- colon-sys )
  last , token ?nul ?unique count+ cp! magic postpone ] ;
: begin here  ; immediate compile-only   ( -- a )
: again chars , ; immediate compile-only ( a -- )
: until $4000 or postpone again ; immediate compile-only ( a --, NB. again !? )
h: here-0 here 0x0000 ;
h: >mark here-0 postpone again ;
: if here-0 postpone until ; immediate compile-only
( : unless ' 0= compile, postpone if ; immediate compile-only )
: then fallthrough; immediate compile-only
h: >resolve here chars over @ or swap! ;
: else >mark swap >resolve ; immediate compile-only
: while postpone if ; immediate compile-only
: repeat swap postpone again postpone then ; immediate compile-only
h: last-cfa last-def @ cfa ;  ( -- u )
: recurse last-cfa compile, ; immediate compile-only
( : tail last-cfa postpone again ; immediate compile-only )
: create postpone : drop compile doVar get-current ! postpone [ ;
: >body cell+ ; ( a -- a )
h: doDoes r> chars here chars last-cfa dup cell+ doLit h: !, ! , ;;
: does> compile doDoes nop ; immediate compile-only
: variable create 0 , ;
: constant create ' doConst make-callable here cell- !, ;
: :noname here-0 magic postpone ] ; ( NB. need postpone! )
: for =>r , here ; immediate compile-only
: next compile doNext , ; immediate compile-only
: aft drop >mark postpone begin swap ; immediate compile-only
xchange _forth-wordlist _system
: hide find-token (smudge) ; ( --, <string> : hide word by name )
xchange _system _forth-wordlist
( : name? find-token nfa ; )
( \ Force tail call )
( : ;tail [compile] ; -2 cells allot here @ $1FFF and here ! 1 cells allot ; )
( immediate )
h: find-empty-cell 0 fallthrough; ( a -- )
h: find-cell >r begin dup@ r@ <> while cell+ repeat rdrop ; ( u a -- a )
: get-order ( -- widn ... wid1 n : get the current search order )
  context
  find-empty-cell
  dup cell- swap
  context - chars dup>r 1- s>d if $32 -throw exit then
  for aft dup@ swap cell- then next @ r> ;
xchange _forth-wordlist root-voc
: forth-wordlist _forth-wordlist ; ( -- wid : push forth vocabulary )
: system _system ;                 ( -- wid : push system vocabulary )
( @warning recursion without using recurse is used )
: set-order ( widn ... wid1 n -- : set the current search order )
  dup [-1] = if drop root-voc 1 set-order exit then ( NB. Recursion! )
  dup #vocs > if $31 -throw exit then
  context swap for aft tuck ! cell+ then next zero ;
: forth root-voc forth-wordlist 2 set-order ; ( -- )
h: not-hidden? nfa c@ $80 and 0= ; ( pwd -- )
h: .words
    begin
      ?dup
    while dup not-hidden? if dup .id then @ repeat cr ;
: words
  get-order begin ?dup while swap dup cr u. colon-space @ .words 1- repeat ;
xchange root-voc _forth-wordlist
( : previous get-order swap drop 1- set-order ; ( -- )
( : also get-order over swap 1+ set-order ;     ( wid -- )
: only [-1] set-order ;                         ( -- )
( : order get-order for aft . then next cr ;    ( -- )
( : anonymous get-order 1+ here 1 cells allot swap set-order ; ( -- )
: definitions context @ set-current ;           ( -- )
h: (order)                                      ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r (order) over r@ xor
    if
      1+ r> -rot exit
    then rdrop
  then ;
: -order get-order (order) nip set-order ;             ( wid -- )
: +order dup>r -order get-order r> swap 1+ set-order ; ( wid -- )
: update [-1] block-dirty ! ; ( -- )
h: blk-@ blk @ ;              ( -- k : retrieve current loaded block )
h: +block blk-@ + ;           ( -- )
: save 0 here h: -save (save) throw ;;  ( -- : save blocks )
: flush block-dirty @ 0= ?exit 0 [-1] -save ; ( -- )
: block ( k -- a )
  1depth
  dup $3F u> if $23 -throw exit then
  dup blk !
  $A lshift ( <-- b/buf * ) ;
( : dblock if $23 -throw exit then block ; ( ud -- )
h: c/l* ( c/l * ) 6 lshift ;            ( u -- u )
h: c/l/ ( c/l / ) 6 rshift ;            ( u -- u )
h: line c/l* swap block + c/l ;         ( k u -- a u )
h: loadline line evaluate ;             ( k u -- )
: load 0 l/b-1 for 2dup 2>r loadline 2r> 1+ next 2drop ; ( k -- )
h: pipe [char] | emit ;                 ( -- )
( h: .line line -trailing $type ;       ( k u -- )
h: .border 3 spaces c/l [char] - nchars cr ; ( -- )
h: #line dup 2 u.r ;                    ( u -- u : print line number )
( : thru over- for dup load 1+ next drop ; ( k1 k2 -- )
( : message l/b extract .line cr ;      ( u -- )
h: retrieve block drop ;                ( k -- )
: list                                  ( k -- )
  dup retrieve
  cr
  .border
  0 begin
    dup l/b <
  while
    2dup #line pipe line $type pipe cr 1+
  repeat .border 2drop ;
h: cold ( -- : performs a cold boot  )
   $12 retrieve io!
   forth
   empty
   0 rp!
   <boot> @execute ;
h: hi hex ." eFORTH v" $1984 0 u.r cr decimal here . fallthrough;
h: .free $4000 here - u. cr ;             ( -- : print unused program space )
h: normal-running hi quit ;                                ( -- : boot word )
\ h: validate over cfa <> if drop-0 exit then nfa ; ( pwd cfa -- nfa | 0 )
\ h: address $1FFF and ; ( u -- u : mask off address bits )
\ h: search-for-cfa ( wid cfa -- nfa | 0 : search for CFA in a word list )
\   cells address >r
\   begin
\     dup
\   while
\     dup@ over r@ -rot  within
\     if dup@ r@ validate ?dup if rdrop nip exit then then
\     @
\   repeat rdrop ;
\ h: name ( cwf -- a | 0 )
\    >r
\    get-order
\    begin
\      dup
\    while
\      swap r@ search-for-cfa ?dup if >r 1- ndrop r> rdrop exit then
\    1- repeat rdrop ;
\ h: ?instruction ( i m e -- i t )
\    >r over-and r> = ;
\ ( a -- : find word by address, and print )
\ h: .name dup address cells 5u.r space  ( a -- )
\          name ?dup if word.count type then ;
\ h: .instruction                    ( u -- : decompile a single instruction )
\    0x8000  0x8000 ?instruction if [char] L emit $7FFF and 5u.r exit then
\     $6000   $6000 ?instruction if [char] A emit  drop ( .alu ) exit then
\     $6000   $4000 ?instruction if [char] C emit .name exit then
\     $6000   $2000 ?instruction if [char] Z emit .name exit then
\    [char] B emit .name ;
\ h: decompiler ( previous current -- : decompile starting at address )
\   >r
\   begin dup r@ u< while
\     d5u.r colon-space
\     dup@
\     d5u.r space .instruction cr cell+
\   repeat rdrop drop ;
\ : see ( --, <string> : decompile a word )
\   token (find) ?not-found
\   swap      2dup= if drop here then >r
\   cr colon-space dup .id dup cr
\   cfa r> decompiler space [char] ; emit
\   dup compile-only? if ."  compile-only" then
\   dup inline?       if ."  inline"       then
\       immediate?    if ."  immediate"    then cr ;
: .s ( -- ) cr sp@ for aft r@ pick . then next ."  <sp" cr ; ( -- )
h: dm+ chars for aft dup@ 5u.r cell+ then next ;        ( a u -- a )
( h: dc+ chars for aft dup@ space decompile cell+ then next ; ( a u -- a )
: dump ( a u -- )
  $10 + \ align up by dump-width
  4 rshift ( <-- equivalent to "dump-width /" )
  for
    aft
      cr dump-width 2dup
      over 5u.r colon-space
      dm+ ( dump-width dc+ ) \ <-- dc+ is optional
      -rot
      2 spaces $type
    then
  next drop ;
[last]              [t] _forth-wordlist t!
[t] _forth-wordlist [v] current         t!
0 tlast meta!
there [t] cp t!
[t] (literal)      [v] <literal> t! ( set literal execution vector )
[t] cold 2/        [t] <cold>    t! ( set starting word in boot-loader )
[t] normal-running [v] <boot>    t! ( set user visible boot vector )
there    [t] header-length t! \ Set Length First!
checksum [t] header-crc t!    \ Calculate image CRC
finished
bye
