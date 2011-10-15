Standard Interface Library.


v1.5.9 - Current stable version


Status:

In Delphi 7.0 all is working good, but some there are some pending features.
In other Delphies (5.0 & 6.0) I can't give credit because we can no longer have those.

In Kylix (3.0) the library itself compiles and is functional, but the operating system layer is currently under construction.
Now we are working to provide an Free Pascal port which will be functional in a few weeks from now. 
We will provide a version that compiles in both linux and windows ports of the fpc compiler.

How to install:

In Delphi (in Kylix is aproximately the same), download the zip archive containing 
the current release, and unzip it to some directory, lets call it: <sil>, 
and then add to the library search path in delphi (or kylix) these directories: 

<sil>\<ver>\source\lib
<sil>\<ver>\source\lib\hw\x86
<sil>\<ver>\source\lib\lc\<language>
<sil>\<ver>\source\lib\os\<os>
<sil>\<ver>\source\lib\rtl\<dpNN>
<sil>\<ver>\source\tool
<sil>\<ver>\source\tool\os\<os>
<sil>\<ver>\source\tool\lc\<language>
<sil>\<ver>\source\fwk\url
<sil>\<ver>\source\fwk\xml
<sil>\<ver>\source\fwk\cipher
<sil>\<ver>\source\fwk\mime
<sil>\<ver>\source\fwk\email
<sil>\<ver>\source\fwk\log
<sil>\<ver>\source\fwk\vcl
<sil>\<ver>\source\fwk\shared
<sil>\<ver>\source\fwk\shared\os\win
<sil>\<ver>\source\fwk\pool
<sil>\<ver>\source\fwk\cfg

Replace as you need:

  <ver>         with the sil version you have. ie: 1.5.9
  <os>          with 'win' or 'linux'
  <language>    with 'sp' or 'en'
  <dpNN>        with 'dp50' or 'dp60' (Delphi 7 is the same as Delphi 6)


Testing it:

Simply create a new project, and then include "Sil" to the uses clause.

Then you can use all the basic features from this only uses!!

For example: 

- All the lists (

    Sil.List.StringList, 
    Sil.List.PointerList, 
    Sil.List.InterfaceList, 
    Sil.List.ValueList).

- The basic conversion types (
    Sil.Int, 
    Sil.Float, 
    Sil.Str, 
    Sil.Date, 
    Sil.Time, and so on)

- the list continues ...


Guide to the prefixes on the file's name:

A typical file is named: SilXYSomething.pas

Where X indicates to which layer of the library corresponds the given unit, and is one of:

A: Low level layer of assembly routines, in theory idependent from de operating system.
B: Base library layer which contains tools and simple type definitions.
L: Library layer, it contains all the useful code implemented in such a way that is independent from its  underlying operating system, because it uses the upper abstraction layer of OS.
O: Operating System abstraction layer.
S: Services layer.
V: VCL/CLX Component layer.


and Y simply indicates the content of the archive, and is one of:

c: Declaration of simple nonlocalizable constants.
d: Declaration of localizable strings in form of resourcestring.
e: Declaration of types, enumerated types, set types, aliased types, etc.
f: Declaration of global functions, which generally are dependant of some other upper layers.
g: Declaration of typed initialized constants which normally serves as a mapping between different types.
h: Header of DLL, normally declaration of pointer to functions exported by it.
i: Declaration of Interfaces.
j: Declaration of Abstract Tools (our invention ...), static classes which only can have virtual abstract CLASS function or procedures.
k: Definition of Abstract Classes, which implements some or several interfaces but in a reusable manner.
m: Definition of Concrete Classes, which implement a concrete task in a manner which is generally not very reusable.
s: Definition of modules, units which only publish types and tools defined elswhere in the library.
t: Definition of Concrete Tools (our invention), static classes which only can have CLASS functions or procedures but that can inherit of an Abstract Tool.
v: Global and Local services singletons.


Some other units, which don not have prefixes, are only a summary which englobes some specific subkind of the library, for example: SilData, publishes all the interfaces and tools required to instantiate and use the SilDataRowset service, but which has no code nor definitions of any other kind.

Summary of this type of units:

Sil.pas:
All the SIL in only one (but sloooooow ...) unit.

SilTool.pas:
Utilities and services which are not included in the base Sil but that are as important as the ones in there.

SilClasses.pas:
All the classes which implements some of the utilities in the library. Include this to inherit of any one of them.

SilMM.pas:
A Multi-Media Layer which encapsulates the usage of Win32 API MM functions (reimplemented in Linux)

SilAVL.pas:
An AVL tree implementation. Is used in implementing indexes for the DataRowsets.

SilCoder.pas:
Some builtin coders (ISO88591, UUCoder, Base64, etc)

SilData.pas:
Implementation of a basic in-memory rowset, which supports indexing.

SilDataClasses.pas:
The classes used to implement the data rowset published here to allow inheritance.

SilSignature.pas:
Signs a module with a exports names in some funny way to allow us to identify a shared library which was compiled with this version of Sil. Is used to the SharedObject support described below.

SilDLL.pas:
A unit which can only be included in a shared library (DLL) and serves to initialize a shared memory section which is used to communicate with the main executable. Of course, the executable must be compiled with Sil!!

SilModule.pas:
A unit which may be included in any module, and exports the functionality required by the above one.

SilSharemem.pas:
Used inside a shared library to initialize a global delphi memory manager which supports allocations of memory blocks accross module boundaries. (Same as Delphi's ShareMem.DLL, but without the DLL!!)

SilExport.pas:
This unit is used in the module which is the one who exports all the entry points which are necessary to support the global memory manager as required by the above unit.

SilLibrary.pas:
Used to support the entry points needed by a shared library to become a Shared Object (our invention ...)

SilTokens.pas:
Support base to declare and use Token types and Symbol tables. 

SilLexer.pas:
Implementation of a pattern matching engine which is used to tokenize the input to convert it in a form which is used by some parser in the next phase.

SilEval.pas:
A simple (yet powerful!) expression evaluator which support C-like operator syntax, function call, and variables managed by the evaluator host.

SilScript.pas:
A simple script engine which uses the expression evaluator above, and adds some C-like constructs of execution control. Such as: while, switch, for, if, do ... etc, and adds some others: repeat.

SilHttp.pas:
A HTTP protocol parser implementation based on the tokenizer above.

