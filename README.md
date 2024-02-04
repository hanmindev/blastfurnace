# BlastFurnace

This is the BlastFurnace compiler for the [Ingot](https://hanmin.dev/ingot-docs/) programming language.

This is currently in early development and is not ready for use.

## Progress

Front-End:
- [x] Import Resolution
- [x] Lexer
- [x] Parser
- [x] AST
- [ ] Semantic Analyzer
  - [x] Name Resolution
  - [x] Object Binding
  - [ ] Type Checking

Middle-End:
- [ ] Constant Folding
- [ ] Various Code Optimizations

Back-End:
- [ ] Code Generation into HMASM

Linker:
- [ ] Combine HMASM to .mcfunction files

Language Features:

Parses:
- [x] Comments
- [x] Variables
- [x] Functions
- [x] If Statements
- [x] While Loops
- [x] For Loops
- [x] Structs
- [ ] Struct Methods
- [ ] Arrays
- [ ] Compounds
- [ ] Pointers
- [ ] Macro system
- [ ] Execute Context
- [ ] Dynamic Access
- [ ] Modules

Other Features:
- [ ] Configurations
- [ ] External Module System
 - [ ] Importing Modules
 - [ ] Exporting Modules
 - [ ] Module Resolution

Setup Tool:
- [ ] Setup tool to install BlastFurnace
- [ ] Setup tool to setup a project