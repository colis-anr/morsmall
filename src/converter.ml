(******************************************************************************)
(*                                                                            *)
(*                                  Morsmall                                  *)
(*                       A concise AST for POSIX shell                        *)
(*                                                                            *)
(*   Copyright (C) 2017  Yann Régis-Gianas, Ralf Treinen, Nicolas Jeannerod   *)
(*                                                                            *)
(*   This program is free software: you can redistribute it and/or modify     *)
(*   it under the terms of the GNU General Public License as published by     *)
(*   the Free Software Foundation, either version 3 of the License, or        *)
(*   (at your option) any later version.                                      *)
(*                                                                            *)
(*   This program is distributed in the hope that it will be useful,          *)
(*   but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*   GNU General Public License for more details.                             *)
(*                                                                            *)
(*   You should have received a copy of the GNU General Public License        *)
(*   along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                            *)
(******************************************************************************)

open Libmorbig.CST

let rec complete_command__to__command_option : complete_command -> AST.command option = function
  | CompleteCommand_Empty ->
     None
  | CompleteCommand_CList_Separator (clist', sep') ->
     Some (clist'__to__command clist'
           |> separator'__to__command sep')
  | CompleteCommand_CList clist' ->
     Some (clist'__to__command clist')

and clist__to__command : clist -> AST.command = function
  | CList_CList_SeparatorOp_AndOr (clist', sep_op', and_or') ->
     AST.Seq (
         clist'__to__command' clist'
         |> separator_op'__to__command' sep_op',
         and_or'__to__command' and_or'
       )
  | CList_AndOr and_or' ->
     and_or'__to__command and_or'

and and_or__to__command : and_or -> AST.command = function
  | AndOr_Pipeline pipeline' ->
     pipeline'__to__command pipeline'
  | AndOr_AndOr_AndIf_LineBreak_Pipeline (and_or', _, pipeline') ->
     AST.And (
         and_or'__to__command' and_or',
         pipeline'__to__command' pipeline'
       )
  | AndOr_AndOr_OrIf_LineBreak_Pipeline (and_or', _, pipeline') ->
     AST.Or (
         and_or'__to__command' and_or',
         pipeline'__to__command' pipeline'
       )

and pipeline__to__command : pipeline -> AST.command = function
  | Pipeline_PipeSequence pipe_sequence' ->
     pipe_sequence'__to__command pipe_sequence'
  | Pipeline_Bang_PipeSequence pipe_sequence' ->
     AST.Not (pipe_sequence'__to__command' pipe_sequence')

and pipe_sequence__to__command : pipe_sequence -> AST.command = function
  | PipeSequence_Command command' ->
     command'__to__command command'
  | PipeSequence_PipeSequence_Pipe_LineBreak_Command (pipe_sequence', _, command') ->
     AST.Pipe (
         pipe_sequence'__to__command' pipe_sequence',
         command'__to__command' command'
       )

and command__to__command : command -> AST.command = function
  | Command_SimpleCommand simple_command' ->
     simple_command'__to__command simple_command'
  | Command_CompoundCommand compound_command' ->
     compound_command'__to__command compound_command'
  | Command_CompoundCommand_RedirectList (compound_command', redirect_list') ->
     compound_command'__to__command' compound_command'
     |> redirect_list'__to__command redirect_list'
  | Command_FunctionDefinition function_definition' ->
     function_definition'__to__command function_definition'

and compound_command__to__command : compound_command -> AST.command = function
  | CompoundCommand_BraceGroup brace_group' ->
     brace_group'__to__command brace_group'
  | CompoundCommand_Subshell subshell' ->
     subshell'__to__command subshell'
  | CompoundCommand_ForClause for_clause' ->
     for_clause'__to__command for_clause'
  | CompoundCommand_CaseClause case_clause' ->
     case_clause'__to__command case_clause'
  | CompoundCommand_IfClause if_clause' ->
     if_clause'__to__command if_clause'
  | CompoundCommand_WhileClause while_clause' ->
     while_clause'__to__command while_clause'
  | CompoundCommand_UntilClause until_clause' ->
     until_clause'__to__command until_clause'

and subshell__to__command : subshell -> AST.command  = function
  | Subshell_Lparen_CompoundList_Rparen compound_list' ->
     AST.Subshell (compound_list'__to__command' compound_list')

and compound_list__to__command : compound_list -> AST.command = function
  | CompoundList_Term term'
  | CompoundList_NewLineList_Term (_, term') ->
     term'__to__command term'
  | CompoundList_Term_Separator (term', sep')
  | CompoundList_NewLineList_Term_Separator (_, term', sep') ->
     term'__to__command term'
     |> separator'__to__command sep'

and term__to__command : term -> AST.command = function
  | Term_Term_Separator_AndOr (term', sep', and_or') ->
     AST.Seq (
         term'__to__command' term'
         |> separator'__to__command' sep',
         and_or'__to__command' and_or'
       )
  | Term_AndOr and_or' ->
     and_or'__to__command and_or'

and for_clause__to__command : for_clause -> AST.command = function
  | ForClause_For_Name_DoGroup (name', do_group')
  | ForClause_For_Name_SequentialSep_DoGroup (name', _, do_group')
  | ForClause_For_Name_LineBreak_In_SequentialSep_DoGroup (name', _, _, do_group') ->
     AST.For (
         name'__to__name' name' ,
         None ,
         do_group'__to__command' do_group'
       )
  | ForClause_For_Name_LineBreak_In_WordList_SequentialSep_DoGroup (name', _, wordlist', _, do_group') ->
     AST.For (
         name'__to__name' name' ,
         Some (wordlist'__to__word_list' wordlist') ,
         do_group'__to__command' do_group'
       )

and wordlist__to__word_list : wordlist -> AST.word list = function (*FIXME*)
  | WordList_WordList_Word (wordlist', word') ->
     (wordlist'__to__word_list wordlist')
     @ [word'__to__word word']
  | WordList_Word word' ->
     [word'__to__word word']

and case_clause__to__command : case_clause -> AST.command = function
  | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseList_Esac (word', _, _, case_list') ->
     AST.Case (
         word'__to__word' word' ,
         case_list'__to__case_list case_list'
       )
  | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseListNS_Esac (word', _, _, case_list_ns') ->
     AST.Case (
         word'__to__word' word' ,
         case_list_ns'__to__case_list case_list_ns'
       )
  | CaseClause_Case_Word_LineBreak_In_LineBreak_Esac (word', _, _) ->
     AST.Case (
         word'__to__word' word' ,
         []
       )

and case_list_ns__to__case_list : case_list_ns -> (AST.pattern_list' * AST.command' option) list = function (*FIXME*)
  | CaseListNS_CaseList_CaseItemNS (case_list', case_item_ns') ->
     (case_list'__to__case_list case_list')
     @ [case_item_ns'__to__case case_item_ns']
  | CaseListNS_CaseItemNS case_item_ns' ->
     [case_item_ns'__to__case case_item_ns']

and case_list__to__case_list : case_list -> (AST.pattern_list' * AST.command' option) list = function (*FIXME*)
  | CaseList_CaseList_CaseItem (case_list', case_item') ->
     (case_list'__to__case_list case_list')
     @ [case_item'__to__case case_item']
  | CaseList_CaseItem case_item' ->
     [case_item'__to__case case_item']

and case_item_ns__to__case : case_item_ns -> AST.pattern_list' * AST.command' option = function
  | CaseItemNS_Pattern_Rparen_LineBreak (pattern', _)
  | CaseItemNS_Lparen_Pattern_Rparen_LineBreak (pattern', _) ->
     (
        pattern'__to__pattern_list' pattern' ,
        None
     )
  | CaseItemNS_Pattern_Rparen_CompoundList_LineBreak (pattern', compound_list', _)
  | CaseItemNS_Lparen_Pattern_Rparen_CompoundList_LineBreak (pattern', compound_list', _) ->
     (
        pattern'__to__pattern_list' pattern' ,
        Some (compound_list'__to__command' compound_list')
     )

and case_item__to__case : case_item -> AST.pattern_list' * AST.command' option = function
  | CaseItem_Pattern_Rparen_LineBreak_Dsemi_LineBreak (pattern', _, _)
  | CaseItem_Lparen_Pattern_Rparen_LineBreak_Dsemi_LineBreak (pattern', _, _) ->
     (
        pattern'__to__pattern_list' pattern' ,
        None
     )
  | CaseItem_Pattern_Rparen_CompoundList_Dsemi_LineBreak (pattern', compound_list', _)
  | CaseItem_Lparen_Pattern_Rparen_CompoundList_Dsemi_LineBreak (pattern', compound_list', _) ->
     (
        pattern'__to__pattern_list' pattern' ,
        Some (compound_list'__to__command' compound_list')
     )

and pattern__to__pattern_list : pattern -> AST.pattern list = function
  | Pattern_Word word' ->
     [word'__to__word word']
  | Pattern_Pattern_Pipe_Word (pattern', word') ->
     (pattern'__to__pattern_list pattern')
     @ [word'__to__word word']

and if_clause__to__command : if_clause -> AST.command = function
  | IfClause_If_CompoundList_Then_CompoundList_ElsePart_Fi (compound_list', compound_list2', else_part') ->
     AST.If (
         compound_list'__to__command' compound_list' ,
         compound_list'__to__command' compound_list2' ,
         Some (else_part'__to__command' else_part')
       )
  | IfClause_If_CompoundList_Then_CompoundList_Fi (compound_list', compound_list2') ->
     AST.If (
         compound_list'__to__command' compound_list' ,
         compound_list'__to__command' compound_list2' ,
         None
       )

and else_part__to__command : else_part -> AST.command = function
  | ElsePart_Elif_CompoundList_Then_CompoundList (compound_list', compound_list2') ->
     AST.If (
         compound_list'__to__command' compound_list' ,
         compound_list'__to__command' compound_list2' ,
         None
       )
  | ElsePart_Elif_CompoundList_Then_CompoundList_ElsePart (compound_list', compound_list2', else_part') ->
     AST.If (
         compound_list'__to__command' compound_list' ,
         compound_list'__to__command' compound_list2' ,
         Some (else_part'__to__command' else_part')
       )
  | ElsePart_Else_CompoundList compound_list' ->
     compound_list'__to__command compound_list'

and while_clause__to__command : while_clause -> AST.command = function
  | WhileClause_While_CompoundList_DoGroup (compound_list', do_group') ->
     AST.While (
         compound_list'__to__command' compound_list' ,
         do_group'__to__command' do_group'
       )

and until_clause__to__command : until_clause -> AST.command = function
  | UntilClause_Until_CompoundList_DoGroup (compound_list', do_group') ->
     AST.Until (
         compound_list'__to__command' compound_list' ,
         do_group'__to__command' do_group'
       )

and function_definition__to__command : function_definition -> AST.command = function
  | FunctionDefinition_Fname_Lparen_Rparen_LineBreak_FunctionBody (fname', _, function_body') ->
     AST.Function (
         fname'__to__name' fname' ,
         function_body'__to__command' function_body'
       )

and function_body__to__command : function_body -> AST.command = function
  | FunctionBody_CompoundCommand compound_command' ->
     compound_command'__to__command compound_command'
  | FunctionBody_CompoundCommand_RedirectList (compound_command', redirect_list') ->
     compound_command'__to__command' compound_command'
     |> redirect_list'__to__command redirect_list'

and fname__to__name : fname -> AST.name = function
  | Fname_Name name -> name__to__name name

and brace_group__to__command : brace_group -> AST.command = function
  | BraceGroup_LBrace_CompoundList_RBrace compound_list' ->
     compound_list'__to__command compound_list'

and do_group__to__command : do_group -> AST.command = function
  | DoGroup_Do_CompoundList_Done compound_list' ->
     compound_list'__to__command compound_list'

and simple_command__to__command (simple_command : simple_command) : AST.command =
  let (assignments', words', io_redirect_list) =
    match simple_command with

    | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cmd_prefix', cmd_word', cmd_suffix') ->
       let assignments', io_redirect_list = cmd_prefix'__to__assignments'_io_redirect_list [] [] cmd_prefix' in
       let words', io_redirect_list = cmd_suffix'__to__words'_io_redirect_list [] io_redirect_list cmd_suffix' in
       (assignments', cmd_word'__to__word' cmd_word' :: words', io_redirect_list)

    | SimpleCommand_CmdPrefix_CmdWord (cmd_prefix', cmd_word') ->
       let assignments', io_redirect_list = cmd_prefix'__to__assignments'_io_redirect_list [] [] cmd_prefix' in
       (assignments', [cmd_word'__to__word' cmd_word'], io_redirect_list)

    | SimpleCommand_CmdPrefix cmd_prefix' ->
       let assignments', io_redirect_list = cmd_prefix'__to__assignments'_io_redirect_list [] [] cmd_prefix' in
       (assignments', [], io_redirect_list)

    | SimpleCommand_CmdName_CmdSuffix (cmd_name', cmd_suffix') ->
       let words', io_redirect_list = cmd_suffix'__to__words'_io_redirect_list [] [] cmd_suffix' in
       ([], cmd_name'__to__word' cmd_name' :: words', io_redirect_list)

    | SimpleCommand_CmdName cmd_name' ->
       ([], [cmd_name'__to__word' cmd_name'], [])
  in
  List.fold_left
    (
      fun command io_redirect ->
      io_redirect__to__command io_redirect command
    )
    (AST.Simple (assignments', words'))
    io_redirect_list

and cmd_name__to__word : cmd_name -> AST.word = function
  | CmdName_Word word' ->
     word'__to__word word'

and cmd_word__to__word : cmd_word -> AST.word = function
  | CmdWord_Word word' ->
     word'__to__word word'

and cmd_prefix__to__assignments'_io_redirect_list (assignments' : AST.assignment' list) (io_redirect_list : io_redirect list) : cmd_prefix -> AST.assignment' list * io_redirect list = function
  | CmdPrefix_IoRedirect io_redirect' ->
     assignments',
     io_redirect'.value :: io_redirect_list

  | CmdPrefix_CmdPrefix_IoRedirect (cmd_prefix', io_redirect') ->
     cmd_prefix'__to__assignments'_io_redirect_list
       assignments'
       (io_redirect'.value :: io_redirect_list)
       cmd_prefix'

  | CmdPrefix_AssignmentWord assignment_word' ->
     (assignment_word'__to__assignment' assignment_word') :: assignments',
     io_redirect_list

  | CmdPrefix_CmdPrefix_AssignmentWord (cmd_prefix', assignment_word') ->
     cmd_prefix'__to__assignments'_io_redirect_list
       ((assignment_word'__to__assignment' assignment_word') :: assignments')
       io_redirect_list
       cmd_prefix'

(* FIXME: words' ? *)
and cmd_suffix__to__words'_io_redirect_list (words' : AST.word' list) (io_redirect_list : io_redirect list) : cmd_suffix -> AST.word' list * io_redirect list = function
  | CmdSuffix_IoRedirect io_redirect' ->
     words',
     io_redirect'.value :: io_redirect_list

  | CmdSuffix_CmdSuffix_IoRedirect (cmd_suffix', io_redirect') ->
     cmd_suffix'__to__words'_io_redirect_list
       words'
       (io_redirect'.value :: io_redirect_list)
       cmd_suffix'

  | CmdSuffix_Word word' ->
     (word'__to__word' word') :: words',
     io_redirect_list

  | CmdSuffix_CmdSuffix_Word (cmd_suffix', word') ->
     cmd_suffix'__to__words'_io_redirect_list
       ((word'__to__word' word') :: words')
       io_redirect_list
       cmd_suffix'

and redirect_list__to__command redirect_list (command' : AST.command') : AST.command =
  match redirect_list with
  | RedirectList_IoRedirect io_redirect' ->
     command'
     |> io_redirect'__to__command io_redirect'
  | RedirectList_RedirectList_IoRedirect (redirect_list', io_redirect') ->
     command'
     |> io_redirect'__to__command' io_redirect'
     |> redirect_list'__to__command redirect_list' (*FIXME: check order of the redirections*)

and io_redirect__to__command (io_redirect : io_redirect) (command' : AST.command') : AST.command =
  match io_redirect with
  | IoRedirect_IoFile io_file' ->
     let kind, word = io_file'__to__kind_word io_file' in
     AST.Redirection (command', None, kind, word)
  | IoRedirect_IoNumber_IoFile (io_number, io_file') ->
     let kind, word = io_file'__to__kind_word io_file' in
     AST.Redirection (command', Some (io_number__to__descr io_number), kind, word)
  | IoRedirect_IoHere io_here' ->
     let trim, word = io_here'__to__trim_word io_here' in
     AST.HereDocument (command', None, trim, word)
  | IoRedirect_IoNumber_IoHere (io_number, io_here') ->
     let trim, word = io_here'__to__trim_word io_here' in
     AST.HereDocument (command', Some (io_number__to__descr io_number), trim, word)

and io_file__to__kind_word io_file =
  let kind, filename' =
    AST.(match io_file with
    | IoFile_Less_FileName filename' -> AST.Input, filename'
    | IoFile_LessAnd_FileName filename' -> AST.InputDuplicate, filename'
    | IoFile_Great_FileName filename' -> AST.Output, filename'
    | IoFile_GreatAnd_FileName filename' -> AST.OutputDuplicate, filename'
    | IoFile_DGreat_FileName filename' -> AST.OutputAppend, filename'
    | IoFile_LessGreat_FileName filename' -> AST.InputOutput, filename'
    | IoFile_Clobber_FileName filename' -> AST.OutputClobber, filename')
  in
  ( kind , filename'__to__word filename' )

and filename__to__word : filename -> AST.word = function
  | Filename_Word word' ->
     word'__to__word word'

and io_here__to__trim_word = function
  | IoHere_DLess_HereEnd (_, word'_ref) ->
     (false, word'__to__word !word'_ref)
  | IoHere_DLessDash_HereEnd (_, word'_ref) ->
     (true, word'__to__word !word'_ref)

and separator_op__to__command (sep_op : separator_op) (command : AST.command) : AST.command =
  match sep_op with
  | SeparatorOp_Uppersand -> AST.Async command
  | SeparatorOp_Semicolon -> command

and separator__to__command (sep : separator) (command : AST.command) : AST.command =
  match sep with
  | Separator_SeparatorOp_LineBreak (sep_op', _) ->
     separator_op'__to__command sep_op' command
  | Separator_NewLineList _ ->
     command

and sequential_sep__to__command _ (command : AST.command) : AST.command =
  command

and word__to__word : word -> AST.word = function
  | Word word -> word

and name__to__name : name -> AST.name = function
  | Name name -> name

and assignment_word__to__assignment : assignment_word -> AST.assignment = function
  | AssignmentWord (name, word) ->
     (name__to__name name, word__to__word word)

and io_number__to__descr : io_number -> AST.descr  = function
  | IONumber io_number -> int_of_string io_number



(* Located -> Located versions.

   Sadly, we have to eta-expand everything in here, because OCaml does
   not like values on the right-hand side of a [let rec]. Otherwise,
   one could write [let rec a = a]. *)

and keep_located : 'a 'b. ('a -> 'b) -> 'a located -> 'b located =
  fun f x ->
  { value = f x.value ;
    position = x.position }

and keep_located_1 : 'a 'b 'c. ('a -> 'b -> 'c) -> 'a located -> 'b -> 'c located =
  fun f x y ->
  { value = f x.value y ;
    position = x.position }

and complete_command'__to__command'_option complete_command' =
  keep_located complete_command__to__command_option complete_command'

and clist'__to__command' clist' =
  keep_located clist__to__command clist'

and and_or'__to__command' (and_or': and_or') : AST.command' =
  keep_located and_or__to__command and_or'

and pipeline'__to__command' pipeline' =
  keep_located pipeline__to__command pipeline'

and pipe_sequence'__to__command' pipe_sequence' =
  keep_located pipe_sequence__to__command pipe_sequence'

and command'__to__command' command' =
  keep_located command__to__command command'

and compound_command'__to__command' (compound_command' : compound_command') : AST.command' =
  keep_located compound_command__to__command compound_command'

and subshell'__to__command' subshell' =
  keep_located subshell__to__command subshell'

and compound_list'__to__command' compound_list' =
  keep_located compound_list__to__command compound_list'

and term'__to__command' term' =
  keep_located term__to__command term'

and for_clause'__to__command' for_clause' =
  keep_located for_clause__to__command for_clause'

and wordlist'__to__word_list' wordlist' =
  keep_located wordlist__to__word_list wordlist'

and case_clause'__to__command' case_clause' =
  keep_located case_clause__to__command case_clause'

and case_list_ns'__to__case_list' case_list_ns' =
  keep_located case_list_ns__to__case_list case_list_ns'

and case_list'__to__case_list' case_list' =
  keep_located case_list__to__case_list case_list'

and case_item_ns'__to__case' case_item_ns' =
  keep_located case_item_ns__to__case case_item_ns'

(* and case_item'__to__case' (case_item' : case_item') =
 *   keep_located case_item__to__case case_item' *)

and pattern'__to__pattern_list' pattern' =
  keep_located pattern__to__pattern_list pattern' (*FIXME*)

and if_clause'__to__command' if_clause' =
  keep_located if_clause__to__command if_clause'

and else_part'__to__command' else_part' =
  keep_located else_part__to__command else_part'

and while_clause'__to__command' while_clause' =
  keep_located while_clause__to__command while_clause'

and until_clause'__to__command' until_clause' =
  keep_located until_clause__to__command until_clause'

and function_definition'__to__command' function_definition' =
  keep_located function_definition__to__command function_definition'

and function_body'__to__command' function_body' =
  keep_located function_body__to__command function_body'

and fname'__to__name' fname' =
  keep_located fname__to__name fname'

and brace_group'__to__command' brace_group' =
  keep_located brace_group__to__command brace_group'

and do_group'__to__command' do_group' =
  keep_located do_group__to__command do_group'

and simple_command'__to__command' simple_command' =
  keep_located simple_command__to__command simple_command'

and cmd_name'__to__word' cmd_name' =
  keep_located cmd_name__to__word cmd_name'

and cmd_word'__to__word' cmd_word' =
  keep_located cmd_word__to__word cmd_word'

(* and cmd_prefix'__to__assignments_io_redirect_list' assignments io_redirect_list cmd_prefix' =
 *   keep_located (cmd_prefix__to__assignments_io_redirect_list assignments io_redirect_list) cmd_prefix' *)

(* and cmd_suffix'__to__words'_io_redirect_list' words' io_redirect_list cmd_suffix' : AST.word' list * io_redirect list =
 *   keep_located (cmd_suffix__to__words'_io_redirect_list words' io_redirect_list) cmd_suffix' *)

and redirect_list'__to__command' redirect_list' =
  keep_located redirect_list__to__command redirect_list'

and io_redirect'__to__command' (io_redirect' : io_redirect') (command' : AST.command') : AST.command' =
  keep_located_1 io_redirect__to__command io_redirect' command'

and io_file'__to__kind_word' io_file' =
  keep_located io_file__to__kind_word io_file'

and io_here'__to__trim_word' io_here' =
  keep_located io_here__to__trim_word io_here'

and filename'__to__word' filename' =
  keep_located filename__to__word filename'

and separator_op'__to__command' (sep_op' : separator_op') (command' : AST.command') : AST.command' =
  keep_located_1 separator_op__to__command sep_op' command'

and separator'__to__command' sep' command =
  keep_located_1 separator__to__command sep' command

and sequential_sep'__to__command' seq_sep' command =
  keep_located_1 sequential_sep__to__command seq_sep' command

and word'__to__word' word' =
  keep_located word__to__word word'

and name'__to__name' name' =
  keep_located name__to__name name'

and assignment_word'__to__assignment' assignment_word' =
  keep_located assignment_word__to__assignment assignment_word'


(* Located -> Non-located versions.

   Sadly, we have to eta-expand everything in here, because OCaml does
   not like values on the right-hand side of a [let rec]. Otherwise,
   one could write [let rec a = a]. *)

and erase_located : 'a 'b. ('a -> 'b) -> 'a located -> 'b =
  fun f x -> f x.value

and complete_command'__to__command_option complete_command' =
  erase_located complete_command__to__command_option complete_command'

and clist'__to__command clist' =
  erase_located clist__to__command clist'

and and_or'__to__command and_or' =
  erase_located and_or__to__command and_or'

and pipeline'__to__command pipeline' =
  erase_located pipeline__to__command pipeline'

and pipe_sequence'__to__command pipe_sequence' =
  erase_located pipe_sequence__to__command pipe_sequence'

and command'__to__command command' =
  erase_located command__to__command command'

and compound_command'__to__command compound_command' =
  erase_located compound_command__to__command compound_command'

and subshell'__to__command subshell' =
  erase_located subshell__to__command subshell'

and compound_list'__to__command (compound_list' : compound_list') : AST.command =
  erase_located compound_list__to__command compound_list'

and term'__to__command term' =
  erase_located term__to__command term'

and for_clause'__to__command for_clause' =
  erase_located for_clause__to__command for_clause'

and wordlist'__to__word_list wordlist' =
  erase_located wordlist__to__word_list wordlist'

and case_clause'__to__command case_clause' =
  erase_located case_clause__to__command case_clause'

and case_list_ns'__to__case_list case_list_ns' =
  erase_located case_list_ns__to__case_list case_list_ns'

and case_list'__to__case_list case_list' =
  erase_located case_list__to__case_list case_list'

and case_item_ns'__to__case case_item_ns' =
  erase_located case_item_ns__to__case case_item_ns'

and case_item'__to__case (case_item' : case_item') : AST.pattern_list' * AST.command' option =
  erase_located case_item__to__case case_item'

and pattern'__to__pattern_list pattern' =
  erase_located pattern__to__pattern_list pattern'

and if_clause'__to__command if_clause' =
  erase_located if_clause__to__command if_clause'

and else_part'__to__command else_part' =
  erase_located else_part__to__command else_part'

and while_clause'__to__command while_clause' =
  erase_located while_clause__to__command while_clause'

and until_clause'__to__command until_clause' =
  erase_located until_clause__to__command until_clause'

and function_definition'__to__command function_definition' =
  erase_located function_definition__to__command function_definition'

and function_body'__to__command function_body' =
  erase_located function_body__to__command function_body'

and fname'__to__name fname' =
  erase_located fname__to__name fname'

and brace_group'__to__command brace_group' =
  erase_located brace_group__to__command brace_group'

and do_group'__to__command do_group' =
  erase_located do_group__to__command do_group'

and simple_command'__to__command simple_command' =
  erase_located simple_command__to__command simple_command'

and cmd_name'__to__word cmd_name' =
  erase_located cmd_name__to__word cmd_name'

and cmd_word'__to__word cmd_word' =
  erase_located cmd_word__to__word cmd_word'

and cmd_prefix'__to__assignments'_io_redirect_list (assignments' : AST.assignment' list) (io_redirect_list : io_redirect list) (cmd_prefix' : cmd_prefix') : (AST.assignment' list * io_redirect list) =
  erase_located (cmd_prefix__to__assignments'_io_redirect_list assignments' io_redirect_list) cmd_prefix'

and cmd_suffix'__to__words'_io_redirect_list (words' : AST.word' list) (io_redirect_list : io_redirect list) (cmd_suffix' : cmd_suffix') : (AST.word' list * io_redirect list) =
  erase_located (cmd_suffix__to__words'_io_redirect_list words' io_redirect_list) cmd_suffix'

and redirect_list'__to__command redirect_list' command =
  erase_located redirect_list__to__command redirect_list' command

and io_redirect'__to__command (io_redirect' : io_redirect') (command' : AST.command') : AST.command =
  erase_located io_redirect__to__command io_redirect' command'

and io_file'__to__kind_word io_file' =
  erase_located io_file__to__kind_word io_file'

and io_here'__to__trim_word io_here' =
  erase_located io_here__to__trim_word io_here'

and filename'__to__word filename' =
  erase_located filename__to__word filename'

and separator_op'__to__command sep_op' command =
  erase_located separator_op__to__command sep_op' command

and separator'__to__command sep' command =
  erase_located separator__to__command sep' command

and sequential_sep'__to__command seq_sep' command =
  erase_located sequential_sep__to__command seq_sep' command

and word'__to__word word' =
  erase_located word__to__word word'

and name'__to__name name' =
  erase_located name__to__name name'

and assignment_word'__to__assignment assignment_word' =
  erase_located assignment_word__to__assignment assignment_word'
