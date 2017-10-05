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

let rec complete_command__to__command = function
  | CompleteCommand_CList_Separator (clist', sep') ->
     clist'__to__command clist'
     |> separator'__to__command sep'
  | CompleteCommand_CList clist' ->
     clist'__to__command clist'

and complete_command_list__to__command_list x =
  List.map complete_command__to__command x

and clist__to__command = function
  | CList_CList_SeparatorOp_AndOr (clist', sep_op', and_or') ->
     AST.Seq (
         clist'__to__command clist'
         |> separator_op'__to__command sep_op',
         and_or'__to__command and_or'
       )
  | CList_AndOr and_or' ->
     and_or'__to__command and_or'

and and_or__to__command = function
  | AndOr_Pipeline pipeline' ->
     pipeline'__to__command pipeline'
  | AndOr_AndOr_AndIf_LineBreak_Pipeline (and_or', _, pipeline') ->
     AST.And (
         and_or'__to__command and_or',
         pipeline'__to__command pipeline'
       )
  | AndOr_AndOr_OrIf_LineBreak_Pipeline (and_or', _, pipeline') ->
     AST.Or (
         and_or'__to__command and_or',
         pipeline'__to__command pipeline'
       )

and pipeline__to__command = function
  | Pipeline_PipeSequence pipe_sequence' ->
     pipe_sequence'__to__command pipe_sequence'
  | Pipeline_Bang_PipeSequence pipe_sequence' ->
     AST.Not (pipe_sequence'__to__command pipe_sequence')

and pipe_sequence__to__command = function
  | PipeSequence_Command command' ->
     command'__to__command command'
  | PipeSequence_PipeSequence_Pipe_LineBreak_Command (pipe_sequence', _, command') ->
     AST.Pipe (
         pipe_sequence'__to__command pipe_sequence',
         command'__to__command command'
       )

and command__to__command = function
  | Command_SimpleCommand simple_command' ->
     simple_command'__to__command simple_command'
  | Command_CompoundCommand compound_command' ->
     compound_command'__to__command compound_command'
  | Command_CompoundCommand_RedirectList (compound_command', redirect_list') ->
     compound_command'__to__command compound_command'
     |> redirect_list'__to__command redirect_list'
  | Command_FunctionDefinition function_definition' ->
     function_definition'__to__command function_definition'

and compound_command__to__command = function
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

and subshell__to__command = function
  | Subshell_Lparen_CompoundList_Rparen compound_list' ->
     AST.Subshell (compound_list'__to__command compound_list')

and compound_list__to__command = function
  | CompoundList_Term term'
  | CompoundList_NewLineList_Term (_, term') ->
     term'__to__command term'
  | CompoundList_Term_Separator (term', sep')
  | CompoundList_NewLineList_Term_Separator (_, term', sep') ->
     term'__to__command term'
     |> separator'__to__command sep'

and term__to__command = function
  | Term_Term_Separator_AndOr (term', sep', and_or') ->
     AST.Seq (
         term'__to__command term'
         |> separator'__to__command sep',
         and_or'__to__command and_or'
       )
  | Term_AndOr and_or' ->
     and_or'__to__command and_or'

and for_clause__to__command = function
  | ForClause_For_Name_DoGroup (name', do_group')
  | ForClause_For_Name_SequentialSep_DoGroup (name', _, do_group')
  | ForClause_For_Name_LineBreak_In_SequentialSep_DoGroup (name', _, _, do_group') ->
     AST.For {
         name = name'__to__name name' ;
         word_list = None ;
         body = do_group'__to__command do_group'
       }
  | ForClause_For_Name_LineBreak_In_WordList_SequentialSep_DoGroup (name', _, wordlist', _, do_group') ->
     AST.For {
         name = name'__to__name name' ;
         word_list = Some (wordlist'__to__word_list wordlist') ;
         body = do_group'__to__command do_group'
       }

and wordlist__to__word_list = function (*FIXME*)
  | WordList_WordList_Word (wordlist', word') ->
     (wordlist'__to__word_list wordlist')
     @ [word'__to__word word']
  | WordList_Word word' ->
     [word'__to__word word']

and case_clause__to__command = function
  | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseList_Esac (word', _, _, case_list') ->
     AST.Case {
         word = word'__to__word word' ;
         cases = case_list'__to__case_list case_list'
       }
  | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseListNS_Esac (word', _, _, case_list_ns') ->
     AST.Case {
         word = word'__to__word word' ;
         cases = case_list_ns'__to__case_list case_list_ns'
       }
  | CaseClause_Case_Word_LineBreak_In_LineBreak_Esac (word', _, _) ->
     AST.Case {
         word = word'__to__word word' ;
         cases = []
       }

and case_list_ns__to__case_list = function (*FIXME*)
  | CaseListNS_CaseList_CaseItemNS (case_list', case_item_ns') ->
     (case_list'__to__case_list case_list')
     @ [case_item_ns'__to__case case_item_ns']
  | CaseListNS_CaseItemNS case_item_ns' ->
     [case_item_ns'__to__case case_item_ns']

and case_list__to__case_list = function
  | CaseList_CaseList_CaseItem (case_list', case_item') ->
     (case_list'__to__case_list case_list')
     @ [case_item'__to__case case_item']
  | CaseList_CaseItem case_item' ->
     [case_item'__to__case case_item']

and case_item_ns__to__case = function
  | CaseItemNS_Pattern_Rparen_LineBreak (pattern', _)
  | CaseItemNS_Lparen_Pattern_Rparen_LineBreak (pattern', _) ->
     AST.{
        pattern = pattern'__to__word_list pattern' ;
        body = Nop
     }
  | CaseItemNS_Pattern_Rparen_CompoundList_LineBreak (pattern', compound_list', _)
  | CaseItemNS_Lparen_Pattern_Rparen_CompoundList_LineBreak (pattern', compound_list', _) ->
     AST.{
        pattern = pattern'__to__word_list pattern' ;
        body = compound_list'__to__command compound_list'
     }

and case_item__to__case = function
  | CaseItem_Pattern_Rparen_LineBreak_Dsemi_LineBreak (pattern', _, _)
  | CaseItem_Lparen_Pattern_Rparen_LineBreak_Dsemi_LineBreak (pattern', _, _) ->
     AST.{
        pattern = pattern'__to__word_list pattern' ;
        body = Nop
     }
  | CaseItem_Pattern_Rparen_CompoundList_Dsemi_LineBreak (pattern', compound_list', _)
  | CaseItem_Lparen_Pattern_Rparen_CompoundList_Dsemi_LineBreak (pattern', compound_list', _) ->
     AST.{
        pattern = pattern'__to__word_list pattern' ;
        body = compound_list'__to__command compound_list'
     }

and pattern__to__word_list = function
  | Pattern_Word word' ->
     [word'__to__word word']
  | Pattern_Pattern_Pipe_Word (pattern', word') ->
     (pattern'__to__word_list pattern')
     @ [word'__to__word word']

and if_clause__to__command = function
  | IfClause_If_CompoundList_Then_CompoundList_ElsePart_Fi (compound_list', compound_list2', else_part') ->
     AST.If {
         test = compound_list'__to__command compound_list' ;
         then_branch = compound_list'__to__command compound_list2' ;
         else_branch = else_part'__to__command else_part'
       }
  | IfClause_If_CompoundList_Then_CompoundList_Fi (compound_list', compound_list2') ->
     AST.If {
         test = compound_list'__to__command compound_list' ;
         then_branch = compound_list'__to__command compound_list2' ;
         else_branch = AST.Nop
       }

and else_part__to__command = function
  | ElsePart_Elif_CompoundList_Then_CompoundList (compound_list', compound_list2') ->
     AST.If {
         test = compound_list'__to__command compound_list' ;
         then_branch = compound_list'__to__command compound_list2' ;
         else_branch = AST.Nop
       }
  | ElsePart_Elif_CompoundList_Then_CompoundList_ElsePart (compound_list', compound_list2', else_part') ->
     AST.If {
         test = compound_list'__to__command compound_list' ;
         then_branch = compound_list'__to__command compound_list2' ;
         else_branch = else_part'__to__command else_part'
       }
  | ElsePart_Else_CompoundList compound_list' ->
     compound_list'__to__command compound_list'

and while_clause__to__command = function
  | WhileClause_While_CompoundList_DoGroup (compound_list', do_group') ->
     AST.While {
         test = compound_list'__to__command compound_list' ;
         body = do_group'__to__command do_group'
       }

and until_clause__to__command = function
  | UntilClause_Until_CompoundList_DoGroup (compound_list', do_group') ->
     AST.While {
         test = AST.Not (compound_list'__to__command compound_list') ;
         body = do_group'__to__command do_group'
       }

and function_definition__to__command = function
  | FunctionDefinition_Fname_Lparen_Rparen_LineBreak_FunctionBody (fname', _, function_body') ->
     AST.Function {
         name = fname'__to__name fname' ;
         body = function_body'__to__command function_body'
       }

and function_body__to__command = function
  | FunctionBody_CompoundCommand compound_command' ->
     compound_command'__to__command compound_command'
  | FunctionBody_CompoundCommand_RedirectList (compound_command', redirect_list') ->
     compound_command'__to__command compound_command'
     |> redirect_list'__to__command redirect_list'

and fname__to__name = function
  | Fname_Name name -> name__to__name name

and brace_group__to__command = function
  | BraceGroup_LBrace_CompoundList_RBrace compound_list' ->
     compound_list'__to__command compound_list'

and do_group__to__command = function
  | DoGroup_Do_CompoundList_Done compound_list' ->
     compound_list'__to__command compound_list'

and simple_command__to__command = function
  | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cmd_prefix', cmd_word', cmd_suffix') ->
     let assignments, redirections = cmd_prefix'__to__assignments_redirections [] [] cmd_prefix' in
     let words, redirections = cmd_suffix'__to__words_redirections [] redirections cmd_suffix' in
     AST.Simple {
         assignments ;
         words = (cmd_word'__to__word cmd_word') :: words ;
         redirections
       }
  | SimpleCommand_CmdPrefix_CmdWord (cmd_prefix', cmd_word') ->
     let assignments, redirections = cmd_prefix'__to__assignments_redirections [] [] cmd_prefix' in
     AST.Simple {
         assignments ;
         words = [cmd_word'__to__word cmd_word'] ;
         redirections
       }
  | SimpleCommand_CmdPrefix cmd_prefix' ->
     let assignments, redirections = cmd_prefix'__to__assignments_redirections [] [] cmd_prefix' in
     AST.Simple {
         assignments ;
         words = [] ;
         redirections
       }
  | SimpleCommand_CmdName_CmdSuffix (cmd_name', cmd_suffix') ->
     let words, redirections = cmd_suffix'__to__words_redirections [] [] cmd_suffix' in
     AST.Simple {
         assignments = [] ;
         words = (cmd_name'__to__word cmd_name') :: words ;
         redirections
       }
  | SimpleCommand_CmdName cmd_name' ->
     AST.Simple {
         assignments = [] ;
         words = [cmd_name'__to__word cmd_name'] ;
         redirections = []
       }

and cmd_name__to__word = function
  | CmdName_Word word' ->
     word'__to__word word'

and cmd_word__to__word = function
  | CmdWord_Word word' ->
     word'__to__word word'

and cmd_prefix__to__assignments_redirections assignments redirections = function
  | CmdPrefix_IoRedirect io_redirect' ->
     assignments, (io_redirect'__to__redirection io_redirect') :: redirections
  | CmdPrefix_CmdPrefix_IoRedirect (cmd_prefix', io_redirect') ->
     cmd_prefix'__to__assignments_redirections assignments ((io_redirect'__to__redirection io_redirect') :: redirections) cmd_prefix'
  | CmdPrefix_AssignmentWord assignment_word' ->
     (assignment_word'__to__assignment assignment_word') :: assignments, redirections
  | CmdPrefix_CmdPrefix_AssignmentWord (cmd_prefix', assignment_word') ->
     cmd_prefix'__to__assignments_redirections ((assignment_word'__to__assignment assignment_word') :: assignments) redirections cmd_prefix'

and cmd_suffix__to__words_redirections words redirections = function
  | CmdSuffix_IoRedirect io_redirect' ->
     words, (io_redirect'__to__redirection io_redirect') :: redirections
  | CmdSuffix_CmdSuffix_IoRedirect (cmd_suffix', io_redirect') ->
     cmd_suffix'__to__words_redirections words ((io_redirect'__to__redirection io_redirect') :: redirections) cmd_suffix'
  | CmdSuffix_Word word' ->
     (word'__to__word word') :: words, redirections
  | CmdSuffix_CmdSuffix_Word (cmd_suffix', word') ->
     cmd_suffix'__to__words_redirections ((word'__to__word word') :: words) redirections cmd_suffix'

and redirect_list__to__command redirect_list command =
  match redirect_list with
  | RedirectList_IoRedirect io_redirect' ->
     AST.Redirection {
         command ;
         redirection = io_redirect'__to__redirection io_redirect'
       }
  | RedirectList_RedirectList_IoRedirect (redirect_list', io_redirect') ->
     AST.Redirection {
         command ;
         redirection = io_redirect'__to__redirection io_redirect'
       }
     |> redirect_list'__to__command redirect_list'
     
and io_redirect__to__redirection = function
  | IoRedirect_IoFile io_file' ->
     AST.{
        channel = None ;
        container = io_file'__to__container io_file'
     }
  | IoRedirect_IoNumber_IoFile (io_number, io_file') ->
     AST.{
        channel = Some (io_number__to__int io_number) ;
        container = io_file'__to__container io_file'
     }
  | IoRedirect_IoHere io_here' ->
     AST.{
        channel = None ;
        container = io_here'__to__container io_here'
     }
  | IoRedirect_IoNumber_IoHere (io_number, io_here') ->
     AST.{
        channel = Some (io_number__to__int io_number) ;
        container = io_here'__to__container io_here'
     }

and io_file__to__container io_file =
  let kind, filename' = 
    AST.(match io_file with
    | IoFile_Less_FileName filename' -> Less, filename'
    | IoFile_LessAnd_FileName filename' -> LessAnd, filename'
    | IoFile_Great_FileName filename' -> Great, filename'
    | IoFile_GreatAnd_FileName filename' -> GreatAnd, filename'
    | IoFile_DGreat_FileName filename' -> DGreat, filename'
    | IoFile_LessGreat_FileName filename' -> LessGreat, filename'
    | IoFile_Clobber_FileName filename' -> Clobber, filename')
  in
  AST.File {
      kind ;
      word = filename'__to__word filename'
    }

and filename__to__word = function
  | Filename_Word word' ->
     word'__to__word word'

and io_here__to__container = function
  | IoHere_DLess_HereEnd (_, word'_ref) ->
     AST.Here {
         trim = false ;
         word = word'__to__word !word'_ref
       }
  | IoHere_DLessDash_HereEnd (_, word'_ref) ->
     AST.Here {
         trim = true ;
         word = word'__to__word !word'_ref
       }

and separator_op__to__command sep_op command =
  match sep_op with
  | SeparatorOp_Uppersand -> AST.Async command
  | SeparatorOp_Semicolon -> command

and separator__to__command sep command =
  match sep with
  | Separator_SeparatorOp_LineBreak (sep_op', _) ->
     separator_op'__to__command sep_op' command
  | Separator_NewLineList _ ->
     command

and sequential_sep__to__command _ command = command

and word__to__word = function
  | Word word -> word

and name__to__name = function
  | Name name -> name

and assignment_word__to__assignment = function
  | AssignmentWord (name, word) ->
     AST.{
        name = name__to__name name ;
        word = word__to__word word
     }

and io_number__to__int = function
  | IONumber io_number -> int_of_string io_number
               


(* Located versions. Sadly, we have to eta-expand everything in here,
   because OCaml does not like values on the right-hand side of a
   [let rec]. Otherwise, we could write [let rec a = a]. *)

and on_located : 'a 'b. ('a -> 'b) -> 'a located -> 'b =
  fun f x -> f x.value

and complete_command'__to__command complete_command' =
  on_located complete_command__to__command complete_command'

and clist'__to__command clist' =
  on_located clist__to__command clist'

and and_or'__to__command and_or' =
  on_located and_or__to__command and_or'

and pipeline'__to__command pipeline' =
  on_located pipeline__to__command pipeline'

and pipe_sequence'__to__command pipe_sequence' =
  on_located pipe_sequence__to__command pipe_sequence'

and command'__to__command command' =
  on_located command__to__command command'

and compound_command'__to__command compound_command' =
  on_located compound_command__to__command compound_command'

and subshell'__to__command subshell' =
  on_located subshell__to__command subshell'

and compound_list'__to__command compound_list' =
  on_located compound_list__to__command compound_list'

and term'__to__command term' =
  on_located term__to__command term'

and for_clause'__to__command for_clause' =
  on_located for_clause__to__command for_clause'

and wordlist'__to__word_list wordlist' =
  on_located wordlist__to__word_list wordlist'

and case_clause'__to__command case_clause' =
  on_located case_clause__to__command case_clause'

and case_list_ns'__to__case_list case_list_ns' =
  on_located case_list_ns__to__case_list case_list_ns'

and case_list'__to__case_list case_list' =
  on_located case_list__to__case_list case_list'

and case_item_ns'__to__case case_item_ns' =
  on_located case_item_ns__to__case case_item_ns'

and case_item'__to__case case_item' =
  on_located case_item__to__case case_item'

and pattern'__to__word_list pattern' =
  on_located pattern__to__word_list pattern'

and if_clause'__to__command if_clause' =
  on_located if_clause__to__command if_clause'

and else_part'__to__command else_part' =
  on_located else_part__to__command else_part'

and while_clause'__to__command while_clause' =
  on_located while_clause__to__command while_clause'

and until_clause'__to__command until_clause' =
  on_located until_clause__to__command until_clause'

and function_definition'__to__command function_definition' =
  on_located function_definition__to__command function_definition'

and function_body'__to__command function_body' =
  on_located function_body__to__command function_body'

and fname'__to__name fname' =
  on_located fname__to__name fname'

and brace_group'__to__command brace_group' =
  on_located brace_group__to__command brace_group'

and do_group'__to__command do_group' =
  on_located do_group__to__command do_group'

and simple_command'__to__command simple_command' =
  on_located simple_command__to__command simple_command'

and cmd_name'__to__word cmd_name' =
  on_located cmd_name__to__word cmd_name'

and cmd_word'__to__word cmd_word' =
  on_located cmd_word__to__word cmd_word'

and cmd_prefix'__to__assignments_redirections assignments redirections cmd_prefix' =
  on_located (cmd_prefix__to__assignments_redirections assignments redirections) cmd_prefix'
  
and cmd_suffix'__to__words_redirections words redirections cmd_suffix' =
  on_located (cmd_suffix__to__words_redirections words redirections) cmd_suffix'

and redirect_list'__to__command redirect_list' command =
  on_located redirect_list__to__command redirect_list' command

and io_redirect'__to__redirection io_redirect' =
  on_located io_redirect__to__redirection io_redirect'

and io_file'__to__container io_file' =
  on_located io_file__to__container io_file'

and io_here'__to__container io_here' =
  on_located io_here__to__container io_here'

and filename'__to__word filename' =
  on_located filename__to__word filename'
  
and separator_op'__to__command sep_op' command =
  on_located separator_op__to__command sep_op' command

and separator'__to__command sep' command =
  on_located separator__to__command sep' command

and sequential_sep'__to__command seq_sep' command =
  on_located sequential_sep__to__command seq_sep' command

and word'__to__word word' =
  on_located word__to__word word'

and name'__to__name name' =
  on_located name__to__name name'

and assignment_word'__to__assignment assignment_word' =
  on_located assignment_word__to__assignment assignment_word'
