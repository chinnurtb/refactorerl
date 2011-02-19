%%% Copyright Notice © 2007 Eötvös Loránd University and Ericsson Hungary
%%% Software development supported by Ericsson Hungary and
%%% GVOP-3.2.2-2004-07-0005/3.0 ELTE IKKK.

%%% Materials  were  created, authored,  and/or  prepared  by  the Authors  at
%%% Department   of  Programming  Languages   and  Compilers,   Eötvös  Loránd
%%% University,  Budapest,  Hungary  (ELTE)  for Ericsson  Hungary  under  the
%%% agreement  between  Ericsson  Hungary  and  ELTE  IKKK.  Unless  otherwise
%%% specifically stated, no claim to copyright is being asserted and it may be
%%% freely  used as  in the  public domain  in accordance  with  Erlang Public
%%% License.  All rights,  including copyright,  are owned  or  controlled for
%%% these purposes by  the Ericsson Hungary and ELTE.  Copyright exists in all
%%% other original  material published on the  internet and may  belong to the
%%% authors depending on the circumstances of publication.

%%% --------------------------------------------------------------------------
%%% ``The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1,  (the  "License"); you  may  not  use  this file  except  in
%%% compliance with the License. You should have received a copy of the Erlang
%%% Public License along  with this software. If not, it  can be retrieved via
%%% the world wide web at http://www.erlang.org/.

%%% Software distributed under the License is distributed on an "AS IS" basis,
%%% WITHOUT WARRANTY OF  ANY KIND, either express or  implied. See the License
%%% for  the specific  language  governing rights  and  limitations under  the
%%% License.

%%% The Initial  Developer of  the Original Code  is Ericsson  Utvecklings AB.
%%% Portions created by Ericsson  are Copyright 1999, Ericsson Utvecklings AB.
%%% All Rights Reserved.''
%%% --------------------------------------------------------------------------

%%% The Contributors are the Authors listed below. All Rights Reserved.

%%% You may not alter or remove any trademark, copyright or other notice from
%%% copies of the content.

%%% Authors: Zoltán Csörnyei
%%%          Zoltán Horváth
%%%          Roland Király
%%%          Róbert Kitlei
%%%          Tamás Kozsik
%%%          László Lövei
%%%          Tamás Nagy
%%%          Melinda Tóth
%%%          Anikó Víg

%%% Author contact: erlang@plc.inf.elte.hu
%%% --------------------------------------------------------------------------

%% @copyright 2007 Eötvös Loránd University and Ericsson Hungary
%% @author Tamas Nagy <lestat@elte.hu>
%% @author Aniko Vig <viganiko@inf.elte.hu>

%% @doc 
%% This module provides an interface for distel.
%% 
%% @end

-module(d_client).

-export([db_init/0,db_tofrom/1,check_out/1,
         rename/4,
         rename_variable/4,rename_function/4,
	 merge_subexpression/6, extract_function/6,
	 reorder_funpar/4,tuple_funpar/4,var_elim/3]).

%% =====================================================================
%% @spec db_init() ->
%%                 {ok,ok}
%%
%% @doc
%% Initializes the refactor database.
%% It completely destroys the database and data in it. 
%% After that, it reconstructs the storage system.
%% 
%% @end
%% =====================================================================
db_init() ->
    ok = init(),
    db_init:init(),
    {ok,ok}.

%% =====================================================================
%% @spec db_tofrom(File::string()) ->
%%                 {ok, string()}
%%
%% @doc
%% Puts the given module into the refactor database.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% </pre>
%% @end
%% =====================================================================
db_tofrom(File) ->
    ok = init(),
    into_db:parse(File),
    out_from_db:create_code(File, File).

%% =====================================================================
%% @spec check_out(File::string()) ->
%%                 {ok, string()}
%%
%% @doc
%% Gets the given module from the refactor database and reloads 
%% it to the given path.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% </pre>
%% @end
%% =====================================================================
check_out(File) ->
    ok = init(),
    out_from_db:create_code(File, File).

%%%%%%%%%%%%%%%%%%%% Refactorings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec do_refactoring(File::string(), Refactoring::function()) ->
%%                 {atom(), term(), string()}
%%
%% @doc
%% Performs the given refactoring on the given module.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>Refactoring</b> : The function of one of the refactorings.
%% </pre>
%% @end
%% =====================================================================
do_refactoring(File, Refactoring) ->
    ok = init(),
    case catch Refactoring() of
      {ok, Data} ->
        out_from_db:create_code(File, File),
        into_db:set_positions(File),
        {ok, Data, File};
      {warning, Data} ->
        out_from_db:create_code(File, File),
        into_db:set_positions(File),
        {warning, Data, File};
      {Result, Data} ->
        {Result, Data, File}
    end.

%% =====================================================================
%% @spec rename(File::string(), 
%%              Line::integer(), Col::integer(),
%%              Newname::string()) ->
%%                 {atom(), term(), string()}
%%
%% @doc
%% General rename: variable or function.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>Line</b> : The positioned lined in the code.
%% <b>Col</b> : The positioned column in the code.
%% <b>Newname</b> : The new name of the variable.
%% </pre>
%% @end
%% =====================================================================
rename(File, Line, Col, Newname) ->
    do_refactoring(
      File, fun() -> 
		    refac_rename:rename(
		      File, Line, Col, Newname) 
	    end).

%% =====================================================================
%% @spec rename_variable(File::string(), 
%%                       Line::integer(), Col::integer(),
%%                       Newname::string()) ->
%%                 {atom(), term(), string()}
%%
%% @doc
%% Performs the rename variable refactoring on the given module.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>Line</b> : The positioned lined in the code.
%% <b>Col</b> : The positioned column in the code.
%% <b>Newname</b> : The new name of the variable.
%% </pre>
%% @end
%% =====================================================================
rename_variable(File, Line, Col, Newname) ->
    do_refactoring(
      File, fun() -> 
		    refac_ren_var:rename_variable(
		      File, Line, Col, Newname) 
	    end).

%% =====================================================================
%% @spec rename_function(File::string(), 
%%                       Line::integer(), Col::integer(),
%%                       Newname::string()) ->
%%                 {atom(), term(), string()}
%%
%% @doc
%% Performs the rename function refactoring on the given module.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>Line</b> : The positioned lined in the code.
%% <b>Col</b> : The positioned column in the code.
%% <b>Newname</b> : The new name of the function.
%% </pre>
%% @end
%% =====================================================================
rename_function(File, Line, Col, Newname) ->    
    do_refactoring(
      File, fun() -> 
		    refac_ren_fun:rename_function(
		      File, Line, Col, Newname) 
	    end).

%% =====================================================================
%% @spec reorder_funpar(File::string(), 
%%                       Line::integer(), Col::integer(),
%%                       Order::string()) ->
%%                 {atom(), term(), string()}
%%
%% @doc
%% Performs the rename variable refactoring on the given module.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>Line</b> : The positioned lined in the code.
%% <b>Col</b> : The positioned column in the code.
%% <b>Order</b> : The new order of the function parameters.
%% </pre>
%% @end
%% =====================================================================
reorder_funpar(File, Line, Col, Order) ->
    do_refactoring(
      File, fun() -> 
		    refac_reorder_funpar:reorder_funpar(
		      File, Line, Col, Order) 
	    end).

%% =====================================================================
%% @spec tuple_funpar(File::string(), 
%%                       Line::integer(), Col::integer(),
%%                       Number::string()) ->
%%                 {atom(), term(), string()}
%%
%% @doc
%% Performs the rename variable refactoring on the given module.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>Line</b> : The positioned lined in the code.
%% <b>Col</b> : The positioned column in the code.
%% <b>Newname</b> : The number of the function parameters which will 
%%                  be in the tuple.
%% </pre>
%% @end
%% =====================================================================
tuple_funpar(File, Line, Col, Number) ->
    do_refactoring(
      File, fun() -> 
		    refac_tuple_funpar:untuple_or_tuple_funpar(
		      File, Line, Col, Number) 
	    end).

%% =====================================================================
%% @spec var_elim(File::string(), 
%%                       Line::integer(), Col::integer()) ->
%%                 {atom(), term(), string()}
%%
%% @doc
%% Performs the eliminate variable refactoring on the given module.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>Line</b> : The positioned lined in the code.
%% <b>Col</b> : The positioned column in the code.
%% </pre>
%% @end
%% =====================================================================
var_elim(File, Line, Col) ->
    do_refactoring(
      File, fun() -> 
		    refac_var_elim:eliminate_variable(
		      File, Line, Col) 
	    end).

%% =====================================================================
%% @spec merge_subexpression(File::string(), 
%%             FromLine::integer(), FromCol::integer(),
%%             ToLine::integer(), ToCol::integer(),
%%             NewName :: string()) ->
%%                 {atom(), string()}
%%
%% @doc
%% Performs the merge subexpression refactoring on the given module.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>FromLine</b> : The pointed first line number in the editor.
%% <b>FromCol</b> : The pointed first column number in the editor.
%% <b>ToLine</b> : The pointed last line number in the editor.
%% <b>ToCol</b> : The pointed last column number in the editor.
%% <b>NewName</b> : The name of the extract function.
%% </pre>
%% @end
%% =====================================================================
merge_subexpression(File, FromLine, FromCol, ToLine, ToCol,NewName) ->
    do_refactoring(
      File, fun() -> 
		    refac_merge_subexpr:merge_subexpression(
		      File, FromLine, FromCol, ToLine, ToCol,NewName)
	    end).
%% =====================================================================
%% @spec extract_function(File::string(), 
%%             FromLine::integer(), FromCol::integer(),
%%             ToLine::integer(), ToCol::integer(),NewName :: string()) ->
%%                 {atom(), string()}
%%
%% @doc
%% Performs the extract function on the given module.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>FromLine</b> : The pointed first line number in the editor.
%% <b>FromCol</b> : The pointed first column number in the editor.
%% <b>ToLine</b> : The pointed last line number in the editor.
%% <b>ToCol</b> : The pointed last column number in the editor.
%% <b>NewName</b> : The name of the extract function.
%% </pre>
%% @end
%% =====================================================================
extract_function(File, FromLine, FromCol, ToLine, ToCol,NewName) ->
    do_refactoring(
      File, fun() -> 
		    refac_extract_fun:extract_function(
		      File, FromLine, FromCol, ToLine, ToCol, NewName) 
	    end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec init() -> term()
%%                 
%%
%% @doc
%% Starts the refactoring system.
%% It starts the refactorer application. 
%% Either it returns ok, which means the start was succesful, or 
%% returns the reason of the failure. 
%% 
%% @end
%% =====================================================================
init()->
    case application:start(refactorer) of
	{ok, _} -> ok;
	{error, {already_started, _}} -> ok;
	Error -> Error
    end.

%% =====================================================================
%% @spec stop() -> term()
%%                 
%%
%% @doc
%% Stops the refactoring system.
%% 
%% @end
%% =====================================================================
stop()->
    application:stop(refactorer).
