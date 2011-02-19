%%% -*- coding: latin-1 -*-

%%% The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1, (the  "License");  you may  not  use this  file except  in
%%% compliance  with the License.  You should  have received  a copy  of the
%%% Erlang  Public License  along  with this  software.  If not,  it can  be
%%% retrieved at http://plc.inf.elte.hu/erlang/
%%%
%%% Software  distributed under  the License  is distributed  on an  "AS IS"
%%% basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
%%% the License  for the specific language governing  rights and limitations
%%% under the License.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the  Original Code is Eötvös Loránd University.
%%% Portions created  by Eötvös  Loránd University are  Copyright 2008-2009,
%%% Eötvös Loránd University. All Rights Reserved.



%%% ============================================================================
%%% Types

%% @type generator() = {generator,
%%           ModuleName::atom(),
%%           FunctionName::atom(),
%%           Arguments::list()}.
%%
%% The generator record contain a module name, a function name and a argument
%% list. Theses three value describe the `ModName:FunName/FunArity' function
%% where `FunArity' is the length of the `Arguments' list.
%% This record is describe a function call method. The function call produce 
%% (generate) the return value that is interested by us.
%%
%% See the {@link ucl_options:generate/1} function.
-record(generator, {
    modname,
    funname,
    args}).


%% @type fun_result(Arity) = function(Arity) | ModFun | generator()
%%       Arity = natural()
%%       ModFun = {ModName::atom(), FunName::atom()}.
%% This union type is used in the validator functions of the algorithm step.
%% When a step need a function it is not matter how give that. The function can
%% be a function expression, a module name function name pair or a generator
%% record which has a function result. In over all the given function must be
%% an function expression or an exported function with `Arity' arity.


