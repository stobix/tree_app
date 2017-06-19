%%%=========================================================================
%%% Module tree_srv
%%%=========================================================================
%%% @author Joel Ericson <kasettbok@gmail.com>
%%%
%%% @copyright Copylefted using some GNU license or other.
%%%
%%% @version 0.9
%%%-------------------------------------------------------------------------
%%% @doc This module contains various gb_trees to be accessed and updated by various modules.
%%% @end
%%%=========================================================================
%%%=========================================================================
%%%                                 LICENSE
%%%=========================================================================
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU Library General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%=========================================================================
%%%=========================================================================
-module(tree_srv).
-vsn("$Header$").


-behaviour(gen_server).

-export([enter/3,
        store/2,
        new/1,
        lookup/2,
        to_list/1,
        clear/1,
        delete_any/2,
        delete/2,
        insert/3
        ]).

-ifdef(test).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/0,init/1]).
-export([handle_call/3,handle_cast/2]).
-export([terminate/2]).
-export([handle_info/2,code_change/3]).

-include_lib("newdebug/include/debug.hrl").

%%%=========================================================================
%%% Type specifications
%%%=========================================================================
%%% 
%%% @type uniqe_integer() = non_neg_integer(). An integer whose value is not the same as any other currently in use.
%%% @end
%%%=========================================================================



%%%=========================================================================
%%% gen_server functions
%%%=========================================================================

start_link() ->
  ?DEB1({tree,1},"Starting tree server"),
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(_) ->
  ?DEB1({tree,1},"Tree server started"),
  {ok,#{}}.

terminate(_Reason,_State) -> ok.

code_change(_,_,_) -> ok.
handle_info(_,_) -> ok.

%%%=========================================================================
%%% exports
%%%=========================================================================


%%----------------------------------------------
%% @doc Stores a gb_trees tree under id TreeID for later use.
%% Returns with an error if the TreeID is already in use.
%% @spec (term(),gb_trees())-> ok|{error,exists}
%% @end
%%----------------------------------------------
store(TreeID,Tree) ->
  gen_server:call(?MODULE,{store_tree,TreeID,Tree}).

% TODO: Make this work in a parallel environment.
%%----------------------------------------------
%% @doc Updates the entry Entry with key Key in the tree TreeID using gb_trees:enter
%% @spec (term(),term(),gb_trees()) -> ok
%% @end
%%----------------------------------------------
enter(Key,Entry,TreeID) ->
  gen_server:cast(?MODULE,{update,Key,Entry,TreeID}). 


insert(Key,Entry,TreeID) ->
  gen_server:cast(?MODULE,{insert,Key,Entry,TreeID}).

%%----------------------------------------------
%% @doc creates a new tree and associates it with TreeID.
%% @spec (term())-> ok|{error,exists}
%% @end
%%----------------------------------------------
new(TreeID) ->
  gen_server:call(?MODULE,{store_tree,TreeID,gb_trees:empty()}).

%%----------------------------------------------
%% @doc returns the entry Entry with key Key in the tree TreeID.
%% @spec (term(),term())-> {value,Entry}|none
%% @end
%%----------------------------------------------
lookup(Key,TreeID) ->
  gen_server:call(?MODULE,{lookup,Key,TreeID}).

to_list(TreeID) ->
  gen_server:call(?MODULE,{to_list,TreeID}).

clear(TreeID) ->
  gen_server:cast(?MODULE,{clear,TreeID}).

delete_any(Key,TreeID) ->
  gen_server:cast(?MODULE,{remove,Key,TreeID}).

delete(Key,TreeID) ->
  gen_server:cast(?MODULE,{delete,Key,TreeID}).

%%%=========================================================================
%%% gen_server callback functions.
%%%=========================================================================

doTree(TreeID,Trees,Fun) ->
    case maps:find(TreeID,Trees) of
        {ok,Tree} ->
            Fun(Tree);
        error ->
            error
    end.

% Returns an updated tree map if the tree was found - otherwise returns the old tree.
updateTree(TreeID,Trees,Fun) ->
    case maps:find(TreeID,Trees) of
        {ok,Tree} ->
            NewTree=Fun(Tree),
            Trees#{TreeID=>NewTree};
        error ->
            Trees
    end.

on2(F,Arg)->fun(Brg) -> F(Arg,Brg) end.
on3(F,Arg,Brg)->fun(Crg) -> F(Arg,Brg,Crg) end.

handle_call({to_list,TreeID},_From,Trees) ->
  Reply=doTree(TreeID,Trees,fun gb_trees:to_list/1),
  {reply,Reply,Trees};

handle_call({lookup,Key,TreeID},_From,Trees) ->
  Reply=doTree(TreeID,Trees,on2(Key,fun gb_trees:lookup/2)),
  {reply,Reply,Trees};

handle_call({store_tree,TreeID,Tree},_From,Trees) ->
  ?DEBL({tree,2},"storing a tree ~p into ~p",[TreeID,Trees]),
  case maps:is_key(TreeID,Trees) of
    false -> {reply,ok,Trees#{TreeID=>Tree}};
    true ->  {reply,{error,exists},Trees}
  end.

handle_cast({delete,Key,TreeID},Trees) ->
  NewTrees=updateTree(TreeID,Trees,on2(Key,fun gb_trees:delete/2)),
  {noreply,NewTrees};

handle_cast({remove,Key,TreeID},Trees) ->
  NewTrees=updateTree(TreeID,Trees,on2(Key,fun gb_trees:delete_any/2)),
  {noreply,NewTrees};

handle_cast({clear,TreeID},Trees) ->
  {noreply,Trees#{TreeID=>gb_trees:empty()}};

handle_cast({insert,Key,Entry,TreeID},Trees) ->
  NewTrees=updateTree(TreeID,Trees,on3(Key,Entry,fun gb_trees:insert/3)),
  {noreply,NewTrees};
    

handle_cast({update,Key,Entry,TreeID},Trees) ->
  NewTrees=updateTree(TreeID,Trees,on3(Key,Entry,fun gb_trees:enter/3)),
  {noreply,NewTrees}.

