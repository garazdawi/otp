%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxGridCellBoolRenderer).
-moduledoc """
This class may be used to format boolean data in a cell.

See:
* `m:wxGridCellRenderer`

* `m:wxGridCellFloatRenderer`

* `m:wxGridCellNumberRenderer`

* `m:wxGridCellStringRenderer`

This class is derived, and can use functions, from:

* `m:wxGridCellRenderer`

wxWidgets docs: [wxGridCellBoolRenderer](https://docs.wxwidgets.org/3.2/classwx_grid_cell_bool_renderer.html)
""".
-include("wxe.hrl").
-export([destroy/1,new/0]).

%% inherited exports
-export([draw/8,getBestSize/6,parent_class/1]).

-type wxGridCellBoolRenderer() :: wx:wx_object().
-export_type([wxGridCellBoolRenderer/0]).
-doc false.
parent_class(wxGridCellRenderer) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "".
-spec new() -> wxGridCellBoolRenderer().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxGridCellBoolRenderer_new),
  wxe_util:rec(?wxGridCellBoolRenderer_new).

-doc "Destroys the object".
-spec destroy(This::wxGridCellBoolRenderer()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGridCellBoolRenderer),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxGridCellBoolRenderer_destroy),
  ok.
 %% From wxGridCellRenderer
-doc false.
getBestSize(This,Grid,Attr,Dc,Row,Col) -> wxGridCellRenderer:getBestSize(This,Grid,Attr,Dc,Row,Col).
-doc false.
draw(This,Grid,Attr,Dc,Rect,Row,Col,IsSelected) -> wxGridCellRenderer:draw(This,Grid,Attr,Dc,Rect,Row,Col,IsSelected).
