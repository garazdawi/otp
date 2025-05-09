%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2005-2025. All Rights Reserved.
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
%%
-record(ieval, {level = 1,        % Current call level
		line = -1,        % Current source code line (of module)
		module,           % MFA which called the currently
		function,         %  interpreted function
		arguments,        %
                error_info = [],  % [{error_info,Map}] | []

		%% True if the current expression is at the top level
		%% (i.e. the next call will leave interpreted code).
		top = false
	       }).

-define(DB_REF_KEY(Mod), {Mod,db}).
