-- xmtn-hooks.lua --- mtn Lua hook functions used in all xmtn automate
-- stdio sessions
--
-- Copyright (C) 2010 Stephen Leake
--
-- Author: Stephen Leake
-- Keywords: tools
--
-- This file is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This file is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this file; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
-- Boston, MA  02110-1301  USA.

function get_mtn_command(host)
   -- Return a mtn command line for the remote host. 
   -- 
   -- If the remote host is a Windows machine (ie file: on a Windows
   -- machine), we need the Cygwin mtn executable, since the Win32
   -- executable does not support file: or ssh:.
   --
   -- But we have no way to tell what the remote machine is. So we let
   -- the lisp code figure that out from user options, and it provides
   -- the mtn command to this hook by defining the XMTN_SYNC_MTN
   -- environment variable.

   return os.getenv("XMTN_SYNC_MTN");
end


-- end of file
