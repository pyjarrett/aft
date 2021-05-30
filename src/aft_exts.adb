-------------------------------------------------------------------------------
-- Aft, Auxiliary (or Ada) file tools.
--
-- Copyright (C) 2021, The Aft developers (see AUTHORS file)
--
-- Aft is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Aft is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;

with Aft.Extension_Count;

procedure Aft_Exts is
    -- Reports the number of files with the given extension.

    package ACL renames Ada.Command_Line;
    package AEC renames Aft.Extension_Count;

    use Ada.Text_IO;

    Current : AEC.Running_Total;
begin
    if ACL.Argument_Count = 0 then
        Put_Line ("No search locations provided to count.");
        return;
    end if;

    for Count in 1 .. ACL.Argument_Count loop
        if not AEC.Recursively_Add (Current, ACL.Argument(Count)) then
            Put_Line ("Unable to search " & ACL.Argument(Count));
        end if;
    end loop;

    AEC.Report (Current);
end Aft_Exts;
