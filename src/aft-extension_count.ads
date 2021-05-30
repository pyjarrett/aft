-------------------------------------------------------------------------------
-- Aft, Ada file tools.
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

with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

package Aft.Extension_Count is

    type Running_Total is private;

    function Recursively_Add
       (R : in out Running_Total; File_Or_Dir_Name : String) return Boolean;

    procedure Report (R : in Running_Total);

private

    use Ada.Strings.Unbounded;

    type Result_Type is record
        Count : Natural := 0;
    end record;

    package Exts_Maps is new Ada.Containers.Ordered_Maps
       (Key_Type => Unbounded_String, Element_Type => Result_Type);

    type Running_Total is record
        Results : Exts_Maps.Map;
    end record;

end Aft.Extension_Count;
