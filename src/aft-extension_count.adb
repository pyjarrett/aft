-------------------------------------------------------------------------------
-- Aft, Ada file tools.
--
-- Copyright (C) 2021, The Aft developers (see AUTHORS file)
--
-- Aft is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- Aft is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program. If not, see <https://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Directories.Hierarchical_File_Names;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with GNAT.Formatted_String;
with GNAT.OS_Lib;

package body Aft.Extension_Count is
    package AD renames Ada.Directories;
    package OS renames GNAT.OS_Lib;

    function Is_Current_Or_Parent_Directory (Dir_Entry : AD.Directory_Entry_Type) return Boolean is
        --  Return true if the entry is "." or "..".
        Name : constant String := AD.Simple_Name (Dir_Entry);
    begin
        return
            AD.Hierarchical_File_Names.Is_Parent_Directory_Name (Name)
            or else AD.Hierarchical_File_Names.Is_Current_Directory_Name (Name);
    end Is_Current_Or_Parent_Directory;

    procedure Count_Extension
       (R : in out Running_Total; File_Or_Dir_Name : String) is
        use Exts_Maps;
        Extension : constant Unbounded_String  := To_Unbounded_String(AD.Extension (File_Or_Dir_Name));
        Current   : Exts_Maps.Cursor := R.Results.Find (Extension);
    begin
        if Current = Exts_Maps.No_Element then
            R.Results.Insert (Extension, (Count => 1));
        else
            R.Results(Current).Count := R.Results(Current).Count + 1;
        end if;
    end Count_Extension;

    procedure Internal_Add
       (R : in out Running_Total; File_Or_Dir_Name : String) is
        use Ada.Directories;
    begin
        if not AD.Exists (File_Or_Dir_Name) then
            return;
        end if;

        if OS.Is_Directory (File_Or_Dir_Name) then
            -- Recursively add this directory.
            declare
                Filter : constant AD.Filter_Type := (AD.Ordinary_File | AD.Directory => True, others => False);
                Dir_Search : Search_Type;
                Next_Entry : Directory_Entry_Type;
            begin
                AD.Start_Search (Search    => Dir_Search,
                                 Directory => File_Or_Dir_Name,
                                 Pattern   => "*",
                                 Filter    => Filter);
            while More_Entries (Dir_Search) loop
                Get_Next_Entry (Dir_Search, Next_Entry);
                if not Is_Current_Or_Parent_Directory (Next_Entry) then
                    case Kind (Next_Entry) is
                        when Directory => Internal_Add (R, AD.Full_Name(Next_Entry));
                        when Ordinary_File => Internal_Add (R, AD.Full_Name (Next_Entry));
                            when others => null;
                    end case;
                end if;
            end loop;
            End_Search (Dir_Search);
            end;
        elsif OS.Is_Regular_File (File_Or_Dir_Name) then
            Count_Extension (R, File_Or_Dir_Name);
        end if;
    end Internal_Add;

    function Recursively_Add
       (R : in out Running_Total; File_Or_Dir_Name : String) return Boolean is
    begin
        Put_Line ("Recursively adding: " & File_Or_Dir_Name);

        Internal_Add (R, File_Or_Dir_Name);
        return True;
    end Recursively_Add;

    procedure Report (R : in Running_Total) is
        use Ada.Text_IO.Unbounded_IO;
        Column_Width : constant := 30;
    begin
        for Cursor in R.Results.Iterate loop
            Put (Exts_Maps.Key(Cursor));
            Set_Col (Column_Width);
            Put (Item => R.Results(Cursor).Count, Width => Column_Width);
            New_Line;
        end loop;
    end Report;

end Aft.Extension_Count;
