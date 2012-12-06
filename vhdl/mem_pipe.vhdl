--
-- Copyright (C) 2012 Chris McClelland
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_ctrl_pkg.all;

entity mem_pipe is
	port(
		clk_in         : in    std_logic;
		reset_in       : in    std_logic;

		-- Command & write data pipe
		cmdData_in     : in    std_logic_vector(15 downto 0);
		cmdValid_in    : in    std_logic;
		cmdReady_out   : out   std_logic;

		-- Read data response pipe
		rspData_out    : out   std_logic_vector(15 downto 0);
		rspValid_out   : out   std_logic;
		rspReady_in    : in    std_logic;

		-- Memory controller interface
		--mcAutoMode_out : out   std_logic;
		mcReady_in     : in    std_logic;
		mcCmd_out      : out   MCCmdType;
		mcAddr_out     : out   std_logic_vector(22 downto 0);
		mcData_out     : out   std_logic_vector(15 downto 0);
		mcData_in      : in    std_logic_vector(15 downto 0);
		mcRDV_in       : in    std_logic
	);
end entity;
