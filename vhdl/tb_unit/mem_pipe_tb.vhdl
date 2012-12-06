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
use ieee.std_logic_textio.all;
use std.textio.all;
use work.hex_util.all;
use work.mem_ctrl_pkg.all;

entity mem_pipe_tb is
end mem_pipe_tb;

architecture behavioural of mem_pipe_tb is
	-- Clocks
	signal sysClk     : std_logic;  -- main system clock
	signal dispClk    : std_logic;  -- display version of sysClk, which transitions 4ns before it
	
	-- I/O pipes
	signal cmdData    : std_logic_vector(15 downto 0);
	signal cmdValid   : std_logic;
	signal cmdReady   : std_logic;
	signal rspData    : std_logic_vector(15 downto 0);
	signal rspValid   : std_logic;
	signal rspReady   : std_logic;
	
	-- Memory controller interface
	signal mcCmd      : MCCmdType;
	signal mcAddr     : std_logic_vector(22 downto 0);
	signal mcDataWr   : std_logic_vector(15 downto 0);
	signal mcDataRd   : std_logic_vector(15 downto 0);
	signal mcRDV      : std_logic;
	signal mcReady    : std_logic;
	signal reset      : std_logic;
begin
	-- Instantiate the memory controller for testing
	uut: entity work.mem_pipe
		port map(
			clk_in       => sysClk,
			reset_in     => reset,

			-- I/O pipes
			cmdData_in   => cmdData,
			cmdValid_in  => cmdValid,
			cmdReady_out => cmdReady,
			rspData_out  => rspData,
			rspValid_out => rspValid,
			rspReady_in  => rspReady,

			-- Memory controller interface
			mcReady_in   => mcReady,
			mcCmd_out    => mcCmd,
			mcAddr_out   => mcAddr,
			mcData_out   => mcDataWr,
			mcData_in    => mcDataRd,
			mcRDV_in     => mcRDV
		);

	-- Drive the clocks. In simulation, sysClk lags 4ns behind dispClk, to give a visual hold time
	-- for signals in GTKWave.
	process
	begin
		sysClk <= '0';
		dispClk <= '0';
		wait for 16 ns;
		loop
			dispClk <= not(dispClk);  -- first dispClk transitions
			wait for 4 ns;
			sysClk <= not(sysClk);  -- then sysClk transitions, 4ns later
			wait for 6 ns;
		end loop;
	end process;

	-- Deassert the synchronous reset a couple of cycles after startup.
	--
	process
	begin
		reset <= '1';
		wait until rising_edge(sysClk);
		wait until rising_edge(sysClk);
		reset <= '0';
		wait;
	end process;

	-- Drive the unit under test. Read stimulus from stimulus.sim and write results to results.sim
	process
		variable inLine  : line;
		variable outLine : line;
		file inFile      : text open read_mode is "stimulus.sim";
		file outFile     : text open write_mode is "results.sim";
		function from_mcCmd(cmd : MCCmdType) return string is begin
			case cmd is
				when MC_RD =>
					return "RD ";
				when MC_WR =>
					return "WR ";
				when MC_REF =>
					return "REF";
				when MC_NOP =>
					return "NOP";
				when others =>
					return "ILL";
			end case;
		end function;
	begin
		cmdData <= (others => 'X');
		cmdValid <= '0';
		rspReady <= '1';
		mcReady <= '0';
		mcDataRd <= (others => 'X');
		mcRDV <= '0';
		wait until falling_edge(reset);
		wait until rising_edge(sysClk);
		while ( not endfile(inFile) ) loop
			readline(inFile, inLine);
			while ( inLine.all'length = 0 or inLine.all(1) = '#' or inLine.all(1) = ht or inLine.all(1) = ' ' ) loop
				readline(inFile, inLine);
			end loop;
			cmdData <= to_4(inLine.all(1)) & to_4(inLine.all(2)) & to_4(inLine.all(3)) & to_4(inLine.all(4));
			cmdValid <= to_1(inLine.all(6));
			rspReady <= to_1(inLine.all(8));
			mcReady <= to_1(inLine.all(10));
			mcDataRd <= to_4(inLine.all(12)) & to_4(inLine.all(13)) & to_4(inLine.all(14)) & to_4(inLine.all(15));
			mcRDV <= to_1(inLine.all(17));
			wait for 10 ns;
			write(outLine, from_4(cmdData(15 downto 12)) & from_4(cmdData(11 downto 8)) & from_4(cmdData(7 downto 4)) & from_4(cmdData(3 downto 0)));
			write(outLine, ' ');
			write(outLine, cmdValid);
			write(outLine, ' ');
			write(outLine, cmdReady);
			write(outLine, ' ');
			write(outLine, '|');
			write(outLine, ' ');
			write(outLine, from_4(rspData(15 downto 12)) & from_4(rspData(11 downto 8)) & from_4(rspData(7 downto 4)) & from_4(rspData(3 downto 0)));
			write(outLine, ' ');
			write(outLine, rspValid);
			write(outLine, ' ');
			write(outLine, rspReady);
			write(outLine, ' ');
			write(outLine, '|');
			write(outLine, ' ');
			write(outLine, mcReady);
			write(outLine, ' ');
			write(outLine, from_mcCmd(mcCmd));
			write(outLine, ' ');
			write(outLine, from_4("0" & mcAddr(22 downto 20)) & from_4(mcAddr(19 downto 16)) & from_4(mcAddr(15 downto 12)) & from_4(mcAddr(11 downto 8)) & from_4(mcAddr(7 downto 4)) & from_4(mcAddr(3 downto 0)));
			write(outLine, ' ');
			write(outLine, from_4(mcDataWr(15 downto 12)) & from_4(mcDataWr(11 downto 8)) & from_4(mcDataWr(7 downto 4)) & from_4(mcDataWr(3 downto 0)));
			write(outLine, ' ');
			write(outLine, from_4(mcDataRd(15 downto 12)) & from_4(mcDataRd(11 downto 8)) & from_4(mcDataRd(7 downto 4)) & from_4(mcDataRd(3 downto 0)));
			write(outLine, ' ');
			write(outLine, mcRDV);
			writeline(outFile, outLine);
			wait for 10 ns;
		end loop;
		cmdData <= (others => 'X');
		cmdValid <= '0';
		mcRDV <= '0';
		mcReady <= '0';
		wait;
	end process;
end architecture;
