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

architecture rtl of mem_pipe is
	type StateType is (
		S_IDLE,
		S_SET_PTR,
		S_BEGIN_READ,
		S_EXEC_READ,
		S_INC_READ,
		S_WAIT_RDV_READ,
		S_WAIT_RDY_READ,
		S_BEGIN_WRITE,
		S_EXEC_WRITE,
		S_LOOP_WRITE
	);
	constant CMD_PTR      : std_logic_vector(1 downto 0) := "00";
	constant CMD_RD       : std_logic_vector(1 downto 0) := "01";
	constant CMD_WR       : std_logic_vector(1 downto 0) := "10";
	constant CMD_ILL      : std_logic_vector(1 downto 0) := "11";

	signal state          : StateType := S_IDLE;
	signal state_next     : StateType;
	signal memPtr         : std_logic_vector(22 downto 0) := (others => '0');
	signal memPtr_next    : std_logic_vector(22 downto 0);
	signal wordCount      : std_logic_vector(29 downto 0) := (others => '0');
	signal wordCount_next : std_logic_vector(29 downto 0);
	signal tmpRdData      : std_logic_vector(15 downto 0) := (others => '0');
	signal tmpRdData_next : std_logic_vector(15 downto 0);
begin
	-- Infer registers
	process(clk_in)
	begin
		if ( rising_edge(clk_in) ) then
			if ( reset_in = '1' ) then
				state <= S_IDLE;
				memPtr <= (others => '0');
				wordCount <= (others => '0');
				tmpRdData <= (others => '0');
			else
				state <= state_next;
				memPtr <= memPtr_next;
				wordCount <= wordCount_next;
				tmpRdData <= tmpRdData_next;
			end if;
		end if;
	end process;

	-- Next state logic
	process(state, memPtr, wordCount, cmdData_in, cmdValid_in, rspReady_in, tmpRdData, mcData_in, mcRDV_in, mcReady_in)
	begin
		-- Internal registers keep their values by default
		state_next <= state;
		memPtr_next <= memPtr;
		wordCount_next <= wordCount;
		tmpRdData_next <= tmpRdData;

		-- Memory controller signal defaults
		mcCmd_out <= MC_NOP;
		mcAddr_out <= (others => '0');
		mcData_out <= (others => '0');

		-- Read/write pipe signal defaults
		cmdReady_out <= '0';
		rspData_out <= (others => '0');
		rspValid_out <= '0';
		
		case state is

			-- Register the low-order word of the memory pointer and return to the IDLE state
			when S_SET_PTR =>
				cmdReady_out <= '1';
				if ( cmdValid_in = '1' ) then
					memPtr_next(15 downto 0) <= cmdData_in;
					state_next <= S_IDLE;
				end if;


			-------------------------------------------------------------------------------------------
			-- Read states
			-------------------------------------------------------------------------------------------
				
			-- Register the low-order word of the read count, then if the memory controller is ready,
			-- issue the read command, else switch to EXEC_READ which will wait until it IS ready.
			when S_BEGIN_READ =>
				cmdReady_out <= '1';
				if ( cmdValid_in = '1' ) then
					wordCount_next(15 downto 0) <= cmdData_in;
					if ( mcReady_in = '1' ) then
						mcCmd_out <= MC_RD;
						mcAddr_out <= memPtr;
						state_next <= S_INC_READ;
					else
						state_next <= S_EXEC_READ;
					end if;
				end if;

			-- Wait until the memory controller is ready, then issue a read.
			when S_EXEC_READ =>
				if ( mcReady_in = '1' ) then
					mcCmd_out <= MC_RD;
					mcAddr_out <= memPtr;
					state_next <= S_INC_READ;
				end if;

			-- Increment the memory pointer, decrement the read count.
			when S_INC_READ =>
				memPtr_next <= std_logic_vector(unsigned(memPtr) + 1);
				wordCount_next <= std_logic_vector(unsigned(wordCount) - 1);
				state_next <= S_WAIT_RDV_READ;

			-- Wait until this memory read completes, then either loop back to EXEC_READ to issue
			-- another read command, or return to IDLE.
			when S_WAIT_RDV_READ =>
				if ( mcRDV_in = '1' ) then
					if ( rspReady_in = '1' ) then
						rspData_out <= mcData_in;
						rspValid_out <= '1';
						if ( unsigned(wordCount) = 0 ) then
							state_next <= S_IDLE;
						else
							state_next <= S_EXEC_READ;
						end if;
					else
						tmpRdData_next <= mcData_in;
						state_next <= S_WAIT_RDY_READ;
					end if;
				end if;

			-- Wait until there is room in the response FIFO, then either loop back to EXEC_READ to
			-- issue another read command, or return to IDLE.
			when S_WAIT_RDY_READ =>
				if ( rspReady_in = '1' ) then
					rspData_out <= tmpRdData;
					rspValid_out <= '1';
					if ( unsigned(wordCount) = 0 ) then
						state_next <= S_IDLE;
					else
						state_next <= S_EXEC_READ;
					end if;
				end if;


			-------------------------------------------------------------------------------------------
			-- Write states
			-------------------------------------------------------------------------------------------

			-- Register the low-order word of the read count, then if the memory controller is ready,
			-- issue the read command, else switch to EXEC_READ which will wait until it IS ready.
			when S_BEGIN_WRITE =>
				cmdReady_out <= '1';
				if ( cmdValid_in = '1' ) then
					wordCount_next(15 downto 0) <= cmdData_in;
					state_next <= S_EXEC_WRITE;
				end if;

			-- Wait for memory controller to become ready, then tell command pipe we're ready for the
			-- first word. When it arrives, kick off a memory controller write operation.
			when S_EXEC_WRITE =>
				if ( mcReady_in = '1' ) then
					cmdReady_out <= '1';
					if ( cmdValid_in = '1' ) then
						mcCmd_out <= MC_WR;
						mcAddr_out <= memPtr;
						mcData_out <= cmdData_in;
						memPtr_next <= std_logic_vector(unsigned(memPtr) + 1);
						wordCount_next <= std_logic_vector(unsigned(wordCount) - 1);
						state_next <= S_LOOP_WRITE;
					end if;
				end if;
					
			-- Either loop back to EXEC_WRITE to issue another write command, or return to IDLE.
			when S_LOOP_WRITE =>
				if ( unsigned(wordCount) = 0 ) then
					state_next <= S_IDLE;
				else
					state_next <= S_EXEC_WRITE;
				end if;


			-------------------------------------------------------------------------------------------
			-- S_IDLE, etc
			-------------------------------------------------------------------------------------------
			when others =>
				cmdReady_out <= '1';
				if ( cmdValid_in = '1' ) then
					case cmdData_in(15 downto 14) is
						when CMD_PTR =>
							-- Update pointer
							memPtr_next(22 downto 16) <= cmdData_in(6 downto 0);
							state_next <= S_SET_PTR;

						when CMD_RD =>
							-- Read some data
							wordCount_next(29 downto 16) <= cmdData_in(13 downto 0);
							state_next <= S_BEGIN_READ;

						when CMD_WR =>
							-- Write some data
							wordCount_next(29 downto 16) <= cmdData_in(13 downto 0);
							state_next <= S_BEGIN_WRITE;

						when others =>
							null;
					end case;
				end if;
				null;
		end case;
	end process;
	
end architecture;
