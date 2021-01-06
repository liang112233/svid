--------------------
-- svid_master
-- this will connect to Power Chip and the middle man controller
--------------------

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;

ENTITY svid_master IS
  GENERIC(
    cmd_width   : INTEGER := 12;  -- 3 start bits + 4 addr bits + 5 cmd bits, sent by master
    d_width     : INTEGER := 12;  -- 8 cmd_data + 1p+  3 end, sent by master
	rx_d_width  : INTEGER := 15   -- 15 data sent by slave
    );  
  PORT(
    clock   : IN     STD_LOGIC;                              --system clock
    reset_n : IN     STD_LOGIC;                              --reset
    enable  : IN     STD_LOGIC;                              --initiate transaction
    alert   : IN     STD_LOGIC;                              --alert from slave
    rw      : IN     STD_LOGIC;                              --'0' for read, '1' for write
    tx_cmd  : IN     STD_LOGIC_VECTOR(cmd_width-1 DOWNTO 0); --command to transmit
    tx_data : IN     STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);   --data to transmit
    sclk    : BUFFER STD_LOGIC;                              --svid clock/output to slave
    sdio    : INOUT  STD_LOGIC;                              --serial data input output
    busy    : OUT    STD_LOGIC;                              --busy / data ready signal
    rx_data : OUT    STD_LOGIC_VECTOR(rx_d_width-1 DOWNTO 0));       --data received
END svid_master;

ARCHITECTURE logic OF svid_master IS
  TYPE   machine IS(Rest, Execute);                                 --state machine data type, only two major states for Master
  SIGNAL state       : machine;                                      --current state
  SIGNAL slave       : INTEGER;                                      --slave selected for current transaction
  SIGNAL clk_toggles : INTEGER RANGE 0 TO 39;                        --count svid clock toggles
  SIGNAL assert_data : STD_LOGIC;                                    --'1' is tx sclk toggle, '0' is rx sclk toggle
  SIGNAL rw_buffer   : STD_LOGIC;                                    --read/write buffer
  SIGNAL cmd_buffer  : STD_LOGIC_VECTOR(cmd_width-1 DOWNTO 0);       --command buffer
  SIGNAL d_buffer    : STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);         --data buffer
  SIGNAL last_bit_rx : INTEGER RANGE 0 TO (cmd_width+d_width+rx_d_width-1);       --last rx data bit location
BEGIN
  PROCESS(clock, reset_n)
  BEGIN

    IF(reset_n = '0') THEN        --reset system
      busy <= '1';                --set busy signal
      sdio <= 'Z';                --set master data io to high impedance
      rx_data <= (OTHERS => '0'); --clear receive data port
      state <= Rest;             --go to Rest state when reset is exited

    ELSIF(clock'EVENT AND clock = '1') THEN  -- rising edge
      CASE state IS                 --state machine

        WHEN Rest =>
          busy <= '0';              --clock out not busy signal
          sdio <= 'Z';              --set master data io to high impedance
          sclk <= '1';              -- Rest state , sclk signal stay high

          --user input to initiate transaction
          IF(enable = '1') THEN       
            busy <= '1';             --set busy signal
                                  
            rw_buffer <= rw;         --clock in read/write instruction, when rw= '1' means send out 
            cmd_buffer <= tx_cmd;    --clock in command buffer
            d_buffer <= tx_data;     --clock in data buffer
            clk_toggles <= 0;        --initiate clock toggle counter
            last_bit_rx <= (cmd_width+d_width+rx_d_width-1);       --set last rx data bit
            state <= Execute;        --proceed to execute state
          ELSE
            state <= Rest;          --remain in Rest state
          END IF;

        WHEN Execute =>
          busy <= '1';        --set busy signal
          assert_data <= NOT assert_data;  --switch
          
          
          --transmit/receive indicator
          clk_toggles <= clk_toggles + 1;  --increment svid clock toggles counter
            
          --svid clock toggle needed
          IF(clk_toggles < cmd_width+d_width+rx_d_width) THEN  -- one frame length
             sclk <= NOT sclk;  --toggle svid clock
          END IF;
          --transmit svid clock toggle
          IF(assert_data = '1' AND clk_toggles < cmd_width+1) THEN  --send command 
             sdio <= cmd_buffer(cmd_width-1);                                    --clock out command bit
             cmd_buffer <= cmd_buffer(cmd_width-2 DOWNTO 0) & '0';               --shift command transmit buffer
          END IF; 
          
          IF(assert_data = '1' AND rw_buffer = '1' AND clk_toggles > cmd_width) THEN  --write command and data part 
              sdio <= d_buffer(d_width-1);                                                            --clock out data bit
              d_buffer <= d_buffer(d_width-2 DOWNTO 0) & '0';                                         --shift data transmit buffer
          END IF;
            
          IF(assert_data = '1' AND rw_buffer = '0' AND clk_toggles > cmd_width) THEN  --read command and data part of transaction
              sdio <= 'Z';                                                                            --set serial data line to high impedance
          END IF;
        
           --receive svid clock toggle
          IF(assert_data = '0' AND clk_toggles < last_bit_rx + 1) THEN 
             IF(rw_buffer = '0' AND clk_toggles > cmd_width+d_width) THEN --read transaction and data part of transaction
                d_buffer <= d_buffer(d_width-2 DOWNTO 0) & sdio;                 --shift in received bit
             END IF;
           END IF;
            
            --end of transaction
           IF(clk_toggles = (cmd_width+d_width+rx_d_width-1) THEN  
              busy <= '0';              --clock out not busy signal
              sdio <= 'Z';              --set master data io to high impedance
              IF(rw_buffer = '0') THEN  --if transaction was a read
                rx_data <= d_buffer;    --clock out received data to output port
              END IF;
                state <= Rest;          --return to ready state
              ELSE                      --not end of transaction
                state <= Execute;       --remain in execute state
            END IF;
          
      END CASE;
    END IF;
  END PROCESS; 
END logic;
