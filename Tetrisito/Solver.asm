# 20/05/2023 | S2 AY 2022-2023
# John Achapero | CS 21 Lab 1
# cs21project1.asm | MIPS code for Tetrisito, implementation C with bonus 2

#note about inputting strings. if you subtract the number of letters first, 0($sp) is the first letter inputted. (number of letters)($sp) is the last letter inputted.
#this means that consecutive inputted string forces the last inputted string to be the first output (since you start from 0).
#get around this by subtracting as a chunk first, then placing the first inputted string in the lowest available memory address.
.macro input_grid_row(%address) 	#syscall code 8, receives a string.
	add $a0, %address, $0	#%address is a register containing the memory for the row
	li $a1 7		#receive specifically 6 characters, or one row.
	li $v0 8
	syscall
.end_macro
.macro input_piece_row(%address) 	#syscall code 8, receives a string.
	add $a0, %address, $0	#%address is a register containing the memory for the row
	li $a1 5		#receive specifically 4 characters, or one piece row.
	li $v0 8
	syscall
.end_macro
.macro input_int(%register)	#syscall code 5, receives an integer to place in %register
	li $v0 5
	syscall
	add %register, $v0, $0
.end_macro
.macro exit() 			#syscall code 10
	li $v0 10
	syscall
.end_macro


.text
main:	
### PROCESSING START_GRID ###	#need to have a per-row for loop:
		la $t4, start_grid #$t4 stores the start_grid
		addi $t4, $t4, 24 #skip to the 5th row
		addi $t0, $t4, 36 #this is the end counter for the for loop, that is, we've reached the final byte in start_grid
		addi $t6, $0, 0x23 #$t6 stores the ASCII code for '#'.
		addi $t1, $0, 0x2E #$t1 stores the ASCII code for '.'.
start_row_loop: #need a place in memory to store 6 bytes for the first row. why not use $sp?
		add $t2, $0, $sp #original sp, as a for loop counter
		addi $sp, $sp, -6 #allocate 6 bytes in memory for the input row 
		input_grid_row($sp) #receive input
		#iterate through all the characters, freeze if needed
		addi $t3, $0, 6 #for loop 2 end
start_bytes:	lb $t5, 0($sp) #t5 stores the current row character
		sb $t1, 0($t4) #store a period
		bne $t5, $t6, no_freeze_start #branch if current character is NOT #
		addi $t5, $0, 0x58 #replace the current character with X
		sb $t5, 0($t4) #store X in place of the current character
no_freeze_start:addi $sp, $sp, 1 #next byte, also appends counter
		addi $t4, $t4, 1 #next byte for start_grid
		bne $sp, $t2, start_bytes #continue start_bytes loop
		bne $t0, $t4, start_row_loop #continue start_row_loop

### PROCESSING FINAL_GRID ###
		#this is the same as above but for the final grid
		#need to have a per-row for loop:
		la $t4, final_grid #$t4 stores the final_grid
		addi $t4, $t4, 24 #skip to the 5th row
		addi $t0, $t4, 36 #this is the end counter for the for loop, that is, we've reached the final byte in final_grid
		addi $t6, $0, 0x23 #$t6 stores the ASCII code for '#'.
		addi $t1, $0, 0x2E #$t1 stores the ASCII code for '.'.
final_row_loop: #need a place in memory to store 6 bytes for the first row. why not use $sp?
		add $t2, $0, $sp #original sp, as a for loop counter
		addi $sp, $sp, -6 #allocate 6 bytes in memory for the input row 
		input_grid_row($sp) #receive input
		#iterate through all the characters, freeze if needed
		addi $t3, $0, 6 #for loop 2 end
final_bytes:	lb $t5, 0($sp) #t5 stores the current row character
		sb $t1, 0($t4) #store a period
		bne $t5, $t6, no_freeze_final #branch if current character is NOT #
		addi $t5, $0, 0x58 #replace the current character with X
		sb $t5, 0($t4) #store X in place of the current character
no_freeze_final:addi $sp, $sp, 1 #next byte, also appends counter
		addi $t4, $t4, 1 #next byte for final_grid
		bne $sp, $t2, final_bytes #continue final_bytes loop
		bne $t0, $t4, final_row_loop #continue final_row_loop
		
### PROCESSING INPUT PIECES ###
		input_int($t0) #this is the number of input pieces
		la $t1, chosen #load the chosen array from static memory
		sb $t0, 5($t1) #too many bytes assigned to chosen anyway, so just set the 5th byte to number of input pieces
		addi $t1, $0, 0 #counter for a for loop
		addi $t8, $0, 0x23 #stores the ASCII code for #
		la $t6, converted_pieces #load the converted_pieces array from static memory
pieces_loop:	addi $t2, $sp, -16 #allocate 16 bytes for an input piece
		input_piece_row($t2) #receive the first 4 bytes
		addi $t2, $t2, 4 #place in the lowest numbered address
		input_piece_row($t2) #next 4 bytes
		addi $t2, $t2, 4 #next lowest numbered address
		input_piece_row($t2) #next 4 bytes
		addi $t2, $t2, 4 #next lowest numbered address
		input_piece_row($t2) #next 4 bytes
		addi $t2, $t2, -12 #return $sp to the start address of the input piece array
		#need to scroll through until we find all the #s:
		addi $t5, $0, -1 #count the number of loops until the #
		addi $t7, $0, 16 #end loop when at the last byte of the input piece
pieces_charloop:addi $t5, $t5, 1 #why start at offset -1? add here to start at 0, also loads next byte
		lb $t4, 0($t2) #t4 stores the current character
		addi $t2, $t2, 1 #next character
		bne $t4, $t8, notablock #if current char is not a #, move to the next
		addi $t3, $0, 4 #save 4
		div $t5, $t3 #divide the index by 4
		mflo $t3 #i = floor(index/4)
		sb $t3, 0($t6) #store the row i
		mfhi $t3 #j = index mod 4
		sb $t3, 1($t6) #store the column j
		addi $t6, $t6, 2 #move 2 bytes forward in converted_pieces
notablock:	bne $t5, $t7, pieces_charloop #continue cycling through bytes of piece
		addi $t1, $t1, 1 #move to next input piece
		bne $t0, $t1, pieces_loop #continue cycling through input pieces
		
### PREPROCESSING DONE. TIME TO BACKTRACK ###
		la $a0, start_grid #first argument: start address of start_grid
		la $a1, chosen #second argument: start address of chosen
		la $a2, converted_pieces #third argument: start address of converted_pieces
		jal backtrack #call backtrack
		
		beq $v0, $0, notpossible #v0 is 0 if false, 1 if true. 1 prints yes.
		addi $sp, $sp, -4 #allocate 4 bytes in memory for the string (3 chars + end char)
		li $t0, 0x00534559 #store an ASCII 'YES'
		j end_program #print the 
notpossible:	li $t0, 0x00004F4E #store an ASCII 'NO'
end_program:	sw $t0, 0($sp) #store the string into memory
		add $a0, $0, $sp #put the start address of string into a0
		addi $v0, $0, 4 #syscall 4
		syscall #print the string in a0
		addi $sp, $sp, 4 #return stack pointer to original place
		exit() #syscall 10
		
		
### FUNCTION START: BACKTRACK ###
backtrack:	addi $sp, $sp, -144 #duplicating input arguments requires 60 + 8 + 40 = 108 bytes. using all s registers and ra requires (8+1)*4=36 bytes. total 144 bytes,
		sw $ra, 140($sp)
		sw $s0, 136($sp)
		sw $s1, 132($sp)
		sw $s2, 128($sp)
		sw $s3, 124($sp)
		sw $s4, 120($sp)
		sw $s5, 116($sp)
		sw $s6, 112($sp)
		sw $s7, 108($sp)
		
		#firstly, need to duplicate input arguments. 
		#start by duplicating curr_grid:
		add $s0, $a0, $0 #a0 stores the address of currgrid
		addi $s1, $s0, 60 #need 60 bytes
currgriddupeloop:
		lw $s2, 0($s0) #get the argument row
		sw $s2, 0($sp) #duplicate into stack memory
		addi $s0, $s0, 4 #next row
		addi $sp, $sp, 4 #next row in memory
		bne $s0, $s1, currgriddupeloop #loop through rows
		addi $s0, $s0, -60 #$s0 stores the address of currgrid
		#then duplicate chosen
		add $s1, $a1, $0 #s1 stores the address of chosen
		lw $s2, 0($s1) #get first 4 bytes of chosen
		sw $s2, 0($sp) #place first 4 bytes into stack memory
		lw $s2, 4($s1) #get next 4 bytes
		sw $s2, 4($sp) #place in memory
		addi $sp, $sp, 8 #adjust $sp for next for loop
		#then duplicate pieces
		add $s2, $a2, $0 #s2 stores the address of pieces
		addi $s3, $s2, 40 #pieces array has 40 bytes
piecesdupeloop:	lw $s4, 0($s2) #load row of pieces
		sw $s4, 0($sp) #store row of pieces into memory
		addi $s2, $s2, 4 #next row
		addi $sp, $sp, 4 #next row in memory
		bne $s2, $s3, piecesdupeloop #loop through rows
		addi $s2, $s2, -40 #s2 now stores address of pieces
		addi $sp, $sp, -108 #return $sp to the bottom of the stack frame
		
		addi $s1, $0, 0 #result = false
		
		#check if current grid is already right
		add $a0, $s0, $0 #argument: currGrid address
		jal is_final_grid #call function
		bne $v0, $0, backtrack_true #if is_final_grid returns true, then end backtrack and return true
		
		#if not, go crazy
		#firstly, some inventory management.
		add $s0, $sp, $0 #store $sp into s0.
		#this allows us to access currGrid, chosen, and pieces with 1 variable. 0(s0) is currgrid, 60(s0) is chosen, 68(s0) is pieces.
		
		la $at, 60($s0) #60(s0) is start address of chosen
		add $s3, $at, $0 #s3 is the counter for the bigloop, starts from address of first byte of chosen to address of last byte of chosen
		la $s4, chosen #load the start address of chosen
		lb $s4, 5($s4) #number of pieces
		add $s4, $s3, $s4 #break bigloop when the finished iterating through [chosen]
backtrack_bigloop:
		lb $s5, 0($s3)	#s5 is chosen[i]
		bne $s5, $0, continue_bigloop #if true, skip
		
		#now, need to make a copy of chosen. can allocate more memory here:
		addi $sp, $sp, -8 #8 bytes because chosen is 4 words long
		#chosencopy can be accessed through -8($s0)
		lw $at, 60($s0) #60(s0) is start address of chosen
		sw $at, 0($sp) #copy first word of chosen to memory
		lw $at, 64($s0) #get next word of chosen
		sw $at, 4($sp) #store next word, chosen now copied
		
		#next, solve for max_x_of_piece
		la $at, 60($s0) #60(s0) is start address of chosen
		sub $a0, $s3, $at #index of current chosen
		sll $a0, $a0, 3 #multiply by 8 because of how pieces is stored (indices are adjusted by 8)
		la $at, 68($s0) #68(s0) is start address of pieces
		add $a0, $a0, $at #argument: offset + start address of pieces
		jal get_max_x_of_piece #call function
		
		#start small loop
		add $s5, $0, $0 #s5 is now counter for small loop, also offset
		add $s6, $0, 6 #s6 = 6
		sub $s6, $s6, $v0 #end loop when s5 goes from 0 to 6-max_x_of_piece
backtrack_smallloop:
		add $a0, $s0, $0 #first argument: address of currgrid
		la $at, 60($s0) #60(s0) is start address of chosen
		sub $a1, $s3, $at #index of current chosen
		sll $a1, $a1, 3 #multiply by 8 because of how pieces is stored
		la $at, 68($s0) #68(s0) is start address of pieces
		add $a1, $a1, $at #second argument: start address of relevant piece
		add $a2, $s5, $0 #third argument: offset
		
		#note that we need to store nextgrid somewhere.
		addi $sp, $sp, -60 #allocate space for nextgrid. can be accessed through -68($s0)
		addi $a3, $sp, 0 #fourth argument: where to place nextgrid
		jal drop_piece_in_grid #call function
		
		beq $v1, $0, continue_smallloop #if success is false, continue
		#at this point, no longer need success.
		la $at, 60($s0) #load address of chosen
		sub $s7, $s3, $at #s7 = i
		la $at, -8($s0) #-8($s0) is the start address of chosencopy
		add $s7, $s7, $at #s7 is the address of chosencopy[i] 
		addi $at, $0, 1 #store a true
		sb $at, 0($s7) #chosencopy[i] = true
		
		#call clearlines
		la $a0, -68($s0) #-68($s0) is the start address of nextgrid
		jal clearlines #call function
		
		beq $at, $s1, dealloc #if result: return true. this is placed before to function as an OR
		
		la $a0, -68($s0) #first argument: start address of nextgrid
		la $a1, -8($s0) #second argument: start address of chosencopy
		la $a2, 68($s0) #third argument: start address of pieces
		jal backtrack #call function
		add $s1, $v0, $0 #result = backtrack
		
		addi $at, $0, 1 #store a true to compare to
		beq $at, $s1, dealloc #if result: return true
		
continue_smallloop:
		add $s5, $s5, 1 #append to loop counter
		addi $sp, $sp, 60 #deallocate nextGrid at the end of smallloop
		bne $s5, $s6, backtrack_smallloop #next xoffset
		
		add $sp, $sp, 8 #deallocate chosenCopy at the end of bigloop
continue_bigloop: #this continue is found prior to the allocation of chosenCopy, so deallocation should not happen
		addi $s3, $s3, 1 #go to next byte in chosen
		bne $s3, $s4, backtrack_bigloop #continue cycling through chosen
		addi $v0, $s1, 0 #result = false, a jump needs to occur for result to be true
		j end_backtrack #ends backtrack while returning false
dealloc:	addi $sp, $sp, 68 #deallocate memory, return true, end backtrack
backtrack_true:	addi $v0, $0, 1 #return true, end backtrack
end_backtrack:	lw $ra, 140($sp)
		lw $s0, 136($sp)
		lw $s1, 132($sp)
		lw $s2, 128($sp)
		lw $s3, 124($sp)
		lw $s4, 120($sp)
		lw $s5, 116($sp)
		lw $s6, 112($sp)
		lw $s7, 108($sp)
		addi $sp, $sp, 144
		jr $ra
### FUNCTION END: BACKTRACK ###		
		
### FUNCTION START: IS_FINAL_GRID ###
is_final_grid:	addi $sp, $sp, -32
		sw $ra, 0($sp)
		sw $s0, 4($sp)
		sw $s1, 8($sp)
		sw $s2, 12($sp)
		sw $s3, 16($sp)
		sw $s4, 20($sp)
		add $s0, $a0, $0 #save argument: start address of grid1
		la $s1, final_grid #final_grid is grid 2
		add $s2, $s0, 60 #60 bytes per grid
is_equal_loop:	lw $s3, 0($s0) #get a word in grid1
		lw $s4, 0($s1) #and a word in grid2
		bne $s3, $s4, is_not_equal #need to be equal. if not, end function, return false
		addi $s0, $s0, 4 #next word in grid1
		addi $s1, $s1, 4 #next word in grid2
		bne $s0, $s2, is_equal_loop #end loop when at the end of the grids
		addi $v0, $0, 1 #no jumps? return true
		j is_equal_end #end function
is_not_equal:	addi $v0, $0, 0 #jumped? return false
is_equal_end:	lw $ra, 0($sp)
		lw $s0, 4($sp)
		lw $s1, 8($sp)
		lw $s2, 12($sp)
		lw $s3, 16($sp)
		lw $s4, 20($sp)
		addi $sp, $sp, 32
		jr $ra
### FUNCTION END: IS_FINAL_GRID ###
		
### FUNCTION START: GET_MAX_X_OF_PIECE ###
get_max_x_of_piece:
		addi $sp, $sp, -32
		sw $ra, 0($sp)
		sw $s0, 4($sp)
		sw $s1, 8($sp)
		sw $s2, 12($sp)
		add $s0, $a0, $0 #save argument: start address of piece
		addi $v0, $0, -1 #max_x = -1
		addi $s1, $s0, 8 #always 4 blocks in a piece, always 2 coords per block, total 8 coords, break loop at end of coords
get_max_loop:	lb $s2, 1($s0) #load the second coordinate of the piece
		ble $s2, $v0, get_max_next #if second coordinate <= max_x, do nothing
		add $v0, $s2, $0 #if s2 > v0, change max_x to second coordinate
get_max_next:	addi $s0, $s0, 2 #move to next block in piece
		bne $s0, $s1, get_max_loop #itearte until at last block of piece
		lw $ra, 0($sp)
		lw $s0, 4($sp)
		lw $s1, 8($sp)
		lw $s2, 12($sp)
		addi $sp, $sp, 32
		jr $ra
### FUNCTION END: GET_MAX_X_OF_PIECE ###

### FUNCTION START: DROP_PIECE_IN_GRID ###
#first argument: address of currgrid
#second argument: start address of relevant piece
#third argument: offset
drop_piece_in_grid:
		addi $sp, $sp, -104 #need to create a copy of the grid and piece, so minimum 60 + 8 = 68 bytes in memory
		#also use 8 + 1 registers, so store another 36 bytes
		#totalling 104
		sw $ra, 100($sp)
		sw $s0, 96($sp)
		sw $s1, 92($sp)
		sw $s2, 88($sp)
		sw $s3, 84($sp)
		sw $s4, 80($sp)
		sw $s5, 76($sp)
		sw $s6, 72($sp)
		sw $s7, 68($sp)
		
		#first, duplicate grid
		add $s0, $a0, $0 #s0 stores the address of grid
		add $s3, $a3, $0 #s3 stores the address of nextgrid. by default, set nextgrid to be grid
		addi $s1, $s0, 60 #60 bytes in a grid
griddupeloop:	lw $s2, 0($s0) #load a word from grid
		sw $s2, 0($s3) #store that word into nextgrid (nextgrid starts as grid)
		sw $s2, 0($sp) #also store that word into memory (deepcopy of grid)
		addi $s0, $s0, 4 #next word in grid
		addi $s3, $s3, 4 #next word in nextgrid
		addi $sp, $sp, 4 #next word in deepcopy(grid)
		bne $s0, $s1, griddupeloop #iterate through bytes in grid
		addi $s0, $sp, -60 #s0 now stores the address of gridcopy
		
		#next, duplicate piece. piece is 2 words long
		add $s1, $a1, $0 #s1 stores the address of piece
		lw $s2, 0($s1) #load first word of piece
		sw $s2, 0($sp) #store in memory
		lw $s2, 4($s1) #second word loaded
		sw $s2, 4($sp) #store in memory
		addi $sp, $sp, -60 #return $sp to start of gridcopy
		
		addi $s2, $0, 100 #maxY = 100
		add $s3, $s1, $0 #counter for blocks in piece
		add $s4, $s3, 8 #end loop when iterated through all blocks in piece
drop_block_loop:lb $s5, 0($s3) #s5 = block[0]
		addi $s6, $0, 6 #store 6
		mult $s5, $s6 #block[0] * 6
		mflo $s5 #s5 = 6 * block[0]
		lb $s6, 1($s3) #s6 = block[1]
		add $s5, $s5, $s6 #s5 = 6*block[0] + block[1]
		add $s5, $s5, $a2 #s5 = 6*block[0] + block[1] + yoffset
		add $s5, $s0, $s5 #s5 is now the address of the associated piece to put in
		addi $s6, $0, 0x23 #ASCII code for #
		sb $s6, 0($s5) #current piece is now a #
		add $s3, $s3, 2 #append loop counter; move to next block
		bne $s3, $s4, drop_block_loop #not yet last block in piece? continue loop
drop_while_loop:addi $s3, $0, 1 #canStillGoDown = True
		add $s4, $s0, $0 #save the start address of gridcopy, this is a loop counter
		add $s5, $s4, 60 #iterate through all 60 bytes of gridcopy
goDown_check_loop:
		lb $s7, 0($s4) #s7 is gridcopy[i][j] 
		addi $s6, $0, 0x23 #ASCII code for #
		bne $s7, $s6, continue_check_loop #if gridcopy[i][j] != #: continue the loop
		sub $s6, $s4, $s0 #current index
		addi $s7, $0, 6 #store 6
		div $s6, $s7 #index/6
		mflo $s6 #s6 = index/6, which is i
		mfhi $at #at = index%6, which is j
		addi $s6, $s6, 1 #s6 = i + 1
		addi $s7, $0, 10 #store 10
		beq $s6, $s7, goDown_false #at this point, gridcopy[i][j] == '#' and i+1 == 10, set to false
		addi $s7, $0, 6 #s7 = 6
		mult $s6, $s7 #[i+1] * 6
		mflo $s6 #s6 = [i+1] * 6
		add $s6, $s6, $at #s6 = (i+1)*6 + j, which is the index
		add $s6, $s6, $s0 #address of gridCopy[i+1][j]
		lb $s6, 0($s6) #s6 = gridcopy[i+1][j]
		addi $s7, $0, 0x58 #stores the ascii code for X
		beq $s6, $s7, goDown_false #at this point, gridcopy[i][j] == '#' and gridcopy[i+1][j] == 'X', set to false
		j continue_check_loop #coninue loop
goDown_false:	addi $s3, $0, 0 #set canStillGoDown to false
continue_check_loop:
		add $s4, $s4, 1 #next byte in gridcopy
		bne $s4, $s5, goDown_check_loop #continue loop if not done with iterating through gridcopy
		
		beq $s3, $0, drop_while_end #if canStillGoDown is False, break
		addi $s3, $s3, 53 #s3 = 8*6 + 5 = 53, which is the index for [8,5]
		add $s3, $s3, $s0 #s3 is now the address for gridcopy[8,5]
		addi $s4, $s0, -1 #stop after reaching index -1
move_down_loop: lb $s5, 0($s3) #gridcopy[i][j]
		addi $s6, $0, 0x23 #stores the ASCII code for #
		bne $s5, $s6, continue_move_down
		addi $s6, $0, 0x2E #stores the ASCII code for .
		sb $s6, 0($s3) #gridcopy[i][j] = '.'
		sub $s5, $s3, $s0 #s5 = index
		addi $s6, $0, 6 #store a 6
		div $s5, $s6 #index/6
		mflo $s5 #s5 = index/6, which is i
		mfhi $s7 #s7 = index%6, which is j
		addi $s5, $s5, 1 #s5 = i + 1
		mult $s5, $s6 #(i+1)*6
		mflo $s5 #s5 = (i+1)*6
		add $s5, $s5, $s7 #s5 = (i+1)*6 + j, which is the index
		add $s5, $s0, $s5 #s5 is now the address of gridcopy[i+1][j]
		addi $s6, $0, 0x23 #stores the ASCII code for '#'
		sb $s6, 0($s5) #gridcopy[i+1][j] = '#'
continue_move_down:
		addi $s3, $s3, -1 #go to the left and up gridwise
		bne $s3, $s4, move_down_loop #not done moving down rows? continue
		j drop_while_loop #continue while(true), a branch will break it if needed
drop_while_end:
		add $s3, $s0, $0 #store the start address of gridcopy
		addi $s4, $s0, 60 #iterate through all of gridcopy
		addi $s6, $0, 0x23 #stores the ASCII code for #
maxY_loop:	lb $s5, 0($s3) #s5 = gridcopy[i][j]
		bne $s5, $s6, continue_maxY #continue if gridcopy[i][j] != '#'
		sub $s5, $s3, $s0 #s5 = index
		addi $s6, $0, 6 #store 6
		div $s5, $s6 #index/6
		mflo $s5 #s5 = index/6, which is i
		bge $s5, $s2, continue_maxY #do nothing if i >= maxY
		add $s2, $s5, $0 #if i < maxY, maxY = i
continue_maxY:	addi $s3, $s3, 1 #next byte
		bne $s3, $s4, maxY_loop #continue until done with gridcopy
		
		addi $s3, $0, 3 #store a 3
		ble $s2, $s3, drop_piece_fail #if max<=3, branch to end
		addi $a0, $s0, 0 #first argument: start address of gridcopy
		jal freeze_blocks #call function
		addi $v1, $0, 1 #v1 to true
		
		#need to copy $v0 to nextgrid.
		add $s0, $a3, $0 #start address of nextgrid, which is an argument
		add $s1, $v0, $0 #v0 is the address of freeze_blocks(gridCopy)
		addi $s2, $a3, 60 #scroll through all of nextgrid
copy_nextgrid:	lw $at, 0($s1) #get a word in freeze_blocks(gridCopy)
		sw $at, 0($s0) #place as appropriate nextgrid word
		addi $s0, $s0, 4 #next word in freeze_blocks(gridcopy)
		addi $s1, $s1, 4 #next word in nextgrid
		bne $s0, $s2, copy_nextgrid #not done copying? continue
		j end_drop_piece #end function
drop_piece_fail:addi $v1, $0, 0 #return false, end function
end_drop_piece:	lw $ra, 100($sp)
		lw $s0, 96($sp)
		lw $s1, 92($sp)
		lw $s2, 88($sp)
		lw $s3, 84($sp)
		lw $s4, 80($sp)
		lw $s5, 76($sp)
		lw $s6, 72($sp)
		lw $s7, 68($sp)
		addi $sp, $sp, 104
		jr $ra
### FUNCTION END: DROP_PIECE_IN_GRID ###

### FUNCTION START: FREEZE_BLOCKS ###
freeze_blocks:	addi $sp, $sp, -32
		sw $ra, 0($sp)
		sw $s0, 4($sp)
		sw $s1, 8($sp)
		sw $s2, 12($sp)
		sw $s3, 16($sp)
		addi $s0, $a0, 24 #only need to freeze 36 bottom blocks
		addi $s1, $s0, 60 #until the last block
freeze_loop:	addi $s2, $0, 0x23 #stores the ASCII code for '#'
		lb $s3, 0($s0) #load a byte from the grid
		bne $s2, $s3, dont_freeze #if byte != '#', don't freeze it
		addi $s2, $0, 0x58 #stores the ASCII code for 'X'
		sb $s2, 0($s0) #store an 'X' in place of '#'
dont_freeze:	addi $s0, $s0, 1 #next byte
		bne $s0, $s1, freeze_loop #not done freezing? continue the loop
		add $v0, $a0, $0 #return the start address of the grid
		lw $ra, 0($sp)
		lw $s0, 4($sp)
		lw $s1, 8($sp)
		lw $s2, 12($sp)
		lw $s3, 16($sp)
		addi $sp, $sp, 32
		jr $ra
### FUNCTION END: FREEZE_BLOCKS ###

### FUNCTION START: CLEARLINES ###
clearlines:	addi $sp, $sp, -32
		sw $ra, 0($sp)
		sw $s0, 4($sp)
		sw $s1, 8($sp)
		sw $s2, 12($sp)
		sw $s3, 16($sp)
		sw $s4, 20($sp)
		add $s0, $a0, $0 #s0 stores the start address of the grid
		addi $s0, $s0, 59 #move to the last byte
		addi $s1, $a0, 23 #stop when reaching 24($s0), which is the last byte of the 4th row.
clearlines_loop:addi $s2, $s0, -6 #start a for loop to check the row byte by byte if all are x
		addi $s3, $0, 1 #all X? assume true
		addi $at, $0, 0x58 #ASCII code for 'X' 
clearlines_check_loop:
		lb $s4, 0($s0) #load a byte for the row
		beq $s4, $at, clearlines_stillX #keep allX true as long as the current char is still X
		add $s3, $0, $0 #all X? now false, didn't branch
clearlines_stillX: 
		addi $s0, $s0, -1 #move to the previous byte (iterating down remember)
		bne $s0, $s2, clearlines_check_loop
		#at this point, done checking the current row.
		
		beq $s3, $0, continue_clearlines_loop #if allX is false, continue the loop, or move to the next row
		#otherwise, go crazy.
		#need a new loop to iterate through every row
		add $s2, $s0, $0 #save s0 
		addi $s0, $s0, 6 #return $s0 to the start of the current row
		addi $s3, $a0, 23 #stop when reaching 24($s0), which is the last byte of the 4th row.
move_down_rows:	lb $at, -6($s0) #get the byte above the current byte
		sb $at, 0($s0) #replace the current byte with the byte above it
		addi $s0, $s0, -1 #previous byte
		bne $s0, $s3, move_down_rows
		addi $s0, $s2, 6 #restore s0 back to the start of current row. this allows checking for consecutive rows
continue_clearlines_loop:
		bne $s0, $s1, clearlines_loop #not done with all the lines in the grid? continue
		lw $ra, 0($sp)
		lw $s0, 4($sp)
		lw $s1, 8($sp)
		lw $s2, 12($sp)
		lw $s3, 16($sp)
		lw $s4, 20($sp)
		addi $sp, $sp, 32
		jr $ra
### FUNCTION END: CLEARLINES ###

.data
#data note: iterating linearly from 0(data_name), grids go from right to left, top to bottom.
#upper right most is 0 index. lower left most has index 8*(numberofwords) - 1.
start_grid: #need at least 60 bytes for the start grid. 60 bytes is 15 words minimum
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E

final_grid: 
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E
.word 0x2E2E2E2E

#note that input pieces are a maximum of 5, so we can just allocate the max memory already
#chosen will require at most 5 bytes, so 2 words:
chosen:
.word 0x00000000
.word 0x00000000

#every piece contains 4 blocks. each array contains 4*2 = 8 bytes.
#maximum of 5 pieces, so 5*8 = 40 bytes, or 10 words.
converted_pieces:
.word 0x00000000
.word 0x00000000
.word 0x00000000
.word 0x00000000
.word 0x00000000
.word 0x00000000
.word 0x00000000
.word 0x00000000
.word 0x00000000
.word 0x00000000
