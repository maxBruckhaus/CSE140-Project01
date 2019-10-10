#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "computer.h"
#undef mips			/* gcc already has a def for mips */

unsigned int endianSwap(unsigned int);

void PrintInfo (int changedReg, int changedMem);
unsigned int Fetch (int);
void Decode (unsigned int, DecodedInstr*, RegVals*);
int Execute (DecodedInstr*, RegVals*);
int Mem(DecodedInstr*, int, int *);
void RegWrite(DecodedInstr*, int, int *);
void UpdatePC(DecodedInstr*, int);
void PrintInstruction (DecodedInstr*);

/*Globally accessible Computer variable*/
Computer mips;
RegVals rVals;

/*
 *  Return an initialized computer with the stack pointer set to the
 *  address of the end of data memory, the remaining registers initialized
 *  to zero, and the instructions read from the given file.
 *  The other arguments govern how the program interacts with the user.
 */
void InitComputer (FILE* filein, int printingRegisters, int printingMemory,
  int debugging, int interactive) {
    int k;
    unsigned int instr;

    /* Initialize registers and memory */

    for (k=0; k<32; k++) {
        mips.registers[k] = 0;
    }
    
    /* stack pointer - Initialize to highest address of data segment */
    mips.registers[29] = 0x00400000 + (MAXNUMINSTRS+MAXNUMDATA)*4;

    for (k=0; k<MAXNUMINSTRS+MAXNUMDATA; k++) {
        mips.memory[k] = 0;
    }

    k = 0;
    while (fread(&instr, 4, 1, filein)) {
	/*swap to big endian, convert to host byte order. Ignore this.*/
        mips.memory[k] = ntohl(endianSwap(instr));
        k++;
        if (k>MAXNUMINSTRS) {
            fprintf (stderr, "Program too big.\n");
            exit (1);
        }
    }

    mips.printingRegisters = printingRegisters;
    mips.printingMemory = printingMemory;
    mips.interactive = interactive;
    mips.debugging = debugging;
}

unsigned int endianSwap(unsigned int i) {
    return (i>>24)|(i>>8&0x0000ff00)|(i<<8&0x00ff0000)|(i<<24);
}

/*
 *  Run the simulation.
 */
void Simulate () {
    char s[40];  /* used for handling interactive input */
    unsigned int instr;
    int changedReg=-1, changedMem=-1, val;
    DecodedInstr d;
    
    /* Initialize the PC to the start of the code section */
    mips.pc = 0x00400000;
    while (1) {
        if (mips.interactive) {
            printf ("> ");
            fgets (s,sizeof(s),stdin);
            if (s[0] == 'q') {
                return;
            }
        }

        /* Fetch instr at mips.pc, returning it in instr */
        instr = Fetch (mips.pc);

        printf ("Executing instruction at %8.8x: %8.8x\n", mips.pc, instr);

        /* 
	 * Decode instr, putting decoded instr in d
	 * Note that we reuse the d struct for each instruction.
	 */
        Decode (instr, &d, &rVals);

        /*Print decoded instruction*/
        PrintInstruction(&d);

        /* 
	 * Perform computation needed to execute d, returning computed value 
	 * in val 
	 */
        val = Execute(&d, &rVals);

	UpdatePC(&d,val);

        /* 
	 * Perform memory load or store. Place the
	 * address of any updated memory in *changedMem, 
	 * otherwise put -1 in *changedMem. 
	 * Return any memory value that is read, otherwise return -1.
         */
        val = Mem(&d, val, &changedMem);

        /* 
	 * Write back to register. If the instruction modified a register--
	 * (including jal, which modifies $ra) --
         * put the index of the modified register in *changedReg,
         * otherwise put -1 in *changedReg.
         */
        RegWrite(&d, val, &changedReg);

        PrintInfo (changedReg, changedMem);
    }
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the
 *  simulated instruction, otherwise -1.
 *  Previously initialized flags indicate whether to print all the
 *  registers or just the one that changed, and whether to print
 *  all the nonzero memory or just the memory location that changed.
 */
void PrintInfo ( int changedReg, int changedMem) {
    int k, addr;
    printf ("New pc = %8.8x\n", mips.pc);
    if (!mips.printingRegisters && changedReg == -1) {
        printf ("No register was updated.\n");
    } else if (!mips.printingRegisters) {
        printf ("Updated r%2.2d to %8.8x\n",
        changedReg, mips.registers[changedReg]);
    } else {
        for (k=0; k<32; k++) {
            printf ("r%2.2d: %8.8x  ", k, mips.registers[k]);
            if ((k+1)%4 == 0) {
                printf ("\n");
            }
        }
    }
    if (!mips.printingMemory && changedMem == -1) {
        printf ("No memory location was updated.\n");
    } else if (!mips.printingMemory) {
        printf ("Updated memory at address %8.8x to %8.8x\n",
        changedMem, Fetch (changedMem));
    } else {
        printf ("Nonzero memory\n");
        printf ("ADDR	  CONTENTS\n");
        for (addr = 0x00400000+4*MAXNUMINSTRS;
             addr < 0x00400000+4*(MAXNUMINSTRS+MAXNUMDATA);
             addr = addr+4) {
            if (Fetch (addr) != 0) {
                printf ("%8.8x  %8.8x\n", addr, Fetch (addr));
            }
        }
    }
}

/*
 *  Return the contents of memory at the given address. Simulates
 *  instruction fetch. 
 */
unsigned int Fetch ( int addr) {
    return mips.memory[(addr-0x00400000)/4];
}

/* Decode instr, returning decoded instruction. */
void Decode ( unsigned int instr, DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */
    unsigned int rs, rt, rd, temp;
    //assert(d->op <= 43);
    
    d->op = instr >> 26; //shifting 26 bits to the right results in first 6 bits
    
    temp = instr << 6; //$rs
    temp = temp >> 27;
    rs = temp;
    
    temp = instr << 11; //$rt
    temp = temp >> 27;
    rt = temp;
    
    temp = instr << 16; //$rd
    temp = temp >> 27;
    rd = temp;
    
    if(d->op == 0){
        d->type = R;
        d->regs.r.rs = rs;
        d->regs.r.rt = rt;
        d->regs.r.rd = rd;
        d->regs.r.shamt = (instr << 21) >> 27;
        d->regs.r.funct = (instr << 26) >> 26;
    }
    else if(d->op == 2 || d->op == 3){
        d->type = J;
        d->regs.j.target = (instr << 6) >> 4 | (mips.pc >> 28) << 28;
    }
    else{
        d->type = I;
        d->regs.i.rs = rs;
        d->regs.i.rt = rt;
        
        
        unsigned int lmb = (instr << 16) >> 31; //9am-11am AOA 142
        unsigned int imm;
        
        if(lmb == 0){
            imm = (instr << 16)>>16 & 0x0000FFFF;	// in the mips data path, imm extends 16 bits.
            
            if((d->op==35) | (d->op==43) | (d->op==9)){	//lw|sw|addiu
                if(((instr << 16)>>16) >> 15){
                    imm = (instr << 16)>>16 | 0xFFFF0000;    //extend
            d->regs.i.addr_or_immed = imm;
                }
            }
        }
        
//        else if(lmb == 1){
//            imm = (instr << 16)>>16 | 0xFFFF0000;    //extend
//            d->regs.i.addr_or_immed = imm;
//        }
    
    }
}

/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction ( DecodedInstr* d) {
    /* Your code goes here */
    char* instr = (char*) malloc(25);
    // R-type instructions
    if (d->op == 0){
        if (d->regs.r.funct == 0){
            instr = "sll";
        }else if (d->regs.r.funct == 2){
            instr = "srl";
        }else if (d->regs.r.funct == 8){
            instr = "jr";
        }else if (d->regs.r.funct == 33){
            instr = "addu";
        }else if (d->regs.r.funct == 35){
            instr = "subu";
        }else if (d->regs.r.funct == 36){
            instr = "and";
        }else if (d->regs.r.funct == 37){
            instr = "or";
        }
        // J-type instructions
    }else if (d->op == 2){
        instr = "j";
    }else if (d->op == 3){
        instr = "jal";
        // I-type instructions
    }else if (d->op == 4){
        instr = "beq";
    }else if (d->op == 5){
        instr = "bne";
    }else if (d->op == 9){
        instr = "addiu";
    }else if (d->op == 10){
        instr = "slti";
    }else if (d->op == 12){
        instr = "andi";
    }else if (d->op == 13){
        instr = "ori";
    }else if (d->op == 15){
        instr = "lui";
    }else if (d->op == 35){
        instr = "lw";
    }else if (d->op == 43){
        instr = "sw";
    }

    printf("%s\t", instr);

    // Print R-type instruction
    if (d->op == 0 && d->regs.r.funct != 8){
        printf("$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
        // Print R-type jump
    }else if (d->regs.r.funct == 8){
        printf("$%d\n", d->regs.i.rs);
        // Print J-type
    }else if (d->op == 2 || d->op == 3){
        printf("$%d", d->regs.j.target);
        // Print I-type
    }else{
        // beq, bne
        if (d->op == 4 || d->op == 5){
            printf("$%d, $%d, 0x%8.8x\n", d->regs.i.rs, d->regs.i.rt, mips.pc+16);
        // andi, ori, lui
        }else if (d->op == 12 || d->op == 13 || d->op == 15){
            printf("$%d, $%d, 0x%x\n", d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed);
        // lw, sw
        }else if (d->op == 35 || d->op == 43){
            printf("$%d, %d($%d)\n", d->regs.i.rt, d->regs.i.addr_or_immed, d->regs.i.rs);
        }else{
            printf("$%d, $%d, $%d\n", d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed);
        }
    }
}

/* Perform computation needed to execute d, returning computed value */
int Execute ( DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */
    
    //R type
    if(d->op == 0){
        if(d->regs.r.funct == 0){ //sll
            return (mips.registers[d->regs.r.rt] << d->regs.r.shamt); //rd = rt << shamt
        }
        else if(d->regs.r.funct == 2){ //srl
            return (mips.registers[d->regs.r.rt] >> d->regs.r.shamt); //rd = rt >> shamt
        }
    }
    if(d->op == 2){
        
    }
    if(d->op == 42){
        
    }
    if(d->op == 37){
        
    }
    if(d->op == 36){
        
    }
    if(d->op == 35){
        
    }
    if(d->op == 33){
        
    }
    if(d->op == 8){
        
    }
    
    
    //J type
    
    
    //I type
    
    
    
  return 0;
}

/* 
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC ( DecodedInstr* d, int val) {
    mips.pc+=4;
    /* Your code goes here */
    //we need branches and jump
    
    if(d->op == 2){ //if j
        mips.pc = d->regs.j.target;
    }
    
    if(d->op == 3){ // if jal
        mips.pc = d->regs.j.target;
    }
    else if(d->regs.r.funct == 8){ //if jr which is now r type
        mips.pc = val;
    }
    //need one for branch
    
}

/*
 * Perform memory load or store. Place the address of any updated memory 
 * in *changedMem, otherwise put -1 in *changedMem. Return any memory value 
 * that is read, otherwise return -1. 
 *
 * Remember that we're mapping MIPS addresses to indices in the mips.memory 
 * array. mips.memory[0] corresponds with address 0x00400000, mips.memory[1] 
 * with address 0x00400004, and so forth.
 *
 */
int Mem( DecodedInstr* d, int val, int *changedMem) {
    /* Your code goes here */
  return 0;
}

/* 
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite( DecodedInstr* d, int val, int *changedReg) {
    /* Your code goes here */
}
