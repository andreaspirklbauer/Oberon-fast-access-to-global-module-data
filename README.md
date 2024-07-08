# Oberon-fast-access-to-global-module-data
In Project Oberon 2013 (http://projectoberon.com), an access to a global variable generally requires two instructions: one memory load (LD) instruction to fetch the base address of the data section of the accessed module from a table global to the system, and one LD, ADD or STR instruction to access the actual data at a given offset from this base address.

Note: In this respository, the term "Project Oberon 2013" refers to a re-implementation of the original "Project Oberon" on an FPGA development board around 2013, as published at www.projectoberon.com.

This repository explores various possible ways to improve the way global variables are accessed.

Variant 1 is the principal variant of this repository. It implements the approach used in Extended Oberon (http://github.com/andreaspirklbauer/Oberon-extended).

--------------------------------------------------------------------------

**0. Project Oberon 2013** (implemented at www.projectoberon.com)

This is the *current* (as of February 2020) default implementation in Project Oberon 2013 (http://www.projectoberon.com).
It generates the following instruction sequences:

**Case a. For determining the absolute address of a global or external variable:**

*a.1. For mno = 0 (non-imported global variable)*

If the offset from the static base is <64KB:

     LD RH, MT, mno*4    ; RH := static base of the accessed module, using the module table pointed to by the MT register
     ADD RH, RH, offset  ; access the object inside the module pointed to by RH (=static base)

If the offset from the static base is >64KB:

     LD RH, MT, mno*4
     MOV1+U RH+1, offset DIV 10000H     ; upper 16 bits
     IOR RH+1, RH+1, offset MOD 10000H  ; lower 16 bits
     ADD RH, RH, RH+1                   ; RH := RH + RH+1

This instruction sequence makes it possible to declare global variables at an offset >64KB from the static base and access it within the *same* module.

*a.2. For mno > 0 (external global variable)*

     LD RH, MT, mno*4    ; RH := static base of the accessed module, using the module table pointed to by the MT register
     ADD RH, RH, offset  ; access the object inside the module pointed to by RH (=static base)

Here, the compiler (currently) generates the two-instruction sequence shown above, where *offset* is the export number of the accessed external object, to be fixed up by the module loader.

However, since the ADD instruction has an *offset* (immediate operand) field of 16 bits only, this implies that for *external* global variables, offsets >64KB are (currently) not supported. The exporting module can *declare* such global variables and access them in its own module, but clients cannot access them at offsets >64KB.

**Case b. For loading the value of global variable into a register:**

For mno = 0 and for mn0 > 0, and for any offset (which, however, is assumed to be <1MB)

     LD RH, MT, mno*4    ; RH := static base of the accessed module, using the module table pointed to by the MT register
     LD RH, RH, offset   ; access the object inside the module pointed to by RH (=static base)

I.e. the instruction are the same, no matter what.

This is acceptable, since the LD instruction has an immediate operand field of 20 bits, implying a maximum offset of 2^20 = 1MB (which is enough for the Project Oberon 2013 implementation - which runs on hardware with 1MB *total* memory).

--------------------------------------------------------------------------

**1. Variant 1**  (implemented in subdirectory [**Sources/FPGAOberon2013/Variant1**](Sources/FPGAOberon2013/Variant1) for Project Oberon 2013 and in Extended Oberon). This is the recommended implementation.

Generate the following instruction sequence for *non-imported* and also for *imported* global objects. The compiler generates a LD + LD or LD + ADD and the module loader computes the static base and patches the instruction sequence to MOV1+U + IOR, as shown below.

**Case a. For determining the absolute address of a global variable:**  (for both mno = 0 and mno > 0)

     MOV1+U RH, (staticbase + offset) DIV 10000H         ; upper 16 bits of RH := (static base + offset) DIV 10000H
     IOR RH, RH, (staticbase + offset) MOD 10000H        ; lower 16 bits of RH := (static base + offset) MOD 10000H

**Case b. For loading the value of a global variable into a register:**  (for both mno = 0 and mno > 0)

     MOV1+U RH, (staticbase + offset) DIV 10000H         ; upper 16 bits of RH := (static base + offset) DIV 10000H
     LD RH, RH, (staticbase + offset) MOD 10000H         ; RH := RH + (static base + offset) MOD 10000H

I.e. the *module loader* computes the static base instead of issuing an instruction that uses the MT register to obtain the static base from a table global to the system.

This variant eliminates the need for the global module table starting at MTOrg+4.

In order to make a pre-linked binary file (M.bin) relocatable, all MOV+U instructions that were previously part of a fixup chain are marked with the "B" bit in the (otherwise unused) "b" operand field of the MOV instruction during the process of linking (see the fixup code at the end of procedure *ORL.Link*):

      U = 20000000H; V = 10000000H; B = 100000H;  (*modifier bits*)
      MOV = 40000000H; IOR = 40060000H;  (*F1 register instructions*)
      C4 = 10H; C16 = 10000H; C20 = 100000H; C24 = 1000000H; C28 = 10000000H;

      (*fixup of LDR/STR/ADD*)
      adr := mod.code + fixorgD*4;
      WHILE adr # mod.code DO
        ..
        SYSTEM.PUT(adr, MOV+U+B + pno*C24 + (offset - Start + binStart) DIV C16); (*mark as fixed up by setting the B bit*)
        ..
        adr := adr - disp*4
      END ;

This makes it possible to load any pre-linked binary to *any* memory location and adjust all absolute memory adresses contained in instructions that were previously part of the fixup chain. This is illustrated in the following code excerpt.

      i := M.code;
      WHILE i < M.imp DO j := i - oldStart + Start; SYSTEM.GET(j, x);
        IF x DIV C28 * C28 + x DIV C16 MOD C8 * C16 = MOV+U+B THEN  (*marked as fixed up via the B bit*)
          SYSTEM.GET(j+4, y); im := x MOD C16 * C16 + y MOD C16 - oldStart + newStart;  (*relocate its operand*)
          SYSTEM.PUT(j, x DIV C16 * C16 + im DIV C16);
          SYSTEM.PUT(j+4, y DIV C16 * C16 + im MOD C16); INC(i, 4)
        END ;
        INC(i, 4)
      END ;

--------------------------------------------------------------------------

**2. Variant 2**  (implemented in subdirectory [**Sources/FPGAOberon2013/Variant2**](Sources/FPGAOberon2013/Variant2) for Project Oberon 2013 (see appendix 2 for the changes made)

This variant is similar to variant 1, but only changes the way *imported* global objects (mno > 0) are accessed. See variant 1 for the general case, which includes *non-imported* global objects.

**Case a. For determining the absolute address of a global variable:**  (for mno > 0 only)

     MOV1+U RH, (staticbase + offset) DIV 10000H         ; upper 16 bits of RH := (static base + offset) DIV 10000H
     IOR RH, RH, (staticbase + offset) MOD 10000H        ; lower 16 bits of RH := (static base + offset) MOD 10000H

**Case b. For loading the value of a global variable into a register:**  (for mno > 0 only)

     MOV1+U RH, (staticbase + offset) DIV 10000H         ; upper 16 bits of RH := (static base + offset) DIV 10000H
     LD RH, RH, (staticbase + offset) MOD 10000H         ; RH := RH + (static base + offset) MOD 10000H

for imported global variables. Let the *module loader* compute the static base instead of issuing (or fixing up) an instruction that uses the MT register, *and* add the offset during that process.

In this variant only the access to *imported* objects is modified in the way described above. This means that for *non-imported* global objects declared in the *same* module, if the offset is >64KB, the compiler continues to generate the following four-instruction sequence

     LD RH, MT, mno*4
     MOV1+U RH+1, offset DIV 10000H     ; upper 16 bits
     IOR RH+1, RH+1, offset MOD 10000H  ; lower 16 bits
     ADD RH, RH, RH+1                   ; RH := RH + RH+1

To make this work, procedure *ORG.Put1a* needs to be changed to the following code (which adds the ELSE clause):

     PROCEDURE Put1a(op, a, b, im: LONGINT);
       VAR r: INTEGER;
     BEGIN (*same as Put1, but with range test  -10000H <= im < 10000H*)
       IF (im >= -10000H) & (im <= 0FFFFH) THEN Put1(op, a, b, im)
       ELSIF op = Mov THEN
         Put1(Mov+U, a, 0, im DIV 10000H);
         IF im MOD 10000H # 0 THEN Put1(Ior, a, a, im MOD 10000H) END
       ELSE r := RH;
         IF b = RH THEN incR END ;
         Put1(Mov+U, RH, 0, im DIV 10000H);
         IF im MOD 10000H # 0 THEN Put1(Ior, RH, RH, im MOD 10000H) END ;
         Put0(op, a, b, RH);
         IF RH > r THEN DEC(RH) END
       END
     END Put1a;

--------------------------------------------------------------------------

**3. Variant 3**  (implemented in subdirectory [**Sources/FPGAOberon2013/Variant3**](Sources/FPGAOberon2013/Variant3) for Project Oberon 2013 (see appendix 3 for the changes made)

This is a variation (and earlier version) of variant 2, where the *static base* and the *offset* are computed separately.

Generate the following instruction sequences:

a. For determining the absolute address of a global variable:

     MOV1+U RH, staticbase DIV 10000H         ; upper 16 bits of RH := static base of the accessed module DIV 10000H
     IOR RH, RH, staticbase MOD 10000H        ; lower 16 bits of RH := static base of the accessed module MOD 10000H
     ADD RH, RH, offset                       ; access the object inside the module pointed to by RH (=static base)

b. For loading the value of global variable into a register:

     MOV1+U RH, staticbase DIV 10000H         ; upper 16 bits of RH := static base of the accessed module DIV 10000H
     IOR RH, RH, staticbase MOD 10000H        ; lower 16 bits of RH := static base of the accessed module MOD 10000H
     LD RH, RH, offset                        ; access the object inside the module pointed to by RH (=static base)

In sum, we replace *one* memory load (LD) instruction with *two* register instructions (MOV1+U and IOR).

This variant eliminates the need for the global module table starting at MTOrg (and therefore the MT register).

For case a, this variant imposes a 64KB restriction, as the ADD instruction has an offset field of 16 bits (2^16 = 64KB).

For case b, this variant imposes a 1MB restriction, as the LD instruction has an offset field of 20 bits (2^20 = 1MB).

First, the compiler issues the following three-instruction sequence (the first 2 instructions are generated in *ORG.GetSB*)

      | 0110 (4) | reg (4) | mno (8) | pc-fixorgD (16) |      (max mno = 255, max disp = 2^16-1 = 64K-1 words)
      | 0100 (4) | reg (4) | reg (4) | IOR (4) | 0 (16) |
      | LD (4)   | reg (4) | reg (4) | pno (20) | 

which the module loader fixes up to (see *Modules.Load*)

      | 0110 (4) | RH (4) | 0 (4)  | MOV1+U (4)  | staticbase DIV 10000H (16) |   =  MOV1+U RH, staticbase DIV 10000H
      | 0100 (4) | RH (4) | RH (4) | IOR (4) | staticbase MOD 10000H (16) |       =  IOR RH, RH, staticbase MOD 10000H
      | LD (4)   | RH (4) | RH (4) | offset from static base (20) |               =  LD RH, RH, offset       

which is the above instruction sequence.

We generate a MOV1+U instruction (= form 1 MOV = F1 MOV instruction with the U bit set) followed by an IOR instruction, because the regular MOV1 instruction has an address field of 16 bits only. Recall the following definitions of form 0 (F0) and form 1 (F1) register instructions:

     F0  | 00uv | a (4) | b (4) | op (4) | unused (12) | c (4) |      ; form 0
     F1  | 01uv | a (4) | b (4) | op (4) | im (16) |                  ; form 1

The modifier bit u = 1 changes the effect of the MOV instruction as follows:

     F0   MOV0+U = form 0 MOV', u = 1, v = 0         ; R.a := H
     F0   MOV0+U = form 0 MOV', u = 1, v = 1         ; R.a := [N, Z, C, V]
     F1   MOV1+U = form 1 MOV', u = 1, v unusued     ; R.a := [imm left shifted 16 bits]

We use the F1 MOV1+U = form 1 MOV1' instruction with the u bit set (u = 1) and v = 0 (the v bit is not used and set to 0). 

*Advantages:*

1. This solution works for *all* memory addresses, not just for addresses < 64KB
2. It eliminates the need for the global module table pointed to by the MT register.

*Disadvantages:*

1. It is not actually faster than the main version, but runs at the same speed (for accesses to external module data). On a typical implementation of Oberon on RISC, two register instructions take the same time as one memory load instruction on the RISC5 hardware.

2. Code density is sacrificed, as we now use 3 instructions instead of 2 for *every* access to a global or external variable.

We generate 3 instructions even in the case where the absolute memory address of the static base of the accessed module (at run time) ends up at less than 2^16 bytes = 64KB, in which case a two-instruction sequence

     MOV RH, staticbase        ; RH := static base of the accessed module
     LD RH, RH, offset         ; access the object inside the module pointed to by RH (=static base)

would actually suffice. However, it is impossible to know at compile time whether the absolute memory address (at run time) of the static base of a module will be less than 2^16 = 64KB or not.

--------------------------------------------------------------------------

**4. Variant 4**  (implemented in subdirectory [**Sources/FPGAOberon2013/Variant4**](Sources/FPGAOberon2013/Variant4) for Project Oberon 2013 (see appendix 4 for the changes made)

Generate the following instruction sequences, whenever *staticbase* < 10000H = 64KB, i.e. whenever it fits in the 16 bit address field of a MOV instruction:

a. For determining the absolute address of a global variable:

     MOV RH, staticbase   ; RH := static base of the accessed module, as computed by the module loader
     ADD RH, RH, offset   ; access the object inside the module pointed to by RH (=static base)

b. For loading the value of global variable into a register:

     MOV RH, staticbase   ; RH := static base of the accessed module, as computed by the module loader
     LD RH, RH, offset    ; access the object inside the module pointed to by RH (=static base)

Otherwise, generate the instruction sequences of variant 0 (= status quo of Project Oberon 2013):

This variant eliminates the need for the global module table starting at MTOrg (and therefore the MT register).

For case a, this variant imposes a 64KB restriction, as the ADD instruction has an offset field of 16 bits (2^16 = 64KB).

For case b, this variant imposes a 1MB restriction, as the LD instruction has an offset field of 20 bits (2^20 = 1MB).

Recall that in Project Oberon 2013, LD instructions for loading the static base of a module are generated by the compiler as follows

     | LD (4) | reg (4) | mno (4) | pc-fixorgD (20) |         (max mno = 15, max disp = 2^20-1 = 1M-1 words)

The module loader will fix up such instructions to (see the fixup code at the end of *Modules.Load*)

     | LD (4) | reg (4) | MT (4) | offset for imported module in MT table (20) |    (max offset = 2^20-1 = 1M-1 words)

to arrive at the following two-instruction sequence for accessing global module data at run time (e.g.)

     LD RH, MT, mno*4    ; RH := static base of the accessed module, using the module table pointed to by the MT register
     LD RH, RH, offset   ; access the object inside the module pointed to by RH (=static base)

The code of this variant changes the fixup code of the module loader such that the *first* LD instruction of the above instruction sequence is replaced with a (faster) register instruction (MOV) at module load time, whenever *staticbase* < 10000H = 64KB, i.e. whenever it fits in the 16 bit address field of the MOV instruction

Note that in this variant, the 64KB restriction is for the *static base* only. One could, for example, access a global variable at absolute address 64KB + 25KB.

This approach speeds up access to global variables of modules *Kernel*, *FileDir*, *Files*, *Modules*, *Input*, *Display*, *Viewers*, *Fonts* and *Texts* (which is the last module in the module hierarchy with an absolute static base address of <64KB).

One may argue that this is an optimization in the wrong place, as access to global variables is (or should be) rare. However, a number of global variables in the *inner core* module *Kernel* are accessed *each time* the predefined procedure NEW is called or when the Oberon garbage collector is invoked. The module loader also accesses global module data extensively. Performance measurements may provide additional insights into whether the additional effort is justified.

*Advantages:*

1. This solution replaces one memory load instruction by one register instruction (faster).
2. Code density is not sacrificed, as the number of instructions generated stays the same.
3. There are no changes to the object file format. Only the module loader/linker is affected.

*Disadvantages:*

1. It only works for accesses to memory at absolute memory locations less than 2^12 bytes = 64KB, as only 16 bits can be used in the MOV instruction.
2. It assumes that module blocks are allocated at the lower portion of  the address space starting at address zero (which is the case in Project Oberon 2013, but not necessarily the case in other implementations of the Oberon system).

*Comment:*

Note that the MOV instruction does not use (the 4 bits of) the operand **b**. Thus, if the instruction format were changed such that the MOV instruction makes use of those 4 bits as part of the address field, i.e. if it were no longer defined as

     | 01uv (4) | a (4) | b (4) | MOV (4) | offset (16) |           (max offset = 2^16-1 = 64KB-1 bytes, MOV = 0)

but as

     | 01uv (4) | a (4) | offset[16:19] (4) | MOV (4) | offset[0:15] (16) |    (max total offset = 2^20-1 = 1MB-1 bytes, MOV = 0)

then a module space of 2^20 bytes = 1 MB could be covered by the above solution, which easily includes the entire Oberon system.

--------------------------------------------------------------------------

**5. Variant 5**  (not implemented)

Generate the following instruction sequences, whenever *staticbase + offset* < 10000H = 64KB, i.e. whenever it fits in the 16 bit address field of a MOV instruction:

a. For determining the absolute address of a global variable:

     MOV RH, staticbase + offset   ; RH := static base of the accessed module + offset, as computed by the module loader

b. For loading the value of global variable into a register:

     MOV RH, staticbase + offset   ; RH := static base of the accessed module + offset, as computed by the module loader
     LD RH, RH, 0                  ; access the object located at the absolute address stored in register RH

This variant eliminates the need for the global module table starting at MTOrg (and therefore the MT register).

For case a, this variant imposes a 64KB restriction, as the MOV instruction has an offset field of 16 bits (2^16 = 64KB).

For case b, this variant imposes a 1MB restriction, as the LD instruction has an offset field of 20 bits (2^20 = 1MB).

Note that the 64KB restriction is for the *sum* of staticbase and offset. One could, for example, no longer access a global variable at absolute address 64KB + 25KB. This makes no sense and one should use variant 4 in this case.

--------------------------------------------------------------------------

**6. Variant 6** (not implemented)

Generate the following instruction sequences, whenever *staticbase* < 10000H = 64KB, i.e. whenever it fits in the 16 bit address field of a MOV instruction:

a. For determining the absolute address of a global variable:

     MOV RH, staticbase   ; RH := static base of the accessed module, as computed by the module loader
     ADD RH, RH, offset   ; access the object inside the module pointed to by RH (=static base)

b. For loading the value of global variable into a register:

     LD RH, MT, impmod + offset - 20H   ; where MT is a register that permanently holds the value 20H and the address field is 20 bits wide

This variant eliminates the need for the global module table starting at MTOrg (and therefore the MT register).

For case a, this variant imposes a 64KB restriction, as the ADD instruction has an offset field of 16 bits (2^16 = 64KB).

For case b, this variant imposes a 1MB restriction, as the LD instruction has an offset field of 20 bits (2^20 = 1MB).

Note that the 1MB restriction is for the *sum* of (staticbase of) impmod and offset (-20H).

Variant 6 no longer embeds the module number (mno) *and* the fixup chain in the instruction, as generated by the compiler. Instead, the module number (mno) - along with the address of the LD instruction to be fixed up (instadr) and the offset from the base address of the imported module's data section (offset) - is added as a separate section to the object file as follows:

Additional fixup section on the object file:

     mno1  instadr1  offset1
     mno2  instadr2  offset2
     mno3  instadr3  offset3
     ...

The module loader uses this information to compute the static base of the imported module using the instruction

     SYSTEM.GET(mod.imp + (mno-1)*4, impmod);   (*impmod = static base of imported module*)

and to fixup the (single) LD instruction located at absolute memory address *mod + instadr* to

     LD RH, MT, impmod + offset - 20H   ; where MT is a register that permanently holds the value 20H and the address field is 20 bits wide

*Advantages:*

1. A two instruction sequence LD + LD is replaced a single LD instruction.
2. This works for all memory addresses up to 2^20 = 1MB, not just addresses up to 64KB

*Disadvantages:*

1. This only works for the instruction sequence LD + LD (as generated in *ORG.load*), but not for LD + ADD and LD + STR
2. This only works for memory addresses up to 2^20 and not for the full 32-bit address space as the original two-instruction sequence (this is not a problem in Project Oberon 2013 which uses only 2^20 bytes = 1MB of memory anyway, but on systems with larger memory sizes, this would become a restriction).
3. The object file format has changed
4. The object file is longer (one additional entry for *each* external load accees)
5. The fixup process at module load time is slower

--------------------------------------------------------------------------
**2. Preparing your system to use faster access to global module data**

**PREREQUISITES**: A current version of Project Oberon 2013 (see http://www.projectoberon.com).

------------------------------------------------------
**STEP 1**: Download and import the files to build the improved decoder tool to your system

Download all files from the [**Sources**](Sources/) directory of this repository. Convert the *source* files to Oberon format (Oberon uses CR as line endings) using the command [**dos2oberon**](dos2oberon), available in this repository (example shown for Linux or macOS):

     for x in *.Mod ; do ./dos2oberon $x $x ; done

Import the files to your Oberon system. If you use an emulator, click on the *PCLink1.Run* link in the *System.Tool* viewer, copy the files to the emulator directory, and execute the following command on the command shell of your host system:

     cd oberon-risc-emu
     for x in *.Mod ; do ./pcreceive.sh $x ; sleep 0.5 ; done

------------------------------------------------------
**STEP 2:** Build a modified Oberon compiler and boot linker/loader

     ORP.Compile ORS.Mod/s ORB.Mod/s ~
     ORP.Compile ORG.Mod/s ORP.Mod/s ~
     ORP.Compile ORL.Mod/s ORTool.Mod/s ~
     System.Free ORTool ORP ORG ORB ORS ORL ~

------------------------------------------------------
**STEP 3:** Use the new toolchain on your original system to rebuild the entire system and compiler

Compile the *inner core* and load it onto the boot area of the local disk:

Project Oberon 2013:

     ORP.Compile Kernel.Mod FileDir.Mod Files.Mod Modules.Mod ~    # modules for the "regular" boot file
     ORL.Link Modules ~                                            # generate a pre-linked binary file of the "regular" boot file (Modules.bin)
     ORL.Load Modules.bin ~                                        # load the "regular" boot file onto the boot area of the local disk

Compile the remaining modules of the Oberon system:

     ORP.Compile Input.Mod Display.Mod/s Viewers.Mod/s ~
     ORP.Compile Fonts.Mod/s Texts.Mod/s Oberon.Mod/s ~
     ORP.Compile MenuViewers.Mod/s TextFrames.Mod/s ~
     ORP.Compile System.Mod/s Edit.Mod/s Tools.Mod/s ~

Re-compile the modified Oberon compiler itself before (!) restarting the system:

     ORP.Compile ORS.Mod/s ORB.Mod/s ~
     ORP.Compile ORG.Mod/s ORP.Mod/s ~
     ORP.Compile ORL.Mod/s ORX.Mod/s ORTool.Mod/s ~

The last step is necessary because the modified version of your Oberon system uses a different object file format. If you don't re-compile the compiler before restarting the Oberon system, you won't be able to start it afterwards!

------------------------------------------------------
**STEP 4:** Restart the Oberon system

You are now running a modified Oberon system with an improved decoder tool *ORTool.DecObj*.

Re-compile any other modules that you may have on your system.

--------------------------------------------------------------------------

# Appendix 2: Changes made to Project Oberon 2013 for Variant 2

**ORG.Mod**

```diff
--- FPGAOberon2013/ORG.Mod  2019-05-30 17:58:14.000000000 +0200
+++ Oberon-fast-access-to-global-data/Sources/FPGAOberon2013/Variant2/ORG.Mod  2020-02-28 19:03:39.000000000 +0100
@@ -51,6 +51,11 @@
 
   (*instruction assemblers according to formats*)
 
+  PROCEDURE incR;
+  BEGIN
+    IF RH < MT-1 THEN INC(RH) ELSE ORS.Mark("register stack overflow") END
+  END incR;
+
   PROCEDURE Put0(op, a, b, c: LONGINT);
   BEGIN (*emit format-0 instruction*)
     code[pc] := ((a*10H + b) * 10H + op) * 10000H + c; INC(pc)
@@ -63,11 +68,18 @@
   END Put1;
 
   PROCEDURE Put1a(op, a, b, im: LONGINT);
+    VAR r: INTEGER;
   BEGIN (*same as Put1, but with range test  -10000H <= im < 10000H*)
     IF (im >= -10000H) & (im <= 0FFFFH) THEN Put1(op, a, b, im)
-    ELSE Put1(Mov+U, RH, 0, im DIV 10000H);
+    ELSIF op = Mov THEN
+      Put1(Mov+U, a, 0, im DIV 10000H);
+      IF im MOD 10000H # 0 THEN Put1(Ior, a, a, im MOD 10000H) END
+    ELSE r := RH;
+      IF b = RH THEN incR END ;
+      Put1(Mov+U, RH, 0, im DIV 10000H);
       IF im MOD 10000H # 0 THEN Put1(Ior, RH, RH, im MOD 10000H) END ;
-      Put0(op, a, b, RH)
+      Put0(op, a, b, RH);
+      IF RH > r THEN DEC(RH) END
     END
   END Put1a;
 
@@ -81,11 +93,6 @@
     code[pc] := ((op+12) * 10H + cond) * 1000000H + (off MOD 1000000H); INC(pc)
   END Put3;
 
-  PROCEDURE incR;
-  BEGIN
-    IF RH < MT-1 THEN INC(RH) ELSE ORS.Mark("register stack overflow") END
-  END incR;
-
   PROCEDURE CheckRegs*;
   BEGIN
     IF RH # 0 THEN ORS.Mark("Reg Stack"); RH := 0 END ;
```

**Modules.Mod**

```diff
--- FPGAOberon2013/Modules.Mod  2020-02-26 01:15:33.000000000 +0100
+++ Oberon-fast-access-to-global-data/Sources/FPGAOberon2013/Variant2/Modules.Mod  2020-02-29 12:33:31.000000000 +0100
@@ -1,6 +1,12 @@
 MODULE Modules;  (*Link and load on RISC; NW 20.10.2013 / 8.1.2019*)
   IMPORT SYSTEM, Files;
   CONST versionkey = 1X; MT = 12; DescSize = 80;
+    U = 20000000H; V = 10000000H;  (*modifier bits*)
+    MOV = 40000000H; IOR = 40060000H;  (*F1 register instructions*)
+    F2 = -2;  (*F2 memory instruction*)
+    BC = 0E7000000H; BL = 0F7000000H;  (*F3 branch instructions*)
+    C4 = 10H; C6 = 40H; C8 = 100H; C10 = 400H; C12 = 1000H; C14 = 4000H; C16 = 10000H; C18 = 40000H;
+    C20 = 100000H; C22 = 400000H; C24 = 1000000H; C26 = 4000000H; C28 = 10000000H; C30 = 40000000H;
 
   TYPE Module* = POINTER TO ModDesc;
     Command* = PROCEDURE;
@@ -135,30 +141,32 @@
         adr := mod.code + fixorgP*4;
         WHILE adr # mod.code DO
           SYSTEM.GET(adr, inst);
-          mno := inst DIV 100000H MOD 10H;
-          pno := inst DIV 1000H MOD 100H;
-          disp := inst MOD 1000H;
+          mno := inst DIV C20 MOD C4;
+          pno := inst DIV C12 MOD C8;
+          disp := inst MOD C12;
           SYSTEM.GET(mod.imp + (mno-1)*4, impmod);
           SYSTEM.GET(impmod.ent + pno*4, dest); dest := dest + impmod.code;
           offset := (dest - adr - 4) DIV 4;
-          SYSTEM.PUT(adr, (offset MOD 1000000H) + 0F7000000H);
+          SYSTEM.PUT(adr, (offset MOD C24) + BL);
           adr := adr - disp*4
         END ;
         (*fixup of LDR/STR/ADD*)
         adr := mod.code + fixorgD*4;
         WHILE adr # mod.code DO
           SYSTEM.GET(adr, inst);
-          mno := inst DIV 100000H MOD 10H;
-          disp := inst MOD 1000H;
+          mno := inst DIV C20 MOD C4;
+          disp := inst MOD C12;
           IF mno = 0 THEN (*global*)
-            SYSTEM.PUT(adr, (inst DIV 1000000H * 10H + MT) * 100000H + mod.num * 4)
+            SYSTEM.PUT(adr, (inst DIV C24 * C4 + MT) * C20 + mod.num * 4)
           ELSE (*import*)
-            SYSTEM.GET(mod.imp + (mno-1)*4, impmod); v := impmod.num;
-            SYSTEM.PUT(adr, (inst DIV 1000000H * 10H + MT) * 100000H + v*4);
-            SYSTEM.GET(adr+4, inst); vno := inst MOD 100H;
+            SYSTEM.GET(mod.imp + (mno-1)*4, impmod);
+            SYSTEM.GET(adr+4, inst); vno := inst MOD C8;
             SYSTEM.GET(impmod.ent + vno*4, offset);
-            IF ODD(inst DIV 100H) THEN offset := offset + impmod.code - impmod.data END ;
-            SYSTEM.PUT(adr+4, inst DIV 10000H * 10000H + offset)
+            IF ODD(inst DIV C8) THEN INC(offset, impmod.code) ELSE INC(offset, impmod.data) END ;
+            vno := inst DIV C24 MOD C4; (*RH*)
+            SYSTEM.PUT(adr, MOV+U + vno*C24 + offset DIV C16);
+            IF inst DIV C30 = F2 THEN inst := (inst DIV C24) * C24 ELSE inst := IOR + vno*C24 END ;
+            SYSTEM.PUT(adr+4, inst + vno*C20 + offset MOD C16)
           END ;
           adr := adr - disp*4
         END ;
@@ -166,9 +174,9 @@
         adr := mod.data + fixorgT*4;
         WHILE adr # mod.data DO
           SYSTEM.GET(adr, inst);
-          mno := inst DIV 1000000H MOD 10H;
-          vno := inst DIV 1000H MOD 1000H;
-          disp := inst MOD 1000H;
+          mno := inst DIV C24 MOD C4;
+          vno := inst DIV C12 MOD C12;
+          disp := inst MOD C12;
           IF mno = 0 THEN (*global*) inst := mod.data + vno
           ELSE (*import*)
             SYSTEM.GET(mod.imp + (mno-1)*4, impmod);
```

**ORL.Mod**

```diff
--- FPGAOberon2013/ORL.Mod  2020-02-29 12:58:05.000000000 +0100
+++ Oberon-fast-access-to-global-data/Sources/FPGAOberon2013/Variant2/ORL.Mod  2020-02-29 12:32:47.000000000 +0100
@@ -2,6 +2,9 @@
   IMPORT SYSTEM, Kernel, Files, Modules, Texts, Oberon;
   CONST versionkey = 1X; versionkey0 = 0X; MT = 12; DescSize = 80; MnLength = 32;
     noerr* = 0; nofile* = 1; badversion* = 2; badkey* = 3; badfile* = 4; nospace* = 5;
+    U = 20000000H; V = 10000000H;  (*modifier bits*)
+    MOV = 40000000H; IOR = 40060000H;  (*F1 register instructions*)
+    F2 = -2;  (*F2 memory instruction*)
     BC = 0E7000000H; BL = 0F7000000H;  (*F3 branch instructions*)
     C4 = 10H; C6 = 40H; C8 = 100H; C10 = 400H; C12 = 1000H; C14 = 4000H; C16 = 10000H; C18 = 40000H;
     C20 = 100000H; C22 = 400000H; C24 = 1000000H; C26 = 4000000H; C28 = 10000000H; C30 = 40000000H;
@@ -163,12 +166,14 @@
           IF mno = 0 THEN (*global*)
             SYSTEM.PUT(adr, (inst DIV C24 * C4 + MT) * C20 + mod.num * 4)
           ELSE (*import*)
-            SYSTEM.GET(mod.imp + (mno-1)*4, impmod); v := impmod.num;
-            SYSTEM.PUT(adr, (inst DIV C24 * C4 + MT) * C20 + v*4);
+            SYSTEM.GET(mod.imp + (mno-1)*4, impmod);
             SYSTEM.GET(adr+4, inst); vno := inst MOD C8;
             SYSTEM.GET(impmod.ent + vno*4, offset);
-            IF ODD(inst DIV C8) THEN offset := offset + impmod.code - impmod.data END ;
-            SYSTEM.PUT(adr+4, inst DIV C16 * C16 + offset)
+            IF ODD(inst DIV C8) THEN INC(offset, impmod.code) ELSE INC(offset, impmod.data) END ;
+            vno := inst DIV C24 MOD C4; (*RH*)
+            SYSTEM.PUT(adr, MOV+U + vno*C24 + (offset - Start) DIV C16);
+            IF inst DIV C30 = F2 THEN inst := (inst DIV C24) * C24 ELSE inst := IOR + vno*C24 END ;
+            SYSTEM.PUT(adr+4, inst + vno*C20 + (offset - Start) MOD C16)
           END ;
           adr := adr - disp*4
         END ;
```

--------------------------------------------------------------------------

# Appendix 3: Changes made to Project Oberon 2013 for Variant 3

**ORG.Mod**

```diff
--- FPGAOberon2013/ORG.Mod  2019-05-30 17:58:14.000000000 +0200
+++ Oberon-fast-access-to-global-data/Sources/FPGAOberon2013/Variant3/ORG.Mod  2020-02-26 00:48:17.000000000 +0100
@@ -71,6 +71,11 @@
     END
   END Put1a;
 
+  PROCEDURE Put1b(a, mno, off: LONGINT);
+  BEGIN (*emit MOV' instruction to be fixed up by loader*)
+    code[pc] := ((a+96) * 100H + mno) * 10000H + (off MOD 10000H); INC(pc)
+  END Put1b;
+
   PROCEDURE Put2(op, a, b, off: LONGINT);
   BEGIN (*emit load/store instruction*)
     code[pc] := ((op * 10H + a) * 10H + b) * 100000H + (off MOD 100000H); INC(pc)
@@ -147,7 +152,7 @@
   PROCEDURE GetSB(base: LONGINT);
   BEGIN
     IF version = 0 THEN Put1(Mov, RH, 0, VarOrg0)
-    ELSE Put2(Ldr, RH, -base, pc-fixorgD); fixorgD := pc-1
+    ELSE Put1b(RH, -base, pc-fixorgD); fixorgD := pc-1; Put1(Ior, RH, RH, 0)
     END
   END GetSB;
```

**ORL.Mod**

```diff
--- FPGAOberon2013/ORL.Mod  2020-02-29 08:48:23.000000000 +0100
+++ Oberon-fast-access-to-global-data/Sources/FPGAOberon2013/Variant3/ORL.Mod  2020-02-29 10:48:09.000000000 +0100
@@ -158,17 +158,21 @@
         adr := mod.code + fixorgD*4;
         WHILE adr # mod.code DO
           SYSTEM.GET(adr, inst);
-          mno := inst DIV C20 MOD C4;
-          disp := inst MOD C12;
+          mno := inst DIV C16 MOD C8;
+          disp := inst MOD C16;
           IF mno = 0 THEN (*global*)
-            SYSTEM.PUT(adr, (inst DIV C24 * C4 + MT) * C20 + mod.num * 4)
+            SYSTEM.PUT(adr, (inst DIV C24) * C24 + (mod.data - Start) DIV C16); (*MOV'*)
+            SYSTEM.GET(adr+4, inst);
+            SYSTEM.PUT(adr+4, inst + (mod.data - Start) MOD C16) (*IOR*)
           ELSE (*import*)
-            SYSTEM.GET(mod.imp + (mno-1)*4, impmod); v := impmod.num;
-            SYSTEM.PUT(adr, (inst DIV C24 * C4 + MT) * C20 + v*4);
-            SYSTEM.GET(adr+4, inst); vno := inst MOD C8;
+            SYSTEM.GET(mod.imp + (mno-1)*4, impmod);
+            SYSTEM.PUT(adr, (inst DIV C24) * C24 + (impmod.data - Start) DIV C16); (*MOV'*)
+            SYSTEM.GET(adr+4, inst);
+            SYSTEM.PUT(adr+4, inst + (impmod.data - Start) MOD C16); (*IOR*)
+            SYSTEM.GET(adr+8, inst); vno := inst MOD C8;
             SYSTEM.GET(impmod.ent + vno*4, offset);
             IF ODD(inst DIV C8) THEN offset := offset + impmod.code - impmod.data END ;
-            SYSTEM.PUT(adr+4, inst DIV C16 * C16 + offset)
+            SYSTEM.PUT(adr+8, inst DIV C16 * C16 + offset)
           END ;
           adr := adr - disp*4
         END ;
```

**Modules.Mod**

```diff
--- FPGAOberon2013/Modules.Mod  2019-01-17 16:43:23.000000000 +0100
+++ Oberon-fast-access-to-global-data/Sources/FPGAOberon2013/Variant3/Modules.Mod  2020-02-26 00:58:07.000000000 +0100
@@ -148,17 +148,21 @@
         adr := mod.code + fixorgD*4;
         WHILE adr # mod.code DO
           SYSTEM.GET(adr, inst);
-          mno := inst DIV 100000H MOD 10H;
-          disp := inst MOD 1000H;
+          mno := inst DIV 10000H MOD 100H;
+          disp := inst MOD 10000H;
           IF mno = 0 THEN (*global*)
-            SYSTEM.PUT(adr, (inst DIV 1000000H * 10H + MT) * 100000H + mod.num * 4)
+            SYSTEM.PUT(adr, (inst DIV 1000000H) * 1000000H + mod.data DIV 10000H); (*MOV'*)
+            SYSTEM.GET(adr+4, inst);
+            SYSTEM.PUT(adr+4, inst + mod.data MOD 10000H) (*IOR*)
           ELSE (*import*)
-            SYSTEM.GET(mod.imp + (mno-1)*4, impmod); v := impmod.num;
-            SYSTEM.PUT(adr, (inst DIV 1000000H * 10H + MT) * 100000H + v*4);
-            SYSTEM.GET(adr+4, inst); vno := inst MOD 100H;
+            SYSTEM.GET(mod.imp + (mno-1)*4, impmod);
+            SYSTEM.PUT(adr, (inst DIV 1000000H) * 1000000H + impmod.data DIV 10000H); (*MOV'*)
+            SYSTEM.GET(adr+4, inst);
+            SYSTEM.PUT(adr+4, inst + impmod.data MOD 10000H); (*IOR*)
+            SYSTEM.GET(adr+8, inst); vno := inst MOD 100H;
             SYSTEM.GET(impmod.ent + vno*4, offset);
             IF ODD(inst DIV 100H) THEN offset := offset + impmod.code - impmod.data END ;
-            SYSTEM.PUT(adr+4, inst DIV 10000H * 10000H + offset)
+            SYSTEM.PUT(adr+8, inst DIV 10000H * 10000H + offset)
           END ;
           adr := adr - disp*4
         END ;
```


--------------------------------------------------------------------------

# Appendix 4: Changes made to Project Oberon 2013 for Variant 4

**ORL.Mod**

```diff
--- FPGAOberon2013/ORL.Mod  2020-02-29 08:48:23.000000000 +0100
+++ Oberon-fast-access-to-global-data/Sources/FPGAOberon2013/Variant4/ORL.Mod  2020-02-29 11:22:30.000000000 +0100
@@ -161,10 +161,17 @@
           mno := inst DIV C20 MOD C4;
           disp := inst MOD C12;
           IF mno = 0 THEN (*global*)
-            SYSTEM.PUT(adr, (inst DIV C24 * C4 + MT) * C20 + mod.num * 4)
+            IF mod.data - Start < C16 THEN v := inst DIV C24 MOD C4;
+              SYSTEM.PUT(adr, (v+40H) * C24 + mod.data - Start)  (*MOV RH SB*)
+            ELSE SYSTEM.PUT(adr, (inst DIV C24 * C4 + MT) * C20 + mod.num * 4);
+            END
           ELSE (*import*)
-            SYSTEM.GET(mod.imp + (mno-1)*4, impmod); v := impmod.num;
-            SYSTEM.PUT(adr, (inst DIV C24 * C4 + MT) * C20 + v*4);
+            SYSTEM.GET(mod.imp + (mno-1)*4, impmod);
+            IF mod.data - Start < C16 THEN v := inst DIV C24 MOD C4;
+              SYSTEM.PUT(adr, (v+40H) * C24 + impmod.data - Start)  (*MOV RH SB*)
+            ELSE v := impmod.num;
+              SYSTEM.PUT(adr, (inst DIV C24 * C4 + MT) * C20 + v*4);
+            END ;
             SYSTEM.GET(adr+4, inst); vno := inst MOD C8;
             SYSTEM.GET(impmod.ent + vno*4, offset);
             IF ODD(inst DIV C8) THEN offset := offset + impmod.code - impmod.data END ;
```

**Modules.Mod**

```diff
--- FPGAOberon2013/Modules.Mod	2019-10-04 14:49:41.000000000 +0200
+++ Oberon-fast-access-to-global-module-data/Sources/FPGAOberon2013/Variant4/Modules.Mod	2019-10-20 18:14:21.000000000 +0200
@@ -151,10 +151,17 @@
           mno := inst DIV 100000H MOD 10H;
           disp := inst MOD 1000H;
           IF mno = 0 THEN (*global*)
-            SYSTEM.PUT(adr, (inst DIV 1000000H * 10H + MT) * 100000H + mod.num * 4)
+            IF mod.data < 10000H THEN v := inst DIV 1000000H MOD 10H;
+              SYSTEM.PUT(adr, (v+40H) * 1000000H + mod.data)  (*MOV RH SB*)
+            ELSE SYSTEM.PUT(adr, (inst DIV 1000000H * 10H + MT) * 100000H + mod.num * 4)
+            END
           ELSE (*import*)
-            SYSTEM.GET(mod.imp + (mno-1)*4, impmod); v := impmod.num;
-            SYSTEM.PUT(adr, (inst DIV 1000000H * 10H + MT) * 100000H + v*4);
+            SYSTEM.GET(mod.imp + (mno-1)*4, impmod);
+            IF mod.data < 10000H THEN v := inst DIV 1000000H MOD 10H;
+              SYSTEM.PUT(adr, (v+40H) * 1000000H + impmod.data)  (*MOV RH SB*)
+            ELSE v := impmod.num;
+              SYSTEM.PUT(adr, (inst DIV 1000000H * 10H + MT) * 100000H + v*4);
+            END ;
             SYSTEM.GET(adr+4, inst); vno := inst MOD 100H;
             SYSTEM.GET(impmod.ent + vno*4, offset);
             IF ODD(inst DIV 100H) THEN offset := offset + impmod.code - impmod.data END ;
```
