# x86-Multitasking-Kernel-.ASM
x86 ASM Multitasking Kernel for 16bit Registers.
Multitasking kernel for smaller systems with 16bit registers. Code uses the TSR (Terminate and Stay Resident) and ISR (Interrupt Service Routine) of the DOS of the operating system to make it possible. There are separate code segments, data segments, and extra segments for each task currently handled in this kernel. If you run this .asm file as it is, three tasks run simultaneously to display the word "Multitasking" on the screen. You can use any string to display on the screen. You can also increase the number of threads working at the same time. You can use Dosbox and NASM to compile the .asm file and run it on the console of Dosbox.<br />
## Main Features
- Complete Implementation of Threads
- Doubly linked List Implementation of PCB (Process Control Block)
- Create a new thread for a new task
- Delete the thread from PCB
- Suspend the Thread
- Resume the thread
- Save State
- Restore State
- Get the next thread
