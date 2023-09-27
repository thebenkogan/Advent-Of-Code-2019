exception Halt

val read_mem : string list -> int array
(** [read_mem lines] is the intcode program represented by the input *)

val run : int list ref -> int array -> int -> int * int array * int
(** [read_mem input_buffer mem pos] runs the intcode program with memory [mem] 
    starting at index [pos] until the first output if there is one. Otherwise,
    it will raise [Halt]. At any input instruction, it reads the next value from 
    the [input_buffer]. Returns a tuple of the output value, finishing memory, 
    and the position of the next instruction to execute. *)
