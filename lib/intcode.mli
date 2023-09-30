exception Halt

type mem

val get_addr : mem -> int -> int
(** [get_addr mem addr] is the value in memory at the given address. *)

val set_addr : mem -> int -> int -> unit
(** [set_addr mem addr value] puts [value] in the given address in memory. *)

val read_mem : string list -> mem
(** [read_mem lines] is the intcode program represented by the input *)

type result = { output : int; mem : mem; pos : int; rel_base : int }

val run : int list ref -> mem -> int -> int -> result
(** [read_mem input_buffer mem pos rel_base] runs the intcode program with memory [mem] 
    starting at index [pos] until the first output if there is one. Otherwise,
    it will raise [Halt]. At any input instruction, it reads the next value from 
    the [input_buffer]. Returns a tuple of the output value, finishing memory, 
    and the position of the next instruction to execute. *)

val run_until_halt : int list ref -> mem -> int
(** [run_until_halt input_buffer mem] runs the intcode program with memory [mem] 
    starting at the first instruction until a halt is reached. Returns the last
    value outputted by the program before halthing. *)
