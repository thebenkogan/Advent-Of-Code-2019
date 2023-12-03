exception Halt

type mem

val get_addr : mem -> int -> int
(** [get_addr mem addr] is the value in memory at the given address. *)

val set_addr : mem -> int -> int -> unit
(** [set_addr mem addr value] puts [value] in the given address in memory. *)

val read_mem : string list -> mem
(** [read_mem lines] is the intcode program represented by the input *)

type state = { mem : mem; pos : int; rel_base : int }

val initial_state : mem -> state

exception NeedInput of state

val run : int list ref -> state -> int * state
(** [read_mem input_buffer state] runs the intcode program with the provided
    state until the first output if there is one. Otherwise, it will raise [Halt]. 
    At any input instruction, it reads the next value from the [input_buffer]. 
    Returns a tuple of the output value and the finishing state *)

val run_until_halt : int list ref -> mem -> int
(** [run_until_halt input_buffer mem] runs the intcode program with memory [mem] 
    starting at the first instruction until a halt is reached. Returns the last
    value outputted by the program before halthing. *)
