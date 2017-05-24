type 'a t

val empty    : 'a t
val is_empty : 'a t -> bool

val singleton : 'a   -> 'a t
val select    : 'a t -> 'a option

val fwd : 'a t -> 'a t
val rwd : 'a t -> 'a t

val reset : 'a t -> 'a t

val select_map : ('a -> 'a) -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val of_list : 'a list -> 'a t
val to_list : 'a t    -> 'a list
val front   : 'a t -> 'a list
val back    : 'a t -> 'a list

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** {1:Slider modification} *)

val insert : 'a -> 'a t -> 'a t
(** [insert y slider] is a new slider with y inserted before the selection. *)

val remove : 'a t -> 'a t
(** [remove slider] is a new slider with the selected element dropped.

    The previous item is selected if there are items in front of the slider.
    The next item is selected, if the front is empty.
    An empty slider results from removing the last item in a slider. *)