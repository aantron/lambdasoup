(* This file is part of Lambda Soup, released under the BSD 2-clause license.
   See docs/LICENSE for details, or visit
   https://github.com/aantron/lambda-soup. *)

(** Easy functional HTML scraping and manipulation for OCaml. *)



(** {2 Types} *)

type element
type general
type soup
(** "Phantom" types for use with ['a node]. See explanation below. *)

type 'a node
(** HTML document nodes. These come in three varieties: [element node]
    represents a node that is known to be an element, [soup node] represents an
    entire document, and [general node] represents a node that might be
    anything, including elements, documents, text content, and other kinds of
    HTML nodes that Lambda Soup doesn't yet support.

    Throughout Lambda Soup, if a function can operate on any kind of node, the
    argument is typed as [_ node]. If an element node or the entire document is
    required, the argument type is [element node] or [soup node],
    respectively. *)

type 'a nodes
(** Sequence of nodes. This is always instantiated as either [element nodes] or
    or [general nodes]. The sequence is {e lazy} in the sense that only as many
    elements as needed are evaluated. This can be used with {!with_stop} to
    traverse only part of a document until some condition is reached. *)



(** {2 High-level interface} *)

val parse : string -> soup node
(** Parses the given HTML text and evaluates to a document node containing the
    top-level HTML nodes found. Entity references are resolved, and the
    character encoding is detected automatically.

    If you need to parse XML, or control over parsing, or want to feed Lambda
    Soup something other than bytes, see
    {{: #2_Parsingsignals} Parsing signals}. *)

val select : string -> (_ node) -> element nodes
(** [select selector node] is all the descendants of [node] matching CSS
    selector [selector]. All
    {{: http://www.w3.org/TR/selectors/#selectors} CSS3 selectors} are
    supported, with the following exceptions, each of which only makes sense
    when doing layout or displaying an interactive interface:

{[
:link, :visited, :hover, :active, :focus, :target, :lang, :enabled,
:disabled, :checked, :indeterminate, ::first-line, ::first-letter,
::selection, ::before, ::after
]}

    There is also no support for XML namespace selectors. Instead, Lambda Soup
    supports the apparently-dropped
    {{: http://www.w3.org/TR/2001/CR-css3-selectors-20011113/#content-selectors}
    [:contains("foo")]} pseudo-class. You can see some examples of supported
    selectors in the
    {{: https://github.com/aantron/lambda-soup/blob/master/test/test.ml#L47}
    tests}.

    Lambda Soup selector syntax supports an extension of CSS. In regular CSS,
    the selector cannot start with a combinator such as [>]. Instead, you have
    to write things such as [* > p], which has a different meaning than what
    [> p] might mean. Lambda Soup allows selectors such as [> p], [+ p], and
    [~ p] to select immediate children of [node], adjacent next siblings, and
    all next siblings, respectively. In addition, you can use the empty selector
    to select [node] itself. Note, in this latter case, that if [node] is not an
    element (for example, it is often the soup node), the select will result in
    nothing, because select always results in sequences of {e element} nodes
    only. *)

val select_one : string -> (_ node) -> element node option
(** Like [select], but evaluates to at most one element. Note that there is also
    [R.select_one] if you don't want an option result, which is explained at
    {!require}. *)

val ($) : (_ node) -> string -> element node
(** [node $ selector] is the same as
    [node |> select_one selector |> require]. *)

val ($?) : (_ node) -> string -> element node option
(** [node $? selector] is the same as [node |> select_one selector]. *)

val ($$) : (_ node) -> string -> element nodes
(** [node $$ selector] is the same as [node |> select selector]. *)



(** {2 Options} *)

val require : 'a option -> 'a
(** [require (Some v)] evaluates to [v], and [require None] raises [Failure].
    Many functions in Lambda Soup return options, such as [select_one] above.
    When you know, better than the type system, that a value will definitely be
    present, [require] can be used to conveniently eliminate the option and get
    the value out.

    Note that there is also a module [R] provided, which contains, for each
    function that evaluates to an option, a version of the function that is
    post-composed with [require], and thus returns an unwrapped value (or raises
    [Failure]). So, instead of writing [soup |> children |> first |> require],
    you can write [soup |> children |> R.first].

    As an alternative to using [require] and [R], consider using an option
    monad.
 *)



(** {2 Early termination} *)

type 'a stop = {throw : 'b. 'a -> 'b}
(** A "polymorphic exception handler"/"continuation" used for early termination.
    See [with_stop] below. *)

val with_stop : ('a stop -> 'a) -> 'a
(** [with_stop (fun stop -> e)] evaluates to [v] if [e] evaluates to [v]. If
    [e] calls [stop.throw w], [with_stop] evaluates to [w]. In plainer words,
    [with_stop] creates an exception handler, to which the body [e] can return a
    value by calling [stop.throw]. This is meant for use with lazy node
    sequences. For example, here is an expression that finds the first node with
    a [draggable] attribute, stopping traversal immediately when that occurs:

{[
with_stop (fun stop ->
  some_root_node
  |> descendants
  |> elements
  |> fold (fun _ e ->
    if has_attribute "draggable" e then stop.throw (Some e)
    else None) None)
]}

    Of course, the [fold] can be done more easily using [filter] and [first],
    declared below, so this is only a demonstration.
 *)



(** {2 Element access} *)

val name : element node -> string
(** Given an element node, evaluates to its tag name. For example, an element
    [<a id="foo"></a>] has tag name [a]. All tag names are converted to
    lowercase. *)

val attribute : string -> element node -> string option
(** [attribute attr element] evaluates to [Some v] if [element] has attribute
    [attr] and it is set to value [v], and [None] if [element] does not have
    [attr]. In simpler words, [attribute] gets [attr] from [element]. *)

val classes : element node -> string list
(** Evaluates to the class list of the given element. *)

val id : element node -> string option
(** Evaluates to the id of the given element, if it is present. *)

val has_attribute : string -> element node -> bool
(** [has_attribute attr element] evaluates to [true] if and only if [element]
    has [attr]. *)

val fold_attributes : ('a -> string -> string -> 'a) -> 'a -> element node -> 'a
(** [fold_attributes f init element] applies [f] successively to the names and
    values of the attributes of [element]. The first [string] argument to [f] is
    the attribute name, and the second is the value. *)

val element : (_ node) -> element node option
(** Given a general node, asserts that it is an element [e]. If so, evaluates to
    [Some e]. Otherwise, evaluates to [None]. In simpler words, performs a
    checked coercion of a general node up to an element. *)

val elements : (_ nodes) -> element nodes
(** Given a sequence of nodes, transforms it into a sequence containing only the
    element nodes, i.e. filters out non-elements. *)

val is_element : (_ node) -> bool
(** Evalautes to [true] if and only if the given node is an element. *)



(** {2 Content access} *)

val leaf_text : (_ node) -> string option
(** [leaf_text n] is defined recursively as follows:

    - If [n] is a text node with value [s], [leaf_text n] evaluates to [Some s].
    - If [n] is an element or soup node, then, [leaf_text n] filters out all
      text children of [n] containing only whitespace. If there is only one
      child [n'] remaining, it evaluates to [leaf_text n']. If there are no
      children remaining, it evaluates to [Some ""]. If there are two or more
      children remaining, it evaluates to [None].

    Less formally, [leaf_text n] "drills down" to a single leaf node contained
    under [n], and extracts its text, if any. While doing this, it ignores
    whitespace. If the choice of leaf is ambiguous, the result is [None].
    
    Here are some examples of what [leaf_text] produces ([=>]) for various
    nodes:

{[
some text                                =>   Some "some text"
<p>some text</p>                         =>   Some "some text"
<div><p>some text</p></div>              =>   Some "some text"
<div> <p>some text</p></div>             =>   Some "some text"
<div><p>some text</p><p>more</p></div>   =>   None
<div></div>                              =>   Some ""
]}

 *)

val texts : (_ node) -> string list
(** Given a node [n], evaluates to the content of all text nodes that are
    descendants of [n]. If [n] is itself a text node, returns [n]'s content. *)

val trimmed_texts : (_ node) -> string list
(** Same as [texts], but all strings are passed through [String.trim], and then
    all empty strings are filtered out. *)



(** {2 Elementary traversals} *)

val children : (_ node) -> general nodes
(** The sequence of all children of a node [n], including non-element children.
    To get child elements, use [children |> elements] or [$$ "> *"]. If [n] is
    not itself an element or the document, it cannot have children, so the
    traversal is empty. *)

val descendants : (_ node) -> general nodes
(** Sequence of all descendants of a node [n]. [n] is not considered its own
    descendant. To get only the elements, use [descendants |> elements] or
    [$$ "*"]. As with [children], if [n] is not an element or the document, it
    cannot have descendants, so the traversal is empty. *)

val ancestors : (_ node) -> element nodes
(** Sequence of ancestors of a node [n]. [n] is not considered its own
    ancestor. The document node is not included in the traversal. Ancestors are
    ordered by proximity to [n], i.e. the sequence goes up the DOM tree to a
    root element. *)

val next_siblings : (_ node) -> general nodes
(** Sequence of siblings of a node [n] that follow [n] in its parent's child
    list. *)

val previous_siblings : (_ node) -> general nodes
(** Sequence of siblings of a node [n] that precede [n] in its parent's child
    list. The sequence is ordered according to proximity to [n], i.e. the
    nearest node to [n] is first. This is the opposite order of these nodes in
    the parent's child list. *)



(** {2 Combinators} *)

val fold : ('a -> 'b node -> 'a) -> 'a -> 'b nodes -> 'a
(** [fold f init t] folds [f] over the nodes of [t], i.e. if [t] is
    [n, n', n'', ...], evaluates [f (f (f init n) n') n'' ...]. *)

val filter : ('a node -> bool) -> 'a nodes -> 'a nodes
(** [filter f t] is the sequence consisting of the nodes [n] of [t] for which
    [f n] evaluates to [true]. *)

val map : ('a node -> 'b node) -> 'a nodes -> 'b nodes
(** [map f t] is the sequence consisting of nodes [f n] for each node [n] of
    [t]. *)

val filter_map : ('a node -> 'b node option) -> 'a nodes -> 'b nodes
(** [filter_map f t] is the sequence consisting of nodes [n'] for each node [n]
    of [t] for which [f n] evaluates to [Some n']. Nodes [n] for which [f n]
    evaluates to [None] are dropped. *)

val flatten : ('a node -> 'b nodes) -> 'a nodes -> 'b nodes
(** [flatten f t] is the sequence consisting of the concatenation of all the
    sequences [f n] for each [n] in [t]. *)

val iter : ('a node -> unit) -> 'a nodes -> unit
(** [iter f t] applies [f] to each node in [t]. *)

val to_list : 'a nodes -> 'a node list
(** Converts the given node sequence to a list. *)



(** {2 Projection} *)

val nth : int -> 'a nodes -> 'a node option
(** [nth n t] evaluates to the [n]th element of [t], if it is present. Note that
    the index is 1-based. This is for consistency with the CSS [:nth-child]
    selectors. *)

val first : 'a nodes -> 'a node option
(** Evaluates to the first node of the given sequence, if the sequence has at
    least one node. *)

val last : 'a nodes -> 'a node option
(** Evaluates the entire given sequence and returns the last node, if one was
    encountered. *)

val count : 'a nodes -> int
(** Evaluates to the number of nodes in the given sequence. *)

val index_of : (_ node) -> int
(** Evaluates to the index of the given node in its parent's child list. If the
    node has no parent, the index is 1. Note that indices are 1-based, according
    to CSS convention. *)

val index_of_element : element node -> int
(** Evaluates to the element index of the given element in the parent's child
    list. That is, the index of the given element when the parent's non-element
    children are disregarded. The index is 1-based, in line with CSS
    convention. *)



(** {2 Convenience} *)

val tags : string -> (_ node) -> element nodes
(** Evaluates to all descendant elements of the given node that have the given
    tag name. For example, the following is a sequence of all [a] elements under
    [some_root_node]:

{[
some_root_node |> tags "a"
]}

    It is equivalent to

{[
some_root_node
|> descendants |> elements |> filter (fun e -> name e = "a")
]}

    and

{[
some_root_node $$ "a"
]}

    Note that tag names are case-insensitive.
 *)

val tag : string -> (_ node) -> element node option
(** Like [tags], but evaluates to only the first element, if there is one. So,
    the following selects the first [a] element under [some_root_node]:

{[
some_root_node |> tag "a"
]}

    If you expect the element to be there, you can do

{[
some_root_node |> R.tag "a"
]}

    These are equivalent to [some_root_node $? "a"] and [some_root_node $ "a"],
    respectively.
 *)

val parent : (_ node) -> element node option
(** Given a node, evaluates to its parent element, if it has one. Note that root
    nodes do not have a parent {e element}, as their parent is the {e document}
    node (a.k.a. the soup). [parent] therefore evaluates to [None] for root
    nodes. [parent n] is equivalent to [n |> ancestors |> first]. *)

val child : (_ node) -> general node option
(** [child n] evaluates to [n]'s first child, if [n] has one. Equivalent to
    [n |> children |> first]. *)

val child_element : (_ node) -> element node option
(** [child_element n], evaluates to [n]'s first child element, if [n] has one.
    Equivalent to [n |> children |> elements |> first]. *)

val next_sibling : (_ node) -> general node option
(** [next_sibling n] is the next sibling of [n] in [n]'s parent's child list, if
    there is such a sibling. It is equivalent to
    [n |> next_siblings |> first]. *)

val previous_sibling : (_ node) -> general node option
(** Like [next_sibling], but for the preceding sibling instead. *)

val next_element : (_ node) -> element node option
(** [next_element n] is the next sibling of [n] that is an element. It is
    equivalent to [n |> next_siblings |> elements |> first]. *)

val previous_element : (_ node) -> element node option
(** Like [next_element], but for the preceding siblings instead. *)

val no_children : (_ node) -> bool
(** Evaluates to [true] if and only if the given node has no child nodes. *)

val at_most_one_child : (_ node) -> bool
(** Evaluates to [true] if and only if the given node has at most one child
    node. *)

val is_root : (_ node) -> bool
(** Evaluates to [true] if and only if the given node is not a soup (document)
    node, and either has no parent, or its parent is a soup node. In other
    words, determines whether the node is a top-level non-document node. *)



(** {2 Printing} *)

val to_string : (_ node) -> string
(** Converts the node tree rooted at the given node to an HTML5 string,
    preserving whitespace nodes and not minding human readability
    considerations. *)

val pretty_print : (_ node) -> string
(** Converts the node tree rooted at the given node to a string formatted for
    easy reading. Note that this can change the whitespace structure of the
    HTML, so pretty-printed HTML may display differently in a browser than the
    original parsed document. Pretty-printing is meant for inspection,
    debugging, content diffs, etc., not browser viewing. *)



(** {2 Parsing signals}

    Lambda Soup uses {{: https://github.com/aantron/markup.ml} Markup.ml}
    internally to parse and write markup. If you wish to:

    - avoid intermediate strings when reading or writing,
    - control how parsing is done, or
    - run the input or output of Lambda Soup through a filter without having to
      re-parse it,

    then you should use the functions below instead of [parse] and [to_string].

    See the {{: http://aantron.github.io/markup.ml/} Markup.ml documentation}
    for the types involved. The
    {{: https://github.com/aantron/markup.ml#overview-and-basic-usage} overview}
    may be a good place to start.
 *)

val signals : (_ node) -> (Markup.signal, Markup.sync) Markup.stream
(** Converts the node tree rooted at the given node to a stream of Markup.ml
    signals. This underlies [to_string] and [pretty_print].

    You can use this function together with [Markup.write_xml] to output XML
    instead of HTML. *)

val from_signals : (Markup.signal, Markup.sync) Markup.stream -> soup node
(** Converts a stream of Markup.ml signals to a node tree. This underlies
    [parse].

    You can use this function together with [Markup.parse_xml] to load XML into
    Lambda Soup.

    At the moment, namespaces are ignored. *)



(** {2 Equality} *)

val equal : (_ node) -> (_ node) -> bool
(** [equal n n'] recursively tests the node trees rooted at [n] and [n'] for
    equality. To test [true], the trees must be identical, including whitespace
    text nodes. Class attributes and other multi-valued attributes are compared
    literally: classes must be listed in the same order, with the same amount of
    whitespace in the attribute value. During comparison, adjacent text nodes
    are merged, and empty text nodes are ignored. This is the standard
    normalization procedure. *)

val equal_modulo_whitespace : (_ node) -> (_ node) -> bool
(** [equal_modulo_whitespace n n'] is like [equal n n'], but all text nodes have
    their values passed through [String.trim]. Nodes that become empty are then
    ignored for the purpose of comparison, as in [equal]. This is analogous to
    the operation of [trimmed_texts]. *)



(** {2 Mutation} *)

val create_element :
    ?id:string ->
    ?class_:string ->
    ?classes:string list ->
    ?attributes:(string * string) list ->
    ?inner_text:string ->
    string ->
        element node
(** [create_element tag] creates a new element with the name [tag].

    If [~attributes] is specified, the given attributes are added to the
    element. [~attributes] defaults to [[]].

    If [~classes] is specified, the class names are concatenated into a single
    string [s] and the [class] attribute is set on the element to the resulting
    value. This takes precedence over [~attributes].

    If [~class] is specified, the class is set on the element. This takes
    precedence over both [~attributes] and [~classes].

    If [~id] is specified, the id is set. This takes precedence over
    [~attributes].

    If [~inner_text] is specified, a text node is created with the given string,
    and made the single child of the new element. *)

val create_text : string -> general node
(** Creates a new text node with the given content. *)

val create_soup : unit -> soup node
(** Creates a new document node. *)

val append_child : element node -> (_ node) -> unit
(** [append_child element n] adds [n] to the end of the child list of
    [element]. *)

val prepend_child : element node -> (_ node) -> unit
(** [prepend_child element n] adds [n] to the beginning of the child list of
    [element]. *)

val insert_at_index : int -> element node -> (_ node) -> unit
(** [insert_at_index k element n] makes [n] the [k]th child of [element]. Note
    that the index is 1-based. Pre-existing children with indices formerly [k]
    or higher are moved after [n]. If [k] is outside the range of valid indices,
    [n] is inserted at the beginning or end of the list, respectively if [k] is
    less than one or greater than the number of pre-existing children plus
    one. *)

val insert_before : (_ node) -> (_ node) -> unit
(** [insert_before n n'] inserts [n'] immediately before [n] in [n]'s parent's
    child list. *)

val insert_after : (_ node) -> (_ node) -> unit
(** [insert_after n n'] inserts [n'] immediately after [n] in [n]'s parent's
    child list. *)

val delete : (_ node) -> unit
(** Deletes the given node by unlinking it from its parent. If the node has
    children, they are implicitly deleted by this operation as well, in the
    sense that they become unreachable from the document. *)

val clear : (_ node) -> unit
(** Deletes all children of the given node. *)

val replace : (_ node) -> (_ node) -> unit
(** [replace n n'] replaces [n] with [n'] by modifying [n]'s parent's child
    list. All children of [n] are implicitly deleted by this operation because
    they are unlinked. *)

val swap : element node -> element node -> unit
(** [swap element element'] replaces [element] with [element']. All children of
    [element] are transferred to [element'], and all original children of
    [element'] are transferred to [element]. *)

val wrap : (_ node) -> element node -> unit
(** [wrap n element] inserts [element] in the place of [n], and then makes [n]
    [element]'s child. All original children of [element] are unlinked. *)

val unwrap : (_ node) -> unit
(** [unwrap n] unlinks [n], and inserts all of [n]'s children as children of
    [n]'s parent. *)

val append_root : soup node -> (_ node) -> unit
(** [append_root soup n] adds [n] as the last root node of [soup]. *)

val set_name : string -> element node -> unit
(** Sets the tag name of the given element to the given value. *)

val set_attribute : string -> string -> element node -> unit
(** [set_attribute attr v element] sets the value of attribute [attr] on
    [element] to [v]. If the attribute is not present, it is added to [element].
    If it is already present, the value is replaced. *)

val delete_attribute : string -> element node -> unit
(** Removes the given attribute from the given element. If the attribute is not
    present, has no effect. *)

val add_class : string -> element node -> unit
(** [add_class c element] adds class [c] to [element], if [element] does not
    already have class [c]. *)

val remove_class : string -> element node -> unit
(** [remove_class c element] removes class [c] from [element], if [element] has
    class [c]. *)



(** {2 Option convenience module} *)

(** For each function [f] in Lambda Soup whose result type is an option,
    provides a function [R.f] such that [R.f ...] = [f ... |> require], that is,
    a version of [f] that can raise [Failure] instead of returning an option.
    See the corresponding functions in the main interface for documentation. *)
module R :
sig
  val select_one : string -> (_ node) -> element node
  val attribute : string -> element node -> string
  val id : element node -> string
  val element : (_ node) -> element node
  val leaf_text : (_ node) -> string
  val nth : int -> 'a nodes -> 'a node
  val first : 'a nodes -> 'a node
  val last : 'a nodes -> 'a node
  val tag : string -> (_ node) -> element node
  val parent : (_ node) -> element node
  val child : (_ node) -> general node
  val child_element : (_ node) -> element node
  val next_sibling : (_ node) -> general node
  val previous_sibling : (_ node) -> general node
  val next_element : (_ node) -> element node
  val previous_element : (_ node) -> element node
end



(** {2 I/O}

    Lambda Soup is not an I/O library. However, it provides a few simple helpers
    based on standard I/O functions in
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#6_Inputoutput}
    [Pervasives]}. These should not be used for "serious" code. They are only
    for when you need to get something done quickly, and/or don't care about
    corner cases or excellent reliability. In such cases, they allow you to
    avoid writing I/O wrappers or using additional libraries.

    Using these, you can write little command-line scrapers and filters:

{[
let () =
  let soup = read_channel stdin |> parse in
  let () = (* ...do things to soup... *) in

  soup $ "div.view-count" |> R.leaf_text |> print_endline
  (* ...or... *)
  soup |> to_string |> write_channel stdout
]}

    If the above is compiled to a file [scrape], you can then run

{[
curl -L "http://location.com" | ./scrape
]}

    to get the view count or transformed HTML, respectively.
 *)

val read_file : string -> string
(** Reads the entire contents of the file with the given path. Raises
    [Sys_error] on failure. *)

val read_channel : in_channel -> string
(** Reads all bytes from the given channel. *)

val write_file : string -> string -> unit
(** [write_file path data] writes [data] to the file given by [path]. If the
    file already exists, it is truncated (erased). If you want to append to
    file, use
    {{: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#VALopen_out_gen}
    [open_out_gen]} with the necessary flags, and pass the resulting channel to
    [write_channel]. Raises [Sys_error] on failure. *)

val write_channel : out_channel -> string -> unit
(** Writes the given data to the given channel. *)
