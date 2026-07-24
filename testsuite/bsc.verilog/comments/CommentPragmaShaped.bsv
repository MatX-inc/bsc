// A "doc" attribute is passed through to the generated Verilog as a
// comment.  A synthesis directive is a comment whose first word is a
// pragma introducer, so a doc string shaped like one would be emitted as
// a live directive -- a "translate_off" would silently delete the rest
// of the module from the synthesis view.  Such a string must not reach
// the output in directive position.  Prose that merely mentions a
// directive is not in directive position and is left alone.

(* synthesize *)
(* doc="synopsys translate_off", doc="synthesis translate_off" *)
(* doc="pragma translate_off" *)
(* doc="SYNOPSYS TRANSLATE_OFF" *)
(* doc="   synopsys translate_off" *)
(* doc="this emitter writes translate_off around simulation-only logic" *)
module mkCommentPragmaShaped ();
endmodule
