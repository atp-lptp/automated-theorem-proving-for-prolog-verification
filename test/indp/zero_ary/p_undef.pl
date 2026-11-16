% q is undefined: it fails in the completion (q <=> $false in the .fof file),
% but LPTP has no completion for q, so the .pr refutes p by induction.
p :- q, p.
