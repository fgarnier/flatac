%%% main.mp -- do not edit, generated automatically by main.tex
verbatimtex
\documentclass [11pt]{article}
\begin {document}
etex;
input metauml;
beginfig(1);
LaTeX_unitlength := 1.0pt;
w := 20*LaTeX_unitlength;
h :=  20*LaTeX_unitlength;
Class.A("A")()();
Class.B("B")()();
B.e = A.w + (-20, 0);
drawObjects(A, B);
link(inheritance)(B.e -- A.w);
endfig;
verbatimtex
\end{document}
etex;
%%% the end.
end.
endinput;
