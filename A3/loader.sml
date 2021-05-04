CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "ast.sml";
use "typing.sml";
open Typing;
use "parser.yacc.sig";
use "parser.yacc.sml";
use "lexer.lex.sml";
use "interFace.sml";
use "evaluator.sml";
open Evaluator;
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)
