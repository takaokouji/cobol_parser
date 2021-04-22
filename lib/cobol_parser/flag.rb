# frozen_string_literal: true

class CobolParser::Flag
  class << self
    attr_accessor :flags

    def flag(var, name, doc)
      attr_accessor(var)

      add_flags(
        var: var,
        name: name,
        doc: doc
      )
    end

    private

    def add_flags(hash)
      self.flags ||= {}
      flags[hash[:name]] = hash
    end
  end

  flag(:flag_trace, "trace", "generate trace code (executed section/paragraph)")
  flag(:flag_traceall, "traceall", "generate trace code (executed section/paragraph/statements)")
  flag(:flag_syntax_only, "syntax-only", "syntax error checking only; don't emit any output")
  flag(:flag_static_call, "static-call", "output static function calls for the call statement")
  flag(:flag_debugging_line, "debugging-line", "enable debugging lines ('d' in indicator column)")
  flag(:flag_source_location, "source-location", "generate source location code (turned on by -debug or -g)")
  flag(:flag_implicit_init, "implicit-init", "do automatic initialization of the cobol runtime system")
  flag(:flag_sign_ascii, "sign-ascii", "numeric display sign ascii (default on ascii machines)")
  flag(:flag_sign_ebcdic, "sign-ebcdic", "numeric display sign ebcdic (default on ebcdic machines)")
  flag(:flag_stack_check, "stack-check", "perform stack checking (turned on by -debug or -g)")
  flag(:flag_fold_copy_lower, "fold-copy-lower", "fold copy subject to lower case (default no transformation)")
  flag(:flag_fold_copy_upper, "fold-copy-upper", "fold copy subject to upper case (default no transformation)")
  flag(:flag_notrunc, "notrunc", "do not truncate binary fields according to picture")
  flag(:flag_functions_all, "functions-all", "allow use of intrinsic functions without function keyword")
  flag(:flag_mfcomment, "mfcomment", "'*' or '/' in column 1 treated as comment (fixed only)")
  flag(:flag_null_param, "null-param", "pass extra null terminating pointers on call statements")

  def initialize(cb)
    @cb = cb
  end
end
