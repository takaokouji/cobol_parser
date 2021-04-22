# frozen_string_literal: true

class CobolParser::Warning
  class << self
    attr_accessor :warnings

    def warndef(var, name, wall, doc)
      attr_accessor(var)

      add_warnings(
        var: var,
        name: name,
        wall: wall == 1,
        doc: doc
      )
    end

    private

    def add_warnings(hash)
      self.warnings ||= {}
      warnings[hash[:name]] = hash
    end
  end

  warndef(:warn_obsolete, "obsolete", 1, "Warn if obsolete features are used")
  warndef(:warn_archaic, "archaic", 1, "Warn if archaic features are used")
  warndef(:warn_redefinition, "redefinition", 1, "Warn incompatible redefinition of data items")
  warndef(:warn_constant, "constant", 1, "Warn inconsistent constant")
  warndef(:warn_parentheses, "parentheses", 1, "Warn lack of parentheses around AND within OR")
  warndef(:warn_strict_typing, "strict-typing", 1, "Warn type mismatch strictly")
  warndef(:warn_implicit_define, "implicit-define", 1, "Warn implicitly defined data items")
  warndef(:warn_call_params, "call-params", 0, "Warn non 01/77 items for CALL params")
  warndef(:warn_column_overflow, "column-overflow", 0, "Warn text after column 72, FIXED format")
  warndef(:warn_terminator, "terminator", 0, "Warn lack of scope terminator END-XXX")
  warndef(:warn_truncate, "truncate", 0, "Warn possible field truncation")
  warndef(:warn_linkage, "linkage", 0, "Warn dangling LINKAGE items")
  warndef(:warn_unreachable, "unreachable", 0, "Warn unreachable statements")

  def initialize(cb)
    @cb = cb

    self.class.warnings.each_value do |x|
      instance_variable_set("@#{x[:var]}", x[:wall])
    end
  end
end
