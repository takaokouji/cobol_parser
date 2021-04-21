# frozen_string_literal: true

require_relative "program"

module CobolParser::ProgramHelper
  def build_program(last_program, nest_level)
    # TODO: cb_reset_78 (); - scanner.lev78.clear
    # TODO: cb_reset_in_procedure (); - scanner.in_procedure = false
    # TODO: cb_clear_real_field ();
    CobolParser::Program.new(self, last_program, nest_level)
  end
end
