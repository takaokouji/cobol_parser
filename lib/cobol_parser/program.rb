# frozen_string_literal: true

# Program
class CobolParser::Program
  attr_accessor :next_program
  attr_accessor :program_id
  attr_accessor :source_name
  attr_accessor :orig_source_name
  attr_accessor :entry_list
  attr_accessor :file_list
  attr_accessor :exec_list
  attr_accessor :label_list
  attr_accessor :reference_list
  attr_accessor :alphabet_name_list
  attr_accessor :class_name_list
  attr_accessor :parameter_list
  attr_accessor :locale_list
  attr_accessor :symbolic_list
  attr_accessor :global_list
  attr_accessor :cb_return_code
  attr_accessor :cb_sort_return
  attr_accessor :cb_call_params
  attr_accessor :class_spec_list
  attr_accessor :interface_spec_list
  attr_accessor :function_spec_list
  attr_accessor :program_spec_list
  attr_accessor :property_spec_list
  attr_accessor :working_storage
  attr_accessor :local_storage
  attr_accessor :linkage_storage
  attr_accessor :screen_storage
  attr_accessor :local_file_list
  attr_accessor :global_file_list
  attr_accessor :global_handler
  attr_accessor :collating_sequence
  attr_accessor :cursor_pos
  attr_accessor :crt_status
  attr_accessor :returning # RETURNING
  attr_accessor :word_table

  # internal variables
  attr_accessor :loop_counter
  attr_accessor :decimal_index
  attr_accessor :decimal_index_max
  attr_accessor :decimal_point # '.' or ','
  attr_accessor :currency_symbol # '$' or user-specified
  attr_accessor :numeric_separator # ',' or '.'
  attr_accessor :nested_level # Nested program level
  attr_accessor :flag_main # Gen main function
  attr_accessor :flag_common # COMMON PROGRAM
  attr_accessor :flag_initial # INITIAL PROGRAM
  attr_accessor :flag_recursive # RECURSIVE PROGRAM
  attr_accessor :flag_screen # have SCREEN SECTION
  attr_accessor :flag_validated # End program validate
  attr_accessor :flag_chained # PROCEDURE CHAINING
  attr_accessor :flag_global_use # USE GLOBAL
  attr_accessor :gen_decset # Gen decimal_set_int
  attr_accessor :gen_udecset # Gen decimal_set_uint
  attr_accessor :gen_ptrmanip # Gen cob_pointer_manip
  attr_accessor :gen_file_error # Gen error routine
  attr_accessor :prog_type # Program type

  module Helper
    def build_program(last_program, nest_level)
      # TODO: cb_reset_78 (); - scanner.lev78.clear
      # TODO: cb_reset_in_procedure (); - scanner.in_procedure = false
      @cb.clear_real_field
      CobolParser::Program.new(self, last_program, nest_level)
    end
  end

  def initialize(context, last_program, nest_level)
    @cb = context

    @global_handler = Array.new(5)
    @word_table = {}

    @next_program = last_program
    @nested_level = nest_level
    @decimal_point = "."
    @currency_symbol = "$"
    @numeric_separator = ","
    if nest_level > 0
      @global_file_list = last_program.global_file_list
      @collating_sequence = last_program.collating_sequence
      @function_spec_list = last_program.function_spec_list
      @class_spec_list = last_program.class_spec_list
      @interface_spec_list = last_program.interface_spec_list
      @program_spec_list = last_program.program_spec_list
      @property_spec_list = last_program.property_spec_list
      @alphabet_name_list = last_program.alphabet_name_list
      @class_name_list = last_program.class_name_list
      @locale_list = last_program.locale_list
      @symbolic_list = last_program.symbolic_list
      @decimal_point = last_program.decimal_point
      @numeric_separator = last_program.numeric_separator
      @currency_symbol = last_program.currency_symbol
      @cb_return_code = last_program.cb_return_code
    else
      @cb.functions_are_all = @cb.flag_functions_all
    end
  end
end
