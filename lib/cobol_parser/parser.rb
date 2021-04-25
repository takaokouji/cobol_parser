# frozen_string_literal: true

require "racc/parser.rb" # rubocop:disable Style/RedundantFileExtensionInRequire
require_relative "parser.rule"
require_relative "context"
require_relative "tree"
require_relative "tree_helper"
require_relative "scanner"
require_relative "ast_generator"

module CobolParser
  class Parser < Racc::Parser
    include Context::Helper
    include TreeHelper

    attr_accessor :perform_stack
    attr_accessor :current_statement
    attr_accessor :next_label_id
    attr_accessor :current_linage
    attr_accessor :current_storage
    attr_accessor :eval_level
    attr_accessor :eval_inc
    attr_accessor :eval_inc2
    attr_accessor :prog_end
    attr_accessor :depth
    attr_accessor :inspect_keyword
    attr_accessor :check_unreached
    attr_accessor :samearea
    attr_accessor :stack_progid
    attr_accessor :eval_check
    attr_accessor :term_array
    attr_accessor :linage_file
    attr_accessor :next_label_list

    def initialize(context, options = {})
      super()

      @context = context
      @context.parser = self

      Scanner.new(context, options)

      init_constants
      init_reserved
    end

    def parse(path_or_io)
      @perform_stack = nil
      @current_statement = nil
      @next_label_id = 0
      @current_linage = 0
      @current_storage = nil
      @eval_level = 0
      @eval_inc = 0
      @eval_inc2 = 0
      @prog_end = false
      @depth = 0
      @inspect_keyword = 0
      @check_unreached = false
      @samearea = 1
      @stack_progid = []
      @eval_check = []
      @term_array = []
      @linage_file = nil
      @next_label_list = nil
      self.current_program = cb_build_program(nil, 0)
      cb_build_registers
      current_program.flag_main = cb_flag_main

      s = if path_or_io.respond_to?(:read)
            path_or_io.read
          else
            File.read(path_or_io)
          end
      @tokens = scanner.lex(s)

      do_parse

      if !current_program.flag_validated
        current_program.flag_validated = true
        # TODO: cb_validate_program_body (current_program);
      end
      if @depth > 1
        cb_error("Multiple PROGRAM-ID's without matching END PROGRAM")
      end
      if cb_errorcount > 0
        # TODO: YYABORT;
        return
      end

      # TODO: below
      # if !current_program.entry_list {
      #   emit_entry(current_program->program_id, 0, NULL);
      # }

      CobolParser::AstGenerator.generate(@cb)
    end

    def next_token
      token = @tokens.next
      [token.name, token.value]
    rescue StopIteration
      [false, "$end"]
    end

    private

    def emit_statement(x)
      current_program.exec_list = cb_cons(x, current_program.exec_list)
    end
  end
end
