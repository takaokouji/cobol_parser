# frozen_string_literal: true

require "racc/parser.rb" # rubocop:disable Style/RedundantFileExtensionInRequire
require "forwardable"
require_relative "scanner"
require_relative "parser.rule"
require_relative "ast_generator"

class CobolParser::Parser < Racc::Parser
  extend Forwardable

  attr_reader :scanner

  def_delegators :@cb,
                 :current_program,
                 :current_program=

  def initialize(context, options = {})
    super()

    @cb = context
    @scanner = CobolParser::Scanner.new(context, options)
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
    self.current_program = @cb.build_program(nil, 0)
    @cb.build_registers
    current_program.flag_main = @cb.flag_main

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
      @cb.error("Multiple PROGRAM-ID's without matching END PROGRAM")
    end
    if @cb.errorcount > 0
      # TODO: YYABORT;
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
end
