# frozen_string_literal: true

require "racc/parser.rb" # rubocop:disable Style/RedundantFileExtensionInRequire
require "forwardable"
require_relative "scanner"
require_relative "parser.rule"
require_relative "ast_helper"

class CobolParser::Parser < Racc::Parser
  extend Forwardable
  include CobolParser::AstHelper

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

    to_ast
  end

  def next_token
    token = @tokens.next
    [token.name, token.value]
  rescue StopIteration
    [false, "$end"]
  end

  private

  def to_ast
    s_(:begin,
       s_(:send, nil, :require, s_(:str, "ostruct")),
       s_(:class, s_(:const, nil, to_class_name(current_program.orig_source_name)), nil,
          define_class_body))
  end

  def define_class_body
    class_body = [
      define_initialize,
      *define_public_methods,
      *define_private_methods,
    ].compact
    if class_body.length == 1
      class_body.first
    else
      s_(:begin, *class_body)
    end
  end

  def define_initialize
    names = []
    current_program.working_storage.each_sister do |field|
      names << field.name if field.storage == :WORKING && field.level == 1
    end
    initialize_vars = names.map { |name| initialize_var(name) }

    if initialize_vars.empty?
      s_(:def, :initialize,
         s_(:args))
    elsif initialize_vars.length == 1
      s_(:def, :initialize,
         s_(:args),
         *initialize_vars)
    else
      s_(:def, :initialize,
         s_(:args),
         s_(:begin,
            *initialize_vars))
    end
  end

  def initialize_var(name)
    s_(:ivasgn, to_ivar_name(name),
       s_(:send, nil, to_method_name("new_#{name}")))
  end

  def define_public_methods
    []
  end

  def define_private_methods
    private_methods = []
    private_methods += define_new_var_methods

    return nil if private_methods.empty?

    [s_(:send, nil, :private)] + private_methods
  end

  def define_new_var_methods
    new_var_methods = []
    current_program.working_storage.each_sister do |field|
      new_var_methods << define_new_var_method(field) if field.storage == :WORKING && field.level == 1
    end

    new_var_methods
  end

  def define_new_var_method(field)
    method_name = to_method_name("new_#{field.name}")
    body_ast = define_new_var_method_body(field)

    s_(:def, method_name,
       s_(:args),
       body_ast)
  end

  def define_new_var_method_body(field)
    hash_asts = []
    field.children.each_sister do |f|
      name = to_var_name(f.name)
      iv_ast = if f.children
                 define_new_var_method_body(f)
               else
                 iv = f.initial_value
                 case iv
                 when Integer
                   s_(:int, iv)
                 when String
                   s_(:str, iv)
                 when Float
                   s_(:float, iv)
                 else
                   raise NotImplementedError
                 end
               end
      hash_asts << s_(:pair, s_(:sym, name), iv_ast)
    end

    s_(:send,
       s_(:const, nil, :OpenStruct), :new,
       s_(:hash,
          *hash_asts))
  end

  def to_class_name(name)
    name.sub(/^[^A-Z]/, "C\\1").split(/[-_]/).map(&:capitalize).join.to_sym
  end

  def to_ivar_name(name)
    "@#{to_method_name(name)}".to_sym
  end

  def to_method_name(name)
    # TODO: fix rule
    name.downcase.sub(/^[^a-z_]/, "_\\1").gsub("-", "_").to_sym
  end

  alias_method :to_var_name, :to_method_name
end
