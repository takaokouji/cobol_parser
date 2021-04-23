# frozen_string_literal: true

require "parser/current"

class CobolParser::AstGenerator
  attr_reader :cb
  attr_reader :program

  class << self
    def generate(cb)
      new.generate(cb, cb.current_program)
    end

    def s(type, *children)
      Parser::AST::Node.new(type, children)
    end
  end

  def generate(cb, program)
    @cb = cb
    @program = program

    s(:begin,
      s(:send, nil, :require, s(:str, "ostruct")),
      s(:class, s(:const, nil, make_class_name(program.orig_source_name)), nil,
        define_class_body))
  end

  def s(type, *children)
    self.class.s(type, *children)
  end

  private

  def define_class_body
    class_body = [
      define_initialize,
      *define_public_methods,
      *define_private_methods,
    ].compact
    if class_body.empty?
      nil
    elsif class_body.length == 1
      class_body.first
    else
      s(:begin, *class_body)
    end
  end

  def define_initialize
    names = []
    program.working_storage.each_sister do |field|
      names << field.name if field.storage == :WORKING && field.level == 1
    end
    initialize_vars = names.map { |name| initialize_var(name) }

    if initialize_vars.empty?
      nil
    elsif initialize_vars.length == 1
      s(:def, :initialize,
        s(:args),
        initialize_vars.first)
    else
      s(:def, :initialize,
        s(:args),
        s(:begin,
          *initialize_vars))
    end
  end

  def initialize_var(name)
    s(:ivasgn, make_ivar_name(name),
      s(:send, nil, make_method_name("new_#{name}")))
  end

  def define_public_methods
    []
  end

  def define_private_methods
    private_methods = []
    private_methods += define_new_var_methods

    return nil if private_methods.empty?

    [s(:send, nil, :private)] + private_methods
  end

  def define_new_var_methods
    new_var_methods = []
    program.working_storage.each_sister do |field|
      new_var_methods << define_new_var_method(field) if field.storage == :WORKING && field.level == 1
    end

    new_var_methods
  end

  def define_new_var_method(field)
    method_name = make_method_name("new_#{field.name}")
    body_ast = define_new_var_method_body(field)

    s(:def, method_name,
      s(:args),
      body_ast)
  end

  def define_new_var_method_body(field)
    kwargs_asts = []

    if field.redefines
      kwargs_asts << s(:pair, s(:sym, :redefines), s(:sym, make_var_name(field.redefines.name)))
    end

    field.children.each_sister do |f|
      name = make_var_name(f.name)
      iv_ast = if f.children
                 define_new_var_method_body(f)
               else
                 iv = f.initial_value
                 case iv
                 when Integer
                   s(:int, iv)
                 when String
                   s(:str, iv)
                 when Float
                   s(:float, iv)
                 else
                   raise NotImplementedError
                 end
               end

      if f.flag_occurs
        iv_ast = s(:block,
                   s(:send, s(:const, nil, :Array), :new, s(:int, f.occurs_max)),
                   s(:args),
                   iv_ast)
      end
      kwargs_asts << s(:pair, s(:sym, name), iv_ast)
    end

    s(:send,
      s(:const, nil, :OpenStruct), :new,
      s(:kwargs,
        *kwargs_asts))
  end

  def make_class_name(name)
    # TODO: fix rule
    name.sub(/^[^A-Z]/, "C\\1").split(/[-_]/).map(&:capitalize).join.to_sym
  end

  def make_ivar_name(name)
    "@#{make_method_name(name)}".to_sym
  end

  def make_method_name(name)
    # TODO: fix rule
    name.downcase.sub(/^[^a-z_]/, "_\\1").gsub("-", "_").to_sym
  end

  alias_method :make_var_name, :make_method_name
end
