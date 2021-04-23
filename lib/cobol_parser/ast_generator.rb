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
    reset

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

  def reset
    @cb = nil
    @program = nil
    @new_var_methods = {}
  end

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
    initialize_vars = program.working_storage.each_sister.select { |f|
      f.storage == :WORKING && f.level == 1
    }.map { |f|
      initialize_var(f)
    }

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

  def initialize_var(field)
    ivar_name = make_ivar_name(field)
    if field.pic
      s(:ivasgn, ivar_name, literal(field.initial_value))
    else
      @new_var_methods[field] = define_new_var_method(field)

      s(:ivasgn, ivar_name,
        s(:send, nil, make_method_name("new_#{field.name}")))
    end
  end

  def define_public_methods
    []
  end

  def define_private_methods
    private_methods = []
    private_methods += @new_var_methods.values if @new_var_methods.length > 0

    return nil if private_methods.empty?

    [s(:send, nil, :private)] + private_methods
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
      kwargs_asts << s(:pair, s(:sym, :redefines), s(:sym, make_var_name(field.redefines)))
    end

    field.children.each_sister do |f|
      name = make_var_name(f)
      iv_ast = if f.children
                 define_new_var_method_body(f)
               else
                 literal(f.initial_value)
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

  def literal(value)
    case value
    when Integer
      s(:int, value)
    when String
      s(:str, value)
    when Float
      s(:float, value)
    else
      raise NotImplementedError
    end
  end

  def make_class_name(name)
    # TODO: fix rule
    name.sub(/^[^A-Z]/, "C\\1").split(/[-_]/).map(&:capitalize).join.to_sym
  end

  def make_ivar_name(field)
    "@#{make_var_name(field)}".to_sym
  end

  def make_method_name(name)
    # TODO: fix rule
    name.downcase.sub(/^[^a-z_]/, "_\\1").gsub("-", "_").to_sym
  end

  def make_var_name(field)
    if field.filler?
      (field.name.sub(/^WORK\$/, "_filler_") + "_").to_sym
    else
      make_method_name(field.name)
    end
  end
end
