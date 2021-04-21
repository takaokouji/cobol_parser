# frozen_string_literal: true

require "forwardable"
require_relative "word"

class CobolParser::Tree
  extend Forwardable

  # enum cb_category
  CATEGORY = %i[
    UNKNOWN
    ALPHABETIC
    ALPHANUMERIC
    ALPHANUMERIC_EDITED
    BOOLEAN
    INDEX
    NATIONAL
    NATIONAL_EDITED
    NUMERIC
    NUMERIC_EDITED
    OBJECT_REFERENCE
    DATA_POINTER
    PROGRAM_POINTER
  ].freeze

  CATEGORY_TO_CLASS_TABLE = {
    UNKNOWN: :UNKNOWN,
    ALPHABETIC: :ALPHABETIC,
    ALPHANUMERIC: :ALPHANUMERIC,
    ALPHANUMERIC_EDITED: :ALPHANUMERIC,
    BOOLEAN: :BOOLEAN,
    INDEX: :INDEX,
    NATIONAL: :NATIONAL,
    NATIONAL_EDITED: :NATIONAL,
    NUMERIC: :NUMERIC,
    NUMERIC_EDITED: :ALPHANUMERIC,
    OBJECT_REFERENCE: :OBJECT,
    DATA_POINTER: :POINTER,
    PROGRAM_POINTER: :POINTER,
  }.freeze

  class << self
    def inherited(subclass)
      super
      subclass.attributes.merge(attributes)
    end

    def attributes
      @attributes ||= Set.new
    end

    def attribute(*names)
      names.each do |name|
        name = name.to_sym
        attr_accessor(name)

        attributes << name
      end
    end

    def write_attribute(*names)
      names.each do |name|
        name = name.to_sym
        attr_writer(name)

        attributes << name
      end
    end
  end

  write_attribute :category
  attribute :source_file
  attribute :source_line

  attr_reader :cb

  def_delegators :@cb,
                 :current_program

  def initialize(cb, attributes = {})
    @cb = cb

    if attributes.key?(:category) && !CATEGORY.include?(attributes[:category])
      raise ArgumentError, "Invalid Tree category: #{attributes[:category].inspect}"
    end

    attributes.each do |name, value|
      instance_variable_set("@#{name}", value)
    end
  end

  def category
    return nil if self == @cb.error_node
    return @category if @category != :UNKNOWN

    case self
    when CobolParser::Tree::Cast
      # TODO: tree.c:473
      raise NotImplementedError
    when CobolParser::Tree::Reference
      @category = if offset
                    :ALPHANUMERIC
                  else
                    value.category
                  end
    when CobolParser::Tree::Field
      @category = if children
                    :ALPHANUMERIC
                  elsif usage == :POINTER && level != 88
                    :DATA_POINTER
                  elsif usage == :PROGRAM_POINTER && level != 88
                    :PROGRAM_POINTER
                  else
                    case level
                    when 66
                      if rename_thru
                        :ALPHANUMERIC
                      else
                        redefines.category
                      end
                    when 88
                      :BOOLEAN
                    else
                      pic.category
                    end
                  end
    when CobolParser::Tree::AlphabetName, CobolParser::Tree::LocaleName
      @category = :ALPHANUMERIC
    when CobolParser::Tree::BinaryOp
      @category = :BOOLEAN
    else
      $stderr.printf("Unknown tree %s Category %s\n", self.class.to_s, @category.to_s)
      abort
    end

    @category
  end

  def tree_class
    CATEGORY_TO_CLASS_TABLE[category]
  end

  def inspect
    attrs = self.class.attributes.map { |x|
      v = instance_variable_get("@#{x}")
      if v
        "@#{x}: #{v.inspect}"
      end
    }.compact
    "#<#{self.class.name}:#{object_id} #{attrs.join(", ")}>"
  end

  alias_method :to_s, :inspect
end

require_relative "tree/const"
require_relative "tree/integer"
require_relative "tree/literal"
require_relative "tree/reference"
require_relative "tree/picture"
require_relative "tree/field"
require_relative "tree/label"
require_relative "tree/cast"
require_relative "tree/alphabet_name"
require_relative "tree/locale_name"
require_relative "tree/list"
require_relative "tree/binary_op"
