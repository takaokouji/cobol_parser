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
  end

  attribute :category
  attribute :source_file
  attribute :source_line

  attr_reader :cb

  def_delegators :@cb,
                 :current_program

  def initialize(context, attributes = {})
    @cb = context

    if attributes.key?(:category) && !CATEGORY.include?(attributes[:category])
      raise ArgumentError, "Invalid Tree category: #{attributes[:category].inspect}"
    end

    attributes.each do |name, value|
      instance_variable_set("@#{name}", value)
    end
  end

  def inspect
    attrs = self.class.attributes.map { |x| "#{x}: #{send(x).inspect}" }
    "#<#{self.class.name}:#{object_id} #{attrs.join(", ")}>"
  end

  alias_method :to_s, :inspect
end

require_relative "tree/const"
require_relative "tree/literal"
require_relative "tree/reference"
require_relative "tree/picture"
