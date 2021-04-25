# frozen_string_literal: true

require_relative "word"
require_relative "context"
require_relative "error_helper"
require_relative "gettext_helper"

module CobolParser
  class Tree
    include Context::Helper
    include ErrorHelper
    include GettextHelper

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
    end

    attribute :category
    attribute :source_file
    attribute :source_line

    def initialize(context, attributes = {})
      @context = context

      if attributes.key?(:category) && !CATEGORY.include?(attributes[:category])
        raise ArgumentError, "Invalid Tree category: #{attributes[:category].inspect}"
      end

      attributes.each do |name, value|
        instance_variable_set("@#{name}", value)
      end
    end

    def cb_tree_category
      return nil if self == cb_error_node
      return @category if @category != :UNKNOWN

      case self
      when Cast
        # TODO: tree.c:473
        raise NotImplementedError
      when Reference
        @category = if offset
                      :ALPHANUMERIC
                    else
                      value.cb_tree_category
                    end
      when Field
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
                          redefines.cb_tree_category
                        end
                      when 88
                        :BOOLEAN
                      else
                        pic.category
                      end
                    end
      when AlphabetName, LocaleName
        @category = :ALPHANUMERIC
      when BinaryOp
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

    def level_from_name
      if /^\d+$/ =~ name
        l = name.to_i
        return l if [66, 77, 78, 88].include?(l) || (1..49).include?(l)
      end

      cb_error_x(self, "Invalid level number '%s'", name)
      0
    end

    def inspect
      attrs = self.class.attributes.map { |x|
        v = instance_variable_get("@#{x}")
        if v
          if v.is_a?(Tree)
            # TODO: inspect more information for debug
            "@#{x}: #<#{v.class.name}:#{v.object_id} @category: #{v.instance_variable_get("@category").inspect}>"
          else
            "@#{x}: #{v.inspect}"
          end
        end
      }.compact
      "#<#{self.class.name}:#{object_id} #{attrs.join(", ")}>"
    end

    alias_method :to_s, :inspect
  end
end

require_relative "tree/alphabet_name"
require_relative "tree/binary_op"
require_relative "tree/cast"
require_relative "tree/const"
require_relative "tree/field"
require_relative "tree/file"
require_relative "tree/integer"
require_relative "tree/label"
require_relative "tree/list"
require_relative "tree/literal"
require_relative "tree/locale_name"
require_relative "tree/perform"
require_relative "tree/perform_varying"
require_relative "tree/picture"
require_relative "tree/program"
require_relative "tree/reference"
require_relative "tree/system_name"
