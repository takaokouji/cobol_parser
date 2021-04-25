# frozen_string_literal: true

module CobolParser
  # Label
  class Tree::Label < Tree
    attribute :name
    attribute :section
    attribute :exit_label
    attribute :exit_label_ref
    attribute :children
    attribute :orig_name
    attribute :id
    attribute :is_section
    attribute :is_entry
    attribute :need_begin
    attribute :need_return
    attribute :is_global

    module Helper
      def cb_build_label(name, section)
        Tree::Label.new(@context, name, section)
      end
    end

    def initialize(context, name, section)
      super(context, category: :UNKNOWN)

      @id = cb_id
      self.cb_id += 1
      @name = cb_define(name, self)
      @orig_name = name
      @section = section
    end
  end
end
