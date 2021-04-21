# frozen_string_literal: true

# Label
class CobolParser::Tree::Label < CobolParser::Tree
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

  class << self
    def build(cb, name, section)
      p = new(cb, category: :UNKNOWN)
      p.name = cb.define(name, p)
      p.orig_name = p.name
      p.section = section

      p
    end
  end

  def initialize(cb, attributes = {})
    super

    return if @id

    @id = @cb.id
    @cb.id += 1
  end
end
