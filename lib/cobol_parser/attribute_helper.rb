# frozen_string_literal: true

module CobolParser::AttributeHelper
  def inherited(subclass)
    super
    subclass.read_attributes.merge(read_attributes)
    subclass.write_attributes.merge(write_attributes)
  end

  def attributes
    @read_attributes & @write_attributes
  end

  def read_attributes
    @read_attributes ||= Set.new
  end

  def write_attributes
    @write_attributes ||= Set.new
  end

  def attribute(*names)
    names.each do |name|
      define_attribute(name, :accessor)
    end
  end

  def write_attribute(*names)
    names.each do |name|
      define_attribute(name, :writer)
    end
  end

  def read_attribute(*names)
    names.each do |name|
      define_attribute(name, :reader)
    end
  end

  private

  def define_attribute(name, type)
    name = name.to_sym
    send("attr_#{type}", name)

    read_attributes << name if %i[accessor reader].include?(type)
    write_attributes << name if %i[accessor writer].include?(type)
  end
end
