# frozen_string_literal: true

# System-name
class CobolParser::Tree::SystemName < CobolParser::Tree
  CATEGORY = %i[
    CALL_CONVENTION_NAME
    CODE_NAME
    COMPUTER_NAME
    DEVICE_NAME
    ENTRY_CONVENTION_NAME
    EXTERNAL_LOCALE_NAME
    FEATURE_NAME
    LIBRARY_NAME
    SWITCH_NAME
    TEXT_NAME
  ].freeze

  attribute :system_category
  attribute :token

  def initialize(context, attributes = {})
    super

    raise ArgumentError, "Invalid system_category: #{@system_category.inspect}" if !CATEGORY.include?(@system_category)
  end
end
