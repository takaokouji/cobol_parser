# frozen_string_literal: true

module CobolParser
  # System-name
  class Tree::SystemName < Tree
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

    module Helper
      def build_system_name(system_category, token)
        Tree::SystemName.new(self, system_category, token)
      end
    end

    def initialize(context, system_category, token)
      raise ArgumentError, "Invalid system_category: #{system_category.inspect}" if !CATEGORY.include?(system_category)

      super(context, category: :UNKNOWN, system_category: system_category, token: token)
    end
  end
end
