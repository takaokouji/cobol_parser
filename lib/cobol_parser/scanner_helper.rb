# frozen_string_literal: true

module CobolParser
  module ScannerHelper
    def cb_set_in_procedure
      @context.scanner.in_procedure = true
    end

    def cb_reset_in_procedure
      @context.scanner.in_procedure = false
    end

    def cb_reset_78
      @context.scanner.lev78.clear
    end
  end
end
