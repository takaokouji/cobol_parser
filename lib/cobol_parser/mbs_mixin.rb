# frozen_string_literal: true

module CobolParser
  # multibyte string mixin
  module MbsMixin
    module_function

    # The number of characters should be counted as 1 for single-byte characters
    # and 2 for multi-byte characters
    #
    # @param [String] str
    # @return [Integer]
    def mbs_length(str)
      return str.length if str.ascii_only?

      str.length + str.chars.reject(&:ascii_only?).length
    end

    # Like String#slice(nth),
    # but the nth should be counted as 1 for single-byte characters
    # and 2 for multi-byte characters
    #
    # @param [String] str
    # @param [Integer] nth
    # @return [String]
    def mbs_slice_nth(str, nth)
      return str[nth] if str.ascii_only?

      return str[nth] if [0, 1].include?(nth)

      mbs_len = mbs_length(str)

      nth += mbs_len if nth < 0

      return nil if nth >= mbs_len || nth < 0

      index = 0
      mbs_index = 0
      str.chars.each do |c|
        break if mbs_index == nth

        index += 1
        mbs_index += 1

        next if c.ascii_only?

        mbs_index += 1
        if mbs_index > nth
          index -= 1
          break
        end
      end
      str[index]
    end

    # Like String#slice(nth, length),
    # but the nth and length should be counted as 1 for single-byte characters
    # and 2 for multi-byte characters
    #
    # @param [String] str
    # @param [Integer] nth
    # @param [Integer] length
    # @return [String]
    def mbs_slice_nth_length(str, nth, length)
      return str[nth, length] if str.ascii_only?

      return "" if length == 0

      mbs_len = mbs_length(str)

      nth += mbs_len if nth < 0

      return nil if nth >= mbs_len || nth < 0

      first_nth = nth
      last_nth = nth + length - 1

      first_index = 0
      last_index = 0
      mbs_index = 0
      str.chars.each do |c|
        first_index = last_index if mbs_index <= first_nth

        break if mbs_index == last_nth

        last_index += 1
        mbs_index += 1

        next if c.ascii_only?

        mbs_index += 1
        if mbs_index > last_nth
          last_index -= 1
          break
        end
      end
      str[first_index..last_index]
    end

    # Like String#slice(nth, range),
    # but the range's value should be counted as 1 for single-byte characters
    # and 2 for multi-byte characters
    #
    # @param [String] str
    # @param [Range] range
    # @return [String]
    def mbs_slice_range(str, range)
      return str[range] if str.ascii_only?

      nth = range.first
      length = if range.last >= 0
                 range.last - nth
               else
                 (mbs_length(str) + range.last) - nth
               end
      length += 1 if !range.exclude_end?

      mbs_slice_nth_length(str, nth, length)
    end
  end
end
