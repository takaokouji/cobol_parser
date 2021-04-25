# frozen_string_literal: true

require "strscan"

# Picture
module CobolParser
  class Tree::Picture < Tree
    PIC_ALPHABETIC = 0x01
    PIC_NUMERIC = 0x02
    PIC_NATIONAL = 0x04
    PIC_EDITED = 0x08
    PIC_ALPHANUMERIC = (PIC_ALPHABETIC | PIC_NUMERIC)
    PIC_ALPHABETIC_EDITED = (PIC_ALPHABETIC | PIC_EDITED)
    PIC_ALPHANUMERIC_EDITED = (PIC_ALPHANUMERIC | PIC_EDITED)
    PIC_NUMERIC_EDITED = (PIC_NUMERIC | PIC_EDITED)
    PIC_NATIONAL_EDITED = (PIC_NATIONAL | PIC_EDITED)

    module Helper
      def cb_build_picture(str)
        Tree::Picture.new(self, str)
      end
    end

    class InvalidPictureStringError < StandardError
      def initialize(str, reason = nil)
        error_message = "Invalid picture string - '#{str}'"
        error_message += ": #{reason}" if reason

        super(error_message)
      end
    end

    attribute :size # byte size
    attribute :orig # original picture string
    attribute :str # packed picture string
    attribute :field_category # field category
    attribute :digits # the number of digit places
    attribute :scale # 1/10^scale
    attribute :have_sign # have 'S'

    def initialize(context, str)
      super(self, category: :UNKNOWN)

      begin
        parse_picture_string(str)
      rescue InvalidPictureStringError => e
        cb_error(e.message)
      end
    end

    private

    def parse_picture_string(str)
      raise InvalidPictureStringError, str, "too long" if str.length > 50

      pattern =
        /(?<cs>(?<c>[AX9NS,.VP0B\/*Z+\-CD#{Regexp.quote(current_program.currency_symbol)}])\k<c>*)(?:\(0*(?<n>[^)]+)\))?/
      ss = StringScanner.new(str)
      pic_str_list = []
      while (orig = ss.scan(pattern))
        c = ss[:c]
        n = 1
        if ss[:n]
          raise InvalidPictureStringError, str, "#{ss[:c]}(#{ss[:n]}) is too many digits" if ss[:n].length > 9

          n = ss[:n].to_i
          raise InvalidPictureStringError, str, "#{ss[:c]}(#{ss[:n]}) is 0" if n == 0
        end
        n += ss[:cs].length - 1

        pic_str = pic_str_list.last
        if pic_str && c == pic_str[:c]
          pic_str[:n] += n
          pic_str[:orig] += orig
        else
          pic_str_list << { c: c, n: n, orig: orig }
        end
      end

      raise InvalidPictureStringError, str if ss.rest.length > 0

      buff = String.new
      category = 0
      size = 0
      digits = 0
      scale = 0
      s_count = 0
      v_count = 0
      p_char_seen = false
      s_char_seen = false
      skip = false
      pic_str_list.each.with_index do |ps, idx|
        if skip
          skip = false
          next
        end

        c, n = ps[:c], ps[:n]

        case c
        when "A"
          raise InvalidPictureStringError, str if s_char_seen || p_char_seen

          category |= PIC_ALPHABETIC
        when "X"
          raise InvalidPictureStringError, str if s_char_seen || p_char_seen

          category |= PIC_ALPHANUMERIC
        when "9"
          category |= PIC_NUMERIC
          digits += n
          scale += n if v_count > 0
        when "N"
          raise InvalidPictureStringError, str if s_char_seen || p_char_seen

          category |= PIC_NATIONAL
        when "S"
          category |= PIC_NUMERIC
          raise InvalidPictureStringError, str if (category & PIC_ALPHABETIC) != 0

          s_count += n
          raise InvalidPictureStringError, str if s_count > 1 || idx != 0

          s_char_seen = true
        when ",", ".", "V"
          if [",", "."].include?(c)
            category |= PIC_NUMERIC_EDITED
            raise InvalidPictureStringError, str if s_char_seen || p_char_seen
          end

          if c == "V" || c == current_program.decimal_point
            category |= PIC_NUMERIC
            raise InvalidPictureStringError, str if (category & PIC_ALPHABETIC) != 0

            v_count += n
            raise InvalidPictureStringError, str if v_count > 1
          end
        when "P"
          category |= PIC_NUMERIC
          raise InvalidPictureStringError, str if (category & PIC_ALPHABETIC) != 0
          raise InvalidPictureStringError, str if p_char_seen

          at_beginning = false
          at_end = false
          case buff.length
          when 0
            # P.....
            at_beginning = true
          when 1
            # VP....
            # SP....
            at_beginning = true if ["V", "S"].include?(buff[-1])
          when 2
            # SVP...
            at_beginning = true if buff[-2..-1] == "SV"
          end
          if !pic_str_list[idx + 1] || (pic_str_list[idx + 1][:orig] == "V" && !c_n_list[idx + 2])
            # .....P
            # ....PV
            at_end = true
          end

          raise InvalidPictureStringError, str if !at_beginning && !at_end

          p_char_seen = true
          v_count += 1 if at_beginning # implicit V

          digits += n
          if v_count > 0
            scale += n
          else
            scale -= n
          end
        when "0", "B", "/"
          category |= PIC_EDITED
          raise InvalidPictureStringError, str if s_char_seen || p_char_seen
        when "*", "Z"
          category |= PIC_NUMERIC_EDITED
          raise InvalidPictureStringError, str if (category & PIC_ALPHABETIC) != 0
          raise InvalidPictureStringError, str if s_char_seen || p_char_seen

          digits += n
          scale += n if v_count > 0
        when "+", "-"
          category |= PIC_NUMERIC_EDITED
          raise InvalidPictureStringError, str if (category & PIC_ALPHABETIC) != 0
          raise InvalidPictureStringError, str if s_char_seen || p_char_seen

          digits += n - 1
          s_count += 1
          # FIXME: need more check
        when "C"
          category |= PIC_NUMERIC_EDITED
          raise InvalidPictureStringError, str if !(pic_str_list[idx + 1][:orig] == "R" && !pic_str_list[idx + 2])
          raise InvalidPictureStringError, str if s_char_seen || p_char_seen

          skip = true
          s_count += 1
        when "D"
          category |= PIC_NUMERIC_EDITED
          raise InvalidPictureStringError, str if !(pic_str_list[idx + 1][:orig] == "B" && !pic_str_list[idx + 2])
          raise InvalidPictureStringError, str if s_char_seen || p_char_seen

          skip = true
          s_count += 1
        when current_program.currency_symbol
          category |= PIC_NUMERIC_EDITED
          digits += n - 1
          # FIXME: need more check
        else
          raise InvalidPictureStringError, str
        end

        # calculate size
        size += n if !["V", "P"].include?(c)
        size += n if ["C", "D", "N"].include?(c)

        buff << "#{c}(#{n})"
      end

      raise InvalidPictureStringError, str if size == 0 && v_count > 0

      # set picture
      @orig = str
      @size = size
      @digits = digits
      @scale = scale
      @have_sign = s_count

      case category
      when PIC_ALPHABETIC
        @field_category = :ALPHABETIC
      when PIC_NUMERIC
        @field_category = :NUMERIC
        cb_error("Numeric field cannot be larger than 36 digits") if digits > 36
      when PIC_ALPHANUMERIC, PIC_NATIONAL
        @field_category = :ALPHANUMERIC
      when PIC_NUMERIC_EDITED
        @str = buff
        @field_category = :NUMERIC_EDITED
      when PIC_EDITED, PIC_ALPHABETIC_EDITED, PIC_ALPHANUMERIC_EDITED, PIC_NATIONAL_EDITED
        @str = buff
        @field_category = :ALPHANUMERIC_EDITED
      else
        raise InvalidPictureStringError
      end
    end
  end
end
