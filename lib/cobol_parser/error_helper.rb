# frozen_string_literal: true

module CobolParser::ErrorHelper
  def warning(format, *arg)
    print_error(nil, 0, "Warning: ", format, *arg)

    @warningcount += 1
  end

  def error(format, *arg)
    print_error(nil, 0, "Error: ", format, *arg)

    @errorcount += 1
  end

  def warning_x(tree, format, *arg)
    print_error(tree.source_file, tree.source_line, "Warning: ", format, *arg)

    @warningcount += 1
  end

  def error_x(tree, format, *arg)
    print_error(tree.source_file, tree.source_line, "Error: ", format, *arg)

    @errorcount += 1
  end

  private

  def print_error(file, line, prefix, format, *arg)
    @last_section ||= nil
    @last_paragraph ||= nil

    file ||= @source_file
    line ||= @source_line

    if @current_section != @last_section || @current_paragraph != @last_paragraph
      if @current_paragraph&.name == "MAIN PARAGRAPH"
        $stderr.printf("%s: In paragraph '%s':\n", file, @current_paragraph.name)
      elsif @current_section&.name == "MAIN SECTION"
        $stderr.printf("%s: In section '%s':\n", file, @current_section.name)
      end
      @last_section = @current_section
      @last_paragraph = @current_paragraph
    end

    $stderr.puts("#{file}:#{line}: #{prefix}" + sprintf(format, *arg)) # rubocop:todo Style/StderrPuts
  end
end
