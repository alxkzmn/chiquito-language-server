use codespan_reporting::files::{Files, SimpleFile};
use tower_lsp::lsp_types::{Position, Range};

/// Factory for creating debug symbol references.
pub struct RangeFactory {
    /// Source file reference.
    file: SimpleFile<String, String>,
}

impl RangeFactory {
    /// Creates a new debug symbol reference factory.
    ///
    /// # Arguments
    ///
    /// * `file_path` - Path to the source file.
    /// * `contents` - Contents of the source file.
    ///
    /// # Returns
    ///
    /// A new debug symbol reference factory.
    pub fn new(file_path: &str, contents: &str) -> RangeFactory {
        let file = SimpleFile::new(file_path.to_string(), contents.to_string());

        RangeFactory { file }
    }

    /// Creates a new debug symbol reference.
    ///
    /// # Arguments
    ///
    /// * `start` - Start position of the debug symbol byte reference in the source string.
    /// * `end` - End position of the debug symbol byte reference in the source string.
    pub fn create(&self, start: usize, end: usize) -> Range {
        let start_line_index = self.get_line_index(start);
        let start_col_number = self.get_column_number(start_line_index, start);

        let end_line_index = self.get_line_index(end);
        let end_col_number = self.get_column_number(end_line_index, end);

        Range::new(
            Position::new(start_line_index as u32, start_col_number as u32),
            Position::new(end_line_index as u32, end_col_number as u32),
        )
    }

    fn get_column_number(&self, line_index: usize, start: usize) -> usize {
        match self.file.column_number((), line_index, start) {
            // Subtract 1 to convert to 0-based index
            Ok(number) => number - 1,
            Err(err) => {
                panic!("Column number at {} not found: {}", line_index, err);
            }
        }
    }

    fn get_line_index(&self, start: usize) -> usize {
        match self.file.line_index((), start) {
            Ok(index) => index,
            Err(err) => {
                panic!("Line index at {} not found: {}", start, err);
            }
        }
    }
}
