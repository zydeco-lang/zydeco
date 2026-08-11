#[derive(Default)]
pub(crate) struct SourceEditor {
    characters: Vec<char>,
    cursor: usize,
}

impl SourceEditor {
    pub(crate) fn source(&self) -> String {
        self.characters.iter().collect()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.characters.is_empty()
    }

    pub(crate) fn clear(&mut self) {
        self.characters.clear();
        self.cursor = 0;
    }

    pub(crate) fn insert(&mut self, character: char) {
        self.characters.insert(self.cursor, character);
        self.cursor += 1;
    }

    pub(crate) fn insert_str(&mut self, text: &str) {
        text.chars().for_each(|character| self.insert(character));
    }

    pub(crate) fn newline(&mut self) {
        self.insert('\n');
    }

    pub(crate) fn backspace(&mut self) {
        if self.cursor > 0 {
            self.cursor -= 1;
            self.characters.remove(self.cursor);
        }
    }

    pub(crate) fn delete(&mut self) {
        if self.cursor < self.characters.len() {
            self.characters.remove(self.cursor);
        }
    }

    pub(crate) fn move_left(&mut self) {
        self.cursor = self.cursor.saturating_sub(1);
    }

    pub(crate) fn move_right(&mut self) {
        self.cursor = (self.cursor + 1).min(self.characters.len());
    }

    pub(crate) fn move_home(&mut self) {
        self.cursor = self.line_start();
    }

    pub(crate) fn move_end(&mut self) {
        self.cursor = self.line_end();
    }

    pub(crate) fn move_up(&mut self) {
        let column = self.cursor - self.line_start();
        let current_start = self.line_start();
        if current_start == 0 {
            return;
        }
        let previous_end = current_start - 1;
        let previous_start = self.characters[..previous_end]
            .iter()
            .rposition(|character| *character == '\n')
            .map_or(0, |position| position + 1);
        self.cursor = (previous_start + column).min(previous_end);
    }

    pub(crate) fn move_down(&mut self) {
        let column = self.cursor - self.line_start();
        let current_end = self.line_end();
        if current_end == self.characters.len() {
            return;
        }
        let next_start = current_end + 1;
        let next_end = self.characters[next_start..]
            .iter()
            .position(|character| *character == '\n')
            .map_or(self.characters.len(), |position| next_start + position);
        self.cursor = (next_start + column).min(next_end);
    }

    pub(crate) fn line_count(&self) -> usize {
        self.characters.iter().filter(|character| **character == '\n').count() + 1
    }

    pub(crate) fn cursor_position(&self) -> (usize, usize) {
        let row =
            self.characters[..self.cursor].iter().filter(|character| **character == '\n').count();
        (row, self.cursor - self.line_start())
    }

    fn line_start(&self) -> usize {
        self.characters[..self.cursor]
            .iter()
            .rposition(|character| *character == '\n')
            .map_or(0, |position| position + 1)
    }

    fn line_end(&self) -> usize {
        self.characters[self.cursor..]
            .iter()
            .position(|character| *character == '\n')
            .map_or(self.characters.len(), |position| self.cursor + position)
    }
}

#[cfg(test)]
mod tests {
    use super::SourceEditor;

    #[test]
    fn editing_tracks_unicode_by_character() {
        let mut editor = SourceEditor::default();
        editor.insert_str("a😀\nxy");
        assert_eq!(editor.cursor_position(), (1, 2));

        editor.move_up();
        assert_eq!(editor.cursor_position(), (0, 2));
        editor.backspace();
        assert_eq!(editor.source(), "a\nxy");
        editor.move_down();
        assert_eq!(editor.cursor_position(), (1, 1));
    }
}
