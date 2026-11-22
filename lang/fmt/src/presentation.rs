//! Incremental formatting presentation captured outside `zydeco-surface`'s syntax arena.

use crate::{
    intention::{LineExtent, SourceLine, SurfaceIntentions},
    trivia::{CommentCapture, SpannedEntity, SurfaceTrivia},
};
use zydeco_surface::textual::syntax::{EntityId, FileMap, Parser};

/// Formatting trivia and line-layout intentions captured per parsed source unit.
#[derive(Clone, Default, Debug)]
pub struct FormatPresentation {
    pub intentions: SurfaceIntentions,
    pub trivia: SurfaceTrivia,
    captured_entities: usize,
    captured_arms: usize,
    captured_existentials: usize,
    captured_metadata: usize,
}

impl FormatPresentation {
    /// Capture formatting layout and comments for entities added to `parser` since the previous capture.
    pub fn capture(&mut self, source: &str, parser: &Parser) {
        let file_map = FileMap::local(source, None);
        let entities = parser
            .spans
            .iter_from(self.captured_entities)
            .filter_map(|(entity, span)| {
                let span = span.range();
                source.get(span.start..span.end)?;
                Some(SpannedEntity::new(entity, span.start, span.end))
            })
            .collect::<Vec<_>>();
        let (arm_prefixes, existential_prefixes, metadata_prefixes) = match parser.format_prefixes()
        {
            | Some(prefixes) => (
                &prefixes.arms[self.captured_arms.min(prefixes.arms.len())..],
                &prefixes.existentials
                    [self.captured_existentials.min(prefixes.existentials.len())..],
                &prefixes.metadata[self.captured_metadata.min(prefixes.metadata.len())..],
            ),
            | None => (&[][..], &[][..], &[][..]),
        };

        let comments = CommentCapture::new(source, &entities)
            .with_arm_prefixes(arm_prefixes.iter().map(|prefix| (prefix.first, prefix.start)))
            .with_metadata_prefixes(
                metadata_prefixes.iter().map(|prefix| (prefix.metadata, prefix.start)),
            )
            .with_existential_prefixes(
                existential_prefixes.iter().map(|prefix| (prefix.parameter, prefix.open)),
            );
        let starts = comments.comment_starts();

        let layouts = entities
            .iter()
            .map(|entity| {
                // A recovered node can end immediately after an unrecognized Unicode
                // character. Its last occupied byte is not necessarily a character boundary.
                let occupied_end =
                    source.floor_char_boundary(entity.end().saturating_sub(1)).max(entity.start());
                let first = file_map.line_col(entity.start()).line as usize;
                let last = file_map.line_col(occupied_end).line as usize;
                let presentation_start = starts.presentation(entity.entity(), entity.start());
                let presentation_start =
                    SourceLine(file_map.line_col(presentation_start).line as usize);
                (entity.entity(), LineExtent::new(first, last), presentation_start)
            })
            .collect::<Vec<_>>();
        let arm_layouts = arm_prefixes
            .iter()
            .filter_map(|prefix| {
                let payload = parser.spans[&prefix.payload].range();
                let (payload_start, payload_end) = (payload.start, payload.end);
                source.get(payload_start..payload_end)?;
                let prefix_line = SourceLine(file_map.line_col(prefix.start).line as usize);
                let presentation_start = starts.payload(prefix.payload, payload_start);
                let presentation_line =
                    SourceLine(file_map.line_col(presentation_start).line as usize);
                Some((prefix.payload, prefix_line, presentation_line))
            })
            .collect::<Vec<_>>();
        let existential_layouts = existential_prefixes
            .iter()
            .filter_map(|prefix| {
                let parameter = parser.spans[&EntityId::Pat(prefix.parameter)].range();
                let (parameter_start, parameter_end) = (parameter.start, parameter.end);
                source.get(parameter_start..parameter_end)?;
                let presentation_start =
                    starts.presentation(prefix.parameter.into(), parameter_start);
                let prefix_start = prefix.start.min(presentation_start);
                Some((prefix.parameter, SourceLine(file_map.line_col(prefix_start).line as usize)))
            })
            .collect::<Vec<_>>();
        let existential_open_layouts = existential_prefixes
            .iter()
            .filter_map(|prefix| {
                let parameter = parser.spans[&EntityId::Pat(prefix.parameter)].range();
                let (parameter_start, parameter_end) = (parameter.start, parameter.end);
                source.get(parameter_start..parameter_end)?;
                let open_line = SourceLine(file_map.line_col(prefix.open).line as usize);
                let presentation_start = starts.payload(prefix.parameter.into(), parameter_start);
                let presentation_line =
                    SourceLine(file_map.line_col(presentation_start).line as usize);
                Some((prefix.parameter, open_line, presentation_line))
            })
            .collect::<Vec<_>>();

        self.intentions.record_source_layout(
            source,
            comments.layout_exclusions(),
            layouts,
            arm_layouts,
            existential_layouts,
            existential_open_layouts,
        );
        self.trivia.record_comments(comments);

        self.captured_entities = parser.spans.len();
        if let Some(prefixes) = parser.format_prefixes() {
            self.captured_arms = prefixes.arms.len();
            self.captured_existentials = prefixes.existentials.len();
            self.captured_metadata = prefixes.metadata.len();
        }
    }
}
