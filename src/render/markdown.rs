use crate::utils::decode_bin;

use ansi_colours::AsRGB;
use anyhow::{anyhow, Context, Result};
use crossterm::style::{Color, Stylize};
use crossterm::terminal;
use std::collections::HashMap;
use std::sync::LazyLock;
use syntect::highlighting::{Color as SyntectColor, FontStyle, Style, Theme};
use syntect::parsing::SyntaxSet;
use syntect::{easy::HighlightLines, parsing::SyntaxReference};
use termimad::{MadSkin, crossterm::style::Color as TermimadColor};

/// Comes from <https://github.com/sharkdp/bat/raw/5e77ca37e89c873e4490b42ff556370dc5c6ba4f/assets/syntaxes.bin>
const SYNTAXES: &[u8] = include_bytes!("../../assets/syntaxes.bin");

static LANG_MAPS: LazyLock<HashMap<String, String>> = LazyLock::new(|| {
    let mut m = HashMap::new();
    m.insert("csharp".into(), "C#".into());
    m.insert("php".into(), "PHP Source".into());
    m
});

pub struct MarkdownRender {
    options: RenderOptions,
    syntax_set: SyntaxSet,
    code_color: Option<Color>,
    #[allow(dead_code)]
    md_syntax: SyntaxReference,
    code_syntax: Option<SyntaxReference>,
    prev_line_type: LineType,
    wrap_width: Option<u16>,
    skin: MadSkin,
    table_buffer: Vec<String>,
    in_table: bool,
}

impl MarkdownRender {
    pub fn init(options: RenderOptions) -> Result<Self> {
        let syntax_set: SyntaxSet =
            decode_bin(SYNTAXES).with_context(|| "MarkdownRender: invalid syntaxes binary")?;

        let code_color = options
            .theme
            .as_ref()
            .map(|theme| get_code_color(theme, options.truecolor));
        let md_syntax = syntax_set.find_syntax_by_extension("md").unwrap().clone();
        let line_type = LineType::Normal;
        let wrap_width = match options.wrap.as_deref() {
            None => None,
            Some(value) => match terminal::size() {
                Ok((columns, _)) => {
                    if value == "auto" {
                        Some(columns)
                    } else {
                        let value = value
                            .parse::<u16>()
                            .map_err(|_| anyhow!("Invalid wrap value"))?;
                        Some(columns.min(value))
                    }
                }
                Err(_) => None,
            },
        };
        
        // Create termimad skin with proper styling
        let skin = create_markdown_skin(options.light_theme);
        
        Ok(Self {
            syntax_set,
            code_color,
            md_syntax,
            code_syntax: None,
            prev_line_type: line_type,
            wrap_width,
            options,
            skin,
            table_buffer: Vec::new(),
            in_table: false,
        })
    }

    pub fn render(&mut self, text: &str) -> String {
        let mut output = String::new();
        let mut current_block = String::new();
        let mut in_code_block = false;
        let mut code_lang: Option<String> = None;
        
        for line in text.split('\n') {
            if let Some(lang) = detect_code_block(line) {
                if in_code_block {
                    // End of code block - render accumulated code
                    let rendered_code = self.render_code_block(&current_block, &code_lang);
                    output.push_str(&rendered_code);
                    output.push_str(line); // Include the closing ```
                    output.push('\n');
                    current_block.clear();
                    in_code_block = false;
                    code_lang = None;
                } else {
                    // Start of code block - render any accumulated markdown first
                    if !current_block.is_empty() {
                        let rendered_md = self.render_markdown_block(&current_block);
                        output.push_str(&rendered_md);
                        current_block.clear();
                    }
                    output.push_str(line); // Include the opening ```
                    output.push('\n');
                    in_code_block = true;
                    code_lang = if lang.is_empty() { None } else { Some(lang) };
                }
            } else if in_code_block {
                current_block.push_str(line);
                current_block.push('\n');
            } else {
                // Check if this line is part of a table
                let trimmed = line.trim();
                let is_table_line = trimmed.starts_with('|') || 
                    (trimmed.contains('|') && trimmed.contains("---"));
                
                if is_table_line {
                    // Add to table buffer for later rendering
                    self.table_buffer.push(line.to_string());
                    self.in_table = true;
                } else if self.in_table {
                    // End of table - render the buffered table first
                    if !self.table_buffer.is_empty() {
                        // Flush any non-table content before the table
                        if !current_block.is_empty() {
                            let rendered_md = self.render_markdown_block(&current_block);
                            output.push_str(&rendered_md);
                            current_block.clear();
                        }
                        
                        let table_text = self.table_buffer.join("\n");
                        self.table_buffer.clear();
                        self.in_table = false;
                        
                        let rendered_table = self.skin.term_text(&table_text);
                        output.push_str(&rendered_table.to_string());
                    }
                    // Now add current non-table line to block
                    current_block.push_str(line);
                    current_block.push('\n');
                } else {
                    current_block.push_str(line);
                    current_block.push('\n');
                }
            }
        }
        
        // Render any remaining content
        if !current_block.is_empty() {
            if in_code_block {
                let rendered_code = self.render_code_block(&current_block, &code_lang);
                output.push_str(&rendered_code);
            } else {
                // Flush table buffer first if any
                if self.in_table && !self.table_buffer.is_empty() {
                    let table_text = self.table_buffer.join("\n");
                    self.table_buffer.clear();
                    self.in_table = false;
                    
                    let rendered_table = self.skin.term_text(&table_text);
                    output.push_str(&rendered_table.to_string());
                }
                
                let rendered_md = self.render_markdown_block(&current_block);
                output.push_str(&rendered_md);
            }
        } else if self.in_table && !self.table_buffer.is_empty() {
            // Only table content remaining
            let table_text = self.table_buffer.join("\n");
            self.table_buffer.clear();
            self.in_table = false;
            
            let rendered_table = self.skin.term_text(&table_text);
            output.push_str(&rendered_table.to_string());
        }
        
        // Remove trailing newline if present to match original behavior
        if output.ends_with('\n') {
            output.pop();
        }
        
        output
    }
    
    fn render_markdown_block(&self, text: &str) -> String {
        // Use termimad to render the full markdown block
        // This handles headers, tables, lists, bold, italic, etc.
        let rendered = self.skin.term_text(text);
        rendered.to_string()
    }
    
    fn render_markdown_line(&mut self, line: &str) -> String {
        // For streaming, we need to handle lines individually
        // Check if this looks like a block-level element
        let trimmed = line.trim();
        
        // Check if this is a table line - be more lenient with detection
        let is_table_line = trimmed.starts_with('|') || trimmed.ends_with('|') || 
            (trimmed.contains('|') && (self.in_table || trimmed.contains("---")));
        let is_table_separator = trimmed.contains('|') && 
            (trimmed.contains("---") || trimmed.contains(":--") || trimmed.contains("--:") || trimmed.contains("-|-"));
        
        if is_table_line || is_table_separator {
            // We're in a table - buffer this line
            self.table_buffer.push(line.to_string());
            self.in_table = true;
            // Return empty string - we'll render the whole table when it's complete
            return String::new();
        } else if self.in_table {
            // We just exited a table - render the buffered table
            let table_text = self.table_buffer.join("\n");
            self.table_buffer.clear();
            self.in_table = false;
            
            // Use term_text which properly handles table column sizing
            let rendered_table = self.skin.term_text(&table_text);
            let table_output = rendered_table.to_string();
            
            // Now render the current line too
            let current_line_output = self.render_non_table_line(line);
            
            return format!("{}\n{}", table_output, current_line_output);
        }
        
        self.render_non_table_line(line)
    }
    
    fn render_non_table_line(&self, line: &str) -> String {
        let trimmed = line.trim_start();
        
        if trimmed.starts_with('#') {
            // Header - render as full text to get proper formatting
            let width = self.wrap_width.unwrap_or(120) as usize;
            let rendered = self.skin.text(line, Some(width));
            return rendered.to_string().trim_end().to_string();
        }
        
        if trimmed.starts_with('>') {
            // Blockquote - render with termimad
            let width = self.wrap_width.unwrap_or(120) as usize;
            let rendered = self.skin.text(line, Some(width));
            return rendered.to_string().trim_end().to_string();
        }
        
        if trimmed.starts_with("- ") || trimmed.starts_with("* ") || 
           trimmed.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) && trimmed.contains(". ") {
            // List item - render with termimad
            let width = self.wrap_width.unwrap_or(120) as usize;
            let rendered = self.skin.text(line, Some(width));
            return rendered.to_string().trim_end().to_string();
        }
        
        // For regular text, use inline rendering for **bold**, *italic*, `code`, etc.
        let rendered = self.skin.inline(line);
        let rendered_str = rendered.to_string();
        
        // Apply wrapping if configured
        self.wrap_line(rendered_str, false)
    }
    
    /// Flush any remaining buffered content (like incomplete tables)
    #[allow(dead_code)]
    pub fn flush(&mut self) -> String {
        if self.in_table && !self.table_buffer.is_empty() {
            let table_text = self.table_buffer.join("\n");
            self.table_buffer.clear();
            self.in_table = false;
            
            let rendered = self.skin.term_text(&table_text);
            return rendered.to_string();
        }
        String::new()
    }
    
    fn render_code_block(&self, code: &str, lang: &Option<String>) -> String {
        let mut output = String::new();
        
        // Try to find syntax for the language
        let syntax = lang.as_ref().and_then(|l| self.find_syntax(l).cloned());
        
        for line in code.lines() {
            let highlighted = if let Some(ref syn) = syntax {
                self.highlight_code_line_with_syntax(line, syn)
            } else {
                // Fall back to basic coloring
                match self.code_color {
                    Some(color) => line.with(color).to_string(),
                    None => line.to_string(),
                }
            };
            let wrapped = self.wrap_line(highlighted, true);
            output.push_str(&wrapped);
            output.push('\n');
        }
        
        output
    }
    
    fn highlight_code_line_with_syntax(&self, line: &str, syntax: &SyntaxReference) -> String {
        let ws: String = line.chars().take_while(|c| c.is_whitespace()).collect();
        let trimmed_line: &str = &line[ws.len()..];
        
        if let Some(theme) = &self.options.theme {
            let mut highlighter = HighlightLines::new(syntax, theme);
            if let Ok(ranges) = highlighter.highlight_line(trimmed_line, &self.syntax_set) {
                return format!(
                    "{ws}{}",
                    as_terminal_escaped(&ranges, self.options.truecolor)
                );
            }
        }
        
        line.to_string()
    }

    pub fn render_line(&mut self, line: &str) -> String {
        let (line_type, code_syntax, is_code) = self.check_line(line);
        let output = if is_code {
            self.highlight_code_line(line, &code_syntax)
        } else {
            // Use termimad for markdown rendering
            self.render_markdown_line(line)
        };
        self.prev_line_type = line_type;
        self.code_syntax = code_syntax;
        output
    }

    fn check_line(&self, line: &str) -> (LineType, Option<SyntaxReference>, bool) {
        let mut line_type = self.prev_line_type;
        let mut code_syntax = self.code_syntax.clone();
        let mut is_code = false;
        if let Some(lang) = detect_code_block(line) {
            match line_type {
                LineType::Normal | LineType::CodeEnd => {
                    line_type = LineType::CodeBegin;
                    code_syntax = if lang.is_empty() {
                        None
                    } else {
                        self.find_syntax(&lang).cloned()
                    };
                }
                LineType::CodeBegin | LineType::CodeInner => {
                    line_type = LineType::CodeEnd;
                    code_syntax = None;
                }
            }
        } else {
            match line_type {
                LineType::Normal => {}
                LineType::CodeEnd => {
                    line_type = LineType::Normal;
                }
                LineType::CodeBegin => {
                    if code_syntax.is_none() {
                        if let Some(syntax) = self.syntax_set.find_syntax_by_first_line(line) {
                            code_syntax = Some(syntax.clone());
                        }
                    }
                    line_type = LineType::CodeInner;
                    is_code = true;
                }
                LineType::CodeInner => {
                    is_code = true;
                }
            }
        }
        (line_type, code_syntax, is_code)
    }

    fn highlight_line(&self, line: &str, syntax: &SyntaxReference, is_code: bool) -> String {
        let ws: String = line.chars().take_while(|c| c.is_whitespace()).collect();
        let trimmed_line: &str = &line[ws.len()..];
        let mut line_highlighted = None;
        if let Some(theme) = &self.options.theme {
            let mut highlighter = HighlightLines::new(syntax, theme);
            if let Ok(ranges) = highlighter.highlight_line(trimmed_line, &self.syntax_set) {
                line_highlighted = Some(format!(
                    "{ws}{}",
                    as_terminal_escaped(&ranges, self.options.truecolor)
                ))
            }
        }
        let line = line_highlighted.unwrap_or_else(|| line.into());
        self.wrap_line(line, is_code)
    }

    fn highlight_code_line(&self, line: &str, code_syntax: &Option<SyntaxReference>) -> String {
        if let Some(syntax) = code_syntax {
            self.highlight_line(line, syntax, true)
        } else {
            let line = match self.code_color {
                Some(color) => line.with(color).to_string(),
                None => line.to_string(),
            };
            self.wrap_line(line, true)
        }
    }

    fn wrap_line(&self, line: String, is_code: bool) -> String {
        if let Some(width) = self.wrap_width {
            if is_code && !self.options.wrap_code {
                return line;
            }
            wrap(&line, width as usize)
        } else {
            line
        }
    }

    pub fn find_syntax(&self, lang: &str) -> Option<&SyntaxReference> {
        if let Some(new_lang) = LANG_MAPS.get(&lang.to_ascii_lowercase()) {
            self.syntax_set.find_syntax_by_name(new_lang)
        } else {
            self.syntax_set
                .find_syntax_by_token(lang)
                .or_else(|| self.syntax_set.find_syntax_by_extension(lang))
        }
    }
}

/// Create a MadSkin with appropriate styling for terminal output
fn create_markdown_skin(light_theme: bool) -> MadSkin {
    use termimad::crossterm::style::Attribute;
    
    let mut skin = MadSkin::default();
    
    if light_theme {
        // Light theme colors
        skin.bold.set_fg(TermimadColor::Black);
        skin.bold.add_attr(Attribute::Bold);
        skin.italic.set_fg(TermimadColor::DarkGrey);
        skin.italic.add_attr(Attribute::Italic);
        skin.strikeout.add_attr(Attribute::CrossedOut);
        skin.inline_code.set_fg(TermimadColor::DarkRed);
        skin.code_block.set_fg(TermimadColor::DarkRed);
        
        // Headers
        skin.headers[0].set_fg(TermimadColor::DarkBlue);
        skin.headers[0].add_attr(Attribute::Bold);
        skin.headers[0].add_attr(Attribute::Underlined);
        skin.headers[1].set_fg(TermimadColor::DarkBlue);
        skin.headers[1].add_attr(Attribute::Bold);
        skin.headers[2].set_fg(TermimadColor::DarkCyan);
        skin.headers[2].add_attr(Attribute::Bold);
        skin.headers[3].set_fg(TermimadColor::DarkCyan);
        skin.headers[4].set_fg(TermimadColor::DarkGrey);
        skin.headers[5].set_fg(TermimadColor::DarkGrey);
        
        // Blockquotes
        skin.quote_mark.set_fg(TermimadColor::DarkGrey);
        
        // Lists
        skin.bullet.set_fg(TermimadColor::DarkBlue);
        
        // Tables
        skin.table.set_fg(TermimadColor::Black);
        
        // Horizontal rule
        skin.horizontal_rule.set_fg(TermimadColor::DarkGrey);
    } else {
        // Dark theme colors (default)
        skin.bold.set_fg(TermimadColor::White);
        skin.bold.add_attr(Attribute::Bold);
        skin.italic.set_fg(TermimadColor::Rgb { r: 200, g: 200, b: 200 });
        skin.italic.add_attr(Attribute::Italic);
        skin.strikeout.add_attr(Attribute::CrossedOut);
        skin.inline_code.set_fg(TermimadColor::Yellow);
        skin.code_block.set_fg(TermimadColor::Yellow);
        
        // Headers
        skin.headers[0].set_fg(TermimadColor::Cyan);
        skin.headers[0].add_attr(Attribute::Bold);
        skin.headers[0].add_attr(Attribute::Underlined);
        skin.headers[1].set_fg(TermimadColor::Cyan);
        skin.headers[1].add_attr(Attribute::Bold);
        skin.headers[2].set_fg(TermimadColor::Blue);
        skin.headers[2].add_attr(Attribute::Bold);
        skin.headers[3].set_fg(TermimadColor::Blue);
        skin.headers[4].set_fg(TermimadColor::Magenta);
        skin.headers[5].set_fg(TermimadColor::Magenta);
        
        // Blockquotes
        skin.quote_mark.set_fg(TermimadColor::Yellow);
        
        // Lists
        skin.bullet.set_fg(TermimadColor::Cyan);
        
        // Tables
        skin.table.set_fg(TermimadColor::White);
        
        // Horizontal rule  
        skin.horizontal_rule.set_fg(TermimadColor::DarkGrey);
    }
    
    skin
}

fn wrap(text: &str, width: usize) -> String {
    let indent: usize = text.chars().take_while(|c| *c == ' ').count();
    let wrap_options = textwrap::Options::new(width)
        .wrap_algorithm(textwrap::WrapAlgorithm::FirstFit)
        .initial_indent(&text[0..indent]);
    textwrap::wrap(&text[indent..], wrap_options).join("\n")
}

#[derive(Debug, Clone, Default)]
pub struct RenderOptions {
    pub theme: Option<Theme>,
    pub wrap: Option<String>,
    pub wrap_code: bool,
    pub truecolor: bool,
    pub light_theme: bool,
}

impl RenderOptions {
    pub(crate) fn new(
        theme: Option<Theme>,
        wrap: Option<String>,
        wrap_code: bool,
        truecolor: bool,
        light_theme: bool,
    ) -> Self {
        Self {
            theme,
            wrap,
            wrap_code,
            truecolor,
            light_theme,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LineType {
    Normal,
    CodeBegin,
    CodeInner,
    CodeEnd,
}

fn as_terminal_escaped(ranges: &[(Style, &str)], truecolor: bool) -> String {
    let mut output = String::new();
    for (style, text) in ranges {
        let fg = blend_fg_color(style.foreground, style.background);
        let mut text = text.with(convert_color(fg, truecolor));
        if style.font_style.contains(FontStyle::BOLD) {
            text = text.bold();
        }
        if style.font_style.contains(FontStyle::UNDERLINE) {
            text = text.underlined();
        }
        output.push_str(&text.to_string());
    }
    output
}

fn convert_color(c: SyntectColor, truecolor: bool) -> Color {
    if truecolor {
        Color::Rgb {
            r: c.r,
            g: c.g,
            b: c.b,
        }
    } else {
        let value = (c.r, c.g, c.b).to_ansi256();
        // lower contrast
        let value = match value {
            7 | 15 | 231 | 252..=255 => 252,
            _ => value,
        };
        Color::AnsiValue(value)
    }
}

fn blend_fg_color(fg: SyntectColor, bg: SyntectColor) -> SyntectColor {
    if fg.a == 0xff {
        return fg;
    }
    let ratio = u32::from(fg.a);
    let r = (u32::from(fg.r) * ratio + u32::from(bg.r) * (255 - ratio)) / 255;
    let g = (u32::from(fg.g) * ratio + u32::from(bg.g) * (255 - ratio)) / 255;
    let b = (u32::from(fg.b) * ratio + u32::from(bg.b) * (255 - ratio)) / 255;
    SyntectColor {
        r: u8::try_from(r).unwrap_or(u8::MAX),
        g: u8::try_from(g).unwrap_or(u8::MAX),
        b: u8::try_from(b).unwrap_or(u8::MAX),
        a: 255,
    }
}

fn detect_code_block(line: &str) -> Option<String> {
    let line = line.trim_start();
    if !line.starts_with("```") {
        return None;
    }
    let lang = line
        .chars()
        .skip(3)
        .take_while(|v| !v.is_whitespace())
        .collect();
    Some(lang)
}

fn get_code_color(theme: &Theme, truecolor: bool) -> Color {
    let scope = theme.scopes.iter().find(|v| {
        v.scope
            .selectors
            .iter()
            .any(|v| v.path.scopes.iter().any(|v| v.to_string() == "string"))
    });
    scope
        .and_then(|v| v.style.foreground)
        .map_or_else(|| Color::Yellow, |c| convert_color(c, truecolor))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_render() {
        let options = RenderOptions::default();
        let render = MarkdownRender::init(options).unwrap();
        assert!(render.find_syntax("csharp").is_some());
    }

    #[test]
    fn test_detect_code_block() {
        assert_eq!(detect_code_block("```rust"), Some("rust".into()));
        assert_eq!(detect_code_block("```c++"), Some("c++".into()));
        assert_eq!(detect_code_block("  ```rust"), Some("rust".into()));
        assert_eq!(detect_code_block("```"), Some("".into()));
        assert_eq!(detect_code_block("``rust"), None);
    }
}
