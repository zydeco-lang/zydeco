/* Zydeco syntax highlighting for mdBook's bundled highlight.js.
 * Mirrors editor/vscode/syntaxes/zydeco.tmLanguage.json.
 */
(function () {
    "use strict";

    function zydecoLanguage(hljs) {
        var BLOCK_COMMENT = hljs.COMMENT("/-", "-/", {
            contains: ["self"],
        });

        var DOC_COMMENT = {
            className: "doctag",
            begin: "--\\|",
            end: "$",
            relevance: 10,
        };

        var LINE_COMMENT = hljs.COMMENT("--", "$");

        var CHAR_LITERAL = {
            className: "string",
            begin: "'(?:\\\\(?:u\\{[0-9a-fA-F]{1,6}\\}|.)|[^'\\\\])'",
            relevance: 0,
        };

        var CONSTRUCTOR = {
            className: "symbol",
            begin: "\\+[A-Z][A-Za-z0-9_'?+*=~-]*",
            relevance: 5,
        };

        var DESTRUCTOR = {
            className: "title",
            begin: "\\.[a-z][A-Za-z0-9_'?+*=~-]*",
            relevance: 2,
        };

        var FIELD = {
            className: "attr",
            begin: "#[A-Za-z_][A-Za-z0-9_'?+*=~-]*",
            relevance: 5,
        };

        var META_PREFIX = {
            className: "meta",
            begin: "@(?=\\[|\\()",
            relevance: 5,
        };

        var TYPE_NAME = {
            className: "type",
            begin: "\\b[A-Z][A-Za-z0-9_'?+*=~-]*\\b",
            relevance: 0,
        };

        var NUMBER = {
            className: "number",
            begin: "\\b(?:[0-9]+\\.[0-9]+(?:[eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+|[0-9]+)\\b",
            relevance: 0,
        };

        return {
            name: "Zydeco",
            aliases: ["zy", "zyi"],
            keywords: {
                keyword:
                    "begin end in that do ret match comatch where " +
                    "data codata def define let param " +
                    "as fn val fix pack is " +
                    "pi forall sigma exists",
            },
            contains: [
                DOC_COMMENT,
                LINE_COMMENT,
                BLOCK_COMMENT,
                hljs.QUOTE_STRING_MODE,
                CHAR_LITERAL,
                META_PREFIX,
                CONSTRUCTOR,
                DESTRUCTOR,
                FIELD,
                TYPE_NAME,
                NUMBER,
            ],
        };
    }

    function highlightZydecoBlocks() {
        var hljs = window.hljs;
        if (!hljs || typeof hljs.registerLanguage !== "function") {
            return;
        }
        hljs.registerLanguage("zydeco", zydecoLanguage);

        var blocks = document.querySelectorAll(
            "pre > code.language-zydeco, pre > code.language-zy, pre > code.language-zyi"
        );
        blocks.forEach(function (block) {
            var source = block.textContent;
            block.removeAttribute("data-highlighted");
            block.classList.remove("hljs");
            block.textContent = source;
            if (typeof hljs.highlightElement === "function") {
                hljs.highlightElement(block);
            } else if (typeof hljs.highlightBlock === "function") {
                hljs.highlightBlock(block);
            }
        });
    }

    if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", highlightZydecoBlocks);
    } else {
        highlightZydecoBlocks();
    }
})();
