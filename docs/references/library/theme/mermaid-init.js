/* Render fenced ```mermaid code blocks in mdBook with live light/dark theme sync. */
(function () {
    "use strict";

    var MERMAID_ESM = "https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs";

    function currentMermaidTheme() {
        var classes = document.documentElement.classList;
        if (classes.contains("coal") || classes.contains("navy") || classes.contains("ayu")) {
            return "dark";
        }
        return "default";
    }

    function initMermaid() {
        var codeBlocks = Array.prototype.slice.call(
            document.querySelectorAll("pre > code.language-mermaid")
        );
        if (codeBlocks.length === 0) {
            return;
        }

        import(MERMAID_ESM)
            .then(function (mod) {
                var mermaid = mod.default || mod;
                var containers = codeBlocks.map(function (code) {
                    var pre = code.parentElement;
                    var source = code.textContent;
                    var container = document.createElement("div");
                    container.className = "mermaid";
                    container.setAttribute("data-mermaid-source", source);
                    container.textContent = source;
                    pre.parentNode.replaceChild(container, pre);
                    return container;
                });

                var lastTheme = null;
                function renderAll() {
                    var theme = currentMermaidTheme();
                    if (theme === lastTheme) {
                        return;
                    }
                    lastTheme = theme;
                    mermaid.initialize({ startOnLoad: false, theme: theme });
                    containers.forEach(function (container) {
                        container.removeAttribute("data-processed");
                        container.textContent = container.getAttribute("data-mermaid-source") || "";
                    });
                    mermaid.run({ nodes: containers });
                }

                renderAll();

                var observer = new MutationObserver(renderAll);
                observer.observe(document.documentElement, {
                    attributes: true,
                    attributeFilter: ["class"],
                });
            })
            .catch(function () {
                /* Leave original <pre><code> blocks intact when offline. */
            });
    }

    if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", initMermaid);
    } else {
        initMermaid();
    }
})();
