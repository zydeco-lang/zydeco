/* Add a top-left home link back to the Zydeco documentation landing page. */
(function () {
    "use strict";

    function landingHref() {
        var root = typeof path_to_root === "string" ? path_to_root : "";
        return root + "../";
    }

    function addMenuBarHomeButton(href) {
        var leftButtons = document.querySelector("#mdbook-menu-bar .left-buttons");
        if (!leftButtons || document.getElementById("zydeco-home-button")) {
            return;
        }
        var link = document.createElement("a");
        link.id = "zydeco-home-button";
        link.className = "icon-button";
        link.href = href;
        link.title = "Zydeco documentation home";
        link.setAttribute("aria-label", "Zydeco documentation home");
        link.style.textDecoration = "none";
        link.innerHTML =
            '<span class="fa-svg">' +
            '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 576 512" aria-hidden="true">' +
            '<path d="M575.8 255.5c0 18-15 32.1-32 32.1h-32l.7 160.2c0 2.7-.2 5.4-.5 8.1V472c0 22.1-17.9 40-40 40H456c-1.1 0-2.2 0-3.3-.1c-1.4 .1-2.8 .1-4.2 .1H416 392c-22.1 0-40-17.9-40-40V448 384c0-17.7-14.3-32-32-32H256c-17.7 0-32 14.3-32 32v64 24c0 22.1-17.9 40-40 40H160 128.1c-1.5 0-3-.1-4.5-.2c-1.2 .1-2.4 .2-3.6 .2H104c-22.1 0-40-17.9-40-40V360c0-.9 0-1.9 .1-2.8V287.6H32c-18 0-32-14-32-32.1c0-9 3-17 10-24L266.4 8c7-7 15-8 22-8s15 2 21 7L564.8 231.5c8 7 12 15 11 24z"/>' +
            "</svg>" +
            "</span>";
        leftButtons.appendChild(link);
    }

    function addSidebarHomeItem(href) {
        var chapterList = document.querySelector("#mdbook-sidebar ol.chapter");
        if (!chapterList || document.getElementById("zydeco-sidebar-home")) {
            return;
        }
        var item = document.createElement("li");
        item.id = "zydeco-sidebar-home";
        item.className = "chapter-item expanded";

        var wrapper = document.createElement("span");
        wrapper.className = "chapter-link-wrapper";

        var link = document.createElement("a");
        link.href = href;
        link.textContent = "\u2190 Zydeco";
        link.tabIndex = document.documentElement.classList.contains("sidebar-visible") ? 0 : -1;

        wrapper.appendChild(link);
        item.appendChild(wrapper);

        var spacer = document.createElement("li");
        spacer.className = "spacer";

        chapterList.insertBefore(spacer, chapterList.firstChild);
        chapterList.insertBefore(item, spacer);
    }

    function initHomeLinks() {
        var href = landingHref();
        addMenuBarHomeButton(href);
        addSidebarHomeItem(href);
    }

    if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", initHomeLinks);
    } else {
        initHomeLinks();
    }
})();
