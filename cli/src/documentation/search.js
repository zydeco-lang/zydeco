const search = document.getElementById("search");
const sections = Array.from(document.querySelectorAll("[data-entry]"));
const navigation = Array.from(document.querySelectorAll("[data-nav]"));
const status = document.getElementById("status");
const indexed = sections.map(section => ({ section, text: section.textContent.toLowerCase() }));
function filter() {
  const words = search.value.toLowerCase().split(/\s+/).filter(Boolean);
  indexed.forEach(({ section, text }) => { section.hidden = !words.every(word => text.includes(word)); });
  navigation.forEach(link => { link.hidden = document.getElementById(link.hash.slice(1)).hidden; });
  const count = sections.filter(section => !section.hidden).length;
  status.textContent = `${count} of ${sections.length} sections`;
}
search.addEventListener("input", filter);
document.addEventListener("keydown", event => {
  if (event.key === "/" && document.activeElement !== search) { event.preventDefault(); search.focus(); }
});
filter();
