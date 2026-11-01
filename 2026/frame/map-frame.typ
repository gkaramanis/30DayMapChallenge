// Fixed portrait frame for the 2026 maps. save_map() in R/helpers.R passes
// title, subtitle, caption and the map path as --input values. The text is
// Typst markup: \ breaks a line, and #, * and _ need a \ before them.
// Keep the map box size in sync with map_box in R/helpers.R.

#let i = sys.inputs
#let md(s) = eval(s, mode: "markup")
#let map-box = (x: 0.3in, y: 2.6in, width: 7.4in, height: 7.88in)

#set page(width: 8in, height: 11.08in, margin: 0pt, fill: rgb("#FAFAF6"))
#set text(font: "Metropolis", fill: rgb("#1A1A18"))
#set par(leading: 0.3em, spacing: 0.35em)

#place(top + left, dx: map-box.x, dy: map-box.y,
  block(radius: 12pt, clip: true,
    image(i.map, width: map-box.width, height: map-box.height)))

#place(top + left, dx: map-box.x, dy: 0.45in, block(width: map-box.width, {
  set align(center)
  text(font: "Karst", weight: "bold", size: 36pt, md(i.title))
  parbreak()
  v(8pt)
  {
    set text(size: 22pt)
    set par(leading: 0.5em)
    md(i.subtitle)
  }
}))

#place(bottom + center, dy: -0.3in, text(size: 12pt, md(i.caption)))
