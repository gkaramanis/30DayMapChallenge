// Fixed portrait frame for the 2026 maps. frame_map() in R/helpers.R writes
// the JSON with the text, colours, page size and map box, all in inches.

#let d = json(sys.inputs.data)
#let box = d.box

#set page(width: d.page.width * 1in, height: d.page.height * 1in, margin: 0pt, fill: rgb(d.bg))
#set text(font: "Metropolis", fill: rgb(d.text))
#set par(leading: 0.3em, spacing: 0.35em)

#place(top + left, dx: box.x * 1in, dy: box.y * 1in,
  image(d.panel, width: box.width * 1in, height: box.height * 1in))

#place(top + left, dx: box.x * 1in, dy: 0.45in, block(width: box.width * 1in, {
  set align(center)
  text(font: "Karst", weight: "bold", size: 36pt, d.title)
  parbreak()
  text(size: 22pt, d.subtitle)
}))

#place(bottom + center, dy: -0.3in, text(size: 12pt, d.caption))
