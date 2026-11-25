// SVG snippets shared by the patch pictures. Xeliherb is three green strokes, ralocrop an orange head on a stem;
// a high-yield patch shows a large xeliherb plant and a low-yield patch a small one.
export const xeliherb = (x, y) => `<g transform="translate(${x},${y})">${[-4, 0, 4].map((d) =>
  `<path d="M0,0 q${d},-7 ${d * 1.4},-13" stroke="#4F7A4A" stroke-width="1.8" fill="none" stroke-linecap="round"/>`).join('')}</g>`;

export const xeliYield = (x, y, high) =>
  `<g transform="translate(${x},${y}) scale(${high ? 1.35 : 0.75})">${xeliherb(0, 0)}</g>`;

export const ralocrop = (x, y, s) => `<g transform="translate(${x},${y}) scale(${s})">` +
  '<line x1="8" y1="25" x2="8" y2="12" stroke="#2B2A30" stroke-width="1.6"/>' +
  '<circle cx="8" cy="7.5" r="6" fill="#E07A1F" stroke="#2B2A30" stroke-width="1.4"/></g>';

export const icon = (inner, w) => `<svg class="icon" width="${w}" height="20" viewBox="0 0 ${w} 20" aria-hidden="true">${inner}</svg>`;
