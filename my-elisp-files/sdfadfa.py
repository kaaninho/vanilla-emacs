import requests
from bs4 import BeautifulSoup


def print_secret_message(doc_url):
    response = requests.get(doc_url, headers={"User-Agent": "Mozilla/5.0"})
    response.raise_for_status()

    soup = BeautifulSoup(response.text, "html.parser")
    table = soup.find("table")
    if table is None:
        raise ValueError("No table found in the document.")
    rows = table.find_all("tr")

    # Spalten anhand der Überschriften zuordnen (unabhängig von der Reihenfolge)
    header_cells = [
        c.get_text(strip=True).lower() for c in rows[0].find_all(["td", "th"])
    ]
    col = {}
    for i, h in enumerate(header_cells):
        if "x" in h:
            col["x"] = i
        elif "y" in h:
            col["y"] = i
        else:
            col["char"] = i

    points = {}
    max_x = max_y = 0
    for row in rows[1:]:
        cells = [c.get_text(strip=True) for c in row.find_all(["td", "th"])]
        if len(cells) < 3:
            continue
        try:
            x = int(cells[col["x"]])
            y = int(cells[col["y"]])
        except ValueError:
            continue
        points[(x, y)] = cells[col["char"]]
        max_x = max(max_x, x)
        max_y = max(max_y, y)

    # y=max_y oben, y=0 unten; fehlende Felder werden zu Leerzeichen
    for y in range(max_y, -1, -1):
        print("".join(points.get((x, y), " ") for x in range(max_x + 1)))
