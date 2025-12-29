# FlipLang

<p align="center">
  <strong>Bahasa Pemrograman Modern dengan HTML Templating</strong><br>
  OOP Support • MVC Architecture • CRUD Web App
</p>

---

## 🚀 Fitur

- **HTML-First Templating** - Tag `[f>...</]` dan `[=>...</]`
- **OOP Support** - Classes, constructors, methods
- **MVC Architecture** - Model, View, Controller
- **File Storage** - Baca/tulis file
- **Bootstrap 5 UI** - Tampilan modern

---

## 📖 Sintaks

### Variabel

```flip
[f>
var !name = "FlipLang";
var !items = [1, 2, 3];
var !user = {"name": "John"};
</]
```

### Output

```flip
[f> ec "Hello World"; </]
<p>Nama: [=> !name </]</p>
```

### Control Flow

```flip
[f>
if !age >= 18: { ec "Dewasa"; }

foreach !items as !item: { ec !item; }
</]
```

### Class

```flip
[f>
class User {
    init(!name) { this.name = !name; }
    function sapa() { return "Halo, " + this.name; }
}
var !user = new User("John");
ec !user.sapa();
</]
```

---

## 🔧 Fungsi Standar

| Fungsi | Keterangan |
|--------|------------|
| `str()`, `num()`, `len()` | Konversi & panjang |
| `upper()`, `lower()`, `trim()` | String |
| `push()`, `pop()`, `first()`, `last()` | Array |
| `file_exists()`, `read_file()`, `write_file()` | File I/O |

---

## 📁 Struktur Demo

```
app/
├── index.flip      # List (READ)
├── create.flip     # Form (CREATE)
├── store.flip      # Save handler
├── delete.flip     # Delete (DELETE)
├── controllers/
├── models/
└── data/users.txt
```

---

## 🌐 Menjalankan

```bash
# Download dari Release
flip.exe serve 8080 app

# Buka: http://127.0.0.1:8080/index.flip
```

---

## 📄 Lisensi

MIT License

---

<p align="center">
  <a href="https://github.com/fliplangdev/FlipLang">GitHub</a>
</p>
