# FlipLang

<p align="center">
  <img src="https://img.shields.io/badge/Made%20with-Zig-F7A41D?style=for-the-badge&logo=zig&logoColor=white" alt="Made with Zig">
  <img src="https://img.shields.io/badge/License-MIT-blue.svg?style=for-the-badge" alt="MIT License">
  <img src="https://img.shields.io/badge/Platform-Windows%20%7C%20Linux%20%7C%20macOS-lightgrey?style=for-the-badge" alt="Platform">
</p>

<p align="center">
  <strong>Bahasa Pemrograman Modern dengan HTML Templating</strong><br>
  OOP Support • MVC Architecture • Built-in Web Server • CGI Support
</p>

---

## 🚀 Fitur Utama

- **HTML-First Templating** - Tag `[f>...</]` untuk kode dan `[=>...</]` untuk output
- **OOP Support** - Classes, constructors, methods, dan inheritance
- **MVC Architecture** - Model, View, Controller pattern
- **Built-in Web Server** - HTTP server terintegrasi
- **CGI Support** - Variabel `!_GET`, `!_POST`, `!_SERVER`
- **File Storage** - Baca/tulis file dengan mudah
- **Bootstrap 5 Ready** - Tampilan modern out-of-the-box

---

## 📦 Instalasi

### Download Binary

Download executable dari [Releases](https://github.com/nestapa/FlipLang/releases):

```bash
# Windows
flip.exe serve 8080 app

# Linux/macOS
./flip serve 8080 app
```

### Build dari Source

Pastikan Zig sudah terinstall (versi 0.16+):

```bash
git clone https://github.com/nestapa/FlipLang.git
cd FlipLang
zig build -Doptimize=ReleaseFast
```

Executable akan tersedia di `zig-out/bin/flip`.

---

## 📖 Sintaks Dasar

### Variabel

Semua variabel diawali dengan `!`:

```flip
[f>
var !name = "FlipLang";
var !age = 25;
var !items = [1, 2, 3];
var !user = {"name": "John", "email": "john@example.com"};
</]
```

### Output

```flip
[f> ec "Hello World"; </]

<p>Nama: [=> !name </]</p>
<p>Umur: [=> !age </] tahun</p>
```

### Control Flow

```flip
[f>
// Kondisi
if !age >= 18: {
    ec "Dewasa";
} else: {
    ec "Anak-anak";
}

// Loop
foreach !items as !item: {
    ec !item;
}

// While loop
var !i = 0;
while !i < 5: {
    ec !i;
    !i = !i + 1;
}
</]
```

### Fungsi

```flip
[f>
function tambah(!a, !b) {
    return !a + !b;
}

var !hasil = tambah(10, 20);
ec !hasil;  // Output: 30
</]
```

### Class (OOP)

```flip
[f>
class User {
    init(!name, !email) {
        this.name = !name;
        this.email = !email;
    }
    
    function sapa() {
        return "Halo, " + this.name + "!";
    }
    
    function getInfo() {
        return this.name + " (" + this.email + ")";
    }
}

var !user = new User("John", "john@example.com");
ec !user.sapa();      // Output: Halo, John!
ec !user.getInfo();   // Output: John (john@example.com)
</]
```

---

## 🔧 Fungsi Bawaan

### Konversi & Utilitas

| Fungsi | Keterangan | Contoh |
|--------|------------|--------|
| `str(!val)` | Konversi ke string | `str(123)` → `"123"` |
| `num(!val)` | Konversi ke number | `num("123")` → `123` |
| `len(!val)` | Panjang string/array | `len("hello")` → `5` |
| `type(!val)` | Tipe data | `type([1,2])` → `"array"` |

### String

| Fungsi | Keterangan | Contoh |
|--------|------------|--------|
| `upper(!str)` | Huruf besar | `upper("hello")` → `"HELLO"` |
| `lower(!str)` | Huruf kecil | `lower("HELLO")` → `"hello"` |
| `trim(!str)` | Hapus spasi | `trim("  hi  ")` → `"hi"` |
| `split(!str, !sep)` | Pecah string | `split("a,b", ",")` → `["a","b"]` |
| `replace(!str, !old, !new)` | Ganti substring | `replace("hi", "i", "ello")` |

### Array

| Fungsi | Keterangan | Contoh |
|--------|------------|--------|
| `push(!arr, !item)` | Tambah item | `push(!arr, "new")` |
| `pop(!arr)` | Ambil & hapus terakhir | `pop(!arr)` |
| `first(!arr)` | Item pertama | `first([1,2,3])` → `1` |
| `last(!arr)` | Item terakhir | `last([1,2,3])` → `3` |
| `join(!arr, !sep)` | Gabung array | `join(["a","b"], "-")` → `"a-b"` |

### File I/O

| Fungsi | Keterangan |
|--------|------------|
| `file_exists(!path)` | Cek file ada |
| `read_file(!path)` | Baca file |
| `write_file(!path, !content)` | Tulis file |
| `append_file(!path, !content)` | Tambah ke file |
| `delete_file(!path)` | Hapus file |

---

## 🌐 Web Development

### CGI Variables

FlipLang menyediakan variabel CGI untuk web development:

```flip
[f>
// Query string: ?name=John&age=25
var !name = !_GET["name"];

// Form POST data
var !email = !_POST["email"];

// Server info
var !method = !_SERVER["REQUEST_METHOD"];
var !uri = !_SERVER["REQUEST_URI"];
</]
```

### Contoh Form Handling

```flip
<!DOCTYPE html>
<html>
<head><title>Form Example</title></head>
<body>
    [f>
    if !_SERVER["REQUEST_METHOD"] == "POST": {
        var !name = !_POST["name"];
        ec "<p>Hello, " + !name + "!</p>";
    } else: {
    </]
    
    <form method="POST">
        <input type="text" name="name" placeholder="Nama">
        <button type="submit">Submit</button>
    </form>
    
    [f> } </]
</body>
</html>
```

---

## 📁 Struktur Proyek MVC

```
app/
├── index.flip           # Halaman utama (List)
├── create.flip          # Form create
├── store.flip           # Handler simpan
├── edit.flip            # Form edit
├── update.flip          # Handler update
├── delete.flip          # Handler delete
├── controllers/
│   └── UserController.flip
├── models/
│   └── User.flip
├── views/
│   └── layout.flip
└── data/
    └── users.txt        # File storage
```

---

## 🚀 Menjalankan Server

```bash
# Jalankan server di port 8080, folder app
flip serve 8080 app

# Buka browser
# http://127.0.0.1:8080/index.flip
```

---

## 📝 Contoh Lengkap

Lihat folder `app/` untuk contoh aplikasi CRUD lengkap dengan:
- List users
- Create user
- Edit user
- Delete user
- File-based storage

---

## 🤝 Kontribusi

Kontribusi sangat diterima! Silakan:

1. Fork repository ini
2. Buat branch fitur (`git checkout -b fitur-baru`)
3. Commit perubahan (`git commit -m 'Tambah fitur baru'`)
4. Push ke branch (`git push origin fitur-baru`)
5. Buat Pull Request

---

## 📄 Lisensi

MIT License - lihat file [LICENSE](LICENSE) untuk detail.

---

<p align="center">
  <strong>FlipLang</strong> - Bahasa pemrograman sederhana untuk web development<br>
  <a href="https://github.com/nestapa/FlipLang">GitHub</a> •
  <a href="https://github.com/nestapa/FlipLang/issues">Issues</a> •
  <a href="https://github.com/nestapa/FlipLang/releases">Releases</a>
</p>
