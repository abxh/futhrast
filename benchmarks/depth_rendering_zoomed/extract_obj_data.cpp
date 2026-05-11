
#include "../lib/github.com/abxh/lys/utils/tiny_obj_loader/tiny_obj_loader.h"

#include <algorithm>
#include <bit>
#include <cstdint>
#include <cstdio>
#include <filesystem>
#include <iostream>
#include <type_traits>

extern "C" bool
is_little_endian()
{
  return std::endian::native == std::endian::little;
}

extern "C" bool
is_big_endian()
{
  return std::endian::native == std::endian::big;
}

extern "C"
{

#include <assert.h>
#include <float.h>
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#define BINARY_FORMAT_VERSION 2
#define IS_BIG_ENDIAN (is_big_endian())
#define IS_LITTLE_ENDIAN (is_little_endian())

typedef int (*writer)(FILE*, const void*);
typedef int (*bin_reader)(void*);
typedef int (*str_reader)(char*, void*);

struct primtype_info_t
{
  const char binname[4 + 1]; // Used for parsing binary data.
  const char* type_name;     // Same name as in Futhark.
  const int64_t size;        // in bytes
  const writer write_str;    // Write in text format.
  const str_reader read_str; // Read in text format.
};

static void
remove_underscores(char* buf)
{
  char* w = buf;
  for (char* r = buf; *r; r++) {
    if (*r != '_') {
      *w++ = *r;
    }
  }
  *w++ = 0;
}

#define READ_STR(MACRO, PTR, SUFFIX)                                           \
  remove_underscores(buf);                                                     \
  int j;                                                                       \
  if (sscanf(buf, "%" MACRO "%n", (PTR*)dest, &j) == 1) {                      \
    return !(strcmp(buf + j, "") == 0 || strcmp(buf + j, SUFFIX) == 0);        \
  } else {                                                                     \
    return 1;                                                                  \
  }

static int
read_str_i64(char* buf, void* dest)
{
  READ_STR(SCNi64, int64_t, "i64");
}

static int
write_str_i64(FILE* out, const int64_t* src)
{
  return fprintf(out, "%" PRIi64 "i64", *src);
}

static int
read_str_f32(char* buf, void* dest)
{
  remove_underscores(buf);
  if (strcmp(buf, "f32.nan") == 0) {
    *(float*)dest = (float)NAN;
    return 0;
  } else if (strcmp(buf, "f32.inf") == 0) {
    *(float*)dest = (float)INFINITY;
    return 0;
  } else if (strcmp(buf, "-f32.inf") == 0) {
    *(float*)dest = (float)-INFINITY;
    return 0;
  } else {
    READ_STR("f", float, "f32");
  }
}

static int
write_str_f32(FILE* out, const float* src)
{
  float x = *src;
  if (isnan(x)) {
    return fprintf(out, "f32.nan");
  } else if (isinf(x) && x >= 0) {
    return fprintf(out, "f32.inf");
  } else if (isinf(x)) {
    return fprintf(out, "-f32.inf");
  } else {
    return fprintf(out, "%.*gf32", FLT_DECIMAL_DIG, x);
  }
}

static const struct primtype_info_t f32_info = { .binname = " f32",
                                                 .type_name = "f32",
                                                 .size = 4,
                                                 .write_str =
                                                   (writer)write_str_f32,
                                                 .read_str =
                                                   (str_reader)read_str_f32 };

static const struct primtype_info_t i64_info = { .binname = " i64",
                                                 .type_name = "i64",
                                                 .size = 8,
                                                 .write_str =
                                                   (writer)write_str_i64,
                                                 .read_str =
                                                   (str_reader)read_str_i64 };

static int
write_bin_array(FILE* out,
                const struct primtype_info_t* elem_type,
                const unsigned char* data,
                const int64_t* shape,
                int8_t rank)
{
  int64_t num_elems = 1;
  for (int64_t i = 0; i < rank; i++) {
    num_elems *= shape[i];
  }

  fputc('b', out);
  fputc((char)BINARY_FORMAT_VERSION, out);
  fwrite(&rank, sizeof(int8_t), 1, out);
  fwrite(elem_type->binname, 4, 1, out);
  if (shape != NULL) {
    fwrite(shape, sizeof(int64_t), (size_t)rank, out);
  }

  if (IS_BIG_ENDIAN) {
    for (int64_t i = 0; i < num_elems; i++) {
      const unsigned char* elem = data + i * elem_type->size;
      for (int64_t j = 0; j < elem_type->size; j++) {
        fwrite(&elem[elem_type->size - 1 - j], 1, 1, out);
      }
    }
  } else {
    assert(IS_LITTLE_ENDIAN);
    fwrite(data, (size_t)elem_type->size, (size_t)num_elems, out);
  }

  return 0;
}
}

static bool
process_obj(const std::filesystem::path& path, const std::filesystem::path& out)
{
  std::vector<std::int64_t> vis;
  std::vector<float> vx, vy, vz;
  std::vector<float> nx, ny, nz;
  std::vector<float> tu, tv;

  {
    tinyobj::ObjReaderConfig reader_config;
    reader_config.mtl_search_path =
      path.parent_path(); // Path to material files

    tinyobj::ObjReader reader;
    if (!reader.ParseFromFile(path, reader_config)) {
      if (!reader.Error().empty()) {
        std::cerr << "TinyObjReader: " << reader.Error();
      }
      return false;
    }

    if (!reader.Warning().empty()) {
      std::cout << "TinyObjReader: " << reader.Warning();
    }

    auto& shapes = reader.GetShapes();
    auto& attrib = reader.GetAttrib();

    assert(attrib.vertices.size() % 3 == 0);
    const size_t num_vertices = attrib.vertices.size() / 3;
    vx.reserve(num_vertices);
    vy.reserve(num_vertices);
    vz.reserve(num_vertices);

    for (size_t i = 0; i < num_vertices; i++) {
      vx.push_back(attrib.vertices[3 * i + 0]);
      vy.push_back(attrib.vertices[3 * i + 1]);
      vz.push_back(attrib.vertices[3 * i + 2]);
    }

    size_t total_vertex_indices = 0;
    for (const auto& shape : shapes) {
      for (size_t f = 0; f < shape.mesh.num_face_vertices.size(); f++) {
        total_vertex_indices += shape.mesh.num_face_vertices[f];
        assert(shape.mesh.num_face_vertices[f] == 3 &&
               "assuming only triangles are loaded");
      }
    }
    vis.reserve(total_vertex_indices);

    assert(attrib.normals.size() % 3 == 0);
    const size_t num_normals = attrib.normals.size() / 3;
    nx.reserve(num_normals);
    ny.reserve(num_normals);
    nz.reserve(num_normals);

    assert(attrib.texcoords.size() % 2 == 0);
    const size_t num_texcoords = attrib.texcoords.size() / 2;
    tu.reserve(num_texcoords);
    tv.reserve(num_texcoords);

    for (size_t s = 0; s < shapes.size(); s++) {
      size_t index_offset = 0;
      for (size_t f = 0; f < shapes[s].mesh.num_face_vertices.size(); f++) {
        size_t fv = size_t(shapes[s].mesh.num_face_vertices[f]);

        for (size_t v = 0; v < fv; v++) {
          tinyobj::index_t idx = shapes[s].mesh.indices[index_offset + v];

          assert(idx.vertex_index >= 0);
          vis.push_back(std::int64_t(idx.vertex_index));

          if (idx.normal_index >= 0) {
            nx.push_back(attrib.normals[3 * size_t(idx.normal_index) + 0]);
            ny.push_back(attrib.normals[3 * size_t(idx.normal_index) + 1]);
            nz.push_back(attrib.normals[3 * size_t(idx.normal_index) + 2]);
          }
          if (idx.texcoord_index >= 0) {
            tu.push_back(attrib.texcoords[2 * size_t(idx.texcoord_index) + 0]);
            tv.push_back(attrib.texcoords[2 * size_t(idx.texcoord_index) + 1]);
          }
        }
        index_offset += fv;
      }
    }
  }

  assert(vx.size() > 0);
  assert(vy.size() > 0);
  assert(vz.size() > 0);
  assert(vis.size() > 0);

  const auto file_path = std::string(out) + ".in";
  FILE* f = std::fopen(file_path.c_str(), "wb");

  {
    const std::int64_t shape[] = { (std::int64_t)vx.size() };
    write_bin_array(f,
                    &f32_info,
                    reinterpret_cast<unsigned char*>(vx.data()),
                    shape,
                    sizeof(shape) / sizeof(*shape));
  }
  {
    const std::int64_t shape[] = { (std::int64_t)vy.size() };
    write_bin_array(f,
                    &f32_info,
                    reinterpret_cast<unsigned char*>(vy.data()),
                    shape,
                    sizeof(shape) / sizeof(*shape));
  }
  {
    const std::int64_t shape[] = { (std::int64_t)vz.size() };
    write_bin_array(f,
                    &f32_info,
                    reinterpret_cast<unsigned char*>(vz.data()),
                    shape,
                    sizeof(shape) / sizeof(*shape));
  }
  {
    const std::int64_t shape[] = { (std::int64_t)vis.size() };
    write_bin_array(f,
                    &i64_info,
                    reinterpret_cast<unsigned char*>(vis.data()),
                    shape,
                    sizeof(shape) / sizeof(*shape));
  }
  std::fclose(f);
  return true;
}

int
main(int argc, char* argv[])
{
  if (argc <= 1) {
    std::cout << "Usage: " << (argc == 1 ? argv[0] : "program")
              << " <path-to-obj-file>\n";
    return EXIT_SUCCESS;
  }
  namespace fs = std::filesystem;
  fs::path path(argv[1]);
  if (!fs::exists(path)) {
    std::cerr << "file path " << path << " does not exist\n";
    return EXIT_FAILURE;
  }
  if (!fs::is_regular_file(path)) {
    std::cerr << "file path " << path << " is not a regular file\n";
    return EXIT_FAILURE;
  }
  if (path.extension() != ".obj") {
    std::cerr << "unsupported file type: " << path.extension() << "\n";
    return EXIT_FAILURE;
  }
  const auto out = path.stem();
  process_obj(path, out);
}
