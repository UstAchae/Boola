# Boola

Boola is a Web-based interactive visualisation tool for learning BDD/OBDD algorithms.

## Requirements

- Java 17 or later
- sbt

## Run locally

From this directory, run:

```powershell
sbt run
```

Then open:

```text
http://localhost:8080
```

The Scala backend serves both the `/api` routes and the static frontend files in `frontend/`.

## Docker

The project also includes a Dockerfile:

```powershell
docker build -t boola .
docker run -p 8080:8080 boola
```

## Notes

Generated build folders such as `target/`, `.sbt-boot/`, `.sbt-global/`, `.metals/`, `.bsp/`, and `project/target/` should not be committed.
