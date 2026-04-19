# Boola

Boola can be deployed as a single Scala service. The backend serves the `/api` endpoints and also hosts the static files in `frontend/`.

## Live Demo

Boola is publicly available at:

```text
https://www.boola.io
```

The `www` subdomain redirects to the primary domain:

```text
https://boola.io
```

## Local Run

Requirements:

- Java 21
- sbt

Start the app:

```bash
sbt run
```

The default address is `http://localhost:8080`.

You can also override the host and port with environment variables:

```bash
PORT=8080 HOST=0.0.0.0 sbt run
```

## Docker Deployment

This repository already includes a `Dockerfile`, so you can build and run it directly:

```bash
docker build -t obdd-visualization .
docker run -p 8080:8080 -e PORT=8080 obdd-visualization
```

Then open:

```text
http://localhost:8080
```

Health check endpoint:

```text
GET /api
```

## Cloud Deployment

Because the application reads `PORT`, it is suitable for Docker-based platforms such as:

- Render
- Railway
- Fly.io
- A VPS with Docker

## Render Deployment

This project is ready to deploy to Render as a Docker-based web service.

The repository includes a `render.yaml` file with:

- service name: `boola`
- runtime: `docker`
- plan: `free`
- health check path: `/api`

Manual setup in the Render dashboard:

1. Push the repository to GitHub.
2. In Render, create a new `Web Service`.
3. Connect the repository.
4. Render should detect the included `Dockerfile`.
5. Confirm the service settings and deploy.

After the first successful deploy, the service will be available on a Render subdomain.

Important note for the free plan:

- Free web services spin down after 15 minutes without inbound traffic.
- The next request may take around one minute while the service spins back up.

Typical workflow:

1. Push the repository to GitHub.
2. Create a new web service from the repository on your hosting platform.
3. Choose Docker-based deployment.
4. Let the platform build the image and inject the runtime port.

If the platform asks for a health check path, use:

```text
/api
```

## Custom Domain

This project is currently configured to use the `boola.io` domain.

If you already own `boola.io`, the usual steps are:

1. Deploy the application to Render, Railway, Fly.io, or your own VPS.
2. Add `boola.io` as a custom domain in the hosting platform.
3. Configure DNS records in your domain registrar dashboard as required by the platform.
4. Wait for the platform to issue the HTTPS certificate.

Common DNS setup:

- Root domain `boola.io`: usually an `A` record or `ALIAS/ANAME`
- Subdomain `www.boola.io`: usually a `CNAME`

Recommended redirect policy:

- Use `boola.io` as the primary domain
- Redirect `www.boola.io` to `boola.io` with HTTP 301

## Non-Docker Deployment

You can also package the app as a runnable jar:

```bash
sbt assembly
java -jar target/scala-3.3.3/obdd-backend-assembly-0.1.0-SNAPSHOT.jar
```

Note: keep the `frontend/` directory alongside the running application, because the home page and static assets are served from that folder.
