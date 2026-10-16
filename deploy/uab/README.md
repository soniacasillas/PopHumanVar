# UAB deployment (primary service)

Target: AlmaLinux 9.8, rootless Podman, local MariaDB, existing VCF files at
`/var/www/pophuman/files/vcf`, and Apache as the public reverse proxy.

## Administrator prerequisites

Ask the administrator to:

1. Create `/var/www/pophumanvar` owned by the deployment user, while preserving
   the existing VCF directory as read-only input.
2. Create the databases and accounts using
   `deploy/database/create-uab-databases.sql`.
3. Enable lingering for the deployment account so its user service survives
   logout: `loginctl enable-linger scasillas`.
4. Load Apache proxy modules and add `apache-pophumanvar.conf` to the HTTPS
   virtual host. During testing the URL is
   `https://mrb-38-179.uab.cat/pophumanvar/`; later the root proxy belongs in
   the `pophumanvar.uab.cat` virtual host.
5. Confirm the deployment user can read the VCF tree and that Apache can reach
   `127.0.0.1:8080` under the active SELinux policy.

## Deployment user steps

Clone the repository into `/var/www/pophumanvar/app`, then:

```sh
cd /var/www/pophumanvar/app
podman build -t localhost/pophumanvar:1.0.0 -f Containerfile .

mkdir -p /var/www/pophumanvar/config
cp deploy/uab/pophumanvar.env.example /var/www/pophumanvar/config/pophumanvar.env
chmod 600 /var/www/pophumanvar/config/pophumanvar.env
```

Edit the private environment file and use the read-only application database
password. Then install the user Quadlet:

```sh
mkdir -p ~/.config/containers/systemd
cp deploy/uab/pophumanvar.container ~/.config/containers/systemd/
systemctl --user daemon-reload
systemctl --user enable --now pophumanvar.service
systemctl --user status pophumanvar.service
curl -I http://127.0.0.1:8080/
```

View logs with `journalctl --user -u pophumanvar.service -f`. A code update is
deployed by pulling the reviewed commit, rebuilding the same image tag, and
restarting the service.

The container deliberately listens only on loopback. Apache is the only public
entry point. The VCF directory is mounted read-only and the application writes
only short-lived files under its container temporary directory.

