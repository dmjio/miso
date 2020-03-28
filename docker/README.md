## Docker Based Workflow

It is possible to edit and run miso applications with docker and docker-compose as the only dependencies on the host system.

0. On Windows and Mac open your docker settings and increase the Memory and Swap (I use Memory=6GB and Swap=2GB), otherwise the container will be killed when exceeding the limit.
1. Run `git clone https://github.com/dmjio/miso` and `cd` into `miso/docker`
1. Run `docker-compose build`, this will take a long time
1. Run `docker-compose up`
1. `miso/sample-app` has now the build in `\dist-newstyle\build\x86_64-linux\ghcjs-8.6.0.1\app-0.1.0.0\x\app\build\app\app.jsexe`
1. Open `index.html` on your host-machine and edit any file in `miso/sample-app` and docker will automatically rebuild the app
