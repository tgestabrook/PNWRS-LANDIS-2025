## Set up Docker

```sh
# Add Docker's official GPG key:
sudo apt-get update
sudo apt-get install ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc

# Add the repository to Apt sources:
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
  $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update

sudo apt-get install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

sudo docker run hello-world
```

## Clone Setup files
```shell
git clone https://github.com/tgestabrook/PNWRS-LANDIS-2025.git
cd PNWRS-LANDIS-2025/Docker_setup
docker build -t landis-ii_v8_linux .
```

## Run 
To run an interactive session, clone or copy your scenario directory to the VM and run:
```shell
#docker run -it --mount "type=bind,src=<SCENARIO_FOLDER_FULL_PATH_ON_COMPUTER>,dst=/scenarioFolder" <LANDIS_DOCKER_IMAGE_NAME>
docker run -it --mount "type=bind,src=$HOME/PNWRS-LANDIS-2025/,dst=/LANDIS" landis-ii_v8_linux
```
The path for `src` must be the full path of the scenario directory on the VM, e.g. `/home/user/LANDIS_scenario` and the path for `dst` should be whatever you want the scenario folder to be called 'inside' the docker image, e.g. `/LANDIS_scenario`. After running this command, the terminal should indicate that you are inside the docker image, and if you run `ls` you will see `/LANDIS_scenario`. You can then `cd` to it and run `$LANDIS_CONSOLE scenario.txt` to start the model run. The output files from the model run will appear in  `/home/user/LANDIS_scenario` and will be available outside the docker image. 

To automate model runs, just append the `cd` and `$LANDIS_CONSOLE` commands as follows:
```shell
docker run --mount "type=bind,src=<SCENARIO_FOLDER_FULL_PATH_ON_COMPUTER>,dst=/scenarioFolder" <LANDIS_DOCKER_IMAGE_NAME> /bin/sh -c "cd /scenarioFolder && dotnet $LANDIS_CONSOLE scenario.txt" 
```

