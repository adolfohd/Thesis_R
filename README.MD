
lib: has the basic R functions I need for data curation and other stuff.
data: intended to store the raw and curated datasets


Dockerfile: is needed to create and update Docker container 
	-> docker_image => fitohd/thesis_r

install.R: has the libraries that are to be installed inside the container. The container will have to be updated if any additional packages are needed during execution using this container. 