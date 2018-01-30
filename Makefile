image:
	docker build --tag hrls/auth .
	
push:
	docker push hrls/auth

run:
	docker run -it --rm --name=auth -p 5000:5000 hrls/auth

