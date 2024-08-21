# Scaling Executor Kubernetes

## Description
A simple scaling executor for decentralized autoscaling in Kubernetes clusters.

## Usage
To build the Docker container:

```
docker build -t scaling-executor-kubernetes:latest .
```

To push image to Docker Hub:

```
docker push scaling-executor-kubernetes
```

To deploy in Kubernetes Cluster:

```
kubectl apply -f deploy_kubernetes.yaml
```
