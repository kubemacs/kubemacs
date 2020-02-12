#!/bin/bash -e

function checkEnvForEmpty() {
    property=$1
    value=$2

    if [ ! -n "$value" ]; then
        echo "[error] environment variable '$property' is empty, please define it"
        return 1
    fi
}

function execPrintOutputIfFailure() {
    command=$*

    if [ "$KUBEMACS_INIT_DEBUG" = true ]; then
        $command
        return $?
    fi
    OUTPUT="$(${command} 2>&1)"
    exitCode=$?
    if [ ! "$exitCode" -eq 0 ]; then
        echo "$OUTPUT"
    fi
    return $exitCode
}

function promptEnterOrNo() {
    read -p "$*" prompt
    if [ "$prompt" = "n" ]; then
        return false
    fi
    return
}

KUBEMACS_IMAGE="${KUBEMACS_IMAGE:-}"
KUBEMACS_INIT_SELF_CONTAINER_NAME="${KUBEMACS_INIT_SELF_CONTAINER_NAME:-kubemacs-init}"
KUBEMACS_INIT_DEBUG="${KUBEMACS_INIT_DEBUG:-false}"
KUBEMACS_HOST_KUBECONFIG_NAME="${KUBEMACS_HOST_KUBECONFIG_NAME:-config-kind-my-cluster}"
KUBEMACS_DEFAULT_KIND_NAME="${KUBEMACS_DEFAULT_KIND_NAME:-kind}"
KUBEMACS_GIT_EMAIL="${KUBEMACS_GIT_EMAIL:-}"
KUBEMACS_GIT_NAME="${KUBEMACS_GIT_NAME:-}"
KUBEMACS_TIMEZONE="${KUBEMACS_TIMEZONE:-Pacific/Auckland}"
KUBEMACS_INIT_DEFAULT_REPOS="${KUBEMACS_INIT_DEFAULT_REPOS:-}"
KUBEMACS_INIT_DEFAULT_DIR="${KUBEMACS_INIT_DEFAULT_DIR:-/home/ii}"
KUBEMACS_INIT_ORG_FILE="${KUBEMACS_INIT_ORG_FILE:-}"

if [ $KUBEMACS_INIT_DEBUG = true ]; then
    set -x
fi

# RUN example
#
# docker run -it --rm --privileged --name kubemacs-init \
#    -v $HOME/.kube:/tmp/.kube \
#    -v /var/run/docker.sock:/var/run/docker.sock \
#    -e KUBEMACS_GIT_EMAIL="myemail@example.com" \
#    -e KUBEMACS_GIT_NAME="My Name" \
#    -e KUBEMACS_INIT_DEFAULT_REPOS="https://github.com/cncf/apisnoop" \
#    -e KUBEMACS_INIT_ORG_FILE="\$HOME/apisnoop/deployment/k8s/xip.io/README.org" \
#    --user root \
#    gcr.io/kubemacs/kubemacs:latest /usr/local/bin/self-init.sh

cat <<EOF
| Kubemacs config                                                      |
| -------------------------------------------------------------------- |
| Property                          | Default value    | Current value |
| --------------------------------- | ---------------- | ------------- |
| KUBEMACS_IMAGE                    |                  | $KUBEMACS_INIT_DEBUG |
| KUBEMACS_INIT_SELF_CONTAINER_NAME | kubemacs-init    | $KUBEMACS_INIT_DEBUG |
| KUBEMACS_INIT_DEBUG               | false            | $KUBEMACS_INIT_DEBUG |
| KUBEMACS_HOST_KUBECONFIG_NAME     | config-kind      | $KUBEMACS_INIT_DEBUG |
| KUBEMACS_DEFAULT_KIND_NAME        | kind             | $KUBEMACS_DEFAULT_KIND_NAME |
| KUBEMACS_GIT_EMAIL                |                  | $KUBEMACS_GIT_EMAIL |
| KUBEMACS_GIT_NAME                 |                  | $KUBEMACS_GIT_NAME|
| KUBEMACS_TIMEZONE                 | Pacific/Auckland | $KUBEMACS_TIMEZONE |
| KUBEMACS_INIT_DEFAULT_REPOS       |                  | $KUBEMACS_INIT_DEFAULT_REPOS |
| KUBEMACS_INIT_DEFAULT_DIR         | /home/ii         | $KUBEMACS_INIT_DEFAULT_DIR |
| KUBEMACS_INIT_ORG_FILE            |                  | $KUBEMACS_INIT_ORG_FILE |

EOF

checkEnvForEmpty KUBEMACS_GIT_EMAIL "$KUBEMACS_GIT_EMAIL" || exit 1
checkEnvForEmpty KUBEMACS_GIT_NAME  "$KUBEMACS_GIT_NAME"  || exit 1

promptEnterOrNo "Are you happy with using the config above? [enter|n] " || exit 1

if ! ( [ -f /.dockerenv ] || [ -f /run/.containerenv ] ); then
    echo "[error] this must run in a container"
    exit 1
fi

if ! docker info 2>&1 > /dev/null; then
    echo "[error] cannot talk to the Docker socket"
    exit 1
fi

if ! ( docker ps | grep "$KUBEMACS_INIT_SELF_CONTAINER_NAME" 2>&1 > /dev/null && docker exec -it "$KUBEMACS_INIT_SELF_CONTAINER_NAME" test -f /usr/local/bin/self-init.sh 2>&1 > /dev/null ) then
   echo "[error] could not find $KUBEMACS_INIT_SELF_CONTAINER_NAME container, please name this container accordingly"
   exit 1
fi

if ! mountpoint -q -- /tmp/.kube; then
    echo "[error] directory '/tmp/.kube' is not a mountpoint, please mount your '\$HOME/.kube' folder."
    exit 1
fi

if ! touch "/tmp/.kube/$KUBEMACS_HOST_KUBECONFIG_NAME"; then
    echo "[error] cannot write to '/tmp/.kube', please check permissions"
fi

if kind get clusters | grep "$KUBEMACS_DEFAULT_KIND_NAME" 2>&1 > /dev/null; then
    if promptEnterOrNo "There appears to be a Kind cluster existing called '$KUBEMACS_DEFAULT_KIND_NAME', is it OK to delete it and recreate it? [enter|n] "; then
        execPrintOutputIfFailure kind delete cluster --name "$KUBEMACS_DEFAULT_KIND_NAME"
    fi
fi

echo "[status] creating kind cluster"
execPrintOutputIfFailure kind create cluster --name "$KUBEMACS_DEFAULT_KIND_NAME" --config /usr/share/kubemacs/kind-cluster-config.yaml

execPrintOutputIfFailure kubectl config set clusters.kind-"$KUBEMACS_DEFAULT_KIND_NAME".server "https://127.0.0.1:6443"

sleep 7s
if ! kubectl cluster-info 2>&1 > /dev/null; then
    echo "[error] unable to talk to newly created cluster"
    exit 1
fi

echo "[status] copying KUBECONFIG back to host"
execPrintOutputIfFailure cp -f /root/.kube/config "/tmp/.kube/$KUBEMACS_HOST_KUBECONFIG_NAME"

if [ -z "$KUBEMACS_IMAGE" ]; then
    KUBEMACS_IMAGE="$(docker ps | grep $KUBEMACS_INIT_SELF_CONTAINER_NAME | awk '{$2=$2};1' | cut -d' ' -f2)"
fi

KUBEMACS_IMAGE_NAME=$(echo $KUBEMACS_IMAGE | cut -d':' -f1)
KUBEMACS_IMAGE_TAG=$(echo $KUBEMACS_IMAGE | cut -d':' -f2)

echo "[status] writing kubemacs-kustomize.yaml"
cat <<EOF > /root/kustomization.yaml
apiVersion: kustomize.config.k8s.io/v1beta1
kind: Kustomization
# namespace: apisnoop
# resources:
#   - basic-auth.yaml
  # - namespace.yaml
bases:
  - ../usr/share/kubemacs/apisnoop/deployment/k8s/kubemacs
configMapGenerator:
- name: kubemacs-configuration
  behavior: merge
  literals:
  - TZ=${KUBEMACS_TIMEZONE}
  - GIT_EMAIL=${KUBEMACS_GIT_EMAIL}
  - GIT_NAME=${KUBEMACS_GIT_NAME}
  - INIT_DEFAULT_REPOS=${KUBEMACS_INIT_DEFAULT_REPOS}
  - INIT_DEFAULT_DIR=${KUBEMACS_INIT_DEFAULT_DIR}
  - INIT_ORG_FILE=${KUBEMACS_INIT_ORG_FILE}
images:
  - name: $KUBEMACS_IMAGE_NAME
    newTag: $KUBEMACS_IMAGE_TAG
EOF

echo "[status] loading KUBEMACS image into Kind from Docker"
execPrintOutputIfFailure kind load docker-image --name "$KUBEMACS_DEFAULT_KIND_NAME" --nodes "$KUBEMACS_DEFAULT_KIND_NAME-worker" "$KUBEMACS_IMAGE"

echo "[status] bringing up kubemacs in Kind"
execPrintOutputIfFailure kubectl apply -k /root

echo "[status] complete!"

cat <<EOF | /usr/local/bin/osc52.sh
export KUBECONFIG=~/.kube/$KUBEMACS_HOST_KUBECONFIG_NAME
kubectl exec -it kubemacs-0 -- attach
EOF

cat <<EOF

On your host, run:
  export KUBECONFIG=~/.kube/$KUBEMACS_HOST_KUBECONFIG_NAME
  kubectl -n ii exec -it kubemacs-0 -- attach
or paste in a terminal what was added to your clipboard
EOF
