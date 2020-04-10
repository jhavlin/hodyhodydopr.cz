const LIST = 'list';
const LAST_ID = 'last-id';
const DATA = id => `data-${id}`;
const INITIAL_EGG = { localId: 1, typeId: "sd", local: true, title: "", message: "" };

function getLocalJson(key, fallbackValue) {
    const str = localStorage.getItem(key);
    if (str) {
        try {
            return JSON.parse(str);
        } catch (e) {
            return fallbackValue;
        }
    }
    return fallbackValue;
}

function setLocalJson(key, value) {
    localStorage.setItem(key, JSON.stringify(value));
}

let app;

function handleRotateBarTouchEvent(evt, port) {
    const el = document.elementFromPoint(evt.touches[0].clientX, evt.touches[0].clientY);
    if (el) {
        const segment = el.getAttribute('data-segment-index');
        if (segment) {
            port.send(parseInt(segment));
        }
    }
}

function handleRotateBarTouchStart(evt) {
    handleRotateBarTouchEvent(evt, app.ports.rotateBarTouchStarted);
}

function handleRotateBarTouchMove(evt) {
    handleRotateBarTouchEvent(evt, app.ports.rotateBarTouchMoved);
}

function preventMenu(evt) {
    evt.preventDefault();
    return false;
}

function handleEggTouchEvent(evt, port) {
    if (evt.touches.length > 1) {
        return false;
    }
    const el = document.elementFromPoint(evt.touches[0].clientX, evt.touches[0].clientY);
    if (el) {
        const layer = el.getAttribute('data-layer-index');
        const segment = el.getAttribute('data-segment-index');
        if (layer && segment) {
            port.send([parseInt(layer), parseInt(segment)]);
            return true;
        }
    }
    return false;
}

function handleEggTouchStart(evt) {
    // handleEggTouchEvent(evt, app.ports.eggTouchStarted)
}

function handleEggTouchMove(evt) {
    const drawn = handleEggTouchEvent(evt, app.ports.eggTouchMoved)
    if (drawn) {
        evt.preventDefault();
    }
}

async function loadEgg(urlInfo) {
    if (urlInfo.urlType === 'show') {
        const result = await get('/data/v01/get/' + urlInfo.key);
        const json = await decryptJson(result.encryptedData, result.iv, urlInfo.secret);
        if (json.title) {
            document.title = json.title;
        }
        app.ports.remoteEggLoaded.send(json);
    } else if (urlInfo.urlType === 'local' || urlInfo.urlType === 'implicit') {
        let localId;
        if (urlInfo.localId) {
            localId = urlInfo.localId;
        } else {
            localId = parseInt(localStorage.getItem(LAST_ID) || 1);
        }
        const colors = getLocalJson(`l-${localId}`, []);
        if (!colors.length && ((localId > 1) || (localId === 1 && getLocalJson(LIST, []).filter(i => i.localId !== 1).length > 0))) {
            app.ports.eggNotFound.send({});
        } else {
            app.ports.localEggLoaded.send({ localId, colors });
        }
    }
}

async function sha256(message) {
    // https://stackoverflow.com/questions/18338890/are-there-any-sha-256-javascript-implementations-that-are-generally-considered-t
    // encode as UTF-8
    const msgBuffer = new TextEncoder('utf-8').encode(message);
    // hash the message
    const hashBuffer = await crypto.subtle.digest('SHA-256', msgBuffer);
    // convert ArrayBuffer to Array
    const hashArray = Array.from(new Uint8Array(hashBuffer));
    // convert bytes to hex string
    const hashHex = hashArray.map(b => ('00' + b.toString(16)).slice(-2)).join('');
    return hashHex;
}

const verify = async (base, requiredPrefix) => {
    const chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'.split('');
    const queue = [''];
    let start = 0;
    const tryNext = () => {
        const prefix = queue[start++];
        chars.forEach((c) => {
            queue.push(prefix + c);
        });
        return sha256(base + prefix).then((hash) => {
            if (hash.startsWith(requiredPrefix)) {
                return prefix;
            } else {
                return tryNext()
            }
        });
    }
    return Promise.resolve().then(tryNext);
}

const list = getLocalJson(LIST, [ INITIAL_EGG ]);

app = Elm.Main.init({
    node: document,
    flags: {
        implicitLocalId: parseInt(localStorage.getItem(LAST_ID) || INITIAL_EGG.localId),
        list: list.length ? list : [ INITIAL_EGG ],
    }
});

app.ports.initApp.subscribe(function(urlInfo) {
    loadEgg(urlInfo);
});

app.ports.initTouch.subscribe(function() {
    setTimeout(function() {
        const rotateBar = document.getElementById('rotate-bar');
        if (rotateBar) {
            rotateBar.removeEventListener('touchstart', handleRotateBarTouchStart);
            rotateBar.addEventListener('touchstart', handleRotateBarTouchStart);
            rotateBar.removeEventListener('touchmove', handleRotateBarTouchMove);
            rotateBar.addEventListener('touchmove', handleRotateBarTouchMove);
            rotateBar.removeEventListener('contextmenu', preventMenu);
            rotateBar.addEventListener('contextmenu', preventMenu);
        }
        const pictureEgg = document.getElementById('picture-egg');
        if (pictureEgg) {
            pictureEgg.removeEventListener('touchstart', handleEggTouchStart);
            pictureEgg.addEventListener('touchstart', handleEggTouchStart);
            pictureEgg.removeEventListener('touchmove', handleEggTouchMove);
            pictureEgg.addEventListener('touchmove', handleEggTouchMove);
            pictureEgg.removeEventListener('contextmenu', preventMenu);
            pictureEgg.addEventListener('contextmenu', preventMenu)
        }
    }, 100);
});

app.ports.copyToClipboard.subscribe(function(text) {
    navigator.clipboard.writeText(text);
});

app.ports.saveEggAndList.subscribe(function({ localId, colors, list }) {
    setLocalJson(`l-${localId}`, colors);
    setLocalJson(LIST, list);
    setLocalJson(LAST_ID, localId);
});

app.ports.deleteEgg.subscribe(function({ localId, list }) {
    localStorage.removeItem(`l-${localId}`);
    setLocalJson(LIST, list);
    setLocalJson(LAST_ID, list[0].localId);
});

app.ports.loadEgg.subscribe(function(urlInfo) {
    loadEgg(urlInfo);
});

app.ports.saveList.subscribe(function(list) {
     setLocalJson(LIST, list);
     setLocalJson(LAST_ID, list && list[0].localId);
});

app.ports.saveOnline.subscribe(async function({ colors, eggInfo }) {
    const data = {
        colors,
        typeId: eggInfo.typeId,
        title: eggInfo.title,
        message: eggInfo.message,
    };
    let version = eggInfo.onlineVersion;
    if (eggInfo.key) {
        if (eggInfo.secret && eggInfo.evidence) {
            const { encryptedData, iv } = await encryptJson(data, eggInfo.secret);
            const trySave = async () => {
                const toSend = {
                    encryptedData,
                    iv,
                    evidence: eggInfo.evidence,
                    previousVersion: version,
                    projectKey: eggInfo.key,
                    version: version + 1,
                }
                const result = await post('/data/v01/update', toSend);
                if (result.success) {
                    app.ports.savedOnline.send({
                        localId: eggInfo.localId,
                        key: eggInfo.key,
                        secret: eggInfo.secret,
                        evidence: eggInfo.evidence,
                        onlineVersion: result.version,
                    });
                } else if (!result.success && result.version) {
                    version = result.version;
                    trySave();
                } else if (!result.success && !result.version) {
                    // some bigger problem
                }
            }
            trySave();
        }
    } else {
        try {
            const evidence = byteArrayToHex(generateRandomByteArray(32));
            data.evidence = evidence;
            const key = await generateKey();
            const { encryptedData, iv } = await encryptJson(data, key);

            const trySave = async() => {
                const requestKeyResult = await get('/data/v01/requestProjectKey');
                const keyVerification = await verify(requestKeyResult.projectKey, requestKeyResult.requiredHashPrefix);
                const body = {
                    encryptedData,
                    iv,
                    evidence,
                    projectKey: requestKeyResult.projectKey,
                    keyVerification,
                };
                const result = await post('/data/v01/create', body);
                if (result.result === 'key_used') {
                    trySave()
                } else if (result.result === 'verification_failed') {
                    // that is bad
                } else if (result.result === 'created') {
                    app.ports.savedOnline.send({
                        localId: eggInfo.localId,
                        key: result.projectKey,
                        secret: key,
                        evidence,
                        onlineVersion: 0,
                    });
                }
            }
            trySave();
        } catch (e) {
            console.log(e);
        }
    }
});
