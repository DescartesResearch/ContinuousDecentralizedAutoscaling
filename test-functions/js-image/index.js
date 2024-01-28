const express = require('express');
const sharp = require("sharp");
const imgGen = require("js-image-generator");
const app = express();

app.get('/', (req, res) => {
    const randDimensions = [10, 20, 30, 40];
    const dimIdx = ~~(Math.random() * 4);
    imgGen.generateImage(randDimensions[dimIdx], randDimensions[dimIdx], 100, (err, image) => {
        sharp(image.data).resize(randDimensions[~~(Math.random() * 5)], randDimensions[~~(Math.random() * 5)]).toBuffer();
    });
    res.send(`Hello World!\n`);
});

const port = process.env.PORT || 8080;
app.listen(port, () => {
    console.log('Hello world listening on port', port);
});