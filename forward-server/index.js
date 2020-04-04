const path = require("path");
const express = require("express");
const bodyParser = require("body-parser");
const cors = require("cors");
const csv = require("csvtojson");

const app = express();
const port = 9000;

app.use(cors());
app.use(bodyParser.json());

app.post("/geodata", async (req, res) => {
  const { city = "gothenburg" } = req.body;
  const data = await csv()
    .fromFile(path.resolve(__dirname, `data/${city}.csv`))
    .then((data) =>
      data.map(({ weight, latitude, longitude }) => ({
        weight: parseInt(weight),
        latitude: parseFloat(latitude),
        longitude: parseFloat(longitude),
      }))
    );

  return res.send(data);
});

app.post("/report", async (req, res) => {
  console.log(req.body);
  res.sendStatus(200);
});

app.listen(port, () =>
  console.log(`Forward server listening at http://localhost:${port}`)
);
