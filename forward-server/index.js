require("dotenv").config();
const path = require("path");
const express = require("express");
const bodyParser = require("body-parser");
const cors = require("cors");
const csv = require("csvtojson");

const { getToken, getSystemInfo } = require("./traffic");

const app = express();
const port = process.env.PORT || 9000;

app.use(cors());
app.use(bodyParser.json());

function Token() {
  return {
    token: null,
    setToken(token) {
      this.token = token;
    },
    getToken() {
      return this.token;
    },
  };
}

const token = new Token();

const trafficMiddleware = async (req, res, next) => {
  const maybeToken = token.getToken();
  if (maybeToken) {
    req.traffikToken = maybeToken;
  } else {
    const newToken = await getToken(process.env.VT_APP);
    token.setToken(newToken);
    req.traffikToken = newToken;
  }
  console.log("x", req.traffikToken);
  return next();
};

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

app.get("/trafficHealth", trafficMiddleware, async (req, res) => {
  console.log(req.traffikToken);
  const info = await getSystemInfo(req.traffikToken.access_token);
  console.log(info);
});

app.listen(port, () => console.log(`Forward server listening at ${port}`));
