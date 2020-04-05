require("dotenv").config();
const path = require("path");
const express = require("express");
const bodyParser = require("body-parser");
const cors = require("cors");
const csv = require("csvtojson");
const { v1 } = require("uuid");
const axios = require("axios");

const { getToken, getSystemInfo, getNearbyStops } = require("./traffic");

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

  return next();
};

app.post("/geodata", async (req, res) => {
  // console.log(req.body);
  console.log("Serving geodata");
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
  console.log("Received report");
  const { location } = req.body;

  if (location) {
    const { coords } = location;

    await axios
      .post(process.env.CITY_LIVE_ENDPOINT, {
        lat: `${coords.latitude}`,
        lng: `${coords.longitude}`,
        id: v1(),
      })
      .catch(console.log);

    const { data } = await axios.post(process.env.CITY_LIVE_ENDPOINT, {
      scope: "all",
    });

    const live = data
      .split("}{")
      .map((sub) =>
        sub.replace("[", "").replace("]", "").replace("{", "").replace("}", "")
      )
      .map((sub) => {
        const [latitude, longitude] = sub.split(":").map((x) => parseFloat(x));
        return { latitude, longitude, weight: 100 };
      });

    return res.send(live);
  }
  return res.send([]);
});

app.get("/trafficHealth", trafficMiddleware, async (req, res) => {
  // console.log(req.traffikToken);
  console.log("Serving traffic health");
  const { access_token } = req.traffikToken;
  const trafficSystemInfo = await getSystemInfo(access_token).catch(() => ({
    error: "Something went wrong...",
  }));
  res.send({ trafficSystemInfo });
});

const cache = {};

app.post("/trafficStopsNearby", trafficMiddleware, async (req, res) => {
  console.log("Serving traffic stops nearby");
  const { access_token } = req.traffikToken;
  const { latitude, longitude } = req.body;

  if (cache[`${latitude}.${longitude}`]) {
    const cached = cache[`${latitude}.${longitude}`];
    return res.send(cached);
  }

  const stops = await getNearbyStops(access_token, latitude, longitude).catch(
    () => ({
      nearbyStopLocations: [],
      error: "Something went wrong...",
    })
  );

  if (!stops.error) {
    cache[`${latitude}.${longitude}`] = stops;
  }
  return res.send(stops);
});

app.listen(port, () => console.log(`Forward server listening at ${port}`));
