const axios = require("axios");
const qs = require("qs");
const { dateAndTime } = require("./utils");

const trafficPublicEndPoint = "https://api.vasttrafik.se/bin/rest.exe/v2";

// Generate oAuth2 headers
const headers = (token) => {
  return {
    Authorization: `Bearer ${token}`,
  };
};

// This app is built to work with json
const format = "json";
const defaults = {
  useVas: 0,
  useLDTrain: 0,
  useRegTrain: 0,
  excludeDR: 0,
};

const tokenEndPoint = "https://api.vasttrafik.se:443/token";

const getToken = (key) => {
  return axios
    .post(
      tokenEndPoint,
      qs.stringify({
        grant_type: "client_credentials",
        scope: "forwarding_server",
      }),
      {
        headers: {
          Authorization: `Basic ${key}`,
          "Access-Control-Allow-Origin": "*",
          "Content-Type": "application/x-www-form-urlencoded;",
        },
      }
    )
    .then((res) => {
      return {
        expiry: new Date().getTime() + res.data.expires_in * 1000,
        ...res.data,
      };
    });
};

// Get System Info
const getSystemInfo = (token) => {
  return axios
    .get(`${trafficPublicEndPoint}/systeminfo`, {
      headers: headers(token),
      params: {
        format,
      },
    })
    .then((res) => {
      const {
        TimetableInfo: { TimeTableData, TimeTablePeriod },
      } = res.data.SystemInfo;

      return {
        creationDate: TimeTableData.CreationDate.$,
        dateBegin: TimeTablePeriod.DateBegin.$,
        dateEnd: TimeTablePeriod.DateEnd.$,
      };
    });
};

// Get stop in the nearby location by lat lng
const getNearbyStops = (token, originCoordLat, originCoordLong) => {
  return axios
    .get(`${trafficPublicEndPoint}/location.nearbystops`, {
      headers: headers(token),
      params: { format, originCoordLat, originCoordLong },
    })
    .then((res) => {
      const {
        LocationList: { serverdate, servertime, StopLocation },
      } = res.data;
      return {
        serverdate,
        servertime,
        nearbyStopLocations: StopLocation,
      };
    });
};

// Get stop in the nearby location by address
const getNearbyAddress = (token, originCoordLat, originCoordLong) => {
  return axios
    .get(`${trafficPublicEndPoint}/location.nearbyaddress`, {
      headers: headers(token),
      params: { format, originCoordLat, originCoordLong },
    })
    .then((res) => {
      const {
        LocationList: { serverdate, servertime, StopLocation },
      } = res.data;
      return {
        serverdate,
        servertime,
        StopLocation,
      };
    });
};

// Get stop in the nearby location by an input query
const searchStops = (token, input) => {
  return axios
    .get(`${trafficPublicEndPoint}/location.name`, {
      headers: headers(token),
      params: { format, input },
    })
    .then((res) => {
      const {
        LocationList: { serverdate, servertime, StopLocation },
      } = res.data;
      return {
        serverdate,
        servertime,
        StopLocation,
      };
    });
};

/** Get departureBoard
 * @param date YYYY-MM-DD
 * @param time is in HH:MM
 */
const getDepartureBoard = (token, id) => {
  const { date, time } = dateAndTime();

  return axios
    .get(`${trafficPublicEndPoint}/departureBoard`, {
      headers: headers(token),
      params: { format, id, date, time, ...defaults },
    })
    .then(
      ({
        data: {
          DepartureBoard: { noNamespaceSchemaLocation: omit, ...rest },
        },
      }) => ({ ...rest })
    );
};

/** Get arrivalBoard
 * @param date YYYY-MM-DD
 * @param time is in HH:MM
 */
const getArrivalBoard = (token, id) => {
  const { date, time } = dateAndTime();
  return axios
    .get(`${trafficPublicEndPoint}/arrivalBoard`, {
      headers: headers(token),
      params: { format, id, date, time, ...defaults },
    })
    .then(
      ({
        data: {
          ArrivalBoard: { noNamespaceSchemaLocation: omit, ...rest },
        },
      }) => ({ ...rest })
    );
};

/** Get geometry
 * @param ref of shape 594744%2F215496%2F65992%2F165253%2F80%3F
 */
const getGeometry = (token, ref) => {
  return axios
    .get(`${trafficPublicEndPoint}/geometry`, {
      headers: headers(token),
      params: { format },
    })
    .then((res) => res.data);
};

/** Get journey detail
 * @param ref of shape 594744%2F215496%2F65992%2F165253%2F80%3F
 */
const getJourneyDetail = (token, ref) => {
  return axios
    .get(`${trafficPublicEndPoint}/journeyDetail`, {
      headers: headers(token),
      params: { format },
    })
    .then((res) => res.data);
};

/** Get trip
 * @param config of shape
 * {originId, originCoordLat, originCoordLong, originCoordName,
 * destId, destCoordLat, destCoordLong, destCoordName}
 * optionals viaId, date, time, searchForArrival,
 */
const getTrips = (token, config) => {
  return axios
    .get(`${trafficPublicEndPoint}/trip`, {
      headers: headers(token),
      params: { format, ...config, ...defaults },
    })
    .then((res) => res.data);
};

module.exports = {
  getToken,
  getTrips,
  getJourneyDetail,
  getGeometry,
  getArrivalBoard,
  getDepartureBoard,
  searchStops,
  getNearbyAddress,
  getNearbyStops,
  getSystemInfo,
};
