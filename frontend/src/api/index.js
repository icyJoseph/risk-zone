import axios from "axios";

const api = axios.create({
  baseURL: "http://localhost:9000",
});

export const trafficHealth = () => api.get("/trafficHealth").then(console.log);
