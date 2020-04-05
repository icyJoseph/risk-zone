import axios from "axios";

const baseURL = "http://192.168.0.22:9000/";

export const baseAPI = axios.create({
  baseURL,
});
