import React, { useEffect, useState } from "react";
import MapView, { PROVIDER_GOOGLE, Marker } from "react-native-maps";
import { StyleSheet, View, Dimensions } from "react-native";
import axios from "axios";
import { useLocation } from "../hooks/useLocation";

export function Map() {
  const [points, setPoints] = useState([]);
  const [location, error] = useLocation({ watch: true });

  useEffect(() => {
    if (!error) {
      // send location to forward-server and from there to our backend
      axios
        .post("http://192.168.0.22:9000/report", {
          data: JSON.stringify({ location }),
        })
        .catch(() => console.log("Failed to report"));
    }
  }, [location]);

  useEffect(() => {
    let source;
    async function loadData() {
      source = axios.CancelToken.source();
      const { data } = await axios
        .post("http://192.168.0.22:9000/geodata", {
          data: JSON.stringify({ city: "gothenburg" }),
          CancelToken: source.token,
        })
        .catch((e) => {
          console.log(e);
          return [];
        });

      setPoints(data);
    }
    loadData();

    return () => {
      if (source.cancel) {
        source.cancel("Request cancelled");
      }
    };
  }, []);

  if (points.length === 0) {
    return null;
  }

  return (
    <View style={styles.container}>
      <MapView
        style={styles.mapStyle}
        provider={PROVIDER_GOOGLE}
        initialRegion={{
          latitude: location?.coords?.latitude ?? 57.7118511,
          longitude: location?.coords?.longitude ?? 11.9699815,
          latitudeDelta: 1,
          longitudeDelta: 1,
        }}
        showsUserLocation={true}
        minZoomLevel={10}
        maxZoomLevel={20}
      >
        <MapView.Heatmap
          points={points}
          opacity={0.65}
          radius={20}
          maxIntensity={100}
          gradientSmoothing={10}
          heatmapMode={"POINTS_DENSITY"}
        />
      </MapView>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: "#fff",
    alignItems: "center",
    justifyContent: "center",
  },
  mapStyle: {
    width: Dimensions.get("window").width,
    height: Dimensions.get("window").height,
  },
});
