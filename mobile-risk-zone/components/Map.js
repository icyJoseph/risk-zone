import React, { useEffect, useState } from "react";
import MapView, { PROVIDER_GOOGLE } from "react-native-maps";
import { StyleSheet, View, Dimensions } from "react-native";
import axios from "axios";

export function Map() {
  const [points, setPoints] = useState([]);
  useEffect(() => {
    async function loadData() {
      const { data } = await axios
        .post("http://192.168.0.22:9000/geodata", {
          data: JSON.stringify({ city: "gothenburg" }),
        })
        .catch((e) => {
          console.log(e);
          return [];
        });
      setPoints(data);
    }
    loadData();
  }, []);

  if (points.length === 0) {
    return null;
  }

  return (
    <View style={styles.container}>
      <MapView
        style={styles.mapStyle}
        provider={PROVIDER_GOOGLE}
        region={{
          latitude: 57.7118511,
          longitude: 11.9699815,
          latitudeDelta: 1,
          longitudeDelta: 1,
        }}
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
