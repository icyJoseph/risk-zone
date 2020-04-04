import React, { useEffect, useState } from "react";
import MapView, { PROVIDER_GOOGLE } from 'react-native-maps';
import { StyleSheet, View, Dimensions } from "react-native";
import { csv } from "d3";

export function Map() {
  const [points, setPoints] = useState([]);
  useEffect(() => {
    async function loadData() {
    const response = await csv('http://192.168.0.21:19001/assets/geodata/gothenburg.csv');
      let data = [];
      let i =0;
      response.forEach(function(d) {
        data[i] = { latitude: Number.parseFloat(d["Lat"]), longitude: Number.parseFloat(d["Long"]), weight: Number.parseFloat(d["Age"])};
        i++;
      });
      setPoints(data);
    }
    loadData();
  },[]);

  return (
    <View style={styles.container}>
      <MapView style={styles.mapStyle} 
        provider={PROVIDER_GOOGLE} 
        region={{
            latitude: 57.7118511,
            longitude: 11.9699815,
            latitudeDelta: 1,
            longitudeDelta: 1
          }}>
        <MapView.Heatmap points={points}
                          opacity={0.65}
                          radius={20}
                          maxIntensity={100}
                          gradientSmoothing={10}
                          heatmapMode={"POINTS_DENSITY"}/>
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
