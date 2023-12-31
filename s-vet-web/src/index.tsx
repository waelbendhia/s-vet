import React from "react";
import ReactDOM from "react-dom";
import "./index.css";
import App from "./App";
import reportWebVitals from "./reportWebVitals";
import { Provider } from "react-redux";
import store from "./store";
import { IntlProvider } from "react-intl";

ReactDOM.render(
  <Provider store={store}>
    <IntlProvider locale="fr" defaultLocale="fr">
      <React.StrictMode>
        <App />
      </React.StrictMode>
    </IntlProvider>
  </Provider>,
  document.getElementById("root"),
);

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
