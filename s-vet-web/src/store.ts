import { configureStore } from "@reduxjs/toolkit";
import * as Login from "./Login";
import * as Home from "./Home";
import * as Consultations from "./Consultations";
import * as Pets from "./Pets";
import * as Owners from "./Owners";
import * as Acts from "./Acts";
import * as Settings from "./Settings";
import * as Dashboard from "./Dashboard";
import { createReduxHistoryContext } from "redux-first-history";
import { createBrowserHistory } from "history";

const { createReduxHistory, routerMiddleware, routerReducer } =
  createReduxHistoryContext({
    history: createBrowserHistory(),
  });

const store = configureStore({
  middleware: (getDefaultMiddleware) =>
    getDefaultMiddleware().concat(routerMiddleware),
  reducer: {
    login: Login.reducer,
    home: Home.reducer,
    consultations: Consultations.reducer,
    pets: Pets.reducer,
    owners: Owners.reducer,
    acts: Acts.reducer,
    settings: Settings.reducer,
    dashboard: Dashboard.reducer,
    router: routerReducer,
  },
});

export const history = createReduxHistory(store);

export default store;
